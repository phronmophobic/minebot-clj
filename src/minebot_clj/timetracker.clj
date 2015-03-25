(ns minebot-clj.timetracker
  (:require [minebot-clj.penumbra :as pen
             :refer [IComponent
                     defcomponent
                     IBounds
                     IMouseDown
                     IDraw
                     rectangle
                     draw
                     center
                     label
                     find-offset
                     vertical-layout
                     horizontal-layout
                     bounds
                     make-cid
                     spacer
                     button
                     move
                     key-handler
                     scroll-area
                     text-input
                     start
                     ]]
            [minebot-clj.environment :as env :refer [defr r! rv! ru! r?]])
  (:require [clj-pdf.core :as pdf])
  (:require [environ.core :refer [env]])
  (:require [taoensso.faraday :as far]))



(defmacro cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
  one at a time. If a test returns logical true, cond-let evaluates and
  returns expr with binding-form bound to the value of test and doesn't
  evaluate any of the other tests or exprs. To provide a default value
  either provide a literal that evaluates to logical true and is
  binding-compatible with binding-form, or use :else as the test and don't
  refer to any parts of binding-form in the expr. (cond-let binding-form)
  returns nil."
  [bindings & clauses]
  (let [binding (first bindings)]
    (when-let [[test expr & more] clauses]
      (if (= test :else)
        expr
        `(if-let [~binding ~test]
           ~expr
           (cond-let ~bindings ~@more))))))

(def client-opts
  {:access-key (env :aws-access-key)
   :secret-key (env :aws-secret-key)

   ;; This line below is the only line that is added for DynamoDB Local.
   ;; Remove it (and add your IAM keys above) to run your code in production.
;;   :endpoint "http://localhost:8000"
   })

(def date-format (java.text.SimpleDateFormat. "yyyy-MM-dd"))

(defn dt->str [dt]
  (.format date-format  (.getTime dt)))

(defn str->dt [s]
  (doto (java.util.GregorianCalendar.)
    (.setTime (.parse date-format s))))

(comment
  (far/create-table client-opts :work
                    [:dt :s] ; Primary key named "id", (:n => number type)
                    {:throughput {:read 1 :write 1} ; Read & write capacity (units/sec)
                     :block? true ; Block thread during table creation
                     })
  (far/list-tables client-opts)
 (far/describe-table client-opts :work)
 (far/put-item client-opts
               :work {:dt (dt->num [3 2015])
                      :foorbar "hi there"
                      :times (far/freeze ["hello"])})

 (far/get-item client-opts :work {:dt (dt->num [3 2015])})
 (far/query client-opts :work  {:month [:gt (dt->num [2 2015])]}))



(defcomponent CalendarButton [num bounds on-click]
  IBounds
  (-bounds [_]
    bounds)
  IMouseDown
  (-mouse-down [this [mx my]]
    (when on-click
      (let [[gx gy] (find-offset this)
            [w h] bounds]
        (when (and mx my
                   (>= mx gx)
                   (>= my gy)
                   (< mx (+ gx w))
                   (< my (+ gy h)))
          (on-click)))))

  IDraw
  (draw [this state]
    
    (let [[w h] bounds
          border (rectangle w h)
          {:keys [mx my]}  state
          [gx gy _] pen/*origin*]
      (when (and mx my
                 (>= mx gx)
                 (>= my gy)
                 (< mx (+ gx w))
                 (< my (+ gy h)))
        (penumbra.opengl/color [1 0 1]))
      (draw
       [border
        (center (label (str num)) [w h])]
       state))))


(defn calendar [gcal on-click]
  (let [width 301
        header-border (rectangle width 30)
        days-in-month (.getActualMaximum gcal (java.util.Calendar/DAY_OF_MONTH))
        month-str (-> (java.text.SimpleDateFormat. "MMMM")
                      (.format (.getTime gcal)))
        first-day (doto (.clone gcal)
                    (.set java.util.Calendar/DAY_OF_MONTH 1))
        first-day-offset (dec (.get first-day java.util.Calendar/DAY_OF_WEEK))]
    (vertical-layout
     [header-border
      (center (label month-str) (bounds header-border))]
     (apply
      vertical-layout
      (for [i (range 6)]
        (apply
         horizontal-layout
         (for [j (range 7)
               :let [num (inc
                          (- (+ (* 7 i) j)
                             first-day-offset))]]
           (if (and (>= num 1)
                    (<= num days-in-month))
             (->CalendarButton (make-cid "calendar_button") num
                               [42 42]
                               (fn []
                                 (when on-click
                                   (on-click num)))
                               )
             (spacer 42 42))))))

     ))
  )





(defr time-text "")
(defr window-height 800)
(defr window-width 600)


(defn parse-duration [ts]
  (let [num (read-string (clojure.string/replace ts #"[^0-9.]" ""))]
    (if (or (.endsWith ts "m")
            (.endsWith ts "min")
            (.endsWith ts "minute")
            (.endsWith ts "minutes"))
      [0 num]
      [num 0])))

(defn parse-time [ts]
  (let [num-str (clojure.string/replace ts #"[^0-9]" "")
        num-str-len (count num-str)
        [hour min] (if (<= num-str-len 2)
                     [(read-string num-str) 0]
                     (let [hour-str (subs num-str 0 (- num-str-len 2))
                           min-str (subs num-str (- num-str-len 2) num-str-len)]
                       [(read-string hour-str)
                        (read-string min-str)]))
        time (cond
              (and (.endsWith ts "pm") (< hour 12))
              [(+ 12 hour) min]
              (.endsWith ts "am")
              [hour min]
              (>= hour 9)
              [hour min]
              :else [(+ 12 hour) min])]
    time))

(defn parse-phrase [ts]
  (let [ts-trim (.toLowerCase (clojure.string/replace ts #"\s" ""))
        type-str (first
                  (keep (fn [s]
                          (when (.endsWith ts-trim s)
                            s))
                        ["start" "stop" "break" "work" "lunch"]))
        type (keyword type-str)
        time-str (if type
                   (subs ts-trim 0 (- (.length ts-trim) (.length type-str)))
                   ts-trim)]
    (case type

      (:start :stop)
      [(parse-time time-str) type]

      (:break :work :lunch)
      [(parse-duration time-str) type]

      nil
      (cond
       (some #(.endsWith time-str %) ["h" "hr" "hour" "hours" "min" "mins" "minutes" "minute"])
       [(parse-duration time-str) :work]

       (or (some #(.endsWith time-str %) ["am" "pm"]))
       (let [[hour min :as time] (parse-time time-str)]
         (if (<= hour 12)
           [time :start]
           [time :stop]))

       :else
       (let [num (try
                   (Integer/parseInt time-str)
                   (catch NumberFormatException e
                     (try
                       (Double/parseDouble time-str)
                       (catch NumberFormatException e))))]
         (cond
          (float? num)
          [(parse-duration time-str) :work]

          (or (>= num 3))
          (let [[hour min :as time] (parse-time time-str)]
            (if (<= hour 12)
              [time :start]
              [time :stop]))

          :else
          [(parse-duration time-str) :work]))))))


;; need to make rv! actually work! with shaking.
(defr current-date (java.util.GregorianCalendar.))



(defr times (or (:times current-db-item)
                []))

(defr work-description "")

(defr current-db-item
  (let [db-item 
        (far/get-item client-opts :work {:dt (dt->str current-date)})]
    (rv! work-description
         (or (:work-description db-item)
             ""))
    db-item))

(defn save! [dt times work-description]
  (far/put-item client-opts
                :work
                (merge {:dt (dt->str dt)
                        :times (far/freeze times)}
                       (when (pos? (.length work-description))
                         {:work-description work-description})))
  (env/shake! @(env/get-or-create-renv) 'current-db-item))

(defn add-time! []
  (let [time-text @time-text
         times @times
        dt @current-date
        work-description @work-description
         ]
     (when (pos? (.length time-text))
       (when-let [new-time (try
                             (parse-phrase time-text)
                             (catch Exception e
                               (println "could not parse " time-text e)))]
         (save! dt
                (conj times new-time)
                work-description)
         (rv! time-text "")))))

(defn time-str [[dt type]]
  (if (#{:start :stop} type)
    (let [[hour min] dt
          am? (<= hour 12)
          normal-hour (if (not am?)
                        (- hour 12)
                        hour)
          time-str (str normal-hour ":" (format "%02d" min) (if am? "am" "pm"))]
      (str time-str " " (name type)))
    (let [[hour min] dt]
      (str (when (pos? hour)
             (str hour " hour "))
           (when (pos? min)
             (str min " min "))
           (name type)))))

(defr scroll-offset-y 0)

(defr month-offset 0)

(defr cal-stuff
  [
   (move 290 75
         (button "<"
                 (fn []
                   (ru! month-offset dec)))
         )
   (move 290 75
         (center 
          (button "today"
                  (fn []
                    (rv! month-offset 0)))
          [301 45]))
   (move 556 75
         (button ">"
                 (fn []
                   (ru! month-offset inc)))
         )
   (move 290 120 (calendar (doto (java.util.GregorianCalendar.)
                             (.add  java.util.Calendar/MONTH month-offset))
                           (fn [day]
                             (rv! current-date (doto (.clone @current-date)
                                                 (.set java.util.Calendar/MONTH
                                                        (.get (doto (java.util.GregorianCalendar.)
                                                                (.add  java.util.Calendar/MONTH month-offset))
                                                              java.util.Calendar/MONTH))
                                                  (.set java.util.Calendar/DAY_OF_MONTH day)))
                             )))])

(defr mytest current-date)
(def a 1)





(defn check-start-stop-format [times]
  (let [start-stops (filter (fn [[dt type]]
                              (#{:start :stop} type))
                            times)

        assertions
        [["should be an even number of starts and stops"
          (fn []
            (even? (count start-stops)))]
         ["starts and stops should be in pairs"
          (fn []
            (every? (fn [[type1 type2]]
                      (and (= type1 :start)
                           (= type2 :stop)))
                    (->> start-stops
                         (map second)
                         (partition 2))))]
         ["starts should come before stops"
          (fn []
            (every? (fn [[t1 t2]]
                      (neg? (compare t1 t2)))
                    (->> start-stops
                         (map first)
                         (partition 2))))]]]
    ;; return first false assertion
    (loop [[[msg pred :as assertion] & more] assertions]
      (when assertion
        (if-not (pred)
          msg
          (recur more))))))

(defn hours-from-intervals [times]
  (->> times
       (remove (fn [[dt type]]
                 (#{:start :stop} type)))
       (reduce (fn [hours [[dhours dmin] type]]
                 (let [delta-hours (+ dhours
                                      (/ (float dmin) 60))
                       delta-hours (if (#{:work} type)
                                     delta-hours
                                     (- delta-hours))]
                   (+ hours delta-hours)))
               0)))

(defn hours-from-spans [times]
  (->> times
       (filter (fn [[dt type]]
                 (#{:start :stop} type)))
       (partition 2)
       (reduce (fn [hours [[[hr1 min1] type] [[hr2 min2] type]]]
                 (let [t1 (+ hr1 (/ (float min1) 60))
                       t2 (+ hr2 (/ (float min2) 60))
                       delta-hours (- t2 t1)]
                   (+ hours delta-hours)))
               0)))

(defn calc-time [times]
  (let [times (sort-by (fn [[t type]]
                         [(not (#{:start :stop} type))
                          t])
                       times)]
    (if-let [error (check-start-stop-format times)]
      (throw (Exception. error))
      (let [hours (+ (hours-from-spans times)
                     (hours-from-intervals times))]
        hours))))

(defn format-money [money]
  (.format (java.text.NumberFormat/getCurrencyInstance (java.util.Locale. "en" "US")) money))

(declare render-invoice-pdf)
(defn make-invoice [dt]
  (let [month (.get dt
                    java.util.Calendar/MONTH)
        days-in-month (.getActualMaximum dt (java.util.Calendar/DAY_OF_MONTH))
        dates-for-month (for [day (range 1 (inc days-in-month))]
                          (doto (.clone dt)
                            (.set java.util.Calendar/DAY_OF_MONTH day)))
        dt-times (for [dt dates-for-month
                       :let [times (far/get-item client-opts :work {:dt (dt->str dt)})]
                    :when times]
                   times)

        format-invoice-date (fn [dt]
                              (let [date-format (java.text.SimpleDateFormat. "MM/dd/yyyy")]
                                (.format date-format  (.getTime dt))))
        work-description (for [{:keys [dt times work-description]} dt-times
                               :let [hours-worked (calc-time times)]
                               :when (and hours-worked (pos? hours-worked))]
                           [(format-invoice-date (str->dt dt))
                            (or work-description "programming")
                            hours-worked])
        invoice-name (str "Invoice "
                          (format-invoice-date
                           (doto (.clone dt)
                             (.set java.util.Calendar/DAY_OF_MONTH 1)))
                          " - "
                          (format-invoice-date
                           (doto (.clone dt)
                             (.set java.util.Calendar/DAY_OF_MONTH days-in-month))))
        filename (str "invoice_"
                      (dt->str (doto (.clone dt)
                                 (.set java.util.Calendar/DAY_OF_MONTH 1)))
                      ".pdf")
        output (clojure.java.io/output-stream
                (apply
                 clojure.java.io/file
                 (conj (-> env :invoice :path)
                       filename)))]
    (render-invoice-pdf
     invoice-name
     (format-invoice-date dt)
     (-> env :invoice :to-address)
     work-description
     60
     output)
    ))

(defr todays-invoice-hours
  (try
    (calc-time times)
    (catch Exception e
      (str e))))



(defr components
  [(rectangle window-width window-height)
   cal-stuff
   (move 10 700
         (label (str "invoice hours " todays-invoice-hours)))
   (move 10 640
         (button "make invoice"
                 (fn []
                   (make-invoice current-date))))
   (move 10 500
         (horizontal-layout
          (label "work description")
          (text-input work-description
                      (fn [s]
                        (if (= s :return)
                          (save! current-date
                                 times
                                 work-description)
                          ((key-handler 'work-description) s))))))
   (move 10 10
         (vertical-layout
     
          (label "Time Tracker 3000" :font-size 45)
          (spacer 0 20)
          (label (str "current  " (dt->str current-date)))
          (spacer 0 10)

          (text-input time-text (fn [s]
                                  (if (= s :return)
                                    (add-time!)
                                    ((key-handler 'time-text) s))))
          (spacer 0 20)
          (scroll-area [400 450] [00 scroll-offset-y]
           (apply
            vertical-layout
            (label "times")
            (for [time (sort-by (fn [[t type]]
                                  [(not (#{:start :stop} type))
                                   t])
                                times)]
              (horizontal-layout
               (button "x"
                       (fn []
                         (save! current-date
                                (remove #{time} times)
                                work-description)))
               (spacer 20 0)
               (label (str (time-str time)
                           " " time) ))))
           (fn [dwheel]
             (ru! scroll-offset-y - (int (/ (float dwheel) 100)))))))])



(defr event-handlers
  (pen/make-event-handlers components))







(defn render-invoice-pdf [invoice-name todays-date to-address work-description rate out]
  (pdf/pdf
   [{:title         invoice-name
     :subject       "Some subject"
     :creator       (-> env :invoice :name)
     :right-margin  50
     :author        (-> env :invoice :name)
     :bottom-margin 40
     :left-margin   40
     :top-margin    40
     :size          "a4"
     ;;     :footer        "page"
     }
    [:heading invoice-name]
    [:phrase [:chunk {:style :bold} "Date: "] todays-date]
    [:spacer]
    (-> env :invoice :name)
    (-> env :invoice :address)

    [:paragraph {:style :bold} "To:"]
    to-address

    [:spacer]
    [:line]
    [:spacer]

    [:paragraph {:style :bold} "Overview of services"]
    (into 
     [:table {:header ["Date" "Description of work" "No. hours" "Amount Billed"]
              :width 100
              :border true
              :cell-border true}]
     (concat
      (for [[dt description hours] work-description]
        [dt description (str hours) (format-money (* rate hours))])
      (list
       (let [hours-sum (apply +
                              (map #(nth % 2) work-description))]
         [[:cell [:phrase {:style :bold} "Total"]]
          "" (str hours-sum) (format-money (* (-> env :invoice :rate) hours-sum))]))
      )
     
     )
    ]
   out))



