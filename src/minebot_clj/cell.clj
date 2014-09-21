(ns minebot-clj.cell
  (:require 
            [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        ] :as async]
            [clojure.stacktrace]
            [minebot-clj.astar :refer [astar manhattan-distance euclidean-distance]]
            [minebot-clj.model :as model]
            [minebot-clj.ui :as ui :refer [connect]]
            [minebot-clj.analyze :refer [cell-deps]]
            clj-http.client
            net.cgrand.enlive-html
            )
  (:import (com.trolltech.qt.gui QApplication QPushButton
                                 ;; Checkbox with a text label
                                 QCheckBox
                                 ;; Combined button and popup list
                                 QComboBox
                                 ;; Vista style command link button
                                 QCommandLinkButton
                                 ;; Widget for editing dates based on the QDateTimeEdit widget
                                 QDateEdit
                                 ;; Widget for editing dates and times
                                 QDateTimeEdit
                                 ;; Rounded range control (like a speedometer or potentiometer)
                                 QDial
                                 ;; Spin box widget that takes doubles
                                 QDoubleSpinBox
                                 ;; Focus frame which can be outside of a widget's normal paintable area
                                 QFocusFrame
                                 ;; Combobox that lets the user select a font family
                                 QFontComboBox
                                 ;; Displays a number with LCD-like digits
                                 QLCDNumber
                                 ;; Text or image display
                                 QLabel
                                 ;; One-line text editor
                                 QLineEdit
                                 ;; Menu widget for use in menu bars, context menus, and other popup menus
                                 QMenu
                                 ;; Horizontal or vertical progress bar
                                 QProgressBar
                                 ;; Command button
                                 QPushButton
                                 ;; Radio button with a text label
                                 QRadioButton
                                 ;; Scrolling view onto another widget
                                 QScrollArea
                                 ;; Vertical or horizontal scroll bar
                                 QScrollBar
                                 ;; Resize handle for resizing top-level windows
                                 QSizeGrip
                                 ;; Vertical or horizontal slider
                                 QSlider
                                 ;; Spin box widget
                                 QSpinBox
                                 ;; Tab bar, e.g. for use in tabbed dialogs
                                 QTabBar
                                 ;; Stack of tabbed widgets
                                 QTabWidget
                                 ;; Widget for editing times based on the QDateTimeEdit widget
                                 QTimeEdit
                                 ;; Column of tabbed widget items
                                 QToolBox
                                 ;; Quick-access button to commands or options, usually used inside a QToolBar
                                 QToolButton
                                 QMainWindow
                                 QDialog

                                 QVBoxLayout
                                 QHBoxLayout

                                 QGridLayout
                                 QTextEdit
                                 QGroupBox
                                 QWidget

                                 QKeyEvent
                                 QPainter
                                 QPen

                                 )
           clojure.lang.Reflector
           com.trolltech.qt.gui.QPainter$RenderHints
           (com.trolltech.qt.core QCoreApplication
                                  QUrl
                                  QPoint
                                  QSize
                                  QObject
                                  Qt)
           (com.trolltech.qt.webkit QWebView)
           (com.trolltech.qt.phonon VideoPlayer
                                    VideoWidget
                                    MediaSource)
           (com.trolltech.qt QVariant)))

(def messages (atom []))
(def out (atom *out*))
(defn msg [& s]
  (binding [*out* @out]
    (apply println s))
  (swap! messages (fn [xs s]
                    (let [xs (conj xs s)]
                      (if (> (count xs) 30)
                        (subvec xs 1)
                        xs)))
         s))

(defmacro goe [ & body]
  `(go
    (try
      ~@body
      (catch Exception e#
        (binding [*out* @out]
          (msg "error" (with-out-str
                         (clojure.stacktrace/print-stack-trace e#))))))))

(defn seqable?
  "Returns true if (seq x) will succeed, false otherwise."
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x)))


(defprotocol IEnvironment
  (set-form
    [this name form]
    [this name form dep])
  (set-val [this name newval])

  (shake [this name]))

(def core-syms (set (keys (ns-publics 'clojure.core))))





;;(defrecord Environment [refs forms deps])
"vals are {refs to vals}
   forms are {refs to forms}
   deps are {refs to refs}
"
(defrecord Environment [vals forms deps]

  IEnvironment
  (set-form [this name form]
    (set-form this name form nil))
  (set-form [this name form dep]
    (let [forms (assoc forms name form)

          deps (if dep
                (assoc deps name (set dep))
                (assoc deps name (->> (cell-deps name form)
                                      (remove core-syms)
                                      set)))]
      (shake (Environment. vals forms deps) name)))

  (set-val [this name newval]
    (let [has-val? (contains? vals name)
          oldval (get vals name)]
      (if (or (not has-val?) (not= oldval newval))
          (let [env (Environment. (assoc vals name newval)
                                  forms
                                  deps)]
            (reduce-kv (fn [env other-name other-deps]
                         (if (contains? other-deps name)
                           (shake env other-name)
                           env))
                       env
                       deps))
          this)))


  (shake [this name]
    (let [form (get forms name)
          args (->> (cell-deps name form)
                    (remove core-syms))]
      (if (not (every? #(contains? vals %) args))
        (do
          (msg "couldn't find arg " (remove #(contains? vals %) args))
          this)
        (let [nsym (gensym)
              to-eval `(let [~@(->> args
                                    (mapcat
                                     (fn [arg]
                                       [arg (symbol (str nsym) (str arg))])))]
                         ~form)
              newval (try
                       (let [ns (create-ns nsym)]
                         (doseq [arg args]
                           (intern ns arg (get vals arg)))
                         (eval to-eval))
                       (catch Exception e
                         (msg "error evaling!" e)
                         (throw e))
                       (finally
                         (remove-ns nsym)))]
          (set-val this name newval))))

))

;; (defmacro watch [name]
;;   `(add-watch ~name :p
;;               (fn [_# _# _# v#]
;;                 (println ~(str name) v#)))
;;   )





;; (defmacro defval
;;   ([name val]
;;      (-defval name (cell-deps val) val))
;;   ([name deps val]
;;      (-defval name deps val)))

;; (defmacro deffn [name & args]
;;   `(defval ~name (fn ~@args)))



(def width 20)
(def height 1)



(defonce app (atom nil))
(def env (atom (Environment. {} {} {})))
(def aliases (atom {}))

(defn fetch [url]
  (-> (clj-http.client/get (str "http://" url))
      :body
      net.cgrand.enlive-html/html-snippet
      (net.cgrand.enlive-html/select [:title])
      first
      :content
      first))
(def outch (chan (async/sliding-buffer 1)))



(def inchan (chan (async/sliding-buffer 1)))
(go
 (loop []
   (when-let [val (<! inchan)]
     (swap! env
           (fn [env]
             (set-val env 'inval val)))
     (recur))))


(go
 (loop []
   (when-let [val (<! outch)]
     (msg "val: " val)
     (recur))))


(defn start []
  (.execute (.getNonBlockingMainQueueExecutor (com.apple.concurrent.Dispatch/getInstance))
            (fn []

              (try
                (when (not @app)
                  (ui/init))
                (msg "init")                
                (reset! app (QCoreApplication/instance))
                (let [dialog (QDialog.)
                      layout (QGridLayout.)
                      main (QWidget.)
                      scrollarea (QScrollArea.)
                      cells (atom {})
                      update-ui
                      (fn []
                        (doseq [[cell-name [name value text]] @cells]
                          (when-let [n (get aliases cell-name)]
                            (.setText name n))
                          (if (contains? (:vals @env) cell-name)
                            (let [s (pr-str (get-in @env [:vals cell-name]))]
                              (.setText value s))
                            (.setText value ""))))]
                  (doseq [i (range width) 
                        j (range height)
                        :let [;;cell-widget (QWidget.)
;;                              cell-layout (QHBoxLayout.)
                              text (doto (QTextEdit. "")
                                     (.setMinimumWidth 100)
                                     (.setMaximumHeight 100))
                              ;; &#8634; 
                              name (doto (QLineEdit. "")
                                     (.setMinimumWidth 100))
                              cell-name (symbol (str "$" i "$" j))
                              value (doto (QLabel. "")
                                      (.setMaximumWidth 600)
                                      (.setWordWrap true))
                              ]]
                    (doto layout
                      (.addWidget name i 0)
                      (.addWidget text i 1)
                      (.addWidget value i 2)
                      (.setColumnStretch 1 2)
                      (.setColumnStretch 2 2)
                      (.setSpacing 0)
                      (.setContentsMargins 0 0 0 0))
                    ;;(.setContentsMargins cell-widget 0 0 0 0)
                    (.setContentsMargins value 10 0 10 0)

                    #_(.setLayout cell-widget cell-layout)

                    (connect (.returnPressed name)
                             (fn []
                               (swap! aliases assoc (symbol (.text name)) cell-name)))

                    (.installEventFilter text
                                         (proxy [QObject] []
                                           (eventFilter [obj event]
                                             (if (and (instance? QKeyEvent event)
                                                        (#{(.value com.trolltech.qt.core.Qt$Key/Key_Return)
                                                           (.value com.trolltech.qt.core.Qt$Key/Key_Enter)} (.key event))
                                                        (= (.value (.modifiers event))
                                                           (.value com.trolltech.qt.core.Qt$KeyboardModifier/ControlModifier)))

                                               (do
                                                 (try
                                                   (if (pos? (count (.toPlainText text)))
                                                     (do
                                                       (try
                                                         (swap! env
                                                                (fn [env]
                                                                  (try
                                                                    (let [form (clojure.walk/postwalk-replace
                                                                                @aliases
                                                                                (read-string (.toPlainText text)))
                                                                          new-env (set-form env cell-name form)]
                                                                      new-env)
                                                                    (catch Exception e
                                                                      (msg "error " e)
                                                                      env))))
                                                         (catch Exception e
                                                           (.setText value (str e)))))
                                                     (.setText value ""))
                                                   (catch Exception e
                                                     (msg "exception " e)))
                                                 true)
                                               (proxy-super eventFilter obj event))
                                             )))

                        
                    (swap! cells assoc cell-name [name value text]))                  
                  

                  (.setVerticalSpacing layout 0)
                  (.setHorizontalSpacing layout 0)
                  (.setContentsMargins layout 0 0 0 0)
                  (.setLayout main layout)
                  ;; (.setWidget scrollarea main)
                  (add-watch env :update-ui
                             (fn [_ _ _ _]
                               (ui/qt
                                (update-ui))))
                  (connect (.aboutToQuit @app)
                           (fn []
                             (remove-watch env :update-ui)
                             (msg "quit")))
                  (connect (.focusChanged @app)
                           (fn [old new]
                             (doseq [[cell-name [name value text]] @cells
                                     :when (= name old)]
                               (swap! aliases assoc (symbol (.text name)) cell-name))

                             #_(msg "focus changed " old new)))
                  (.setLayout dialog (doto (QVBoxLayout.)
                                       (.addWidget main)
                                       #_(.addWidget (do (def videoplayer (doto (VideoPlayer. com.trolltech.qt.phonon.Phonon$Category/VideoCategory)))
                                                       videoplayer))
                                       #_(.addWidget (doto (QWebView.)
                                                     (.load (QUrl. "http://google.com/"))))))
                  (.show dialog)
                  (msg "about to exec")
                  (.exec @app)
                  (reset! app nil))
                
                (catch Exception e
                  (msg e))))))





(defn test []
  (.execute
   (.getNonBlockingMainQueueExecutor (com.apple.concurrent.Dispatch/getInstance))
   (fn []
     (msg "init")
     (try
       (reset! app (QCoreApplication/instance))
       (when (not @app)
         (ui/init))       
       (reset! app (QCoreApplication/instance))
       (let [dialog (QWidget.)
             label (QLabel. "text" dialog)]
         (connect (.aboutToQuit @app)
                  (fn []))
         (.show dialog)
         (doto (QPushButton. "button" dialog)
           (.show))
         (msg (.children dialog))
         (.exec @app)
         (reset! app nil))
                
       (catch Exception e
         (msg e))))))

(declare merge-props merge-children)

(defmacro with-painter [[painter paint-device] & body]
  `(let [~painter (QPainter.)
         paint-device# ~paint-device]
     (.begin ~painter paint-device#)
     ~@body
     (.end ~painter)))

(definterface IShaperDrawer
  (shapes [])
  (^void setShapes [shapes]))


(defn qcanvas [parent]
  (let [shapes (atom nil)]
    (proxy [QWidget IShaperDrawer] [parent]
      (shapes []
        @shapes)
      (setShapes [_shapes]
        (reset! shapes _shapes))

      (paintEvent [event]
        (with-painter [painter this]
          (doto painter
            (.setRenderHint com.trolltech.qt.gui.QPainter$RenderHint/Antialiasing))
          (try
           (doseq [[shape-method & args] @shapes]
             (clojure.lang.Reflector/invokeInstanceMethod
              painter (name shape-method) (to-array args)))
           (catch Exception e
             (msg "e"))))
        nil))))



;; (show-ui :canvas-test1139fsdf29lkjjdssdkkkkkkkk
;;          [:QWidget {:size [400 400] }
;;           [:QCanvas {:shapes [[:drawRect 0 0 100 100]
;;                               [:drawArc 100 100 50 50 0 (* 16 180)]
;;                               [:drawRect 25 25 20 20]]
;;                      :size [500 500]} ]])

(declare set-property)
(defn make-node [[btag bprops & bchildren :as current] parent index]
  (cond
   (#{:QCanvas} btag)
   (doto (qcanvas parent)
     (.setParent parent)
     (merge-props nil bprops))

   (#{:QVBoxLayout :QHBoxLayout} btag)
   (let [instance (QWidget. parent)
         class-name (str "com.trolltech.qt.gui." (name btag))
         constructor (.getConstructor (Class/forName class-name)
                                      (into-array Class [QWidget]))
         layout (.newInstance constructor
                              (to-array [instance]))]
     (when (and parent (.layout parent))
       (.insertWidget (.layout parent) index instance))
     (merge-props instance nil bprops)
     (merge-children instance nil bchildren)
     instance)

    :else
    (let [class-name (str (if (= btag :QWebView)
                            "com.trolltech.qt.webkit."
                            "com.trolltech.qt.gui.") (name btag))
        
          constructor (.getConstructor (Class/forName class-name)
                                       (into-array Class [QWidget]))
          instance (.newInstance constructor
                                 (to-array [parent]))]

      (merge-props instance nil bprops)
      (merge-children instance nil bchildren)
      (when (and parent (.layout parent))
        (.insertWidget (.layout parent) index instance))


      instance)))



(defmulti set-property (fn [node property value old-val] property))
(defmethod set-property :pos [node property [x y :as new-val] old-val]
  (when (not= new-val old-val)
    (.setProperty node "pos" (QPoint. x y))))

(defmethod set-property :size [node property [x y :as size] old-val]
  (if (nil? size)
    (.adjustSize node)
    (.setProperty node "size" (QSize. x y))))


(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

(defn my-partial1 [f arg1]
  (case (arg-count f)
    0 (fn [] (f))
    1 (fn [] (f arg1))
    2 (fn [arg2] (f arg1 arg2))
    3 (fn [arg2 arg3] (f arg1 arg2 arg3))
    4 (fn [arg2 arg3 arg4] (f arg1 arg2 arg3 arg4))))


(defmethod set-property :default [node property val old-val]
  (when (not= val old-val)
   (let [pname (name property)]
     (if (not= -1 (.indexOfProperty node pname))
       (.setProperty node pname val)
       (let [field (try (-> node class (.getField pname))
                        (catch java.lang.NoSuchFieldException e))]
         (if (and field
                  (.startsWith (.getName (.getType field)) "com.trolltech.qt.QSignalEmitter$Signal"))
           (ui/connect (.get field node) (my-partial1 val node))
           (msg "got unknown property " property))))))
)

;; (defmethod set-property :on-click [node property f]
;;   (ui/connect (.clicked node) (partial f node)))

;; (defmethod set-property :on-text-changed [node property f]
;;   (ui/connect (.textChanged node) (partial f node)))

;; (defmethod set-property :on-return-pressed [node property f]
;;   (ui/connect (.returnPressed node) (partial f node)))

#_(defmethod set-property :x [node property x]
  )

(defn merge-props [node aprops bprops]
  (when (seq (clojure.set/difference (set (keys aprops))
                                 (set (keys bprops))))
    (msg "don't know how to handle deleting prop keys"))
  (.disconnect node)
  (doseq [[k v] bprops]
    (set-property node k v (get aprops k))))

(declare merge-ui)
(defn merge-children [parent achildren bchildren]
  (let [normalize-f (fn [child]
                      (if (vector? child)
                        [child]
                        child))
        achildren (mapcat normalize-f achildren)
        bchildren (mapcat normalize-f bchildren)
        child-nodes (filter #(.isWidgetType %) (.children parent))]
    (doseq [i (range (max (count achildren) (count bchildren)))
            :let [achild (nth achildren i nil)
                  bchild (nth bchildren i nil)
                  node (nth child-nodes i nil)]]
      (merge-ui node parent achild bchild))))

(defn normalize-ui [ui]
  (if (or (nil? ui)
          (map? (second ui)))
    ui
    (apply vector (first ui) {} (rest ui))))

(defn merge-ui [node parent a b]
  (let [a (normalize-ui a)
        b (normalize-ui b)
        [atag aprops & achildren] a
        [btag bprops & bchildren] b
        new-node
        (cond
         (nil? node)
         (do
           (doto (make-node b parent -1)
             (.show)))     

         (nil? b)
         (do
           (when node
             (doto node
               (.setParent nil))))

         (not= atag btag)
         (do
           (let [old-idx (if (and parent (.layout parent))
                           (.indexOf (.layout parent) node)
                           -1)]
             (.setParent node nil)
             (make-node b parent old-idx)))

         :else
         (do
           (merge-props node aprops bprops)
           (merge-children node achildren bchildren)
           node))]
    ;; (.adjustSize new-node)
    new-node))

(def uis (atom {}))

(defn show-ui
  ([ui]
     (show-ui :default ui))
  ([key ui]
     (let [work (fn []
                  (try
                    (let [[node old] (get @uis key)
                          new-node (merge-ui node nil old ui)]
                      (doto new-node
                        (.show))
                      (swap! uis assoc key [new-node ui]))
                    (catch Exception e
                      (msg e)
                      (msg (with-out-str (clojure.stacktrace/print-stack-trace e))))))]
       (if (nil? (QCoreApplication/instance))
         (.execute
          (.getNonBlockingMainQueueExecutor (com.apple.concurrent.Dispatch/getInstance))
          (fn []
            (try
              (ui/init)
              (work)
              (.exec (QCoreApplication/instance))
              (catch Exception e
                (msg e)))))
         (QApplication/invokeLater work))))
  
  )


(barf)

(let [env-ref env]
  (swap! env
         (fn [env]
           (-> env
               (set-val 'fetch fetch)
               (set-val 'qpoint #(QPoint. %1 %2))
               (set-val 'outch outch)
               (set-val 'show-ui show-ui)
               (set-val 'put! put!)
               (set-val 'set-val #(swap!
 env-ref set-val %1 %2))
               (set-val '>!! >!!)))))



(def env2 (atom (Environment. {} {} {})))

(swap! env2 set-val 'text "")
(swap! env2 set-val 'env-vals (fn []
                                (:vals @env2)))
(swap! env2 set-val 'set-val #(swap!
                               env2 set-val %1 %2))
(swap! env2 set-val 'truncates (fn [s n]
                                 (subs s 0 (min n (count s)))))
(swap! env2 set-val 'find-child (fn [ref name]
                                  (.findChild (.window ref) nil name)))
(swap! env2 set-val 'msg msg)
(swap! env2 set-val 'set-form #(swap! env2 set-form %1 %2))
(swap! env2 set-val 'shake (fn [name]
                             (swap! env2 shake name)))
(swap! env2
       (fn [env2]
         (-> env2
             (set-val 'show-ui show-ui)
             (set-form 'ui '[:QVBoxLayout {:pos [720 0]
                                           :size [650 650]}
                             (for [[k v] (env-vals)]
                               [:QHBoxLayout
                                [:QLabel {:text (truncates (str k) 20)
                                             :minimumWidth 200}]
                                [:QLabel {:text (truncates (str v) 100)
                                          :minimumWidth 200}]])
                             [:QHBoxLayout
                              [:QLineEdit {:objectName "new-name"}]
                              [:QLineEdit {:objectName "new-val"}]
                              [:QPushButton {:text "Add"
                                             :clicked (fn [this]
                                                        (msg "...")
                                                        
                                                        (set-form (symbol (.text (find-child this "new-name")))
                                                                  (read-string (.text (find-child this "new-val"))))
                                                        (shake 'ui))}]]])
             (set-form 'make-ui! '(show-ui :11 ui)))))


(def scratch-env (atom (Environment. {} {} {})))

(swap! scratch-env set-val 'text "")
(swap! scratch-env set-val 'env-vals (fn []
                                (:vals @scratch-env)))
(swap! scratch-env set-val 'set-val #(swap!
                               scratch-env set-val %1 %2))
(swap! scratch-env set-val 'truncates (fn [s n]
                                 (subs s 0 (min n (count s)))))
(swap! scratch-env set-val 'find-child (fn [ref name]
                                  (.findChild (.window ref) nil name)))
(swap! scratch-env set-val 'msg msg)
(swap! scratch-env set-val 'set-form #(swap! scratch-env set-form %1 %2))
(swap! scratch-env set-val 'shake (fn [name]
                             (swap! scratch-env shake name)))


(swap! scratch-env set-val 'show-ui show-ui)
(swap! scratch-env set-form 'make-ui! '(show-ui :11 ui))

(swap! scratch-env set-form 'ui '[:QVBoxLayout {:pos [720 0]
                                                :size [650 650]}
                [:QLabel {"text" "BootScratch!"}]])

