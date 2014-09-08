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
                                 QWidget)
           (com.trolltech.qt.core QCoreApplication)))

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
  (set-cell
    [this name]
    [this name form]
    [this name form dep])

  (shake [this name]))

(def core-syms (set (keys (ns-publics 'clojure.core))))





;;(defrecord Environment [refs forms deps])
"vals are {refs to vals}
   forms are {refs to forms}
   deps are {refs to refs}
"
(defrecord Environment [vals forms deps]

  IEnvironment
  (set-cell [this name]
    (set-cell this name nil))
  (set-cell [this name form]
    (set-cell this name form nil))
  (set-cell [this name form dep]
    (let [forms (assoc forms name form)

          deps (if dep
                (assoc deps name (set dep))
                (assoc deps name (->> (cell-deps name form)
                                      (remove core-syms)
                                      set)))]
      (shake (Environment. vals forms deps) name)))
  (shake [this name]
    (let [oldval (get vals name)
          form (get forms name)
          args (->> (cell-deps name form)
                    (remove core-syms))]
      (msg "args " args)
      (if (not (every? #(or (contains? vals %)) args))
        this
        (let [nsym (gensym)
              _ (msg "preparing eval")
              to-eval `(let [~@(->> args
                                    (mapcat
                                     (fn [arg]
                                       [arg (symbol (str nsym) (str arg))])))]
                         ~form)
              _ (msg "evaling " name (pr-str to-eval))
              newval (try
                       (let [ns (create-ns nsym)]
                         (doseq [arg args]
                           (intern ns arg (get vals arg)))
                         (eval to-eval))
                       (catch Exception e
                         (msg "error evaling!" e)
                         (throw e))
                       (finally
                         (remove-ns nsym)))
              _ (msg "eval done.")]
          (if (not= oldval newval)
            (let [env (Environment. (assoc vals name newval)
                                    forms
                                    deps)]
              (reduce-kv (fn [env other-name other-deps]
                           (if (contains? other-deps name)
                             (shake env other-name)
                             env))
                         env
                         deps))
            this))))

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



(def width 6)
(def height 6)



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
(swap! env
       (fn [env]
         (assoc-in env [:vals 'fetch] fetch)))

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
                            (let [s (pr-str (get-in @env [:vals cell-name]))
                                  s (if (> (.length s) 30)
                                      (str (subs s 0 30) "...")
                                      s)]
                              (.setText value s))
                            (.setText value ""))))]
                  (doseq [i (range width) 
                        j (range height)
                        :let [cell-widget (QWidget.)
                              cell-layout (QHBoxLayout.)
                              text (QLineEdit. "")
                              ;; &#8634; 
                              name (QLineEdit. "")
                              cell-name (symbol (str "$" i "$" j))
                              value (QLabel. "")
                              refresh (doto (QPushButton. "↺")
                                        (.setContentsMargins 0 0 0 0)
                                        (.setFlat true))
                              ]]
                    (.setFixedWidth refresh 20)
                    (doto cell-layout
                      (.addWidget name)
                      (.setStretch 0 2)

                      (.addWidget text)
                      (.setStretch 1 4)

                      (.addWidget value)
                      (.setStretch 2 2)

                      (.addWidget refresh)
                      (.setStretch 3 1)

                      (.setSpacing 0)
                      (.setContentsMargins 0 0 0 0))
                    (.setContentsMargins cell-widget 0 0 0 0)
                    (.setContentsMargins value 10 0 10 0)

                    (.setLayout cell-widget cell-layout)

                    (connect (.returnPressed name)
                             (fn []
                               (swap! aliases assoc (symbol (.text name)) cell-name)
                               (msg "aliases " aliases)))

                    (connect (.returnPressed text)
                             (fn []
                               (try
                                 (if (pos? (count (.text text)))
                                   (do
                                     (try
                                       (swap! env
                                              (fn [env]
                                                (try
                                                  (let [form (clojure.walk/postwalk-replace
                                                              @aliases
                                                              (read-string (.text text)))
                                                        _ (msg "set-cell " cell-name form)
                                                        new-env (set-cell env cell-name form)]
                                                    (msg "new env!")
                                                    new-env)
                                                  (catch Exception e
                                                    (msg "error " e)
                                                    env))))
                                       (msg "env: " @env)
                                       (update-ui)
                                       (catch Exception e
                                         (.setText value (str e)))))
                                   (.setText value ""))
                                 (catch Exception e
                                   (msg "exception " e)))))

                        
                    (swap! cells assoc cell-name [name value text])
                    (.addWidget layout
                                cell-widget
                                i j))                  
                  

                  (.setVerticalSpacing layout 0)
                  (.setHorizontalSpacing layout 0)
                  (.setContentsMargins layout 0 0 0 0)
                  (.setLayout main layout)
                  (.setWidget scrollarea main)
                  (connect (.aboutToQuit @app)
                           (fn []
                             (msg "quit")))
                  (connect (.focusChanged @app)
                           (fn [old new]
                             (doseq [[cell-name [name value text]] @cells
                                     :when (= name old)]
                               (swap! aliases assoc (symbol (.text name)) cell-name))

                             #_(msg "focus changed " old new)))
                  (.setLayout dialog (doto (QVBoxLayout.)
                                       (.addWidget scrollarea)))
                  (.show dialog)
                  (msg "about to exec")
                  (.exec @app)
                  (reset! app nil))
                
                (catch Exception e
                  (msg e))))))







