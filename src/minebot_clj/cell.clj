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
            [minebot-clj.cwidget :refer [defwidget]]
            [clojure.zip :as z]
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
           com.trolltech.qt.gui.QPalette
           (com.trolltech.qt.core QCoreApplication
                                  QUrl
                                  QPoint
                                  QRect
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

(defn myzip [root]
  (z/zipper #(or (vector? %)
                 (seq? %)
                 (map? %))
            seq
            (fn [coll c]
              (cond
               (instance? clojure.lang.MapEntry coll)
               (clojure.lang.MapEntry. (first c) (second c))
               (seq? coll)
               c
               :else
               (into (empty coll) c)))
            root))


(defn zseq [zip]
  (if zip
    (cons zip
          (lazy-seq
           (when-let [right (z/right zip)]
             (zseq right))))))
(defn zchildren [zip]
  (zseq (z/down zip)))



(defprotocol IEnvironment
  (set-form
    [this name form]
    [this name form dep])
  (set-val [this name newval])

  (shake [this name]))

(def core-syms (set (keys (ns-publics 'clojure.core))))


;; Merge <type of thing you want> <resources, handles, state> <old val> <new val>
;; (defprotocol IMergable
;;   (-merge! [obj old-val new-val]
    
;;     ))


;; (defn merge! [obj old new]
;;   (-merge! obj old new))



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
                         (eval '(require '[clojure.zip :as z]))
                         (eval '(import 'com.trolltech.qt.core.QPoint))
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
                                      (.setMaximumHeight 100)
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

(defn draw-shape-dispatch [painter shape-type args]
  shape-type)
(defmulti draw-shape draw-shape-dispatch)


(let [top-size 25
      bottom-size 10
      child-offset 10
      flags 0
      margin 3
      padding 3
      child-right-padding 6]
  (defn scratch-block-height [painter [text & children]]
    (+ top-size
       (apply + (map (partial scratch-block-height painter) (map rest children)))
       (if (seq children)
         bottom-size
         0)))

  (defn scratch-block-width [painter [text & children]]
    (apply max
           (+ (* 2 padding)
              margin
              (.width (.boundingRect painter
                                     (QRect.)
                                     flags
                                     text)))
           (->> children
                (map rest)
                (map (partial scratch-block-width painter))
                (map (partial + child-offset child-right-padding)))))

  (defmethod draw-shape :scratch-block [painter shape-type [text & children :as block]]
    (let [height (scratch-block-height painter block)
          width (scratch-block-width painter block)]
      
      (.drawText painter (QRect. (+ padding (+ padding margin)) margin width height) flags text)
      (draw-shape painter :drawRoundedRect [margin
                                            margin
                                            width
                                            height
                                            5 5])
      (.save painter)
      (.translate painter child-offset top-size)
      (doseq [child children]
        (draw-shape painter :scratch-block (rest child))
        (.translate painter 0 (scratch-block-height painter (rest child))))
      (.restore painter)
      )))

(defprotocol IChildren
  (children [parent])
  (append-child! [parent node])
  (replace-child! [parent old-node new-node]))

(extend-protocol IChildren
  nil
  (children [parent]
    nil)
  (append-child! [parent new-node]
    (.show new-node)
    new-node)
  (replace-child! [parent old-node new-node]
    (.show new-node)
    new-node)

  com.trolltech.qt.gui.QWidget
  (children [parent]
    (if-let [layout (.layout parent)]
      (doall (map #(.widget (.itemAt layout %))
                  (range (.count layout))))
      (filter #(.isWidgetType %) (.children parent))))
  (append-child! [parent new-node]
    (.setParent new-node parent)
    (when (.layout parent)
      (.addWidget (.layout parent) new-node))
    (.show new-node))

  (replace-child! [parent old-node new-node]
    (let [layout (.layout parent)
          index (when layout
                  (.indexOf layout old-node))]
      (.setParent old-node nil)
      (.close old-node)
      (.setParent new-node parent)
      (when layout
        (.insertWidget layout index new-node))
      (.show new-node)
      new-node)))

(defmethod draw-shape :default [painter shape-type args]
  (clojure.lang.Reflector/invokeInstanceMethod
   painter (name shape-type) (to-array args)))

(defn draw-shapes [painter shapes]
  (doseq [shape @shapes]
    (if-not (vector? shape)
      (dorun (map (partial draw-shapes painter) shape))
      (let [[shape-type & args] shape]
        (draw-shape painter shape-type args)))))


(defn QCanvas []
  (let [shapes (atom nil)]
    (proxy [QWidget IShaperDrawer] []
      (shapes []
        @shapes)
      (setShapes [_shapes]
        (reset! shapes _shapes)
        (.repaint this))

      (paintEvent [event]
        (with-painter [painter this]
          (doto painter
            (.setRenderHint com.trolltech.qt.gui.QPainter$RenderHint/Antialiasing))
          (try
            (draw-shapes painter shapes)
            (catch Exception e
              (msg e))))
        nil))))


(definterface IText
  (^String text [])
  (^void setText [^String text]))

(definterface ISelectBlockId
  (^String selectedBlockId [])
  (^void setSelectedBlockId [^String block-id]))

(defprotocol IInit
  (init [block]))

(definterface ISubblocks
  (subblocks []))

(definterface IBlock)



(defwidget QScratchArea
  [_selectedBlockId]
  [ISubblocks ISelectBlockId]
  [[blockSelected 1]
   [selectedBlockMoved 1]]

  (^Object selectedBlockId [this]
    (. this _selectedBlockId))
  (^void setSelectedBlockId [this ^Object block-id]
    (set! (. this _selectedBlockId)
          (if (= "" block-id)
            nil
            block-id)))

  (subblocks [this]
    (mapcat #(.subblocks %) (children this)))

  (mouseMoveEvent [this event]
    (when (. this _selectedBlockId)
      (let [mpos (.pos event)]
        (.emit (.selectedBlockMoved this)
               [(.x mpos)
                (.y mpos) ]))))

  (mousePressEvent [this event]
    (try
      (let [subblocks (filter #(.underMouse %) (.subblocks this))
            depth? (fn [node]
                     (loop [node node
                            depth 1]
                       (if (= (.parent node) this)
                         depth
                         (if-let [parent (.parent node)]
                           (recur parent (inc depth))
                           0))))
            node (loop [depth -1
                        [block & rest] subblocks]
                   (when block
                     (let [new-depth (depth? block)]
                       (if (and (>= new-depth depth) (seq rest))
                         (recur new-depth rest)
                         block))))
            pos (.pos event)]
        (when node
         (.emit (.blockSelected this) (.objectName node))
         (.emit (.selectedBlockMoved this) [(- (.x pos) (.x node))
                                            (- (.y pos) (.y node))])))
      (catch Exception e
        (msg (with-out-str (clojure.stacktrace/print-stack-trace e))))))
  (mouseReleaseEvent [this event]
    (.emit (.blockSelected this) nil)
    ;; (reset! selected nil)
    ))

(defwidget QScratchBlock
  [label]
  [IText minebot_clj.cell.IChildren ISubblocks IBlock]
  [[somethingHappened 1]
   [jump 1]]

  (init [this]
    (let [block this
          label (QLabel.)
          layout (QVBoxLayout.)]
      (.setLayout block layout)
      (set! (. this label) label)
      (doto label
        (.setParent block)
        (.show))))


  (childEvent [this event]
    (when  (.added event)
      (.adjustSize this)))
  
  (block? [this]
    true)

  (subblocks [this]
    (let [block? #(instance? IBlock %)]
      (.emit (.somethingHappened this) "hello" )
      (filter block?
              (tree-seq block?
                        #(.children %)
                        this))))

  (children [this]
    (let [layout (.layout this)]
      (doall (map #(.widget (.itemAt layout %))
                  (range 0 (.count layout))))))

  (append_child_BANG_ [this new-node]
    (let [parent this]
      (.setParent new-node parent)
      (when (.layout parent)
        (.addWidget (.layout parent) new-node))))

  (replace_child_BANG_ [this old-node new-node]
    (let [parent this
          layout (.layout parent)
          index (when layout
                  (.indexOf layout old-node))]
      (.setParent old-node nil)
      (.close old-node)
      (.setParent new-node parent)
      (when layout
        (.insertWidget layout index new-node))
      (.show new-node)
      new-node))

  (text [this]
    (.text (.label this)))
  (setText [this _text]
    (.setText (.label this) (str _text))
    (.adjustSize (.label this))))


(defn QVBoxWidget []
  (doto (QWidget.)
    (.setLayout (QVBoxLayout.))))

(defn QHBoxWidget []
  (doto (QWidget.)
    (.setLayout (QHBoxLayout.))))

(declare set-property)
(defn make-node [zui]
  (let [[btag bprops & bchildren :as current] (z/node zui)
        constructor (get (ns-map (find-ns 'minebot-clj.cell)) (symbol (name btag)))
        instance (if (class? constructor)
                   (.newInstance constructor)
                   (constructor))]
    (merge-props instance nil (-> zui z/down z/right))
    (merge-children instance nil (-> zui z/down z/right z/right zseq))
    instance))



(defmulti set-property (fn [node kv old-val] (-> kv z/down z/node)))
(defmethod set-property :pos [node kv old-val]
  (let [[property [x y :as new-val]] (z/node kv)]
    (when (not= new-val old-val)
      (.setProperty node "pos" (QPoint. x y)))))

(defmethod set-property :background-color [node kv old-val]
  (let [[property [r g b :as new-val]] (z/node kv)]
    (when (not= new-val old-val)
      (let [palette (QPalette.)
            color (com.trolltech.qt.gui.QColor. r g b)]
        (.setColor palette com.trolltech.qt.gui.QPalette$ColorRole/Window color)
        (.setAutoFillBackground node true)
        (.setPalette node palette))
    
      )))

(defmethod set-property :size [node kv old-val]
  (let [[property [x y :as size]] (z/node kv)]
    (if (nil? size)
      (.adjustSize node)
      (.setProperty node "size" (QSize. x y)))))


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


(defmethod set-property :default [node kv old-val]
  (let [[property val] (z/node kv)]
    (when (not= val old-val)
      (let [pname (name property)]
        (if (not= -1 (.indexOfProperty node pname))
          (do
            (.setProperty node pname val))
          (let [field (try (-> node class (.getField pname))
                           (catch java.lang.NoSuchFieldException e))]
            (if (and field
                     (.startsWith (.getName (.getType field)) "com.trolltech.qt.QSignalEmitter$Signal"))
              (ui/connect (.get field node) (my-partial1 val (vary-meta (-> kv z/up z/up)
                                                                        assoc :node node)))
              (msg "got unknown property " property)))))))
)

;; (defmethod set-property :on-click [node property f]
;;   (ui/connect (.clicked node) (partial f node)))

;; (defmethod set-property :on-text-changed [node property f]
;;   (ui/connect (.textChanged node) (partial f node)))

;; (defmethod set-property :on-return-pressed [node property f]
;;   (ui/connect (.returnPressed node) (partial f node)))


(defn merge-props [node aprops zbprops]
  (let [bprops (z/node zbprops)]
    (when-let [bad-keys (seq (clojure.set/difference (set (keys aprops))
                                             (set (keys bprops))))]
      (msg "don't know how to handle deleting prop" bad-keys))
    (.disconnect node)
    (doseq [kv (zchildren zbprops)
            :let [k (-> kv z/down z/node)]]
      (set-property node kv (get aprops k)))))

(defn flatten-seqs [x]
  (filter (complement seq?)
          (rest (tree-seq seq? seq (seq x)))))

(declare merge-ui!)
(defn merge-children [parent achildren zbchildren]
  (let [achildren achildren
        child-nodes (children parent)]
    (doseq [i (range (max (count achildren) (count zbchildren)))
            :let [achild (nth achildren i nil)
                  zbchild (nth zbchildren i nil)
                  node (nth child-nodes i nil)
                  new-node (merge-ui! node
                                      achild zbchild)]]
      (when (nil? node)
        (append-child! parent new-node)))))

(defn normalize-ui [ui]
  (cond
   (nil? ui)
   nil
   (map? (second ui))
   (apply vector (first ui) (second ui) (flatten-seqs (nthrest ui 2)))
   :else
   (apply vector (first ui) {} (flatten-seqs (rest ui)))))


(defn merge-ui! [node a zb]
  (let [a (normalize-ui a)
        zb (and zb (z/edit zb normalize-ui))
        b (and zb (z/node zb))
        [atag aprops & achildren] a
        [btag bprops & bchildren] b
        new-node
        (cond
         (nil? node)
         (do
           (doto (make-node zb)
             (.show)))

         (nil? b)
         (do
           (when node
             (doto node
               (.setParent nil)
               (.close))))

         (not= atag btag)
         (do
           (let [new-node (make-node zb)
                 parent (.parent node)]
             (replace-child! parent node new-node)
             new-node))

         :else
         (do
           (merge-props node aprops (and zb (-> zb z/down z/right)))
           (merge-children node achildren (and zb (-> zb z/down z/right z/right zseq)))
           node))]
    ;; (.adjustSize new-node)
    new-node))

;; Merge <type of thing you want> <resources, handles, state> <old val> <new val>
;; (extend-type QWidget
;;   IMergable
;;   (-merge! [obj old new]
;;     (merge-ui! obj old new)))

(def uis (atom {}))

(defn show-ui
  ([ui]
     (show-ui :default ui))
  ([key ui]
     (let [work (fn []
                  (try
                    (let [[node old] (get @uis key)
                          new-node (merge-ui! node
                                              old (myzip ui))
                          ]
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
               (set-val 'set-val (fn
                                   ([k v]
                                      (swap! env-ref set-val k v))
                                   ([kvs]
                                      (swap! env-ref
                                             (fn [env]
                                               (reduce (fn [env [k v]]
                                                         (set-val env k v))
                                                       env
                                                       kvs))))))
               (set-val '>!! >!!)))))



(def env2 (atom (Environment. {} {} {})))

(swap! env2 set-val 'text "")
(swap! env2 set-val 'env-vals (fn []
                                (:vals @env2)))
(swap! env2 set-val 'set-val
       (fn
         ([k v]
            (swap! env2 set-val k v))
         ([kvs]
            (swap! env2
                   (fn [env]
                     (reduce (fn [env [k v]]
                               (set-val env k v))
                             env
                             kvs))))))
(swap! env2 set-val 'truncates (fn [s n]
                                 (subs s 0 (min n (count s)))))
(swap! env2 set-val 'find-child (fn [ref name]
                                  (.findChild (.window ref) nil name)))
(swap! env2 set-val 'msg msg)
(swap! env2 set-val 'set-form #(swap! env2 set-form %1 %2))
(swap! env2 set-val 'shake (fn [name]
                             (swap! env2 shake name)))

(defmacro defenv2 [sym body]
  `(swap! env2 set-form (quote ~sym) (quote ~body)))

(defenv2 current-num 0)
(defenv2 current-op nil)
(defenv2 next-num nil)
(swap! env2
       (fn [env2]
         (-> env2
             (set-val 'show-ui show-ui)
             (set-form 'ui '[:QVBoxLayout
                             [:QLabel {:text (if next-num
                                               (str next-num)
                                               (str current-num))}]
                             [:QLabel {:text (str "next " (pr-str next-num))}]
                             [:QLabel {:text (str "current " (pr-str current-num))}]
                             [:QLabel {:text (str "op " (pr-str current-op))}]
                             [:QHBoxLayout
                              (for [i (range 10)]
                                [:QPushButton {:text (str i)
                                               :clicked (fn []
                                                          (if current-op
                                                            (set-val 'next-num (+ i (* 10 next-num)))
                                                            (set-val 'current-num (+ i (* 10 current-num)))))}])]
                             [:QHBoxLayout
                              (for [op [#'+ #'-]]
                                [:QPushButton {:text (-> op meta :name name)
                                               :clicked
                                               (fn []
                                                 (set-val {'next-num 0
                                                           'current-op op})
                                                 )}])]
                             [:QHBoxLayout
                              [:QPushButton {:text "="
                                             :clicked
                                             (fn []
                                               (when (and next-num
                                                          current-op)
                                                 (set-val 'current-num
                                                          (current-op current-num next-num))
                                                 (set-val 'next-num nil)
                                                 (set-val 'current-op nil)))}]]
                             [:QPushButton {:text "Clear"
                                            :clicked
                                            (fn []
                                              (set-val {'current-num 0
                                                        'next-num nil
                                                        'current-op nil}))}
                              ]])
             (set-form 'make-ui! '(show-ui :145 ui)))))



(def scratch-env (atom (Environment. {} {} {})))
(defmacro with-scratch [sym body]
  `(swap! scratch-env set-form (quote ~sym) (quote ~body)))

(swap! scratch-env set-val 'env-vals (fn []
                                (:vals @scratch-env)))
(swap! scratch-env set-val 'set-val #(swap!
                               scratch-env set-val %1 %2))
(swap! scratch-env set-val 'find-child (fn [ref name]
                                  (.findChild (.window ref) nil name)))
(swap! scratch-env set-val 'msg msg)
(swap! scratch-env set-val 'set-form #(swap! scratch-env set-form %1 %2))
(swap! scratch-env set-val 'shake (fn [name]
                             (swap! scratch-env shake name)))


(swap! scratch-env set-val 'show-ui show-ui)
(swap! scratch-env set-form 'make-ui! '(apply show-ui ui))
(swap! scratch-env set-val 'normalize-ui normalize-ui)
(swap! scratch-env set-val 'myzip myzip)
(swap! scratch-env set-val 'zseq zseq)



(swap! scratch-env set-val 'blocks
       [[:QScratchBlock {:text "alpha"
                         :objectName "alpha"
                         :pos [100 100]
                         :background-color [255 255 255]}]
        [:QScratchBlock {:text "beta"
                         :objectName "beta"
                         :pos [100 210]
                         :background-color [0 255 255]}
         [:QScratchBlock {:text "charlie"
                          :objectName "charlie"
                          :pos [100 210]
                          :background-color [0 0 255]}]
         [:QScratchBlock {:text "dingo"
                          :objectName "dingo"
                          :pos [100 210]
                          :background-color [0 255 0]}]]])
(swap! scratch-env set-val 'selected-block-id nil)


(swap! scratch-env set-form 'ui
       '[:scratch2378998lksolllllllklllllllll
         [:QVBoxWidget {:size [ 400 800]
                        :pos [750 20]
                        }
                                   
          [:QScratchArea {:background-color [0 0 100]
                          :selectedBlockId (if selected-block-id
                                             selected-block-id
                                             "")
                          :blockSelected
                          (fn [area block-id]

                            (set-val 'selected-block-id block-id))
                          :selectedBlockMoved
                          (fn [area [x y]]
                            (try
                              (let [blocks (myzip
                                            blocks)
                                    find-block (fn find-block [node selected-block-id]
                                                 (when node
                                                   (let [[tag props & children :as b] (normalize-ui (z/node node))]
                                                     (if (= selected-block-id (:objectName props))
                                                       node
                                                       (or (some #(find-block % selected-block-id) (-> node z/down z/right z/right zseq))
                                                           (find-block (z/right node) selected-block-id))))))
                                    zblock (find-block (z/down blocks) selected-block-id)]
                                (when zblock
                                  (let [block (-> (z/node zblock)
                                                  (assoc-in [1 :pos] [x y]))
                                        blocks (-> zblock z/remove
                                                   z/root
                                                   myzip)
                                        area-node (-> area meta :node)
                                        gp (.mapToGlobal area-node (QPoint. x y))
                                        new-parent-node (first
                                                         (filter #(and (not= (.objectName %) selected-block-id)
                                                                       (let [local (.mapFromGlobal % gp)
                                                                             lx (.x local)
                                                                             ly (.y local)
                                                                             width (.width %)
                                                                             height (.height %)]
                                                                         (and (pos? lx)
                                                                              (pos? ly)
                                                                              (< lx width)
                                                                              (< ly height)))
                                                                       (not (.isAncestorOf %
                                                                                           (.findChild area-node
                                                                                                       nil
                                                                                                       selected-block-id)))
)
                                                                 (.subblocks area-node)))
                                        zparent (when (and new-parent-node
                                                           (.objectName new-parent-node))
                                                  (find-block (z/down blocks) (.objectName new-parent-node)))
                                        blocks (cond
                                                zparent
                                                (-> zparent
                                                     (z/edit normalize-ui)
                                                     z/down
                                                     z/right
                                                     (z/insert-right block)
                                                     z/root)
                                                (seq (z/node blocks))
                                                (-> blocks
                                                     z/down
                                                     (z/insert-left block)
                                                     z/root)
                                                :else
                                                [block])]
                                    (set-val 'blocks
                                             blocks))))
                              (catch Exception e
                                (msg e)))
                            )}
           (for [block blocks]
             block)]]])

(swap! scratch-env set-form 'debug-ui
       '(show-ui :debug-ui
                 [:QVBoxWidget {:size nil}
                  [:QTextEdit {:size nil
                               :plainText
                               (with-out-str
                                 (clojure.pprint/pprint
                                  blocks))}]]))



(defn repaint-canvas []
  (ui/qt
   (try
     (let [canvas (.findChild (.window (first (:scratch @uis))) nil "canvas")]
       (.repaint canvas))
     (catch Exception e
       (msg e)))))

(ui/qt
 (try
   (let [widget (.window (first (:lost-children800 @uis)))]
       (msg (seq (.children widget)))
       #_(msg (count (filter #(instance? IBlock %) (.children start)))))
     (catch Exception e
       (msg e)))
 )





