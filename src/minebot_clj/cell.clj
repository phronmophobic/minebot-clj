(ns minebot-clj.cell
  (:require 
   [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                               alts! alts!! thread
                               go-loop
                                        ] :as async]
   [clojure.stacktrace]
            [minebot-clj.astar :refer [astar manhattan-distance euclidean-distance]]
            [minebot-clj.model :as model]
            [minebot-clj.ui :as ui :refer [connect]]
            [minebot-clj.analyze :refer [cell-deps]]
            [minebot-clj.environment :as env :refer [set-ref! ]]
            clj-http.client
            [net.cgrand.enlive-html :as enlive]
            [minebot-clj.cwidget :refer [defwidget]]
            [clojure.zip :as z]
            [minebot-clj.environment :as env :refer [set-ref shake! set-form]]
            ;; time tracker
            [hiccup.core :as  hiccup]
            [hiccup.page :as page]
            [hiccup.bootstrap.page]

            [clojure.data.json :as json]

            hiccup.util)
  (:import (org.w3c.tidy Tidy))
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream))
  (:use [minebot-clj.zipper :exclude [zseq] :as zip])
  (:use [minebot-clj.evaluable])
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
           (com.trolltech.qt QVariant))
  (:gen-class))

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



(defprotocol IRezip
  (make-node [node children] "Makes new node from existing node and new children."))

;; (extend-type Object TreeNode
;;              (branch? [node] false)
;;              (make-node [node children] node))

;; (defn tree-zip
;;   "Make a zipper out of a tree."
;;   [root]
;;   (z/zipper branch? node-children make-node root))

(defn myzip [root]
  (z/zipper #(or (vector? %)
                 (seq? %)
                 (map? %))
            seq
            (fn [coll c]
              (cond
               (satisfies? IRezip coll) (make-node coll c)
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



;; (defprotocol IEnvironment
;;   (set-form
;;     [this name form]
;;     [this name form dep])
;;   (set-val [this name newval])

;;   (shake [this name]))

;; (defprotocol IRefEnvironment
;;   (shake! [this name])
;;   (set-ref [this name ref])
;;   (set-form [this name evaluable dep]))

;; (def core-syms (set (keys (ns-publics 'clojure.core))))


;; Merge <type of thing you want> <resources, handles, state> <old val> <new val>
;; (defprotocol IMergable
;;   (-merge! [obj old-val new-val]
    
;;     ))


;; (defn merge! [obj old new]
;;   (-merge! obj old new))



;; (defrecord RefEnvironment [evaluables deps refs]
;;   IRefEnvironment
;;   (set-ref [this name ref]
;;     (RefEnvironment. evaluables
;;                      deps
;;                      (assoc refs name ref)))
;;   (set-form [this name evaluable dep]
;;     (let [evaluables (assoc evaluables name evaluable)
;;           deps (assoc deps name (set dep))]
;;       (RefEnvironment. evaluables
;;                        deps
;;                        refs)))

;;   (shake! [this name]
;;     (let [deps (get deps name)
;;           ]
;;       (if (not (every? #(contains? refs %) deps))
;;         (do
;;           (msg "couldn't find args "
;;                (for [dep deps
;;                      :when (not (contains? refs dep))]
;;                  dep))
;;           nil)
;;         (let [evaluable (get evaluables name)
;;               bindings (into {}
;;                              (for [dep deps
;;                                    :let [val (-> refs (get dep) deref)]]
;;                                [dep val]))
;;               ref (get refs name)
;;               old-val (deref ref)
;;               new-val (evaluate evaluable bindings)]
;;           ;; (msg "updating " name)
;;           (when (not= old-val new-val)
;;             (ref-set ref new-val)
;;             (doseq [[other-name other-deps] (:deps this)
;;                     :when (contains? other-deps name)]
;;               (shake! this other-name)))))))
  
;;   )
;; (defn ref-environment []
;;   (RefEnvironment. {} {} {}))


;;(defrecord Environment [refs forms deps])
"vals are {refs to vals}
   forms are {refs to forms}
   deps are {refs to refs}
"
;; (defrecord Environment [vals forms deps]

;;   IEnvironment
;;   (set-form [this name form]
;;     (set-form this name form nil))
;;   (set-form [this name form dep]
;;     (let [forms (assoc forms name form)

;;           deps (if dep
;;                 (assoc deps name (set dep))
;;                 (assoc deps name (->> (cell-deps name form)
;;                                       (remove core-syms)
;;                                       set)))]
;;       (shake (Environment. vals forms deps) name)))

;;   (set-val [this name newval]
;;     (let [has-val? (contains? vals name)
;;           oldval (get vals name)]
;;       (if (or (not has-val?) (not= oldval newval))
;;           (let [env (Environment. (assoc vals name newval)
;;                                   forms
;;                                   deps)]
;;             (reduce-kv (fn [env other-name other-deps]
;;                          (if (contains? other-deps name)
;;                            (shake env other-name)
;;                            env))
;;                        env
;;                        deps))
;;           this)))


;;   (shake [this name]
;;     (let [form (get forms name)
;;           args (->> (cell-deps name form)
;;                     (remove core-syms))]
;;       (if (not (every? #(contains? vals %) args))
;;         (do
;;           (msg "couldn't find arg " (remove #(contains? vals %) args))
;;           this)
;;         (let [nsym (gensym)
;;               to-eval `(let [~@(->> args
;;                                     (mapcat
;;                                      (fn [arg]
;;                                        [arg (symbol (str nsym) (str arg))])))]
;;                          ~form)
;;               newval (try
;;                        (let [ns (create-ns nsym)]
;;                          (eval '(require '[clojure.zip :as z]))
;;                          (eval '(use '[minebot-clj.zipper :exclude [zseq]]))
;;                          (eval '(import 'com.trolltech.qt.core.QPoint))
;;                          (doseq [arg args]
;;                            (intern ns arg (get vals arg)))
;;                          (eval to-eval))
;;                        (catch Exception e
;;                          (msg "error evaling!" e)
;;                          (throw e))
;;                        (finally
;;                          (remove-ns nsym)))]
;;           (set-val this name newval))))

;; ))

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
;; (def env (atom (Environment. {} {} {})))
(def aliases (atom {}))

(defn fetch [url]
  (-> (clj-http.client/get (str "http://" url))
      :body
      net.cgrand.enlive-html/html-snippet
      (net.cgrand.enlive-html/select [:title])
      first
      :content
      first))


#_(defn start []
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


(declare IDraw draw-calls)
(defn draw-shapes [painter shapes]
  (cond
   (satisfies? IDraw shapes)
   (draw-shapes painter (draw-calls shapes))

   (vector? shapes)
   (let [[shape-type & args] shapes]
     (draw-shape painter shape-type args))

   (seq? shapes)
   (dorun (map (partial draw-shapes painter) shapes))))

(defmacro with-save [painter & body]
  `(try
     (.save ~painter)
     ~@body
     (finally
       (.restore ~painter))))

(defmethod draw-shape :translate [painter shape-type [[x y] & shapes]]
  (with-save painter
    (.translate painter x y)
    (draw-shapes painter shapes)))

(defmethod draw-shape :drawTextBottom [painter _ [text]]
  (let [height (.height (.boundingRect painter
                                       (QRect.)
                                       0
                                       text))]
    (.drawText painter 0 height text)))


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
            (draw-shapes painter @shapes)
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

(definterface IQDraw
  (^Object onDraw [])
  (^void setOnDraw [^Object ondraw])  )

(definterface IQClick
  (^Object onMousePress [])
  (^void setOnMousePress [^Object onpress])

  (^Object onMouseMove [])
  (^void setOnMouseMove [^Object onmove])

  (^Object onMouseRelease [])
  (^void setOnMouseRelease [^Object onrelease]))

(definterface IScratchArea
  (^Object blocks [])
  (^void setBlocks [^Object blocks]))

(definterface IBlock)

(defn v+ [& vs]
  (apply mapv + vs))

(defn v- [& vs]
  (apply mapv - vs))

(defn vmax [& vs]
  (apply mapv max vs))

(defn vmin [& vs]
  (apply mapv min vs))

(defn runion [& rs]
  [(apply vmin (map first rs))
   (apply vmax (map second rs))])

(defn vdist [[x1 y1] [x2 y2]]
  (let [xx (- x2 x1)
        yy (- y2 y1)]
    (Math/sqrt (+ (* xx xx) (* yy yy)))))


(defprotocol ISize
  (-size [this]))

(defn size [obj]
  (if (satisfies? ISize obj)
    (-size obj)
    [0 0]))

(defprotocol IPadding
  (-padding [this]))

(defn padding [obj]
  (if (satisfies? IPadding obj)
    (-padding obj)
    [0 0 0 0]))

(defprotocol IMargin
  (-margin [this]))

(defn margin [obj]
  (if (satisfies? IMargin obj)
    (-margin obj)
    [0 0 0 0]))

(defprotocol IDraw
  (draw [this painter])
  (draw-calls [this]))
(defprotocol IDrawSelf
  (draw-self [this painter]))
;; (defprotocol IBox
;;   (box [this]
;;     ))



(defprotocol IDrawChildren
  (draw-children [this]))

(defn box [obj]
  (let [[w h] (size obj)
        [pl pu pr pd] (padding obj)
        [ml mu mr md] (margin obj)]
    [(+ w pl pr ml mr)
     (+ h pu pd mu md)]))

(defrecord ScratchArea [blocks selected-block pos])

(defn scratch-click [zblock [x y]]
  (when (and (>= x 0)
             (>= y 0))
    (let [block (zpeek zblock)
          [w h] (box block)]
      (when (and (<= x w)
                 (<= y h))
        (let [zchildren (-> zblock
                            (zget :subblocks)
                            seq)
              match (first (filter #(scratch-click %
                                         (v- [x y]
                                             (-> % :pos)))
                         zchildren))]
          (if match
            match
            zblock))))))





(defrecord ScratchText [text]
  IDraw
  (draw-calls [this]
    [:drawTextBottom text])
  (draw [this painter]
    (let [height (.height (.boundingRect painter
                                         (QRect.)
                                         0
                                         text))]
      (.drawText painter 0 height text)))
  ISize
  (-size [this]
    [(* (count text) 10)
     20]))
(defn scratchtext
  ([text]
     (ScratchText. text)))


(defn vertical-layout [elements]
  (loop [calls nil
         ;; pos [10 h]
         pos [0 0]
         [element & elements] elements]
    (if element
      (recur (conj calls
                   [:translate pos
                    element])
             (let [[w h] (box element)]
               (v+ pos [0 h]))
             elements)
      calls)))

(defrecord ScratchGroup [pos blocks]
  ISize
  (-size [this]
    (apply v+
           (map box blocks)))

  IDraw
  (draw-calls [this]
    [:translate pos (vertical-layout (zzseq (zzget this :blocks)))])

  (draw [this painter]))


(defn scratchgroup [pos blocks]
  (ScratchGroup. pos blocks))



(defrecord ScratchBlock [text subblocks]
  ;; IPersistentCollection
  ;; (seq [this]
  ;;   )
  ;; (count [this])
  ;; (cons [this o])
  IRezip
  (make-node [this children]
    (map->ScratchBlock (into {} children)))

  IDrawChildren
  (draw-children [this]
    (let [top (scratchtext text)
            [w h] (box top)]
      (loop [children [top]
             pos [10 h]
             [block & subblocks] subblocks]
        (if block
          (recur (conj children
                       (assoc block :pos pos))
                 (let [[w h] (box block)]
                   (v+ pos [0 h]))
                 subblocks)
          children))))
  IDraw
  (draw-calls [this]
    (let [top (scratchtext text)
          [w h] (box top)]
      (list
       (scratchtext text)
       [:translate [10 h]
        (vertical-layout (zzseq (zzget this :subblocks)))])))

  (draw [this painter]
    (doseq [child (draw-children this)]
      (with-save painter
        (let [[x y] (:pos child)]
          (.translate painter x y)
          (draw child painter)))))

  ISize
  (-size [this]
    (apply v+
           (box (scratchtext text))
           (if (seq subblocks)
             [10 0]
             [0 0])
           (map box subblocks))))

(defn scratchblock
  ([text]
     (scratchblock text []))
  ([text subblocks]
     (ScratchBlock. text subblocks)))

(defn do-draw [painter calls]
  
  )
(defn click-target [zcalls [x y]]
  (let [calls (zpeek zcalls)]
    (cond
     (instance? ScratchBlock calls)
     (let [target calls
           [w h] (box target)]
       (if (and (>= x 0)
                (>= y 0)
                (<= x w)
                (<= y h))
         (or (click-target (draw-calls calls)
                           [x y])
             target)))

     (satisfies? IDraw calls)
     (click-target (draw-calls calls) [x y])

     (vector? calls)
     (let [call calls
           call-type (first call)]
       (if (= :translate call-type)
         (let [[_ dpos & calls] call]
           (click-target calls
                         (v- [x y]
                             dpos)))))

     (seq? calls)
     (first (filter identity (map #(click-target % [x y]) calls))))))

(defwidget QScratchArea
  [#_ _selectedBlockId myblocks]
  ;; [ISubblocks ISelectBlockId IScratchArea]
  [IScratchArea]
  [[mousePressed 1]
   [mouseReleased 1]
   [mouseMoved 1]
   [onDraw 1]]
  ;; [[blockSelected 1]
  ;;  [selectedBlockMoved 1]]

  (^Object selectedBlockId [this]
    (. this _selectedBlockId))
  (^void setSelectedBlockId [this ^Object block-id]
    (set! (. this _selectedBlockId)
          (if (= "" block-id)
            nil
            block-id)))

  (^Object blocks [this]
    (. this myblocks))
  (^void setBlocks [this ^Object _blocks]
    (set! (. this myblocks)
          _blocks)
    (.repaint this))

  (subblocks [this]
    (mapcat #(.subblocks %) (children this)))

  (paintEvent [this event]
    (with-painter [painter this]
      (try
        (.emit (.onDraw this) painter)
        (catch Exception e
          (msg (with-out-str
                 (clojure.stacktrace/print-stack-trace e))))))
        nil)

  (mouseMoveEvent [this event]
    (let [mpos (.pos event)]
      (.emit (.mouseMoved this) [(.x mpos)
                            (.y mpos)])
      (.repaint this)))

  (mousePressEvent [this event]
    (let [mpos (.pos event)]
      (.emit (.mousePressed this) [(.x mpos)
                            (.y mpos) ])))

  (mouseReleaseEvent [this event]
    (let [mpos (.pos event)]
      (.emit (.mouseReleased this) [(.x mpos)
                                    (.y mpos) ]))    


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
    #_(.adjustSize (.label this))))


(defn QVBoxWidget []
  (let [layout (QVBoxLayout.)]
    (.setAlignment layout
                   (com.trolltech.qt.core.Qt$AlignmentFlag/createQFlags
                    (into-array
                     com.trolltech.qt.core.Qt$AlignmentFlag
                     [com.trolltech.qt.core.Qt$AlignmentFlag/AlignTop])))
   (doto (QWidget.)
     (.setLayout layout))))

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
    #_(.setSizePolicy instance (com.trolltech.qt.gui.QSizePolicy. (com.trolltech.qt.gui.QSizePolicy$Policy/Expanding) (com.trolltech.qt.gui.QSizePolicy$Policy/Expanding)))
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

(defmethod set-property :width [node kv old-val]
  (let [[property width] (z/node kv)
        height (.height node)]
    (.setProperty node "size" (QSize. width height))))

(defmethod set-property :height [node kv old-val]
  (let [[property height] (z/node kv)
        width (.width node)]
    (.setProperty node "size" (QSize. width height))))

(defmethod set-property :minumumSize [node kv old-val]
  (let [[property [x y :as size]] (z/node kv)]
    (.setProperty node "minumumSize" (QSize. x y))))

(defmethod set-property :html [node kv old-val]
  (let [[property html] (z/node kv)]
    (.setHtml node html)))

;; (defmethod set-property :bridge [node kv old-val]
;;   (let [[property bridges] (z/node kv)
;;         frame (-> node .page .mainFrame)]
;;     (doseq [[name obj] bridges]
;;       (.addToJavaScriptWindowObject frame name obj))))

(defmethod set-property :bridge [node kv old-val]
  (let [[property bridges] (z/node kv)
        frame (-> node .page .mainFrame)]
    (when (not= bridges old-val)
      (.disconnect frame)
      (ui/connect (.-javaScriptWindowObjectCleared frame)
                 (fn []
                   (ui/qt
                    (doseq [[name obj] bridges]
                      (.addToJavaScriptWindowObject frame name obj)))))
      (doseq [[name obj] bridges]
        (.addToJavaScriptWindowObject frame name obj)))))

(declare merge-ui!)
(defmethod set-property :viewport [node kv old-val]
  ;; (.setWidgetResizable node true)
  ;; (.setVerticalScrollBarPolicy node
  ;;                                com.trolltech.qt.core.Qt$ScrollBarPolicy/ScrollBarAlwaysOn)
  (let [[property viewport-ui] (z/node kv)
        start-node (if old-val
                     (.widget node))
        viewport-node (merge-ui! start-node old-val (myzip viewport-ui))]
    ;;imageLabel->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
    ;; (.setSizePolicy viewport-node
    ;;                 com.trolltech.qt.gui.QSizePolicy$Policy/Ignored
    ;;                 com.trolltech.qt.gui.QSizePolicy$Policy/Ignored)
    ;; (.resize (.viewport node) (QSize.  600 600))
;;    (.setProperty node "size" (QSize.  100 400))

    (when (nil? old-val)
      (.setWidget node viewport-node))))

(defmethod set-property :plugins [node kv old-val]
  (let [[property val] (z/node kv)]
    (.setAttribute (.settings node)
                   com.trolltech.qt.webkit.QWebSettings$WebAttribute/PluginsEnabled
                   val)))



(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

(defn wrap-signal-fn [ns f arg1]
  (case (arg-count f)
    0 (fn [] (try
               (binding [*ns* ns]
                 (f))
               (catch Exception e
                 (msg (with-out-str
                        (clojure.stacktrace/print-stack-trace e))))))
    1 (fn [] (try
               (binding [*ns* ns]
                 (f arg1))
               (catch Exception e
                 (msg (with-out-str
                        (clojure.stacktrace/print-stack-trace e))))))
    2 (fn [arg2] (try
                   (binding [*ns* ns]
                     (f arg1 arg2))
                   (catch Exception e
                     (msg (with-out-str
                            (clojure.stacktrace/print-stack-trace e))))))
    3 (fn [arg2 arg3] (try
                        (binding [*ns* ns]
                          (f arg1 arg2 arg3))
                        (catch Exception e
                          (msg (with-out-str
                                 (clojure.stacktrace/print-stack-trace e))))))
    4 (fn [arg2 arg3 arg4] (try
                             (binding [*ns* ns]
                               (f arg1 arg2 arg3 arg4))
                             (catch Exception e
                               (msg (with-out-str
                                      (clojure.stacktrace/print-stack-trace e))))))))


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
              (ui/connect (.get field node) (wrap-signal-fn *ns* val (vary-meta (-> kv z/up z/up)
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

(defonce uis (atom {}))

(defn update-size* [node]
  (doseq [child (.children node)
          :when (instance? QWidget child)]
    (update-size* child))
  (.adjustSize node))
(defn update-size [node]
    (doseq [child (.children node)
          :when (instance? QWidget child)]
      (update-size* child)))

(defn show-ui
  ([ui]
     (show-ui :default ui))
  ([key ui]
     (let [outer-ns *ns*
           work (fn []
                  (binding [*ns* outer-ns]
                    (try
                      (let [[node old] (get @uis key)
                            new-node (merge-ui! node
                                                old (myzip ui))]
                        (doto new-node
                          #_(update-size)
                          (.show)
                          )
                        (swap! uis assoc key [new-node ui]))
                      (catch Exception e
                        (msg e)
                        (msg (with-out-str (clojure.stacktrace/print-stack-trace e)))))))]
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


(def renv (atom (env/environment)))
(defmacro r! [name form]
  `(do
     (let [form# (quote ~form)
           evaluable# (if (evaluable? form#)
                        form#
                        (->ClojureEvaluable *ns* form#
                                            (into {}
                                                  [~@(for [[k _] &env]
                                                       [(list 'quote k)
                                                        k])])))
           deps# (-> (dependencies evaluable#)
                     (->> (remove (fn [name#]
                                    (when-let [var# (ns-resolve *ns* name#)]
                                      (-> var# meta :reactive? not)))))
                     set 
                     (disj ~name))]
       (swap! renv set-form (quote ~name) evaluable# deps#)
       (dosync
        (shake! (deref renv) (quote ~name)))
       (when (instance? clojure.lang.IDeref ~name)
         (deref ~name)))))
(defmacro defr [name form]
  `(do
     (defonce ~(vary-meta name assoc :reactive? true) (ref nil))
     (swap! renv set-ref (quote ~name) ~name)
     (r! ~name ~form)))



#_(defr app-html
  (list
   [:h1 "Time Tracker 3000"]
   [:hr]
   [:table
    [:tr
     [:td
      [:h1 "Start"]
      [:div {:style (str "width: " col-width "px; height: 300px; overflow-y: scroll;")}
       (for [hr [9 10 11 12 1 2 3 4 5 6 7 8]
             minute [0 15 30 45]]
         [:div 
          [:a {:href "#"
               :onclick (str "alert('hi "hr"'); return false;")} (str hr ":" (format "%02d" minute))]])]]
     [:td
      [:h1 "End"]
      [:div {:style (str "width: " col-width "px; height: 300px; overflow-y: scroll;")}
       (for [hr [9 10 11 12 1 2 3 4 5 6 7 8]
             minute [0 15 30 45]]
         [:div 
          [:a {:href "#"} (str hr ":" (format "%02d" minute))]])]]
     (let [increments [15 30 45 60]]
       (for [category ["Break" "Extra"]]
         [:td
          [:h1 category]
          [:div {:style "width: 150px; height: 300px; overflow-y: auto;"}
           (for [minutes increments]
             [:div 
              [:a {:href "#"} (str "0" ":" (format "%02d" minutes))]])]])
       )
     ]]))

#_(defr debug-html
  [:div {:style "font-family: Monaco, Courier;"}
   (hiccup.util/escape-html
    (hiccup.page/html5
     app-html))])

#_(defr html
  (hiccup.page/html5
   [:head
    [:title "Example"]
    (hiccup.bootstrap.page/include-bootstrap)]
   [:body
    app-html
    [:br]
    debug-html
    ]))

#_(defr ui-wrapper
  [:QWebView
   {:html html}])

;; (defr make-ui!
;;   (show-ui :timetracker
;;            ui-wrapper))

;; (defr debug-html
;;   (show-ui :debug
;;            [:QTextEdit {:plainText html}]))

;; (show-ui :commandlink2
;;          [:QPushButton {:text "The pushbutton"
;;                         :styleSheet "color: blue;"
;;                         :flat true}])

(defmulti compile-ui (fn [platform & ui]
                       platform))
(defn style-str [m]
  (when (seq m)
    (apply str
           (for [[k v] m
                 :let [v (if (contains? #{:width :height} k)
                           (str v "px")
                           v)]]
             (str (name k) ":" v ";")))))

(defn stylize [[type m & children :as ui]]
  (if-let [style (:style m)]
    (apply vector
           type (assoc m :style (style-str style))
           children)
    ui))



(defmethod compile-ui :web [platform form]
  (let [[type m & children] (normalize-ui form)
        form
        (case type
          :text [:span {:style m} (apply str children)]

          :vlayout [:div {:style m}
                    (for [child children]
                      [:div child])]
          :hlayout [:div {:style m}
                    (for [child children]
                      [:div {:style {:float "left"}}
                       child])
                    [:div {:style {:clear "both"}}]]
          :textfield [:input {:type "text" :value (first children)
                              :style m}]
          :vscroll [:div  {:style (merge {:overflow "auto"}
                                         m)}
                    children]
          :flat-button (let [on-click (get m :on-click)
                             m (dissoc m :on-click)]
                         [:a {:href "#"
                              :onclick (str
                                        (if on-click
                                            (let [[bindings & puts] on-click]
                                              (apply str
                                                     (for [put puts]
                                                       (str "channels.put( '" (pr-str put) "' );")))))
                                        "; return true;")
                              :style m}
                          (first children)])
          :button [:button {:style m} (first children)]
          form)
        [type m & children] (normalize-ui form)]
    (stylize (apply vector type m (for [child children]
                                    (if (vector? child)
                                      (compile-ui platform child)
                                      child))))
    ))

(defn stylize-desktop [[type m & children :as ui]]
  (if-let [style (style-str (:style m))]
    (apply vector
           type (-> m
                    (dissoc :style)
                    (assoc :styleSheet style))
           children)
    (apply vector
           type (dissoc m :style)
           children)))

(defmethod compile-ui :desktop [platform form]
  (let [[type m & children] (normalize-ui form)
        pass-through-props? #{:width :height}
        pass-through (select-keys m pass-through-props?)
        m (into {} (for [[k v] m
                         :when (not (pass-through-props? k))]
                     [k v]))
        form
        (case type
          :text [:QLabel {:style m
                          :text (apply str children)}]
          :vscroll [:QScrollArea {:style m
                                  :viewport (compile-ui
                                             platform
                                             (apply vector
                                                    :vlayout
                                                    children))}]
          :vlayout [:QVBoxWidget {:style (dissoc m :height)}
                    children]
          :hlayout [:QHBoxWidget {:style m}
                    children]
          :textfield [:QLineEdit {:text (first children)
                                  :style m}]
          :flat-button [:QPushButton {:flat true
                                      :style m
                                      :text (first children)}]
          :button [:QPushButton {:style m
                                 :text (first children)}]
          form)
        [type m & children] (normalize-ui form)]
    (stylize-desktop (apply vector type (merge m
                                               pass-through)
                            (for [child children]
                              (if (vector? child)
                                (compile-ui platform child)
                                child))))
    ))

;; (defr application-chan (chan))
;; (defr col-width 95)
;; (defr selected-day "today")
;; (defr start-time nil)
;; (defr end-time nil)
;; (defr test-ui
;;   [:vlayout {}
;;    [:text "Time Tracker 2001"]
;;    [:hlayout {:padding "5px 0 10px 0"}
;;     (for [day ["today" "yesterday"]]
;;       (if (= selected-day day)
;;         [:text day]
;;         [:flat-button {:margin-right "20px"
;;                        :padding "10px"
;;                        :on-click [[]
;;                                   ['selected-day day]]}
;;          day]))]
;;    [:hlayout
;;     [:vscroll {:height 500
;;                :width col-width}
;;      [:vlayout
;;       [:text (str "Start"
;;                   "(" (let [[hr minute] start-time]
;;                         (str hr ":" (format "%02d" minute))) ")")]
;;       (for [hr [9 10 11 12 1 2 3 4 5 6 7 8]
;;             minute [0 15 30 45]]
;;         [:flat-button
;;          {:on-click [;; bindings
;;                      []
;;                      ;;
;;                      ['start-time [hr minute]]
;;                      ;;['application-chan [:start hr minute]]
;;                      ]}
;;          ;; {:on-click (fn []
;;          ;;              (msg "start" hr minute))}
;;          (str hr ":" (format "%02d" minute))])]]
;;     [:vscroll {:height 500
;;                :width col-width}
;;      [:vlayout
;;       [:text (str "End"
;;                   "(" (let [[hr minute] end-time]
;;                         (str hr ":" (format "%02d" minute)))
;;                   ")")
;;        ]
;;       (for [hr [9 10 11 12 1 2 3 4 5 6 7 8]
;;             minute [0 15 30 45]]
;;         [:flat-button
;;          {:on-click [[]
;;                      ['end-time [hr minute]]]}
;;          (str hr ":" (format "%02d" minute))])]]
;;     (let [increments [15 30 45 60]]
;;       (for [category ["Break" "Extra"]]
;;         [:vscroll {:width col-width
;;                    :height 500}
;;          [:vlayout
;;           [:text category]
;;           (for [minutes increments]
;;             [:flat-button (str "0" ":" (format "%02d" minutes))])]]))]
   
;;    ]
;; )

(defn configure-pretty-printer
   "Configure the pretty-printer (an instance of a JTidy Tidy class) to
generate output the way we want -- formatted and without sending warnings.
 Return the configured pretty-printer."
   []
   (doto (new Tidy)
     (.setSmartIndent true)
                                        ;(.setTrimEmptyElements true)
     (.setShowWarnings false)
     (.setQuiet true)))

(defn pretty-print-html
  "Pretty-print the html and return it as a string."
  [html]
  (let [swrtr ( ByteArrayOutputStream.)]
    (.parse (configure-pretty-printer) (ByteArrayInputStream. (.getBytes (str html)))  swrtr)
    (str swrtr)))



;; (defr uic2!
;;   (show-ui :test-compile-ui-descto23kllllllllllllllll
;;            (compile-ui :desktop
;;                        [:vscroll test-ui])))


(definterface ChannelInterface
  (^void put [^String val]))

(let [ch (chan)]
  (goe
   (loop []
     (when-let [val-str (<! ch)]
       (let [[sym val] (read-string val-str)]
         (swap! renv set-form sym (constant-evaluable val) nil)
         (dosync
          (shake! @renv sym)))
       (recur))))
  (defn channel-bridge []
    (proxy [QObject minebot_clj.cell.ChannelInterface] []
      (put [^String val-str]
        (put! ch val-str)))))

;; (defr uic! (show-ui :test-compile-ui45
;;                     [:QWebView {:html
;;                                 (hiccup.page/html5
;;                                  (compile-ui :web test-ui)
;;                                  [:hr]
;;                                  [:pre {:style "font-family: Monaco, Courier;"}
;;                                   (hiccup.util/escape-html
;;                                    (pretty-print-html (hiccup.page/html5 (compile-ui :web test-ui))))])
;;                                 :bridge [["channels" (channel-bridge)]]
;; }]))


 








