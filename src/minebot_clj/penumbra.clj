(ns minebot-clj.penumbra
  (:require [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        mult tap
                                        ] :as async]
            [clojure.stacktrace]
            [minebot-clj.ui :as ui]
            [minebot-clj.environment :as env :refer [defr r! rv! ru! r?]])
  (:require [minebot-clj.zipper :as z :refer [zzedit zzup zzunzip zzseq zzget zzfirst zzrest defzzfn zzroot zznth zzroot? zzpath zzapply]]
            [minebot-clj.core :as bot])
  (:use [minebot-clj.evaluable])
  (:use [penumbra.opengl :exclude [color]]
        [penumbra.utils :only [defn-memo]])
  (:require [penumbra.app :as app]
            [penumbra.app.event :as event]
            [penumbra.app.loop :as loop]
            [penumbra.opengl.core :refer [get-integer gl-import- enum]]
            [penumbra.opengl.context :as context]
            [penumbra.app.controller :as controller]
            [penumbra.opengl.effects :as fx]
            [clojure.walk])
  (:import [org.lwjgl BufferUtils])
  (:import [java.awt Font]
           [java.awt.font TextAttribute]
           [org.newdawn.slick TrueTypeFont]
           [org.newdawn.slick.opengl TextureImpl])
  (:import [java.awt Font]
           [java.util Date]
           [org.newdawn.slick.opengl TextureImpl ])
  (:gen-class))

(defn update-state [state]
  state)

(declare run-key-repeater)
(defn init [state]
  (render-mode :wireframe)
  ;; (app/periodic-update! 30  #'update-state )
  (app/vsync! true)
  
  (assoc state
    :key-repeater (run-key-repeater
                   (let [myapp (app/app)]
                     (fn [key]
                       ;; (event/publish! myapp :key-press key)
                       )))))

(defn reshape [[x y width height] state]
;;  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  (translate 0 0 -4)
  (light 0 :position [1 1 1 0])
  state)


(defmacro defcomponent [name [& fields] & opts+specs]
  `(defrecord ~name [cid# ~@fields]
     IComponent
     (~'cid [this#]
       cid#)
     ~@opts+specs))

(defmacro event-interfaces [& types]
  (let [decls
        (for [type types]
          (let [stype (name type)
                fname (str "-" stype)
                pname (-> stype
                          (clojure.string/split #"-")
                          (->> (map clojure.string/capitalize))
                          (->> (apply str "I"))
                          symbol)]
            `(do
               (defprotocol ~pname
                 (~(symbol fname) [~'this ~'info]))
               (defmethod event-interface-key ~pname [_#]
                 ~type)
)))]
    `(do
       (defmulti ~'event-interface-key (fn [k#]
                                       k#))
       ~@decls)))


(event-interfaces
 :mouse-drag
 :mouse-move
 :mouse-down
 :mouse-up
 :mouse-click
 :mouse-wheel
 :key-press
 :key-type)

(defprotocol IFocus)

(defprotocol IDraw
  (draw [this state]))

(extend-protocol IDraw
  nil
  (draw [this state])
  clojure.lang.PersistentVector
  (draw [this state]
    (doseq [drawable this]
      (draw drawable state))))

(def make-cid gensym)

(defprotocol IComponent
  (cid [this]))

(extend-protocol IComponent
  nil
  (cid [this] nil)
  clojure.lang.PersistentVector
  (cid [this]
    nil))

(defprotocol IBounds
  (-bounds [this]))

(declare origin bounds)
(extend-protocol IBounds
  clojure.lang.PersistentVector
  (-bounds [this]
    (reduce
     (fn [[max-width max-height] elem]
       (let [[ox oy] (origin elem)
             [w h] (bounds elem)]
         [(max max-width (+ ox w))
          (max max-height (+ oy h))]))
     [0 0]
     this)))

(defn bounds [x]
  (when-not (or (satisfies? IBounds x) (satisfies? IComponent x))
    (throw (Exception. (str "Expecting IBounds or IComponent, got " x))))
  (if (satisfies? IBounds x)
    (-bounds x)
    [0 0]))

(defprotocol IOrigin
  (-origin [this]))

(extend-protocol IOrigin
  clojure.lang.PersistentVector
  (-origin [this]
    [0 0]))

(defn origin [x]
  (when-not (or (satisfies? IOrigin x) (satisfies? IComponent x))
    (throw (Exception. (str "Expecting IOrigin or IComponent, got " x))))
  (if (satisfies? IOrigin x)
    (-origin x)
    [0 0]))

(defprotocol IChildren
  (-children [this]))
(defprotocol IReplaceChildren
  (-replace-children [this new-children]))

(extend-protocol IReplaceChildren
  clojure.lang.PersistentVector
  (-replace-children [this new-children]
    new-children))

(extend-protocol IChildren
  clojure.lang.PersistentVector
  (-children [this]
    this))

(defn children [x]
  (when-not (or (satisfies? IChildren x) (satisfies? IComponent x))
    (throw (Exception. (str "Expecting IChildren or IComponent, got " x))))
  (if (satisfies? IChildren x)
    (-children x)
    []))

(defn width [ibounds]
  (let [[width height] (bounds ibounds)]
    width))
(defn height [ibounds]
  (let [[width height] (bounds ibounds)]
    height))

(def ^:dynamic *font-cache* (atom {}))
(def ^:dynamic *font* nil)
(def ^:dynamic *root* nil)
(def ^:dynamic *focus* nil)
(def ^:dynamic *event-handlers* nil)

(def find-focus (memoize zzapply))

(defmacro with-event-bindings [& body]
  `(let [root# (-> ~'state :root deref)
         event-handlers# (-> ~'state :event-handlers deref)
         font-cache# (-> ~'state :font-cache)
         focus-path# (-> ~'state :focus)
         focus# (when focus-path#
                  (cid (find-focus root# focus-path#)))
         ]
     (binding [*root* root#
               *focus* focus#
               *event-handlers* event-handlers#
               *font-cache* font-cache#]
       ~@body)))

(defmacro with-font [f & body]
  `(binding [*font* ~f]
     ~@body))


(defn-memo text-attribute
  "Takes :keyword and returns TextAttribute/KEYWORD"
  [k]
  (eval `(. TextAttribute ~(-> k name (.replace \- \_) .toUpperCase symbol))))


(defmacro with-slate [& body]
  `(let [has-context?# (every? identity (vals (context/current)))
         slate# penumbra.opengl.slate/*slate*]
     (if (or has-context?# slate#)
       (do
         ~@body)
       (penumbra.opengl.slate/with-slate
         ~@body))))

(defn font [name & modifiers]
  (if-let [font (get @*font-cache* (list* name modifiers))]
    font
    (with-slate
      (let [hash (-> (apply hash-map modifiers)
                     (update-in [:size] float)
                     (assoc :family name))
            hash (zipmap (map text-attribute (keys hash)) (vals hash))
            font (TrueTypeFont. (Font. hash) true)]
        (swap! *font-cache* assoc (list* name modifiers) font)
        font))))


(defcomponent Label [text options]
  IBounds
  (-bounds [_]
    (let [f (or *font* (font (get options :font "Tahoma")
                             :size (get options :font-size 20)))]
      [(.getWidth f text)
       (.getHeight f text)]))

  IDraw
  (draw [this state]
    (with-font (or *font* (font (get options :font "Tahoma")
                                :size (get options :font-size 20)))
      (try-with-program
       nil
       (with-disabled [:texture-rectangle :lighting]
         (with-enabled [:texture-2d :blend]
           (let [blend-dst (get-integer :blend-dst)
                 blend-src (get-integer :blend-src)]
             (push-matrix
                (when (or (get options :x)
                          (get options :y))
                  (translate (get options :x 0) (get options :y 0) 0))
                (when-let [color (get options :color)]
                  (apply penumbra.opengl/color color))
                (blend-func :src-alpha :one-minus-src-alpha)
                (TextureImpl/bindNone)
                (.drawString *font* 0 0 text)
                (blend-func blend-src blend-dst))))))
      ))
  )

(defn label [text & options]
  (Label. (make-cid "label") text (apply hash-map options)))


(defcomponent Group [drawables]
  IBounds
  (-bounds [this]
    (reduce
     (fn [[max-width max-height] elem]
       (let [[ox oy] (origin elem)
             [w h] (bounds elem)]
         [(max max-width (+ ox w))
          (max max-height (+ oy h))]))
     [0 0]
     drawables))

  IDraw
  (draw [this state]
    (doseq [drawable drawables]
      (draw drawable state)))
  IChildren
  (-children [this]
    drawables)
  IReplaceChildren
  (-replace-children [this new-children]
    (assoc this :drawables new-children)))

(defn group [& drawables]
  (Group. (make-cid "group") drawables))


(defcomponent Move [x y drawable]
  IOrigin
  (-origin [this]
    [x y])
  IChildren
  (-children [this]
    [drawable])
  IReplaceChildren
  (-replace-children [this new-children]
    (assert (= 1 (count new-children)))
    (assoc this :drawable (first new-children)))
  IBounds
  (-bounds [this]
    (bounds drawable))
  IDraw
  (draw [this state]
    (push-matrix
     (translate x y 0)
     (draw drawable state))))
(defn move [x y drawable]
  (Move. (make-cid "move") x y drawable))

(defn spacer [x y]
  (let [-cid (make-cid "spacer")]
    (reify
      IComponent
      (cid [this]
        -cid)
      IDraw
      (draw [this state])
      IBounds
      (-bounds [this]
        [x y]))))

(defcomponent Path [points]
  IBounds
  (-bounds [this]
    (let [maxx (apply max (map first points))
          maxy (apply max (map second points))]
      [maxx maxy]))
  IDraw
  (draw [this state]
    (push-matrix
     (draw-lines
      (doseq [[[x1 y1] [x2 y2]] (map vector points (rest points))]
        (vertex x1 y1)
        (vertex x2 y2))))))
(defn path [& points]
  (Path. (make-cid "path") points))

(gl-import- glPushAttrib gl-push-attrib)
(gl-import- glPopAttrib gl-pop-attrib)
(defcomponent Polygon [color points]
  IBounds
  (-bounds [this]
    (let [maxx (apply max (map first points))
          maxy (apply max (map second points))]
      [maxx maxy]))
  IDraw
  (draw [this state]
    (push-matrix
     (gl-push-attrib (enum  :CURRENT_BIT))
     (when color
       (apply penumbra.opengl/color color))
     (draw-polygon
      (doseq [[x y] points]
        (vertex x y)))
     (gl-pop-attrib))))
(defn polygon [color & points]
  (Polygon. (make-cid "polygon") color points))

(defcomponent Arc [radius rad-start rad-end steps]
  IDraw
  (draw [this state]
    (let [arc-length (- rad-end rad-start)]
      (draw-line-strip
       (doseq [i (range (inc steps))
               :let [pct (/ (float i) steps)
                     rad (- (+ rad-start
                               (* arc-length pct)))
                     x (* radius (Math/cos rad))
                     y (* radius (Math/sin rad))]]
         (vertex x y))))))

(defn arc [radius rad-start rad-end]
  (Arc. (make-cid "arc") radius rad-start rad-end 10))

(defn rectangle [width height]
  (path [0 0] [0 height] [width height] [width 0] [0 0]))

(defn filled-rectangle [color width height]
  (polygon color [0 0] [0 height]  [width height] [width 0]  [0 0]))



(def ^:dynamic *origin* [0 0 0])
(def ^:dynamic *view* nil )

(gl-import- glScissor gl-scissor)
(gl-import- glVertex3d gl-vertex)
(gl-import- glNormal3d gl-normal)
(gl-import- glTexCoord1d gl-tex-1)
(gl-import- glTexCoord2d gl-tex-2)
(gl-import- glTexCoord3d gl-tex-3)
(gl-import- glRotatef gl-rotate)
(gl-import- glTranslated gl-translate)
(gl-import- glScaled gl-scale)
(gl-import- glLoadIdentity gl-load-identity)

(gl-import- glVertexAttrib1f attribute-1f)
(gl-import- glVertexAttrib2f attribute-2f)
(gl-import- glVertexAttrib3f attribute-3f)
(gl-import- glVertexAttrib4f attribute-4f)

(gl-import- glGetAttribLocation gl-get-attrib-location)

(defn- attribute-location [variable]
  (let [variable (str (.replace (name variable) \- \_) "\0")]
    (if-let [location (@penumbra.opengl.core/*attributes* variable)]
      location
      (if penumbra.opengl.core/*primitive-type*
        (throw (Exception. "Cannot get attribute location while inside glBegin/glEnd"))
        (let [ary (.getBytes variable)
              attribute-buf (-> (BufferUtils/createByteBuffer (count ary))
                                (.put ary)
                                .rewind)
              loc (gl-get-attrib-location (:program penumbra.opengl.core/*program*) attribute-buf)]
          (dosync (alter penumbra.opengl.core/*attributes* #(assoc % variable loc)))
          loc)))))

;; (defn declare-attributes [& attributes]
;;   (doseq [a attributes]
;;     (attribute-location a)))


(defn- set-attrib [variable args]
  (let [loc     (attribute-location variable)
        args    (vec (map float args))]
    (condp = (count args)
      1 (attribute-1f loc (args 0))
      2 (attribute-2f loc (args 0) (args 1))
      3 (attribute-3f loc (args 0) (args 1) (args 2))
      4 (attribute-4f loc (args 0) (args 1) (args 2) (args 3)))))

(def my-renderer
  (reify
    penumbra.opengl.geometry.Renderer
    (vertex- [_ x y z] (gl-vertex x y z))
    (texture- [_ u] (gl-tex-1 u))
    (texture- [_ u v] (gl-tex-2 u v))
    (texture- [_ u v w] (gl-tex-3 u v w))
    (color- [_ r g b a] (fx/color r g b a))
    (attribute- [_ attrib values] (set-attrib attrib values))
    (normal- [_ x y z] (gl-normal x y z))
    (scale- [_ x y z] (gl-scale x y z))
    (translate- [_ x y z]
      (set! *origin* (map + *origin* [x y z]))
      (gl-translate x y z))
    (rotate- [_ angle x y z] (gl-rotate angle x y z))
    (load-identity- [_] (gl-load-identity))
    (transform-matrix- [_] nil)
    (with-transform- [_ f]
      (binding [*origin* *origin*]
        (gl-push-matrix)
        (try
          (f)
          (finally
            (gl-pop-matrix)))))))

(declare find-offset)
(defcomponent ScrollArea [bounds offset drawable on-scroll scroll-offset-y]
  IOrigin
  (-origin [this]
    [0 0])
  IChildren
  (-children [this]
    [drawable])
  IReplaceChildren
  (-replace-children [this new-children]
    (assert (= 1 (count new-children)))
    (assoc this :drawable (first new-children)))
  IBounds
  (-bounds [this]
    bounds)
  IMouseWheel
  (-mouse-wheel [this [[mx my] dwheel]]
    ;; (let [[gx gy] (find-offset this)
    ;;       [width height] bounds]
    ;;   (when (and mx my
    ;;              (>= mx gx)
    ;;              (>= my gy)
    ;;              (< mx (+ gx width))
    ;;              (< my (+ gy height)))
    ;;     (swap! scroll-offset-y - (/ (float dwheel) 50))
    ;;     ))

    (when on-scroll
      (let [[gx gy] (find-offset this)
            [width height] bounds]
        (when (and mx my
                   (>= mx gx)
                   (>= my gy)
                   (< mx (+ gx width))
                   (< my (+ gy height)))
          (on-scroll dwheel))))
    )
  IDraw
  (draw [this state]
    (let [[ox oy] offset
          [gx gy _] *origin*
          [_ _ view-width view-height] *view*
          [width height] bounds]
      (gl-scissor gx
                  (- view-height gy height)
                  (inc width)
                  (inc height))
      (with-enabled :scissor-test
        (push-matrix
         (translate (- ox) (int (- oy)) 0)
         (draw drawable state)
         )))))

(defn scroll-area [bounds offset drawable & [on-scroll]]
  (ScrollArea. (make-cid "scrollarea") bounds offset drawable on-scroll (atom 0)))


(defn display [[dt t] state]
  (render-mode :solid)
  (binding [*origin* [0 0 0]
            penumbra.opengl.core/*renderer* my-renderer
            *view* @penumbra.opengl.core/*view*]
    (with-event-bindings
      (let [[x-origin y-origin w h] *view*]
        (when *root*
          (with-projection (ortho-view x-origin (+ x-origin w) (+ y-origin h) y-origin -1 1)
            (push-matrix
             (load-identity)
             (TextureImpl/bindNone)
             (draw *root* state)))))))

  state)


(defn close [state]
  "Called once, when application ends."
  (when-let [kill-ch (-> state :key-repeater :kill-ch)]
    (close! kill-ch)))

(defn mouse-drag [[dx dy] [mx my] button state]
  "Called when mouse moves with a button pressed. [dx dy] contains relative motion since last time :mouse-drag was called, and [x y] contains absolute position of the mouse. button will be equal to one of :left, :right, :center, :mouse-4, or :mouse-5. If the mouse is moving when two or more buttons are pressed, :mouse-drag will be called once for each button."
  (with-event-bindings
      (doseq [handler (-> *event-handlers* :mouse-drag)]
        (-mouse-drag handler [mx my])))
  state)

(defn mouse-wheel [dwheel state]
  (let [mx (:mx state)
        my (:my state)]
    (with-event-bindings
      (doseq [handler (-> *event-handlers* :mouse-wheel)]
        (-mouse-wheel handler [[mx my]  dwheel]))))

  state)


(defn box-contains? [[x y width height] [px py]]
  (and (<= px (+ x width))
       (>= px x)
       (<= py (+ y height))
       (>= py y)))


(defn mouse-move [[dx dy] [mx my] state]
  "Called the same as :mouse-drag, but when no button is pressed."
  (with-event-bindings
      (doseq [handler (-> *event-handlers* :mouse-move)]
        (-mouse-move handler [mx my])))

  (assoc state
    :mx mx
    :my my))


(defn mouse-down [[x y] button state]
  "Called whenever a button is pressed."
  (with-event-bindings
      (doseq [handler (-> *event-handlers* :mouse-down)]
        (-mouse-down handler [x y])))

  state)


(declare find-elems)
(defn mouse-up [[x y] button state]
  "Called whenever a button is released."

  (with-event-bindings
      (doseq [handler (-> *event-handlers* :mouse-up)]
        (-mouse-up handler [x y]))
      (let [focus-elem (first (filter #(satisfies? IFocus %)
                                      (find-elems [x y])))
            focus (when focus-elem
                    (or (zzpath focus-elem)
                        []))]
        (-> state
            (assoc :focus focus)))))


(defn mouse-click [[x y] button state]
  "Called whenever a mouse is clicked (pressed and released). Gives [x y] of where mouse was originally pressed."
  
  state)

(declare whoo-text event-handlers)




(defn key-repeat-loop [k send-key]
  (let [kill-ch (chan)]
    (go
      (loop [delay 100
             n 0]
        (when (< n 100)
          (let [timeout-ch (timeout delay)
                [val port] (alts! [kill-ch timeout-ch])]
            (when (= port timeout-ch)
              (do
                (send-key k)
                (recur 50 (inc n))))))))
    kill-ch))

(defn run-key-repeater [send-key]
  (let [kill-ch (chan)
        key-up-ch (chan 20)
        key-down-ch (chan 20)]
    (go
      (try
       (loop [keys-in-flight {}]
         (let [[val port] (alts! [key-up-ch key-down-ch kill-ch])]
           (doseq [repeat-loop (vals keys-in-flight)]
                (close! repeat-loop))

           (cond
            (= port key-down-ch)
            (recur (assoc keys-in-flight val (key-repeat-loop val send-key)))

            (= port key-up-ch)
            (recur (dissoc keys-in-flight val))


            (= port kill-ch)
            (do
              (println "key repeater donw")
              (doseq [repeat-loop (vals keys-in-flight)]
                (close! repeat-loop)))))

         )
       (catch Exception e
         (println e)))
      (println "kill key repeater"))
    {:kill-ch kill-ch
     :key-up-ch key-up-ch
     :key-down-ch key-down-ch}))

(defn key-press [key state]
  "Called whenever a key is pressed. If the key is something that would normally show up in a text entry field, key is a case-sensitive string. Examples include “a”, “&”, and " ". If it is not, key is a keyword. Examples include :left, :control, and :escape"
  (with-event-bindings
    (doseq [handler (-> *event-handlers* :key-press)]
      (-key-press handler key)))
  
  (async/put! (-> state :key-repeater :key-down-ch) key)
  state)


(defn key-release [key state]
  "Called whenever a key is released."
  (when key
    (async/put! (-> state :key-repeater :key-up-ch) key))  
  state)


(defn key-type [key state]
  "Called whenever a key is pressed and released. This is the only key event which will expose auto-repeated keys."
  (with-event-bindings
    (doseq [handler (-> *event-handlers* :key-type)]
      (-key-type handler key)))
  state)

(defonce current-app (atom nil))
(defn stop []
  (when-let [app @current-app]
    (controller/stop! app)
    (reset! current-app nil))
  nil)

(defn start [root event-handlers]
  (stop)
  (Thread/sleep 1000)
  (let [callbacks {:display #'display
                   :init #'init
                   :close #'close
                   :reshape #'reshape
                   :mouse-drag #'mouse-drag
                   :mouse-move #'mouse-move
                   :mouse-down #'mouse-down
                   :mouse-up #'mouse-up
                   :mouse-click #'mouse-click
                   :mouse-wheel #'mouse-wheel
                   :key-press #'key-press
                   :key-release #'key-release
                   :key-type #'key-type}
        state {:width 650
               :height 800
               :top 60
               :mx 0
               :my 0
               :font-cache (atom {})
               :focus nil
               :root root
               :event-handlers event-handlers}
        ;; current-app (app/start callbacks state)
        ]
    (reset! current-app (app/create callbacks state))
    (let [out *out*]
      (go
        (binding [*out* out]
          (try
            (add-watch root :repaint
                       (fn [& args]
                         (app/repaint! @current-app)))
            (app/start-single-thread @current-app loop/basic-loop)
            (catch Exception e
              (clojure.stacktrace/print-stack-trace e))
            (finally
              (remove-watch root :repaint))))))
    nil))




(defn button-draw [this state]
  (let [text (:text this)
        blabel (label text)
        [blw blh] (bounds blabel)
        padding 20
        {:keys [mx my]}  state
        [gx gy _] *origin*
        ]

    (when (and mx my
               (>= mx gx)
               (>= my gy)
               (< mx (+ gx blw padding))
               (< my (+ gy blh padding)))
        (penumbra.opengl/color [1 0 1]))
    (draw
     [
      (rectangle (+ blw padding) (+ blh padding))
      
      (move (/ padding 2)
            (/ padding 2)
            (label text))]
     state)))

(declare find-offset)
(defcomponent Button [text on-click]
  IBounds
  (-bounds [_]
    (let [blabel (label text)
          [blw blh] (bounds blabel)
          padding 20]
      [(+ blw padding)
       (+ blh padding)]))
  IMouseDown
  (-mouse-down [this [mx my]]
    (when on-click
      (let [[gx gy] (find-offset this)
            blabel (label text)
            [blw blh] (bounds blabel)
            padding 20]
        (when (and mx my
                   (>= mx gx)
                   (>= my gy)
                   (< mx (+ gx blw padding))
                   (< my (+ gy blh padding)))
          (on-click)))))

  IDraw
  (draw [this state]
    (button-draw this state)))
(defn button [text & [on-click on-hover hover?]]
  (Button. (make-cid "button") text on-click))

(defn wrap-text [text n]
  (loop [[k & text] text
         i 0
         line []
         lines []]
    (cond
     (nil? k)
     (map #(apply str %) (conj lines line))

     (= k \newline)
     (recur text 0 [] (conj lines line))

     (= i (dec n))
     (recur text 0 [] (conj lines (conj line k)))

     :else
     (recur text (inc i) (conj line k) lines)))

)

(declare vertical-layout horizontal-layout)

(declare vertical-layout horizontal-layout)
(defn text-input-components [text focus?]
  (let [text (if (instance? clojure.lang.IDeref text)
               @text
               text)
        text (if-not (string? text)
               (str text)
               text)
        tlabel (label text)
        [tw th] (bounds tlabel)
        padding 10
        lines (clojure.string/split text #"\n")
        labels (apply
                vertical-layout
                (for [line lines]
                  (label line)))
        content
        (vertical-layout
         (spacer 0 padding)
         (horizontal-layout (spacer padding 0) labels (spacer padding 0))
         (spacer 0 padding))
        [content-width content-height] (bounds content)
        border (if focus?
                 (filled-rectangle [1 0 1]
                                   content-width
                                   content-height)
                 (rectangle content-width
                          content-height))]
    [border
     content]))
(defcomponent TextInput [text on-key]
  IFocus

  IKeyPress
  (-key-press [this key]
    (when (and on-key
               (= (cid this) *focus*))
      (on-key key)))

  IBounds
  (-bounds [_]
    (let [comps (text-input-components text nil)
          [ox oy] (origin comps)
          [w h] (bounds comps)]
      [(+ ox w)
       (+ oy h)]))

  IDraw
  (draw [this state]
    (let [focus? (= (cid this) *focus*)]
      (draw (text-input-components text focus?)
            state))))
(defn text-input [initial-text & [on-key]]
  (TextInput. (make-cid "textinput") initial-text on-key))


(defn text-area-components [input focus?]
  (let [{:keys [text width height cursor]} input
        text (if (instance? clojure.lang.IDeref text)
               @text
               text)
        text (if-not (string? text)
               (str text)
               text)
        [cw ch] (bounds (label "." :font "Monospaced"))
        padding 10
        wrap-n (int (/ (- width (* 2 padding))
                       cw))
        lines (wrap-text text wrap-n)
        labels (apply
                vertical-layout
                (for [line lines]
                  (label line :font "Monospaced")))
        cursor-rect (rectangle cw ch)
        [cursor-x cursor-y]
        (loop [x 0
               y 0
               cursor cursor
               [key & text :as all-text] (seq text)
               ]
          (cond

           (<= cursor 0)
           [x y]

           (>= x wrap-n)
           (recur 0 (inc y) cursor all-text)

           (= key \newline)
           (recur 0 (inc y) (dec cursor) text)

           :else
           (recur (inc x) y (dec cursor) text)))
        content
        (vertical-layout
         (spacer 0 padding)
         (horizontal-layout (spacer padding 0)
                            [(move (* cursor-x cw) (* cursor-y ch)
                                   cursor-rect)
                             labels]
                            (spacer padding 0))
         (spacer 0 padding))
        border (rectangle width
                          height)]
    [border
     content]))

(defcomponent TextArea [text on-key width height cursor]
  IFocus

  IKeyPress
  (-key-press [this key]
    (when (and on-key
               (= (cid this) *focus*))
      (on-key key)))

  IBounds
  (-bounds [this]
    (let [comps (text-area-components this false)
          [ox oy] (origin comps)
          [w h] (bounds comps)]
      [(+ ox w)
       (+ oy h)]))

  IDraw
  (draw [this state]
    (let [focus? (= (cid this) *focus*)]
      (draw (text-area-components this focus?)
            state))))
(defn text-area [initial-text & [on-key cursor width height]]
  (->TextArea (make-cid "textinput") initial-text on-key (or width 300) (or height 300) (or cursor 0) ))

;; test text input
;; (defr testtext ".adsf")
;; (defr testcursor 0)
;; (defr testtext2 "adsf")
;; (defr components
;;   (vertical-layout
;;    (text-area testtext ~(text-key-handler 'testtext 'testcursor) testcursor)
;;    (text-input testtext2 ~(key-handler 'testtext2))))
;; (defr event-handlers (make-event-handlers components))


(defn text-key-handler
  ([textsym cursorsym]
   (text-key-handler (env/get-or-create-renv) textsym cursorsym))
  ([renv textsym cursorsym]
   (fn [key]
     (dosync
      (let [old-cursor (env/get-renv-value renv cursorsym)
            old-text (env/get-renv-value renv textsym)
            key (if (= :return key)
                  "\n"
                  key)

            [cursor text]
            (cond
             (= :back key)
             [(dec old-cursor)
              (str
               (when (pos? old-cursor)
                 (subs old-text 0 (dec old-cursor)))
               (subs old-text old-cursor (.length old-text)))]

             (string? key)
             [(inc old-cursor)
              (str
               (subs old-text 0 old-cursor)
               key
               (subs old-text old-cursor (.length old-text)))]

             (= :left key)
             [(dec old-cursor)
              old-text]

             (= :right key)
             [(inc old-cursor)
              old-text]

             :else
             [old-cursor
              old-text])
            cursor (max 0 cursor)
            cursor (min (count text) cursor)]
        (env/set-value renv textsym text {})
        (env/set-value renv cursorsym cursor {}))))))

(defn key-handler
  ([sym]
   (key-handler (env/get-or-create-renv) sym))
  ([renv sym]
   (fn [key]
     (env/update-value
      renv
      sym
      (fn [old-text]
        (let [new-text
              (cond
               (= :back key)
               (subs old-text 0 (max 0 (dec (.length old-text))))
               
               (string? key)
               (str old-text key))]
          (if new-text
            new-text
            old-text)))))))

(defn vertical-layout [& elems]
  (loop [[elem & more :as elems] elems
         offset-y 0
         group-elems []]
    (if (seq elems)
      (let [[_ dy] (bounds elem)
            [_ oy] (origin elem)]
        (recur more (+ offset-y oy dy 1) (conj group-elems
                                               (move 0 offset-y
                                                     elem))))
      (when (seq group-elems)
          (apply group group-elems)))))

(defn horizontal-layout [elem & more]
  (if more
    (let [[dx _] (bounds elem)
          [ox _] (origin elem)]
      [elem (move (+ ox dx 1) 0
                  (apply horizontal-layout more))])
    elem))

(defn center [elem [width height]]
  (let [[ewidth eheight] (bounds elem)]
    (move (int (- (/ width 2)
                  (/ ewidth 2)))
          (int (- (/ height 2)
                  (/ eheight 2)))
          elem)))

(defzzfn zzchildren :children children)
(defmethod z/back :children [_ old-val new-val]
  (-replace-children old-val new-val))


;; (defr ch nil)
;; (defr whoo-text "whoo")
;; (defr status "status ")
;; (defr rwidth 50)
;; (defr rheight 10)

;; (defr event-handlers
;;   (make-event-handlers components))
;; (defr components [(vertical-layout
;;                    (label "Minecraft Foo" :font-size 35)
;;                  (move 0 30
;;                        (vertical-layout
;;                         (horizontal-layout
;;                          (button "Connect!"
;;                                  (fn []
;;                                    (try
;;                                      (go
;;                                        (let [mych (bot/do-something :local)]
;;                                          (println "hi")
;;                                          (r! ch mych)))
;;                                      (catch Exception e
;;                                        (clojure.stacktrace/print-stack-trace e)))))
;;                          (button "disconnect"
;;                                  (fn []
;;                                    (bot/kill-chans))))
                                                
                        
                        
;;                         (rectangle rwidth rheight)
                        

;;                         (move
;;                          0 10
;;                          (horizontal-layout
;;                           (button "speak."
;;                                   (fn []
;;                                     (put! ch (bot/chat whoo-text))
;;                                     (println whoo-text)))
;;                           (text-input whoo-text
;;                                       (fn [key]
;;                                         (let [new-text
;;                                               (cond
;;                                                (= :back key)
;;                                                (subs whoo-text 0 (max 0 (dec (.length whoo-text))))
                                               
;;                                                (string? key)
;;                                                (str whoo-text key))]
;;                                           (cond
;;                                            new-text
;;                                            (r! whoo-text new-text)

;;                                            (= :return key)
;;                                            (do
;;                                              (put! ch (bot/chat whoo-text))
;;                                              (r! whoo-text ""))))))))
;;                         (let [w 100
;;                               h 40
;;                               -cid (make-cid "keys")]
;;                          (reify
;;                            IComponent
;;                            (cid [this]
;;                              -cid)
;;                            IFocus
;;                            IKeyPress
;;                            (-key-press [this key]
;;                              (when (= (cid this) *focus*)
;;                                (let [key-map
;;                                      {" " [0 1 0]
;;                                       :return [0 -1 0]
;;                                       :up [1 0 0]
;;                                       :left [0 0 1]
;;                                       :right [0 0 -1]
;;                                       :down [-1 0 0]}]
;;                                  (if-let [[ox oy oz] (get key-map key)]
;;                                    (let [[x y z] (bot/integerize-position @bot/position)]
;;                                      (reset! bot/path [[(+ ox x)
;;                                                         (+ oy y)
;;                                                         (+ oz z)]]))
;;                                    (cond
;;                                     (= key "d")
;;                                     (when @bot/position
;;                                        (let [face 0
;;                                              [px py pz] (bot/integerize-position @bot/position)
;;                                              [x y z] [(inc px) (dec py) pz]
;;                                              status 2]
;;                                          (put! ch (bot/player-digging 0 x y z 0))))

;;                                     (= key "t")
;;                                     (swap! bot/looking
;;                                             (fn [looking]
;;                                               (if looking
;;                                                 (let [[yaw pitch] looking]
;;                                                   [(+ 90 yaw) pitch])
;;                                                 [0 0])))
                                    
;;                                     )))))
                           
;;                            IBounds
;;                            (-bounds [_]
;;                              [w h])

;;                            IDraw
;;                            (draw [this state]
;;                              (let [focus? (= (cid this) *focus*)]
;;                                (draw (if focus?
;;                                        (filled-rectangle [1 0 1] w h)
;;                                        (rectangle w h))
;;                                      state)))))
;;                         (button "Come"
;;                                 (fn []
;;                                   (bot/try-move-to-player ch)))
;;                         (button "tp to me"
;;                                 (fn []
;;                                   (put! ch (bot/chat "/tp treehugger1234 phronmophobic"))))
;;                         (button "respawn"
;;                                 (fn []
;;                                   (put! ch (bot/respawn))))
;;                         (horizontal-layout
;;                          (button "creative mode"
;;                                  (fn []
;;                                    (put! ch (bot/chat "/gamemode creative"))))
;;                          (button "adventure mode"
;;                                  (fn []
;;                                    (put! ch (bot/chat "/gamemode adventure"))))
;;                          (button "survival mode"
;;                                  (fn []
;;                                    (put! ch (bot/chat "/gamemode survival")))))
;;                         (button "make it day"
;;                                 (fn []
;;                                   (put! ch (bot/chat "/time set day"))))
;;                         (button "toggle rain"
;;                                 (fn []
;;                                   (put! ch (bot/chat "/toggledownfall"))))
;;                         (button "swing arm"
;;                                 (fn []
;;                                   (put! ch (bot/animation @bot/player-id 1))))
                        
;;                         (horizontal-layout
;;                          (button "command")
;;                          (text-input whoo-text))
;;                         )
;;                        )

;;                  (move 0 10
;;                        (label status)))])




(defn find-elems
  ([pt]
   (find-elems *root* pt))
  ([root [mx my]]
   (let [[ox oy] (origin root)
         child-pt [(- mx ox)
                   (- my oy)]
         child-elems (mapcat #(find-elems % child-pt)
                             (zzseq (zzchildren root)))]
     (concat
      child-elems
      (let [[w h] (bounds root)]
        (when (and (< mx w)
                   (< my h)
                   (>= mx 0)
                   (>= my 0))
          [root]))))))

(defn find-offset
  ([target]
   (find-offset *root* target [0 0]))
  ([root target]
   (find-offset root target [0 0]))
  ([root target [sx sy]]
   (if (= (cid root) (cid target))
     [sx sy]
     (if-let [s (seq (children root))]
       (let [[ox oy] (origin root)
             sx (+ ox sx)
             sy (+ oy sy)]
         (first (keep
                 #(find-offset % target [sx sy])
                 s)))
       nil))))

(defn ->local
  ([elem pt]
   (->local *root* elem pt))
  ([root elem [x y]]
   (let [[ox oy] (find-offset root elem)]
     [(- x ox)
      (- y oy)])))

(defn find-event-handlers [comp]
  (remove nil?
          (concat
           (mapcat find-event-handlers (children comp))
           (keep #(when (satisfies? % comp)
                    [(event-interface-key %) comp])
                 [IKeyPress IMouseMove IMouseDown IMouseWheel IMouseDrag IMouseUp IKeyType]))))

(defn make-event-handlers [comp]
  (->> (find-event-handlers comp)
       (group-by first)
       (map (fn [[k v]]
              [k (map second v)]))
       (into {})))





(defn remove-nth [v i]
  (vec (concat (take i v) (drop (inc i) v))))







