(ns minebot-clj.penumbra
  (:require [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        mult tap
                                        ] :as async]
            [clojure.stacktrace]
            [minebot-clj.ui :as ui]
            [minebot-clj.environment :as env :refer [set-ref shake! set-form]])
  (:require [minebot-clj.zipper :as z :refer [zzedit zzup zzunzip zzseq zzget zzfirst zzrest zzfn defzzfn zzroot zznth]])

  (:use [minebot-clj.evaluable])

  (:use [penumbra.opengl :exclude [color]]
        [penumbra.utils :only [defn-memo]])
  (:require [penumbra.app :as app]
            [penumbra.app.event :as event]
            [penumbra.app.loop :as loop]
            [penumbra.opengl.core :refer [get-integer gl-import- enum]]
            [penumbra.app.controller :as controller]
            [clojure.walk])  
  (:import [java.awt Font]
           [java.awt.font TextAttribute]
           [org.newdawn.slick TrueTypeFont]
           [org.newdawn.slick.opengl TextureImpl])
  (:import [java.awt Font]
           [java.util Date]
           [org.newdawn.slick.opengl TextureImpl])

  
  (:gen-class))

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

(defn update-state [state]
  state)

(defn init [state]
  (render-mode :wireframe)
  ;; (app/periodic-update! 30  #'update-state )
  (app/vsync! true)
  state)

(defn reshape [[x y width height] state]
;;  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  (translate 0 0 -4)
  (light 0 :position [1 1 1 0])
  state)




(defmacro defcomponent [name [& fields] & opts+specs]
  `(defrecord ~name [ ~@fields]
       IComponent
       ~@opts+specs))

(defprotocol IDraw
  (draw [this]))

(extend-protocol IDraw
  nil
  (draw [this])
  clojure.lang.PersistentVector
  (draw [this]
    (doseq [drawable this]
      (draw drawable))))

(defprotocol IComponent)

(defprotocol IBounds
  (-bounds [this]))

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

(defmacro with-font [f & body]
  `(binding [*font* ~f]
     ~@body))


(defn-memo text-attribute
  "Takes :keyword and returns TextAttribute/KEYWORD"
  [k]
  (eval `(. TextAttribute ~(-> k name (.replace \- \_) .toUpperCase symbol))))

(defn font [name & modifiers]
  (if-let [font (@*font-cache* (list* name modifiers))]
    font
    (app/with-gl
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
  (draw [this]
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
              (blend-func blend-src blend-dst))))))))
  )

(defn label [text & options]
  (Label. text (apply hash-map options)))

(defcomponent Group [drawables]
  IDraw
  (draw [this]
    (doseq [drawable drawables]
      (draw drawable)))
  IChildren
  (-children [this]
    drawables)
  IReplaceChildren
  (-replace-children [this new-children]
    (assoc this :drawables new-children)))

(defn group [& drawables]
  (Group. drawables))


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
  (draw [this]
    (push-matrix
     (translate x y 0)
     (draw drawable))))
(defn move [x y drawable]
  (Move. x y drawable))

(defcomponent Path [points]
  IBounds
  (-bounds [this]
    (let [maxx (apply max (map first points))
          maxy (apply max (map second points))]
      [maxx maxy]))
  IDraw
  (draw [this]
    (push-matrix
     (draw-lines
      (doseq [[[x1 y1] [x2 y2]] (map vector points (rest points))]
        (vertex x1 y1)
        (vertex x2 y2))))))
(defn path [& points]
  (Path. points))

(gl-import- glPushAttrib gl-push-attrib)
(gl-import- glPopAttrib gl-pop-attrib)
(defcomponent Polygon [color points]
  IBounds
  (-bounds [this]
    (let [maxx (apply max (map first points))
          maxy (apply max (map second points))]
      [maxx maxy]))
  IDraw
  (draw [this]
    (push-matrix
     (gl-push-attrib (enum  :CURRENT_BIT))
     (when color
       (apply penumbra.opengl/color color))
     (draw-polygon
      (doseq [[x y] points]
        (vertex x y)))
     (gl-pop-attrib))))
(defn polygon [color & points]
  (Polygon. color points))

(defcomponent Arc [radius rad-start rad-end steps]
  IDraw
  (draw [this]
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
  (Arc. radius rad-start rad-end 10))

(defn rectangle [width height]
  (path [0 0] [0 height] [width height] [width 0] [0 0]))

(defn filled-rectangle [color width height]
  (polygon color [0 0] [0 height]  [width height] [width 0]  [0 0]))


(defr components [])
(defn display [[dt t] state]
  (render-mode :solid)

  (let [[x-origin y-origin w h] @penumbra.opengl.core/*view*]
    (when-let [root @components]
      (with-projection (ortho-view x-origin (+ x-origin w) (+ y-origin h) y-origin -1 1)
        (push-matrix
         (load-identity)
         (TextureImpl/bindNone)
         (draw root)))))

  )


(defn close [state]
  "Called once, when application ends.")

(defn mouse-drag [[dx dy] [x y] button state]
  "Called when mouse moves with a button pressed. [dx dy] contains relative motion since last time :mouse-drag was called, and [x y] contains absolute position of the mouse. button will be equal to one of :left, :right, :center, :mouse-4, or :mouse-5. If the mouse is moving when two or more buttons are pressed, :mouse-drag will be called once for each button."
  state)


(defn box-contains? [[x y width height] [px py]]
  (and (<= px (+ x width))
       (>= px x)
       (<= py (+ y height))
       (>= py y)))


(declare mouse-handlers)
(defn mouse-move [[dx dy] [mx my] state]
  "Called the same as :mouse-drag, but when no button is pressed."
  (doseq [[htype [cx cy cw ch] handler elem] @mouse-handlers
          :when (= htype :on-hover)
          :let [hover? (and (>= mx cx)
                            (>= my cy)
                            (<= mx (+ cx cw))
                            (<= my (+ cy ch)))]]
    (when-let [new-elem (handler elem hover?)]
      (let [new-root (zzroot new-elem)]
        (r! components new-root))))
  state)


(declare mouse-handlers)
(defn mouse-down [[x y] button state]
  "Called whenever a button is pressed."
  (doseq [[htype [cx cy cw ch] handler elem] @mouse-handlers
          :when (= htype :on-click)]
    (when (and (>= x cx)
               (>= y cy)
               (<= x (+ cx cw))
               (<= y (+ cy ch)))
      (when-let [new-elem (handler elem)]
        (let [new-root (zzroot new-elem)]
          (r! components new-root)))))

  (-> state
      (assoc :move-hello? (not (:move-hello? state)))))


(defn mouse-up [[x y] button state]
  "Called whenever a button is released."
  (-> state
      (assoc :mousedown false)))


(defn mouse-click [[x y] button state]
  "Called whenever a mouse is clicked (pressed and released). Gives [x y] of where mouse was originally pressed."

  
  state)


(defn key-press [key state]
  "Called whenever a key is pressed. If the key is something that would normally show up in a text entry field, key is a case-sensitive string. Examples include “a”, “&”, and " ". If it is not, key is a keyword. Examples include :left, :control, and :escape"
  (cond

   (= :back key)
   (r! whoo-text "")

   (string? key)
   (let [new-text (str @whoo-text key)]
      (r! whoo-text new-text)))
  state)


(defn key-release [key state]
  "Called whenever a key is released."
  state)


(defn key-type [key state]
  "Called whenever a key is pressed and released. This is the only key event which will expose auto-repeated keys."
  state)

(defonce current-app (atom nil))
(defn start []
  (go
   (when @current-app
     (controller/stop! @current-app)
     (reset! current-app nil))
   (reset! *font-cache* {})
   (<! (timeout 100))
   (reset! current-app (app/create
                        {:display #'display
                         :init #'init
                         :close #'close
                         :reshape #'reshape
                         :mouse-drag #'mouse-drag
                         :mouse-move #'mouse-move
                         :mouse-down #'mouse-down
                         :mouse-up #'mouse-up
                         :mouse-click #'mouse-click
                         :key-press #'key-press
                         :key-release #'key-release
                         :key-type #'key-type}
                        {:rot 0 :fluid true
                         :width 650
                         :height 878
                         :top 0
                         :jiggle-y 0
                         :jiggle-x 0}))
   (app/start-single-thread @current-app loop/basic-loop))
  
  )


(defcomponent Button [text on-click]
  IBounds
  (-bounds [_]
    (let [blabel (label text)
          [blw blh] (bounds blabel)
          padding 20]
      [(+ blw padding)
       (+ blh padding)]))

  IDraw
  (draw [this]
    (let [blabel (label text)
          [blw blh] (bounds blabel)
          padding 20]
      (when hover?
        (penumbra.opengl/color [1 0 1]))
      (draw
       [
        (rectangle (+ blw padding) (+ blh padding))
        
        (move (/ padding 2)
              (/ padding 2)
              (label text))]))))
(defn button [text & [on-click on-hover hover?]]
  (let [on-hover (fn [button hover?]
                      (assoc button :hover? hover?))]
    (Button. text on-click)))

(defn vertical-layout [elem & more]
  (if more
    (let [[_ dy] (bounds elem)
          [_ oy] (origin elem)]
      [elem (move 0 (+ oy dy)
                  (apply vertical-layout more))])
    elem))
(defn horizontal-layout [elem & more]
  (if more
    (let [[dx _] (bounds elem)
          [ox _] (origin elem)]
      [elem (move (+ ox dx) 0
                  (apply horizontal-layout more))])
    elem))

(defzzfn zzchildren :children children)
(defmethod z/back :children [_ old-val new-val]
  (-replace-children old-val new-val))



(defr whoo-text "whoo")
(defr status "status")
(r! components [(vertical-layout
                 (label "Minecraft Foo" :font-size 35)
                 (move 0 30
                       (vertical-layout
                        (button "Connect!"
                                (fn [_]
                                  (r! status "foo'")))

                        (move
                         0 10
                         (horizontal-layout
                          (button "speak")

                          (move
                           10 10
                           [(rectangle 400 26)
                            (move 10 0
                                  [
                                   (label whoo-text)])])))))

                 (move 0 10
                       (label status)))])


(defr app-repaint!
  (do
    components
    (app/repaint! @current-app)))

;; returns box to handler
(defn find-mouse-handlers
  ([comp]
   (find-mouse-handlers comp [0 0]))
  ([comp & [[sx sy]]]
   (let [[ox oy] (origin comp)
         sx (+ ox sx)
         sy (+ oy sy)
         clicks (when (map? comp)
                  (for [[k handler] comp
                        :when (and (.startsWith (-> k name) "on-")
                                   handler)]
                    (let [[w h] (bounds comp)]
                      [k [sx sy w h] handler comp])))
         child-clicks (mapcat #(find-mouse-handlers % [sx sy]) (zzseq (zzchildren comp)))]
     (remove nil? (concat child-clicks clicks)))))
(defr mouse-handlers
  (into [] (find-mouse-handlers components)))
