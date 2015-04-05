(ns minebot-clj.modicum
  (:require [minebot-clj.penumbra :as pen
             :refer [IComponent
                     defcomponent
                     IBounds
                     IMouseDown
                     IKeyPress
                     IDraw
                     IChildren
                     IBounds
                     IOrigin
                     rectangle
                     draw
                     center
                     label
                     find-offset
                     vertical-layout
                     horizontal-layout
                     bounds
                     origin
                     make-cid
                     spacer
                     button
                     move
                     key-handler
                     scroll-area
                     text-input
                     start
                     ]]
            [minebot-clj.environment :as env :refer [defr r! rv! ru! r? with-renv]])
  (:require [minebot-clj.evaluable :refer
             [evaluate
              evaluable?
              ->ClojureEvaluable
              constant-evaluable
              dependencies]])
  (:require [clj-pdf.core :as pdf])
  (:require [environ.core :refer [env]])
  (:require [taoensso.faraday :as far])
  (:require [clojure.algo.graph :as graph])

  (:use [compojure.route :only [files not-found]]
        [compojure.handler :only [site]] ; form, query params decode; cookie; session, etc
        [compojure.core :only [defroutes GET POST DELETE ANY context]]
        org.httpkit.server)
  )


(defn mouse-thing [mouse-fn]
  (reify
    IComponent
    (cid [this]
      (pen/make-cid "mouse-thing"))
    IDraw
    (draw [this state]
      nil)
    IMouseDown
    (-mouse-down [this [mx my]]
      (mouse-fn mx my))))

(defn key-thing [key-fn]
  (reify
    IComponent
    (cid [this]
      (pen/make-cid "mouse-thing"))
    IDraw
    (draw [this state]
      nil)
    IKeyPress
    (-key-press [this key]
      (key-fn key))))


(defr myenv (env/environment))


(add-watch myenv :myrepaint
           (fn [k r old new]
             (dosync
              (env/shake! (deref (env/get-or-create-renv)) 'components))))
(defr event-handlers
  (pen/make-event-handlers components))

(defr route-list [])

(defn key-handler2 [text val]
  (fn [s]
    (cond
     (= s :return)
     (env/set-value val (env/get-renv-value text) #{})

     :else
     ((key-handler text) s))
    (dosync
     (env/shake! (deref (env/get-or-create-renv)) 'components))))



(defn handler [request]
  (let [found (first
               (for [[route-ref content-ref _] @route-list
                      :let [route @route-ref
                            content @content-ref]
                      :when (= (:uri request) route)]
                  {:status 200
                   :body content}))]
    (if found
      found
      {:status 200
       :body "not found"}))
  )


(defr stop-server nil)

(defr components
  [(key-thing
    ~(fn [s]
       (when (and (= s "n")
                  (nil? pen/*focus*))
         (dosync
          (with-renv (env/get-or-create-renv)
            [route# "/"
             route-text# "/"
             route-input# (text-input route-text# (key-handler2 'route-text# 'route#))
             content# "hi"
             content-text# "hi"
             content-input# (text-input content-text# (key-handler2 'content-text# 'content#))
             elem# (horizontal-layout
                    route-input#
                    content-input#)
             route-list ~(conj (r? route-list) [(env/get-renv-ref 'route#)
                                                (env/get-renv-ref 'content#)
                                                (env/get-renv-ref 'elem#)])])))))
   (apply
    vertical-layout
    (horizontal-layout
     (button "Start"
             ~(fn []
                (when-not @stop-server
                  (let [stop-fn (run-server #'handler {:port 8082})]
                    (rv! stop-server stop-fn)))))
     (spacer 20 0)
     (button "Stop"
             ~(fn []
                (when @stop-server
                  (@stop-server)
                  (rv! stop-server nil)
                  ))))
    (for [[_ _ elem-ref] route-list]
      @elem-ref))])

;; (def my-routes
;;   (routes
;;    (GET "/foo" [] "Hello Foo")
;;    (GET "/bar" [] "Hello Bar")))






