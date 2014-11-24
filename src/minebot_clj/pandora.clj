(ns minebot-clj.pandora
  (:require [minebot-clj.cell :refer [ref-environment set-form shake! show-ui set-ref  msg]]
            [net.cgrand.enlive-html :as enlive]
            [minebot-clj.ui :as ui :refer [connect]]
            [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        ] :as async])
  (:use [minebot-clj.evaluable])
  (:import com.trolltech.qt.core.QCoreApplication)
  (:gen-class))


(def renv (atom (ref-environment)))
(let [ns *ns*]
  (defmacro r! [name form]
    `(do
       (let [form# (quote ~form)
             evaluable# (if (evaluable? form#)
                          form#
                          (->ClojureEvaluable ~ns form#
                                              (into {}
                                                    [~@(for [[k _] &env]
                                                         [(list 'quote k)
                                                          k])])))
             deps# (-> (dependencies evaluable#)
                       (->> (remove (fn [name#]
                                      (when-let [var# (ns-resolve ~ns name#)]
                                        (-> var# meta :reactive? not)))))
                       set 
                       (disj ~name))]
         (swap! renv set-form (quote ~name) evaluable# deps#)
         (dosync
          (shake! (deref renv) (quote ~name)))
         (when (instance? clojure.lang.IDeref ~name)
           (deref ~name))))))
(defmacro defr [name form]
  `(do
     (let [ref# (ref nil)]
      (defonce ~(vary-meta name assoc :reactive? true) ref#)
      (swap! renv set-ref (quote ~name) ref#))
     (r! ~name ~form)))



(defn content= [content]
  (enlive/pred #(= (list content) (:content %))))

(defn signed-in? [html]
  (boolean
   (seq (enlive/select html
                       [(content= "smith.adriane@gmail.com")]))))

(defr send-javascript! (fn [_] (msg "no javscript yet!")))

(defn play! []
  (send-javascript!
   "$('.playButton a').click()"))
(defn pause! []
  (send-javascript!
   "$('.pauseButton a').click()"))
(defn toggle-play! []
  (send-javascript!
   "var pauseButton = $('.pauseButton a:visible');
var playButton =  $('.playButton a:visible');
playButton.click();
pauseButton.click();")
  )
(defn skip! []
  (send-javascript!
   "$('.skipButton a').click()"))

(defr pandora-ui
  [:QWebView {:url "http://pandora.com/account/sign-in"
              :plugins true
              :loadFinished
              (fn [pui]
                (when-let [webview (-> pui meta :node)]
                  (let [page (.page webview)
                        frame (.currentFrame page)
                        html (enlive/html-snippet
                              (.toHtml frame))
                        url (.toString (.-url webview))]
                    (r! send-javascript!
                        (fn [js]
                          (ui/qt
                           (-> webview
                               .page
                               .currentFrame
                               (.evaluateJavaScript (str js "; null;"))))))

                    (when (not (signed-in? html))
                      (.evaluateJavaScript frame
                                           "document.getElementsByName(\"email\")[0].value =\"smith.adriane@gmail.com\";null;")))))}])


(defn -main [& args]

  (show-ui :pandora
           @pandora-ui)
  
  (future
    (require 'keymaster.core)
    (defonce provider ((resolve 'keymaster.core/make-provider)))
    ((resolve 'keymaster.core/register) provider "F8" (fn [arg]
                                               (toggle-play!)))
    ((resolve 'keymaster.core/register) provider "F9" (fn [arg]
                                               (skip!))))
  (let [ch (chan)]
    (ui/qt
     (connect (.aboutToQuit (QCoreApplication/instance))
              (fn []
                (put! ch true))))
    (<!! ch)))
