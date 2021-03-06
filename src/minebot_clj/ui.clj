(ns minebot-clj.ui
  (:require [minebot-clj.protocol :as protocol :exclude [move]]
            [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        ] :as async]
            [clojure.stacktrace]
            [minebot-clj.astar :refer [astar manhattan-distance euclidean-distance]]
            [minebot-clj.model :as model]
            )
  (:import (java.net Socket ServerSocket)
           java.util.zip.Inflater
           (java.io PrintWriter
                    InputStreamReader
                    BufferedReader
                    BufferedWriter
                    BufferedOutputStream
                    BufferedInputStream
                    ByteArrayOutputStream
                    ByteArrayInputStream
                    DataInputStream
                    DataOutputStream)
           (com.trolltech.qt.gui QApplication QPushButton
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
                                 
                                 )
           (com.trolltech.qt.core QCoreApplication)
           com.trolltech.qt.QSignalEmitter))


(defmacro qt [& body]
  `(QApplication/invokeLater
    (fn []
      ~@body)))

(def msg (atom []))

(defprotocol ISignal
  (connect [signal fn]))

(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))


;; we need to keep strong references to callback functions
;; or else they get garbage collected and stop working.
(def ALL_SIGNALS (atom []))
(extend-type com.trolltech.qt.QSignalEmitter$AbstractSignal
   ISignal
   (connect [signal callback]
     (when (empty? @ALL_SIGNALS)
       (let [cleanup (fn [] (swap! ALL_SIGNALS empty))] 
         (swap! ALL_SIGNALS conj cleanup)
         (.connect (.aboutToQuit (QCoreApplication/instance))
                   cleanup "invoke()")
         ))
     (swap! ALL_SIGNALS conj callback)
     (try
       (let [pcount (if (instance? clojure.lang.RestFn callback)
                      (arg-count signal)
                      (arg-count callback))
             invoke-str (str "invoke("
                             (apply str (interpose ","
                                                   (repeat pcount "Object")))
                             ")")]
         (.connect signal
                   callback
                   invoke-str))
       (catch Exception e
         (swap! msg conj (str "error " e))))))






(defn init [] (QApplication/initialize (make-array String 0)))
(def app (atom nil))
(def ch (atom nil))


#_(use '[clojure.reflect :only [reflect]])
(defn command-ui []
  (reset! msg [])
  (reset! ch (chan (async/dropping-buffer 10)))
  (.execute (.getNonBlockingMainQueueExecutor (com.apple.concurrent.Dispatch/getInstance))
            (fn []
              
              (when (not @app)
                (init))
              (swap! msg conj "init")
              (try
                (reset! app (QCoreApplication/instance))
                (let [dialog (QDialog.)
                      layout (QVBoxLayout.)
                      commands [:come
                                :speak
                                :forward
                                :back
                                :left
                                :right
                                :up
                                :down
                                :turn
                                :dig
                                ]]
                  (doseq [command commands
                          :let [txt (name command)
                                button (doto (QPushButton. txt)
                                         (.setAutoDefault false))]]
                    (.addWidget layout
                                button)
                    (qt
                     (connect (.clicked button)
                              (fn []
                                (put! @ch command)))))
                  (.setLayout dialog layout)
                  (qt
                   (connect (.aboutToQuit @app)
                            (fn []
                              (put! @ch :exit))))
                  (.show dialog)
                  (swap! msg conj "about to exec")
                  (.exec @app)
                  (close! @ch)
                  (reset! ch nil)
                  (reset! app nil))
                
                (catch Exception e
                  (swap! msg conj e)))))
  @ch)



(defn test-ui []
  (go
   (let [ch (command-ui)]
     (loop []
       (when-let [command (<! ch)]
         (println command)
         (recur))))))


