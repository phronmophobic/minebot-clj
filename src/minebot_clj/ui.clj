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

                                 )
           (com.trolltech.qt.core QCoreApplication)
           com.trolltech.qt.QSignalEmitter))

(definterface ISlot
  (^void fire []))

(defmacro qt [& body]
  `(QApplication/invokeLater
    (fn []
      ~@body)))

(def msg (atom []))
(defmacro on-click [button fn]
  `(qt
    (swap! msg conj "invoking click andler")
    (.. ~button clicked (connect
                         ~fn
                         "invoke()"))))


(defn init [] (QApplication/initialize (make-array String 0)))
(def button (atom nil))
(def ch (atom nil))
(defn send-message []
  (put! @ch :forward))
(defn command-ui []
  
  (reset! button nil)
  (reset! ch (chan (async/dropping-buffer 10)))
  (.execute (.getNonBlockingMainQueueExecutor (com.apple.concurrent.Dispatch/getInstance))
            (fn []
              (init)
              (swap! msg conj "init")
              (try
                (let [app (QCoreApplication/instance)]
                  (reset! button (doto (QPushButton. "Forward") (.show)))
                  (on-click @button
                            send-message
                            #_(fn []
                              #_(swap! msg conj "click.")
                              (put! @ch :forward)))
                  (swap! msg conj "about to exec")
                  (.exec app)
                  (close! @ch)
                  (reset! ch nil))
                
                (catch Exception e
                  (swap! msg conj e)))))
  @ch)





