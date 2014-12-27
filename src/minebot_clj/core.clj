(ns minebot-clj.core
  (:require [minebot-clj.protocol :as protocol :exclude [move]]
            [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        ] :as async]
            [clojure.stacktrace]
            [minebot-clj.astar :refer [astar manhattan-distance euclidean-distance]]
            [minebot-clj.model :as model]
            [minebot-clj.ui :as ui]
            [minebot-clj.cell :as cell]
            [minebot-clj.environment :as env :refer [set-ref shake! set-form]]
            [clojure.data.json :as json]
            )
  (:use [minebot-clj.evaluable])
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
                    DataOutputStream))
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
           (com.trolltech.qt.webkit QWebView )
           (com.trolltech.qt.webkit.QWebView$ )
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



(defn byte-input-stream [bytes]
  (-> bytes
      (ByteArrayInputStream.)
      (DataInputStream.)))

(defmulti -write-field (fn [out field-type field-val]
                         field-type))


(defmethod -write-field :bytearray
  [out _ ba]
  (io! (.write out ba 0 (count ba))))


(defmethod -write-field :unsigned-short
  [out _ i]
  (io! (.writeByte out (bit-shift-right (bit-and 0xFF00 i) 8))
       (.writeByte out (bit-and 0xFF i))))



(defmethod -write-field :int
  [out _ i]
  (io! (.writeInt out (int i))))

(defmethod -write-field :short
  [out _ i]
  (io! (.writeShort out (short i))))


(defmethod -write-field :string-utf8
  [out _ s]
  (let [b (.getBytes s "utf-8")]
    (-write-field out :varint (count b))
    (-write-field out :bytearray b)))

(defmethod -write-field :string
  [out _ s]
  (-write-field out :string-utf8 s))

(defmethod -write-field :double
  [out _ i]
  (io! (.writeDouble ^DataOutputStream out (double i))))

(defmethod -write-field :float
  [out _ i]
  (io! (.writeFloat ^DataOutputStream out (float i))))

(defmethod -write-field :bool
  [out _ b]
  (io! (.writeBoolean ^DataOutputStream out b)))



(defn color-dist [rgb1 rgb2]
;; d2 = (30*(r1-r2))**2 + (59*(g1-g2))**2 + (11*(b1-b2))**2;
  (letfn [(dist [c1 c2 k]
            (let [diff (- c1 c2)
                  newk (* k diff)]
              (* newk newk)))]
    (let [ks [30 59 11]]
      (reduce + (map dist rgb1 rgb2 ks)))))

(def colors
  [[208 208 208]
   [222 135 63]
   [170 65 187]
   [108 139 208]
   [201 189 0]
   [64 165 13]
   [206 126 150]
   [53 53 53]
   [144 152 152]
   [51 123 156]
   [128 59 194]
   [48 61 162]
   [92 58 32]
   [57 76 20]
   [169 58 55]
   [31 27 27]
   ]  )

(declare  packet-chan)
(defn socket-chan [host port inchan outchan]
  (packet-chan (Socket. host port)
               inchan
               outchan)
  )

(defn- -read-byte-bare [in]
  (io!
    (let [b (.readByte ^DataInputStream in)]
      b)))

(defn- -read-byte [in]
  (int (-read-byte-bare in)))

(defn- -read-bytearray-bare [in size]
  (io!
    (let [ba (byte-array size)]
         (.readFully ^DataInputStream in ba 0 size)
         ba)))

(defn- -read-bytearray [in size]
  (-read-bytearray-bare in size))

(defn- -read-varint
  [in]
  (loop [a (-read-byte-bare in) t 0 i 0]
    (if (= 0 (bit-and (bit-not 0x7f) a))
      (bit-or t
              (bit-shift-left (bit-and 0x7F a) (* i 7)))
      (recur (-read-byte-bare in)
             (bit-or t
                     (bit-shift-left (bit-and 0x7F a) (* i 7)))
             (inc i)))))


(declare parse-field)
(defn read-packet [in]
  (let [length (-read-varint in)
        _ (assert (pos? length) "bad packet length")
        all-data (-read-bytearray in length)
        bis (byte-input-stream all-data)
        packet-id (parse-field :varint bis)
        data (-read-bytearray bis (.available bis))]
    {:length length
     :packet-id packet-id
     :data data}))


(defmulti parse-field (fn [field-type data]
                        field-type))

(defmethod parse-field :int
  [field-type data]
  (.readInt data))

(defmethod parse-field :long
  [field-type data]
  (.readLong data))


(defmethod parse-field :float
  [field-type data]
  (.readFloat data))


(defmethod parse-field :double
  [field-type data]
  (.readDouble data))

(defmethod parse-field :unsigned-short
  [field-type data]
  (bit-and 0xFFFF (int (.readShort data))))

(defmethod parse-field :unsigned-int
  [field-type data]
  (bit-and 0xFFFFFFFF (long (.readInt data))))

(defmethod parse-field :meta
  [field-type data]
  (let [chunk-x (parse-field :int data)
        chunk-z (parse-field :int data)
        primary-bitmap (parse-field :unsigned-short data)
        add-bitmap (parse-field :unsigned-short data)]
    {:chunk-x chunk-x
     :chunk-z chunk-z
     :primary-bitmap primary-bitmap
     :add-bitmap add-bitmap}))



(defmethod parse-field :metadata
  [field-type data]
  (loop [item (parse-field :byte data)
         metadata {}]
    (if (= item 0x7F)
      metadata
      (let [index (bit-and 0x1F item)
            type (bit-shift-right item 5)
            field-type (get [:byte :short :int :float :string :slot :vector]
                            type)]
        (assoc metadata index (parse-field field-type data))))))

(defmethod parse-field :object-data
  [field-type data]
  nil)

(defmethod parse-field :varint
  [_ data]
  (let [f data]
    (loop [a (.readByte data) t 0 i 0]
      (if (= 0 (bit-and (bit-not 0x7f) a))
        (bit-or t
                (bit-shift-left (bit-and 0x7F a) (* i 7)))
        (recur (.readByte data)
               (bit-or t
                       (bit-shift-left (bit-and 0x7F a) (* i 7)))
               (inc i))))))


(defmethod parse-field :string
  [field-type data]
  (let [length (parse-field :varint data)]
    (String. (-read-bytearray data  length)
             0 length
             "utf-8")))

(defmethod parse-field :short
  [field-type data]
  (.readShort data))

(defmethod parse-field :byte
  [field-type data]
  (.readByte data))

(defmethod parse-field :unsigned-byte
  [field-type data]
  (bit-and 0xFF (short (.readByte data))))

(defmethod parse-field :bool
  [field-type data]
  (not (zero? (.readByte data))))

(declare client-packets)
(defn packet-name
  ([packet]
     (packet-name client-packets packet))
  ([packet-descriptions packet]
     (let [packet-id (if (map? packet)
                       (:packet-id packet)
                       packet)
           desc (first (get packet-descriptions packet-id))
           [pname _ _ _] desc]
       pname)))

(defn parse-packet
  ([packet]
     (parse-packet client-packets packet))
  ([packet-descriptions packet]
     (let [matching-packet-descriptions (if (map? packet-descriptions)
                                          (get packet-descriptions (:packet-id packet))
                                          [packet-descriptions])
           data (:data packet)
           parsed (when (= 1 (count matching-packet-descriptions))
                    (let [desc (first matching-packet-descriptions)
                          [pname pdirection pid fields] desc
                          data-chan (byte-input-stream data)]
                      (try
                        (let [parsed (reduce (fn [parsed field]
                                               (let [[field-name field-type _] field
                                                     field-value (parse-field field-type data-chan)]
                                                 (assoc parsed field-name field-value)))
                                             {:packet-id (:packet-id packet)
                                              :packet-name pname
                                              :packet-data data
                                              :packet-direction pdirection}
                                             fields)]
                          parsed)
                        (catch Exception e
                          (msg "parse error"  pname e)
                          {:packet-id (:packet-id packet)
                           :packet-name pname
                           :parse-error e})))
                    )]
       #_(msg "parsing " (format "%x" (:packet-id packet)) (:packet-name parsed) parsed)
       parsed)))


(defmacro defpacket [packet-name packet-id & fields]
  (let [fields (map vec (partition 3 fields))
        args (->> fields
                  (map #(nth % 2))
                  (filter symbol?))]
    `(defn ~packet-name [~@args]
      [~packet-id ~(keyword packet-name) ~(vec fields)])))



(defmethod -write-field :ignore
  [_ _ _])

(defpacket handshake
   0x00
   :protocol-version :varint 5
   :server-host :string host
   :server-port :unsigned-short port
   :next-state :varint 2)

(defpacket respawn
  0x16
  :respawn :varint 0)

(defpacket login
   0x00
   :name :string name)

(defpacket chat
  0x01
  :message :string msg)

(defpacket keep-alive
  0x00
  :keep-alive-id :int keep-alive-id)


(defmethod -write-field :byte
  [out field-type i]
  (.writeByte ^DataOutputStream out (int i)))

(defmethod -write-field :unsigned-byte
  [out field-type i]
  (.writeByte ^DataOutputStream out (int i)))

(defmethod -write-field :varint
  [out field-type i]
  (loop [a i]
    (if (= 0 (bit-and a (bit-not 0x7f)))
      (-write-field out :byte a)
      (do
        (-write-field out :byte (bit-or 0x80 (bit-and a 0x7F)))
        (recur (bit-shift-right a 7))))))


(defn set-color [out x y c]
  (let [colors (map-indexed vector colors)
        closest (->> colors
                     (sort-by (comp #(color-dist % c) second))
                     first
                     first)
        z (+ 20 (rand-int 10))]
    #_(msg (str "/setblock " "~" x " 4 " "~" y " carpet " closest " replace"))
    (-write-field out :packet (chat (str "/setblock " "~" x " " z  " ~" y " dirt " 0 " replace")))
    (-write-field out :packet (chat (str "/setblock " "~" x "  " z " ~" y " carpet " closest " replace")))
    
    )
  )

(defmethod -write-field :packet
  [out _ packet]
  (let [[packet-id packet-name fields] packet
        baos (ByteArrayOutputStream.)
        byte-out (DataOutputStream. baos)]
    (-write-field byte-out :varint packet-id)    
    (doseq [[field-name field-type field-val] fields]
      (-write-field byte-out field-type field-val))
    (let [data (.toByteArray baos)
          length (count data)]

      (-write-field out :varint length)
      #_(msg "wrote length" length (vec data))
      (-write-field out :bytearray data))))


(declare all-packets)
(defmethod -write-field :parsed-packet
  [out _ {:keys [packet-id length data]}]
  #_(msg "sending packet " (format "0x%X" packet-id) (->> (get all-packets packet-id)
                                                        (map first))
       length
       (format "0x%x" length)
       )
  (-write-field out :varint length)
  (-write-field out :varint packet-id)
  (-write-field out :bytearray (byte-array data)))



(defonce cget-packets (memoize protocol/get-packets))
(def all-packets (reduce
              (fn [xs x]
                (update-in xs [(nth x 2)] conj x))
              {}
              (cget-packets)))
(def client-packets (reduce
              (fn [xs x]
                (update-in xs [(nth x 2)] conj x))
              {}
              (filter #(= :clientbound (second %)) (cget-packets))))
(def server-packets (reduce
              (fn [xs x]
                (update-in xs [(nth x 2)] conj x))
              {}
              (filter #(= :serverbound (second %)) (cget-packets))))


(defpacket place-block
  0x08
  :x :int x
  :y :unsigned-byte y
  :z :int z
  :direction :byte direction
  :held-item :slot slot
  :cursor-x :byte cursor-x
  :cursory-y :byte cursor-y
  :cursor-z :byte cursor-z)

(defmethod -write-field :slot
  [out _ [id count damage]]
  (-write-field out :short id)
  (-write-field out :byte count)
  (-write-field out :short damage)
  (-write-field out :short -1))

(defpacket use-entity
  0x02
  :target :int target
  :mouse :byte mouse)

(defpacket look
  0x05
  :yaw :float yaw
  :pitch :float pitch
  :on-ground :bool on-ground)

;;  0x07 	Status 	Byte 	The action the player is taking against the block (see below)
;; X 	Int 	Block position
;; Y 	Unsigned Byte 	Block position
;; Z 	Int 	Block position
;; Face 	Byte 	The face being hit (see below) 

(defpacket player-digging
  0x07
  :status :byte status
  :x :int x
  :y :unsigned-byte y
  :z :int z
  :face :byte face)

;;  0x04 	X 	Double 	Absolute position
;; Stance 	Double 	Used to modify the players bounding box when going up stairs, crouching, etc…
;; Y 	Double 	Absolute position
;; Z 	Double 	Absolute position
;; On Ground 	Bool 	True if the client is on the ground, False otherwise 

(defpacket player-position
  0x04
  :x :double x
  :stance :double stance
  :y :double y
  :z :double z
  :on-ground? :bool on-ground?)

(defpacket steer-vehicle
  0x0C
  :sideways :float sideways
  :forward :float forward
  :jump? :bool jump?
  :unmount? :bool unmount?)

(defpacket position-look
  0x06
  :x :double x
  :stance :double stance
  :y :double y
  :z :double z
  :yaw :float yaw
  :pitch :float pitch
  :on-ground? :bool on-ground?
  )

(defpacket player
  0x03
  :on-ground? :bool on-ground?)


(defmacro slow [& body]
  `(do
     ~@(for [exp body]
         `(do
            (<! (timeout 3000))
            (msg "doing " ~(str exp))
            ~exp))))


(defn do-keepalive [in out]
  (goe
   (loop []
     (when-let [packet (<! in)]
       (let [keepalive? (and (zero? (:packet-id packet))
                             (= 5 (:length packet)))]
         (when keepalive?
           (let [keep-alive-id (parse-field :int (byte-input-stream (:data packet)))]
             (>! out (keep-alive keep-alive-id))))
         (recur))))
   (msg "stopping keep alive")))

(defn do-position-update [inchan outchan position looking path]
  (let [running (atom true)]
    (goe
     (loop []
       (when-let [packet (<! inchan)]
         (when (= 0x08 (:packet-id packet))
           (let [parsed (parse-packet packet)
                 {:keys [x y z on-ground pitch yaw]} parsed]
             (when @path
               (msg "error trying to move")
               (reset! path nil))
             (reset! looking [yaw pitch])
             (reset! position [x (- y 1.62) z])))
         (recur)))
     (reset! running false)
     (msg "stopping position waiter"))
    (goe
     (loop []
       (when @running
         (when @position
           (when-let [[x y z] (first @path)]
             (reset! position [(+ x 0.5) y (+ z 0.5)])
             (swap! path rest))
           (let [[x y z] @position]
             (if @looking
               (let [[yaw pitch] @looking]
                 (>! outchan (position-look x y (+ y 1.62) z yaw pitch true)))
               (>! outchan (player-position x y (+ y 1.62) z true))))
           (>! outchan (player true))
           (<! (timeout 50)))
         (recur)))
     (reset! running false)
     (msg "stopping position update"))))


(defn minecraft-successors [chunks [x y z]]
  (letfn [(get-block! [pos]
            (let [block (get chunks [:block-data pos] 0)]
              (when (= -1 block)
                (throw (Exception. (str "Hit area with no block " pos))))
              block))]
   (for [dx (range -2 3)
         dy (range -2 3)
         dz (range -2 3)
         :let [[x y z] [(+ x dx)
                        (+ y dy)
                        (+ z dz)]]
         :when (not (= 0 dx dy dz))
         :when (zero? (get-block! [x y z]))
         :when (zero? (get-block! [x (inc y) z]))
         :when (pos? (get-block!  [x (dec y) z]))]
     [x y z])))

(defn minestar [chunks start goal]
  (let [[sx sy sz] start]
    (astar start
           #(let [d (manhattan-distance % goal)]
              (if (< d 4)
                0
                d))
           (partial minecraft-successors chunks))))


(defn vdotproduct [a b]
  (map * a b))

(defn vscale [v k]
  (map #(* % k) v))

(defn vlength [v]
  (->> v
       (map #(* % %))
       (reduce +)
       (Math/sqrt)))

(defn v+ [& vs]
  (apply map + vs))

(defn v- [& vs]
  (apply map - vs))

(defn vnormalize [v]
  (let [length (vlength v)]
   (map #(/ % length) v)))

(declare image)
(def path (atom nil))
;; bugs?
;; walking on water. cactus with height more than 1. foo can't walk through grass
(def position (atom nil))
(def looking (atom nil))

#_(reset! path (minestar @world (mapv (comp int #(Math/floor %) #(+ 0.05 %) double) @position) [-150 70 22]))

(declare update-world)
(declare track-entities)
(declare follow-players)
(declare follow-ui-commands)
(declare players entities world)
(def all-chans (atom []))
(defn kill-chans []
  (doseq [ch @all-chans]
    (close! ch))
  (reset! all-chans []))

;; digital ocean
(defn do-something
  ([] (let [[host port] (protocol/discover-minecraft-server)]
        (do-something host port)))
  ([host]
   (let [host (get {:do "162.243.14.228"} host)]
     (do-something host 25565)))
  ([host port]
     (let [inchan (chan 100)
           mult (async/mult inchan)
           outchan (chan)]
       (msg "connecting")
       (kill-chans)
       (swap! all-chans conj outchan)
       (reset! path nil)
       (reset! position nil)
       (reset! players #{})
       (reset! entities {})
       (send world (constantly (model/->World {})))
       (await world)
       (socket-chan host port inchan outchan)
       (msg "finished connecting")
       
       (goe
        (msg "start")
        (>! outchan (handshake host port))
        (msg "handshook")
        (<! (timeout 1000))
        (>! outchan (login "foo2"))
        (msg "logged in")
        (<! (timeout 1000))
        (>! outchan (chat "/gamemode 1"))
        (>! outchan (chat "hello"))
            ;; (>! outchan (respawn))
        )


       (do-keepalive (async/tap mult (chan 5)) outchan)
       (do-position-update (async/tap mult (chan)) outchan position looking path)
       (update-world (async/tap mult (chan)) world client-packets)
       (track-entities (async/tap mult (chan 10)))
       #_(follow-ui-commands (async/tap mult (chan 10)) outchan)
       #_(follow-players (async/tap mult (chan 10)) outchan)



       outchan)))



(defn deflate [data]
  (let [inflater (Inflater.)
        buffer (byte-array 1024)
        baos (ByteArrayOutputStream.)]
    (.setInput inflater (byte-array data))
    (while (not (.finished inflater))
      (let [count (.inflate inflater buffer)]
        (.write baos buffer 0 count)))
    (.close baos)
    (.toByteArray baos)))



(defn block-id-seq [chunks]
  (for [[[x-base z-base] chunk] chunks
        :let [chunk (:block-data chunk)]
        [y-base chunk] (map-indexed vector chunk)
        :let [coords (for [y-offset (range 16)
                           z-offset (range 16)
                           x-offset (range 16)]
                       [(+ (* 16 x-base) x-offset)
                        (+ (* 16 y-base) y-offset)
                        (+ (* 16 z-base) z-offset)
                        ])]
        coord-block (map vector coords chunk)]
    coord-block))

(defn biome-locator [chunks]
  (reduce 
   (fn [m [pos chunk]]
     (let [biomes (distinct (:biome chunk))]
       (reduce
        (fn [m biome]
          (update-in m [biome] conj pos))
        m
        biomes)))
   {}
   chunks))

;; (def biomes (biome-locator @world))


(defn find-block [chunks block-id n]
  (->> chunks
       (block-id-seq)
       (filter #(= block-id (second %)))
       (take n)))


(defn parse-chunk-column! [meta sky-light? data]
  (let [section-keys (concat
                      [:block-data :block-meta :light-block]
                      (if sky-light? [:light-sky])
                      [:block-add])
        chunk-x (:chunk-x meta)
        chunk-z (:chunk-z meta)
        world (into {}
                    (for [section-key section-keys
                          :let [mask (if (= :block-add section-key)
                                       (:add-bitmap meta)
                                       (:primary-bitmap meta))
                                length (if (= :block-data section-key)
                                         (* 16 16 16)
                                         (* 16 16 8))]
                          chunk-y (range 16)
                          :when (not (zero? (bit-and mask (bit-shift-left 1 chunk-y))))
                          :let [chunk (model/make-chunk section-key (-read-bytearray data length))]]
                      [[section-key [chunk-x chunk-y chunk-z]]
                       chunk]))]
    (assoc world [:biome [chunk-x nil chunk-z]] (model/make-chunk :biome (-read-bytearray data (* 16 16))))))

(defn parse-map-chunk-bulk [data]
  (let [data (byte-input-stream data)
        column-count (parse-field :short data)
        chunk-data-length (parse-field :int data)
        sky-light? (parse-field :bool data)
        chunk-data-compressed (-read-bytearray data chunk-data-length)
        chunk-data (deflate chunk-data-compressed)
        chunk-data-chan (byte-input-stream chunk-data)
        metas (doall
               (map (fn [_]
                      (parse-field :meta data))
                    (range column-count)))]
    (apply merge
           (for [meta metas]
             (parse-chunk-column! meta sky-light? chunk-data-chan)))))

(defn parse-multi-block-change-packet [packet]
  (let [data (byte-input-stream (:data packet))
        chunk-x (parse-field :int data)
        chunk-z (parse-field :int data)
        record-count (parse-field :short data)
        data-size (parse-field :int data)
        records (doall
                 (for [i (range record-count)
                       :let [raw-record (parse-field :unsigned-int data)
                             block-metadata (bit-and 0x0F raw-record)
                             block-id (-> raw-record
                                          (bit-shift-right 4)
                                          (bit-and 0xFFF))
                             block-y (-> raw-record
                                         (bit-shift-right 16)
                                         (bit-and 0xFF))
                             block-z (-> raw-record
                                         (bit-shift-right 24)
                                         (bit-and 0xF))
                             block-x (-> raw-record
                                         (bit-shift-right 28)
                                         (bit-and 0xF))]]
                   {:block-metadata block-metadata
                    :block-id block-id
                    :block-y block-y
                    :block-z block-z
                    :block-x block-x}))]
    {:chunk-x chunk-x
     :chunk-z chunk-z
     :records records}))



(def world (agent (model/->World {})))
(defn update-world [ch world packet-descriptions]
  (let [switch (atom true)
        chunk-packets (chan 1000)]
    (goe
     (loop []
       (when-let [packet (<! ch)]
         (when (= 0x26 (:packet-id packet))
           (>! chunk-packets packet))
         (when (= 0x23 (:packet-id packet))
           (let [{:keys [x y z block-id block-metadata]} (parse-packet packet)]
             (send world #(-> %
                              (assoc [:block-data [x y z]] block-id)
                              (assoc [:block-meta [x y z]] block-metadata)))))
         (when (= 0x22 (:packet-id packet))
           (send world model/multi-block-change (parse-multi-block-change-packet packet)))
         (recur)))
     (reset! switch false)
     (msg "doneeeeeeeeeeeeeeeeeeeeeeeeeee!!!!!!!!!!"))
    (goe
     (loop []
       (let [packet (<! chunk-packets)]
         (when (and @switch packet)
           (let [chunks (parse-map-chunk-bulk (:data packet))]
             (send world model/add-chunks chunks))
           (recur))))
     (msg "yaaaaaaaaaaaaaaaaaaaaaaaaaa~~~~~~~~~"))))

(defn to-fixed-number [n]
  ;; double = (double)abs_int / 32;
  (/ (double n)
     32))

(defn from-fixed-number [n]
  (* (int n)
     32))
(def players (atom #{}))
(def entities (atom {}))
(defn track-entities [ch]
  (goe
   (loop []
     (when-let [packet (<! ch)]
       (let [packet-name (packet-name packet)]
         (case packet-name
           (:entity-properties :entity-metadata :entity-head-look :entity-velocity :entity-look )
           nil

           :spawn-player
           (let [{:keys [entity-id x y z current-item player-name player-uuid] :as spawned-player} (parse-packet packet)]
             (swap! players conj entity-id)
             (swap! entities update-in [entity-id]
                    (fn [entity]
                      (assoc entity
                        :x (to-fixed-number x)
                        :y (to-fixed-number y)
                        :z (to-fixed-number z)
                        :current-item current-item
                        :player-name player-name
                        :player-uuid player-uuid))))
           

           (:spawn-object :spawn-mob)
           (let [{:keys [entity-id type x y z]} (parse-packet packet)]
             (swap! entities update-in [entity-id]
                    (fn [entity]
                      (assoc entity
                        :x (to-fixed-number x)
                        :y (to-fixed-number y)
                        :z (to-fixed-number z)
                        :type type))))
           
           :entity-teleport
           (let [{:keys [x y z entity-id]} (parse-packet packet)]
             (swap! entities update-in [entity-id]
                    (fn [entity]
                      (assoc entity
                        :x (to-fixed-number x)
                        :y (to-fixed-number y)
                        :z (to-fixed-number z)))))
           

           (:entity-look-and-relative-move
            :entity-relative-move)
           (let [{:keys [dx dy dz entity-id]} (parse-packet packet)]
             (swap! entities update-in [entity-id]
                    (fn [{:keys [x y z] :as entity}]
                      (if (nil? entity)
                        entity
                        (assoc entity
                          :x (+ x (to-fixed-number dx))
                          :y (+ y (to-fixed-number dy))
                          :z (+ z (to-fixed-number dz)))))))
           
           nil))
       (recur)))
   (msg "stopped tracking entities"))
  )

(defn integerize-position [pos]
  (mapv (comp int #(Math/floor %) #(+ 0.05 %) double) pos))

(defn try-move-to-player [out]
  (when-let [other-entity-id (first @players)]
    (let [other-player (get @entities other-entity-id)
          other-position (integerize-position ((juxt :x :y :z) other-player))]
      (put! out (chat (str  "calculating path to " other-position)))
      (if-let [new-path (try
                          (minestar @world (integerize-position @position) other-position)
                          (catch Exception e
                            (msg e)
                            nil))]
        (do
          (reset! path new-path)
          (put! out (chat "on my way!")))
        (put! out (chat  "no path found!"))))))

(defn follow-players [ch out]
  (let [done? (atom false)]
   (goe
    (loop []
      (when-let [packet (<! ch)]
        (recur)))
    (reset! done? true))
   (goe
    (<! (timeout 5000))
    (loop []
      (<! (timeout 1000))
      (when (not @done?)
        (when-let [other-entity-id (first @players)]
          (let [other-player (get @entities other-entity-id)
                other-position (integerize-position ((juxt :x :y :z) other-player))]
            (when (and (> (euclidean-distance @position other-position) 10)
                       (empty? @path))
              (>! out (chat (str  "i'm coming! " other-position)))
              (if-let [new-path (try
                                  (minestar @world (integerize-position @position) other-position)
                                  (catch Exception e
                                    (msg e)
                                    nil))]
                (do
                  (reset! path new-path)
                  (>! out (chat "on my way!")))
                (>! out (chat  "no path found!"))))))
        (recur)))
    (msg "stopped following")))
  )



(defn follow-ui-commands [ch out]
  (let [done? (atom false)
        command-chan (ui/command-ui)]
    (goe
     (loop []
       (when-let [packet (<! ch)]
         (recur)))
     (reset! done? true))
    (goe
     (msg "starting commands. ")
     (loop []
       (let [command (<! command-chan)]
         (msg "command: " command)
         (when (and command (not @done?))
           (case command
             :speak
             (do
               (>! out (chat "bark!")))

             :up
             (let [[x y z] (integerize-position @position)]
               (reset! path [[x
                              (inc y)
                              z]]))

             :down
             (let [[x y z] (integerize-position @position)]
               (reset! path [[x
                              (dec y)
                              z]]))

             :dig
             (do
               (when @position
                 (let [face 0
                       [px py pz] (integerize-position @position)
                       [x y z] [(inc px) (dec py) pz]
                       status 2]
                   (>! out (player-digging 0 x y z 0))
                   (>! out (player-digging status x y z 0)))))

             :turn
             (swap! looking
                    (fn [looking]
                      (if looking
                        (let [[yaw pitch] looking]
                          [(+ 90 yaw) pitch])
                        [0 0])))


             :forward
             (let [[x y z] (integerize-position @position)]
               (reset! path [[(inc x)
                              y
                              z]]))

             :left
             (let [[x y z] (integerize-position @position)]
               (reset! path [[x
                              y
                              (inc z)]]))


             :right
             (let [[x y z] (integerize-position @position)]
               (reset! path [[x
                              y
                              (dec z)]]))

             :back
             (let [[x y z] (integerize-position @position)]
               (reset! path [[(dec x)
                              y
                              z]]))

             :come
             (do
               (try-move-to-player out))
             
             :exit
             (do
               (msg "got a quit message!")
               (kill-chans)))
           (recur))))
     (msg "stopped accepting commands")))
  )



(def ignored (atom #{}))
(reset! ignored
        #{0 3 4})

(def ignored-from-server (atom #{}))
(reset! ignored-from-server
        #{ 25 21 18 28 23 22 41 32 19 3 0 24 15 40 56 53 26 4 10 5
          35 38
          })

(defn forward [port]
  (let [cin (chan)
        cout-orig (chan)
        cout (chan)
        _ (go
           (let [handshake (<! cout-orig)]
             (msg "handshake: " handshake
                  (-> handshake :data seq))
             (>! cout handshake))
           (<! cout-orig)
           (>! cout (login "phronmophobic"))
           (async/pipe cout-orig cout))
        ;; _ (async/pipe cout-orig cout)
        cout-mult (async/mult cout)        
        sin (chan)
        sout (chan)
        sout-mult (async/mult sout)
        ssocket (Socket. "0.0.0.0" 25565)
        server-socket (ServerSocket. port)]
    (try
      (packet-chan (.accept server-socket) sin (async/tap sout-mult (chan)))
      (packet-chan ssocket cin (async/tap cout-mult (chan)))
      (async/pipe cin sout)
      (async/pipe sin cout-orig)

      #_(goe
       (let [cout (async/tap cout-mult (chan))]
         (msg "foo" (<! cout))
         (async/untap cout-mult cout)))
      
      #_(update-world (async/tap sout-mult (chan)) client-packets)

      ;; (update-world (async/tap cout-mult (chan)) server-packets ignored)

      cout
      (finally
        (.close server-socket)))))



(defn packet-chan [socket inchan outchan]
  (let [in (DataInputStream. (.getInputStream socket))
        out (DataOutputStream. (.getOutputStream socket))]
    (thread
     (try
       (loop []
         (when-let [packet (read-packet in)]
           (>!! inchan packet)
           (recur)))
       (msg "finished reading")
       (catch Exception e
         (msg "error reading packet" (str e)))
       (finally
         (.close in)
         (.close out)
         (close! outchan)
         (close! inchan))))
    (thread
     (try
       (loop []
         (when-let [packet (<!! outchan)]
           (if (map? packet)
             (-write-field out :parsed-packet packet)
             (-write-field out :packet packet))
           (.flush out)
           (recur)))
       (msg "finished writing")
       (catch Exception e
         (msg "error writing packet"(str e)))
       (finally
         (.close in)
         (.close out)
         (close! inchan)
         (close! outchan))))))









(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (msg "Hello, World!"))



#_(def image (read-string (slurp "/var/tmp/img.out")))

;; (defn rand-move [n]
;;   (let [r (fn []
;;             (- (int (/ n 2)) (rand-int n)))
;;         x (r)
;;         y (r)
;;         z (r)]
;;     (chat (str "/tp " " ~" x " ~0" " ~"z))))

;;  (>!! ch (chat "/tp foo robertsonfamily3"))
;; (go
;;  (>! ch (respawn))
;;  (>! ch (chat "/gamemode 1"))
;;  (dotimes [i 50]
;;    (<! (timeout 300))
;;    (>! ch (chat "/tp foo robertsonfamily3"))
;;    (<! (timeout 100))
;;    (>! ch (rand-move 30))
;;    (>! ch (chat  "/setblock ~0 4 ~0 torch"))))

;; (go
;;  (>! ch (respawn))
;;  (>! ch (chat "/gamemode 1"))
;;  (>! ch (chat "/tp foo robertsonfamily3"))
;;  (>! ch (chat "/tp ~ ~10 ~"))
 
;;  (dotimes [i 10]
;;    (<! (timeout 300))
;;    (>! ch (rand-move 10))
;;    (>! ch (chat  "/setblock ~ ~ ~ minecraft:flowing_water"))))


;; (go
;;  (let [points (for [x (range 0 200 3)
;;                     z (range 0 133 3)] 
;;                 [x z])]

;;    (doseq [[x z] (take 30 (shuffle points))]
;;      (<! (timeout 1000))
;;      (let [y 0
;;            move (str "/tp " x " " y " " z)
;;            move (str "/tp foo phronmophobic")
;;            tnt (str "/summon " " PrimedTnt" )
;;            spider (str "/summon " "Spider")
;;            water (str "/setblock " (inc x) " " (dec y) " " z " " "minecraft:flowing_water")
;;            torch (str "/setblock " (inc x) " " y " " z " " "minecraft:redstone_torch")
;;            ;; chmsg (str "/setblock " x " " y " " z " " "minecraft:tnt")
;;            ]
;;        (msg tnt)
;;        (>! ch (chat move))
;;        (>! ch (chat tnt)))))
;;  (msg "done!"))





#_(defn find [chunks id [x y z] radius]
  (for [x (range (- x radius) (+ x radius))
        z (range (- z radius) (+ z radius))
        y (range 254)
        :let [block (get chunks [:block-data [x y z]])]
        :when (= id block)]
    [x y z]))
;; (def ll (find @world 50 [90 68 -358] 200))
;; (clojure.pprint/pprint (reverse (sort-by second ll)))


(defn ui-work* [work]
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
    (QApplication/invokeLater work)))

(defmacro ui-work [& body]
  `(let [outer-ns# *ns*
         work# (fn []
                 (binding [*ns* outer-ns#]
                   (try
                     ~@body
                     (catch Exception e#
                       (msg e#)
                       (msg (with-out-str (clojure.stacktrace/print-stack-trace e#)))))))]
     (ui-work* work#)))




;; (barf)

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

(def handlers (atom {}))

(defn button [text handler]
  (let [key (hash handler)]
    (swap! handlers assoc key handler)
    [:button {:onclick (str "send("key"); return false;")}
     text]))

(definterface ChannelInterface
  (^void put [^String val]))




(let [outer-ns *ns*]
  (defn channel-bridge []
    (let [ch (chan)]
      (go
        (loop []
          (binding [*ns* outer-ns]
            
            (when-let [str-val (<! ch)]
              (try
               (let [val (json/read-str str-val)
                     [handler-hash & args] val
                     handler (get @handlers handler-hash)]
                 (msg args)
                 (when handler
                   (apply handler args)))
               (catch Exception e
                 (msg (with-out-str
                        (clojure.stacktrace/print-stack-trace e)))))
              (recur))
            )))
      (proxy [QObject minebot_clj.core.ChannelInterface] []
        (put [^String str-val]
          (put! ch str-val))))))

(defn input [text handler]
  (let [key (hash handler)]
    (swap! handlers assoc key handler)
    [:input {:onchange (str "send("key", event.target.value); return false;")
             :value text
             :type "text"
             :size "60"}]))

(defn textarea []
  [:textarea {:rows 30
              :cols 100}])

(defr status nil)
(defr ch nil)
(defr speech "bark!")
(defr summon "Pig")
(defr custom-command "")
(defr html
  [:html
   [:head
    [:script {:type "text/javascript"}
     "function send(){
try{
window.channels.put( JSON.stringify(Array.prototype.slice.call(arguments) ));
}catch (e){
alert(e + '');
}
}
"]
    [:body
     [:h1 "Foo."]
     [:div "Host:"]
     [:input {:type "text"
              :value "162.243.14.228"}]
     [:br]
     (button "Connect"
             (fn []
               (try
                (let [mine-ch (do-something :do)]
                  (r! ch mine-ch))
                (catch Exception e
                  (msg e)))))
     [:button "Connect LAN"]
     [:hr]
     [:h3 "Commands"]

     (button "Speak!"
             (fn []
               (put! ch (chat speech))))
     (input speech
            (fn [s]
              (r! speech s)))
     [:br]

     (button "Come"
             (fn []
               (put! ch (try-move-to-player ch))))
     [:br]
     (button "Respawn"
             (fn []
               (put! ch (respawn ))))
     [:br]
     (button "Creative mode"
             (fn []
               (put! ch (chat "/gamemode 1"))))
     [:br]
     (button "Make it day"
            (fn []
              (put! ch (chat "/time set day"))))
     [:br]
     (button "Summon"
             (fn []
               (put! ch (chat (str "/summon " summon)))))
     (input summon
            (fn [s]
              (r! summon s)))
     [:br]

     (button "Command"
             (fn []
               (put! ch (chat custom-command))))
     (input custom-command
            (fn [s]
              (r! custom-command s)))
     [:br]
     (button "up"
             (fn []
               (let [[x y z] (integerize-position @position)]
               (reset! path [[x
                              (inc y)
                              z]]))))
     [:br]
     (button "down"
             (fn []
               (let [[x y z] (integerize-position @position)]
                 (reset! path [[x
                                (dec y)
                                z]]))))
     [:br]
     (button "turn"
             (fn []
               (swap! looking
                    (fn [looking]
                      (if looking
                        (let [[yaw pitch] looking]
                          [(+ 90 yaw) pitch])
                        [0 0])))))
     [:br]
     (button "dig"
             (fn []
               (do
                 (when @position
                   (let [face 0
                         [px py pz] (integerize-position @position)
                         [x y z] [(inc px) (dec py) pz]
                         status 2]
                     (put! ch (player-digging 0 x y z 0))
                     (put! ch (player-digging status x y z 0)))))))
     [:br]
     (button "forward"
             (fn []
               (let [[x y z] (integerize-position @position)]
               (reset! path [[(inc x)
                              y
                              z]]))))
     [:br]
     (button "left"
             (fn []
               (let [[x y z] (integerize-position @position)]
                 (reset! path [[x
                                y
                                (inc z)]]))
               ))
     [:br]
     (button "right"
             (fn []
               (let [[x y z] (integerize-position @position)]
               (reset! path [[x
                              y
                              (dec z)]]))))
     [:br]
     (button "back"
             (fn []
               (let [[x y z] (integerize-position @position)]
                 (let [[x y z] (integerize-position @position)]
                   (reset! path [[(dec x)
                                  y
                                  z]])))))

     [:br]

     (textarea)
     [:br]
     (button "Execute python"
             (fn []))
     
     
     
]
    ]])





(defr minebot-ui nil)

(let [outer-ns *ns*]
  (defn -main [& args]
    (try
     (binding [*ns* outer-ns]
       (r! minebot-ui
           (cell/show-ui :minebot
                         [:QWebView
                          {:html
                           (hiccup.page/html5
                            html)
                           :bridge [["channels" (channel-bridge)]]}])))
     
     (catch Exception e
       (msg (with-out-str
              (clojure.stacktrace/print-stack-trace e)))
       nil))
    (<!!
     (let [quit-ch (chan)]
       (.execute
        (.getNonBlockingMainQueueExecutor (com.apple.concurrent.Dispatch/getInstance))
        (fn []
          (try
            (>!! quit-ch true)
            (catch Exception e
              (msg e)))))
       quit-ch))))
