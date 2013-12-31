(ns minebot-clj.core
  (:require [minebot-clj.protocol :as protocol :exclude [move]]
            [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        ] :as async]
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
                    DataOutputStream))
  (:gen-class))

(defn chan-seq!
  ([n ch] (chan-seq! (async/take n ch)))
  ([ch]
     (loop [x (<!! ch)
            xs []]
       (if x
         (recur (<!! ch) (conj xs x))
         xs))))


(defn chan-take! [n ch]
  (doall
   (for [i (range n)]
     (<!! ch))))



(defmulti -write-field (fn [out field-type field-val]
                         field-type))


(defmethod -write-field :bytearray
  [out _ ba]
  (io! (.write out ba 0 (count ba))))

;; (defn- -write-bytearray [conn ba]
;;   )


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


(def out (agent nil))
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
  (msg "packet channing")
  (packet-chan (Socket. host port)
               inchan
               outchan)
  (msg "finished packet channding")
  )

;; (defn connect [host port & [inchan outchan]]
;;   (let [socket (Socket. host port)
;;         in (DataInputStream. (.getInputStream socket))
;;         out (DataOutputStream. (.getOutputStream socket))
;;         conn (ref {:in in :out out})]
;;     (when inchan
;;       (thread
;;        (try
;;          (while (nil? (:exit @conn))
;;            (let [packet (read-packet conn)]
;;              (>!! inchan packet)))
;;          (catch Exception e
;;            (msg "error reading packet" (str e)))
;;          (finally
;;            (close! inchan)))))
;;     (when outchan
;;       (thread
;;        (try
;;          (let [out (:out @conn)]
;;            (loop []
;;              (when-let [packet (<!! outchan)]
;;                (-write-field out :packet packet)
;;                (.flush (:out @conn))
;;                (recur)))
;;            (msg "finished writing"))
;;          (catch Exception e
;;            (msg "error writing packet"(str e))))
;;        (.close (:out @conn))
;;        (.close (:in @conn))
;;        (close! inchan)
;;        (close! outchan)
;;        )
;;     (msg "started")
;;     conn)))

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
  (vec (-read-bytearray-bare in size)))

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
        data-chan (async/to-chan (-read-bytearray in length))
        packet-id (parse-field :varint data-chan)
        data (chan-seq! data-chan)]
    {:length length
     :packet-id packet-id
     :data data}))


(defmulti parse-field (fn [field-type data]
                        field-type))



(defn nbyte-number [n data]
  (reduce (fn [x [b i]]
             (bit-or x (bit-shift-left (bit-and 0xFF b) (* i 8))))
           0
           (map vector (reverse (chan-seq! n data)) (range))))

(defmethod parse-field :int
  [field-type data]
  (let [n (nbyte-number 4 data)]
      (if (> n 0x7FFFFFFF)
        (int (+ -1 (- n 0xFFFFFFFF)))
        n)))

(defmethod parse-field :long
  [field-type data]
  (nbyte-number 8 data))


(defmethod parse-field :float
  [field-type data]
  (Float/intBitsToFloat (parse-field :int data)))


(defmethod parse-field :double
  [field-type data]
  (Double/longBitsToDouble (nbyte-number 8 data)))

(defmethod parse-field :unsigned-short
  [field-type data]
  (parse-field :short data))

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


(defmethod parse-field :varint
  [_ data]
  (let [f data]
    (loop [a (<!! f) t 0 i 0]
      (if (= 0 (bit-and (bit-not 0x7f) a))
        (bit-or t
                (bit-shift-left (bit-and 0x7F a) (* i 7)))
        (recur (<!! f)
               (bit-or t
                       (bit-shift-left (bit-and 0x7F a) (* i 7)))
               (inc i))))))


(defmethod parse-field :string
  [field-type data]
  (let [length (parse-field :varint data)]
    (String. (byte-array (chan-seq! length data))
             0 length
             "utf-8")))

(defmethod parse-field :short
  [field-type data]
  (nbyte-number 2 data))

(defmethod parse-field :byte
  [field-type data]
  (byte (<!! data)))

(defmethod parse-field :unsigned-byte
  [field-type data]
  (short (<!! data)))

(defmethod parse-field :bool
  [field-type data]
  (not (zero? (<!! data))))

(declare client-packets)
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
                          data-chan (async/to-chan data)]
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
                          (when (<! data-chan)
                            (msg "had some left over data for " pname))
                          parsed)
                        (catch Exception e
                          #_(msg "parse error" e)
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
   :protocol-version :varint 4
   :server-host :string host
   :server-port :unsigned-short port
   :next-state :varint 2)

(defpacket respawn
  0x16
  :respawn :byte 0)

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
      (msg "wrote length" length (vec data))
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
  (go
   (loop []
     (when-let [packet (<! in)]
       (let [keepalive? (and (zero? (:packet-id packet))
                             (= 5 (:length packet)))]
         (when keepalive?
           (let [keep-alive-id (parse-field :int (:data packet))]
             (>! out (keep-alive keep-alive-id))))
         (recur))))
   
   (msg "stopping keep alive")))
(declare image)
(defn do-something
  ([] (do-something 25565))
  ([port] (do-something "0.0.0.0" port))
  ([host port]
     (let [inchan (chan 10)
           mult (async/mult inchan)
           outchan (chan)]
       (msg "connecting")
       (socket-chan host port inchan outchan)
       (msg "finished connecting")
       (let [position (atom nil)
             running (atom true)]
         (go
          (let [inchan (async/tap mult (chan))]
            (loop []
              (when-let [packet (<! inchan)]
                (when-let [parsed (parse-packet packet)]
                  (when (= :player-position-and-look (:packet-name parsed))
                    (msg "got player position packet")
                    (let [{:keys [x y z on-ground pitch yaw]} parsed]
                      (reset! position parsed)
                      (msg parsed)
                      #_(>! outchan (position-look x (+ y 1.62) y z yaw pitch on-ground)))))
                (recur))))
          (reset! running false)
          (msg "stopping position waiter"))
         (go
            (try
              (loop [i 0]
                (when @position
                  (when (zero? (mod i 2000))
                    (>! outchan (look 7.8 0 false)))
                  (when (= 1000 (mod i 2000))
                    (>! outchan (look 14.85 -88.05 false)))
                  (when (= 500 (mod i 1000))
                    (let [{:keys [x y z]} @position]
                      (>! outchan (player-position x (- y 1.62) y z true))))
                  (when (zero? (mod i 1000))
                    (let [{:keys [x y z]} @position]
                      (>! outchan (player-position x (- y 1.62) y (inc z) true))))
                  (>! outchan (player true))
                  (<! (timeout 50)))
                (when @running
                  (recur (+ i 50))))
              (reset! running false)
              (msg "stopping position update")
              
              )))
       (go
        (>! outchan (handshake host port))
        (<! (timeout 3000))
        (>! outchan (login "foo2"))
        (<! (timeout 3000))
        #_(>! outchan (chat "/gamemode 1"))

        (do-keepalive (async/tap mult (chan)) outchan)
        (>! outchan (chat "hello"))
        ;;     (>! outchan (respawn))

        #_(slow
           
           (>! outchan (chat "/tp 2 5 -3"))
           (>! outchan (chat "/give foo fireworks"))
           
           (>! outchan (respawn))
           (>! outchan (use-entity 0 1))))
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


(defn deep-merge
  "Recursively merges maps. If keys are not maps, the last value wins."
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))


(defn parse-chunk-section [{:keys [chunk-x
                                  chunk-z
                                  primary-bitmap
                                  add-bitmap] :as meta}
                           sky-light?
                           data
                           section-key]
  (let [length (if (= :block-data section-key)
                 (* 16 16 16)
                 (* 16 16 8))
        mask (if (= :block-add section-key)
               add-bitmap
               primary-bitmap)
        chunks (doall
                (for [i (range 16)
                      :when (not (zero? (bit-and mask (bit-shift-left 1 i))))
                      :let [chunk-data (chan-seq! length data)]
                      :when (= :block-data section-key)
                      :let [x-base (* chunk-x 16)
                            z-base (* chunk-z 16)
                            y-base (* i 16)]
                      x-offset (range 16)
                      z-offset (range 16)
                      y-offset (range 16)
                      :let [x (+ x-base x-offset)
                            y (+ y-base y-offset)
                            z (+ z-base z-offset)
                            idx (+ x-offset
                                   (* 16 (+ z-offset
                                            (* 16 y-offset))))]]
                  [[x y z] (nth chunk-data idx)]))]
    (into {}  chunks)))


(defn parse-chunk-column [meta
                          sky-light?
                          data]
  (let [section-keys (concat
                      [:block-data :block-meta :light-block]
                      (if sky-light? [:light-sky])
                      [:block-add])
        chunks (doall
                (map #(parse-chunk-section meta sky-light? data %) section-keys))
        chunks (apply merge chunks)
        biome-data (chan-take! (* 16 16) data)]
    chunks))


(defn parse-map-chunk-bulk [data]
  (let [column-count (parse-field :short data)
        chunk-data-length (parse-field :int data)
        sky-light? (parse-field :bool data)
        chunk-data-compressed (chan-seq! chunk-data-length data)
        chunk-data (deflate chunk-data-compressed)
        chunk-data-chan (async/to-chan chunk-data)
        metas (doall
               (map (fn [_]
                      (parse-field :meta data))
                    (range column-count)))
        chunks (doall
                (map (fn [meta]
                       (parse-chunk-column meta sky-light? chunk-data-chan))
                     metas))
        chunks (apply merge chunks)]
    chunks))


(def all-chunks (atom {}))
(defn print-packets [ch packet-descriptions ignored]
  (thread
   (loop []
     (when-let [packet (<!! ch)]
       (when-not (contains? @ignored (:packet-id packet))
         (msg (map first (get packet-descriptions (:packet-id packet))) (:packet-id packet))
         
         (when (= 0x26 (:packet-id packet))
           (try
             (swap! all-chunks deep-merge (parse-map-chunk-bulk (async/to-chan (:data packet))))
             (catch Exception e
               (msg "there was an exception" e)))))
       (recur)))))

(def ignored (atom #{}))
(reset! ignored
        #{0 3 4})

(def ignored-from-server (atom #{}))
(reset! ignored-from-server
        #{25 21 18 28 23 22 41 32 19 3 0 24 15 40 56 53 26 4 10 5})
(defn forward [port]
  (let [cin (chan)
        cout-orig (chan)
        cout (chan)
        _ (go
           (>! cout (<! cout-orig))
           (<! cout-orig)
           (>! cout (login "phronmophobic"))
           (async/pipe cout-orig cout))
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

      (print-packets (async/tap sout-mult (chan 1000)) client-packets ignored-from-server)
      ;; (print-packets (async/tap cout-mult (chan)) server-packets ignored)

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





