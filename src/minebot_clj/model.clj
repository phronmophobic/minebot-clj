(ns minebot-clj.model)


(defn block-path [section [x y z]]
  (let [x (long x)
        y (long y)
        z (long z)
        rx (mod x 16)
        ry (mod y 16)
        rz (mod z 16)
        x (if (neg? x)
            (long (/ (- x 15) 16))
            (long (/ x 16)))
        y (long (/ y 16))
        z (if (neg? z)
            (long (/ (- z 15) 16))
            (long (/ z 16)))]
    [[x z]
     section
     y
     [rx ry rz]]))

(defn data-chunk-index [[rx ry rz]]
  (+ rx
     (* 16
        (+ rz
           (* ry 16)))))


(deftype BlockChunk [data]
  clojure.lang.Associative
  (assoc [_ k v]
    (BlockChunk. (assoc data (data-chunk-index k) v)))
  
  ;; clojure.lang.Seqable
  ;; (seq [_]
  ;;   (.seq (augment-contents contents)))
  
  clojure.lang.ILookup
  (valAt [_ k]
    (get data (data-chunk-index k)))
  (valAt [_ k not-found]
    (get data (data-chunk-index k) not-found)))

(defn packed-block-chunk-index [[rx ry rz]]
  (let [rx (quot (long rx) 2)]
    (+ rx
       (* 8
          (+ rz
             (* 16 ry))))))

(deftype PackedBlockChunk [data]
  clojure.lang.Associative
  (assoc [_ [rx ry rz] v]
    (let [idx (packed-block-chunk-index [rx ry rz])
          current-val (-> (get data idx 0)
                          (bit-and (if (even? rx)
                                     0xF0
                                     0x0F)))
          new-val (-> v
                      (bit-and 0x0f)
                      (bit-shift-left (if (even? rx)
                                        0
                                        4)))]
      (PackedBlockChunk. (assoc data idx (bit-or current-val new-val )))))
  
  ;; clojure.lang.Seqable
  ;; (seq [_]
  ;;   (.seq (augment-contents contents)))
  
  clojure.lang.ILookup
  (valAt [_ [rx ry rz]]
    (let [val (get data (packed-block-chunk-index [rx ry rz]))]
      (if (nil? val)
        val
        (bit-and 0x0F
                 (if (even? rx)
                   val
                   (bit-shift-right val 4))))))
  (valAt [this k not-found]
    (if-let [val (get this k)]
      val
      not-found)))

(defn biome-chunk-index [[rx ry rz]]
  (+ rx
     (* 16 rz)))

(deftype BiomeChunk [data]
  clojure.lang.Associative
  (assoc [_ k v]
    (BiomeChunk. (assoc data (biome-chunk-index k) v)))
  
  ;; clojure.lang.Seqable
  ;; (seq [_]
  ;;   (.seq (augment-contents contents)))
  
  clojure.lang.ILookup
  (valAt [_ k]
    (get data (biome-chunk-index k)))
  (valAt [_ k not-found]
    (get data (biome-chunk-index k not-found))))

(defn make-chunk [section bytes]
  (case section
    :block-data
    (BlockChunk. (vec bytes))

    (:block-meta :block-add :light-block :light-sky)
    (PackedBlockChunk. (vec bytes))

    :biome
    (BiomeChunk. (vec bytes))))


(defprotocol IWorld
  (add-chunks [world chunks])
  (multi-block-change [world records])
  #_(block-change [world [section [chunk-x chunk-y chunk-z]] block-id block-meta])
  (raw-chunks [world]))

(defn chunk-index [[x y z]]
  (let [x (long x)
        y (long y)
        z (long z)
        x (if (neg? x)
            (long (/ (- x 15) 16))
            (long (/ x 16)))
        y (long (/ y 16))
        z (if (neg? z)
            (long (/ (- z 15) 16))
            (long (/ z 16)))]
    [x y z]))

(defn chunk-subindex [[x y z]]
  (let [x (long x)
        y (long y)
        z (long z)
        rx (mod x 16)
        ry (mod y 16)
        rz (mod z 16)]
    [rx ry rz]))


(deftype World [chunks]
  IWorld
  (add-chunks [_ more-chunks]
    (World. (merge chunks more-chunks)))
  (raw-chunks [_]
    chunks)
  (multi-block-change [_ {:keys [chunk-x chunk-z records]}]
    (World. (reduce
             (fn [chunks {:keys [block-metadata block-id block-x block-y block-z]}]
               (-> chunks
                   (assoc-in [[:block-data [chunk-x (quot block-y 16) chunk-z]]
                              [block-x (mod block-y 16) block-z]]
                             block-id)
                   (assoc-in [[:block-meta [chunk-x (quot block-y 16) chunk-z]]
                              [block-x (mod block-y 16) block-z]]
                             block-metadata)))
             chunks
             records)))

  ;; java.lang.Iterable
  ;; (iterator [this]
  ;;   (.iterator (augment-contents contents)))
 
  clojure.lang.Associative
  (assoc [_ [section k] v]
    (World. (assoc-in chunks [[section (chunk-index k)] (chunk-subindex k)] v)))

  ;; clojure.lang.Seqable
  ;; (seq [_]
  ;;   (.seq (augment-contents contents)))
 
  clojure.lang.ILookup
  (valAt [_ [section k]]
    (let [chunk (get chunks [section (chunk-index k)])]
      (if chunk
        (get chunk (chunk-subindex k) 0)
        nil)))
  (valAt [this k not-found]
    (if-let [val (get this k)]
      val
      not-found)))



