(ns minebot-clj.zipper)


(defn zip [obj]
  [obj nil])

(defn zget [[obj path] k]
  [(get obj k) (conj path [:get obj k])])

(defn znth [[obj path] i]
  [(nth obj i) (conj path [:nth obj i])])


(defmulti back (fn [type & args]
                 type))

(defmethod back :get [_ old-val new-val k]
  (assoc old-val k new-val))

(defmethod back :nth [_ old-val new-val i]
  (if (associative? old-val)
    (assoc old-val i new-val)
    (concat (take i old-val)
            [new-val]
            (drop (inc i) old-val))))

(defmulti znext (fn [[_ [[type & _] & _]]]
                    type))

(defmulti zprev (fn [[_ [[type & _] & _]]]
                    type))

(defmulti back (fn [type & args]
                 type))

(defn zup [[new-val [[type old-val & args] & path]]]
  [(apply back type old-val new-val args) path])

(defmethod znext :nth [[obj [[_ _ i] & _] :as zm]]
  (-> zm
      zup
      (znth (inc i))))

(defmethod zprev :nth [[obj [[_ _ i] & _] :as zm]]
  (-> zm
      zup
      (znth (dec i))))

(defmulti zremove (fn [[_ [[type & _] & _]]]
                    type))

(defmethod zremove :nth [[_ [[_ parent i] & path]]]
  [(vec (concat (subvec parent 0 i)
                (subvec parent (inc i) (count parent))))
   path])

(defmethod zremove :get [[_ [[_ parent k] & path]]]
  [(dissoc parent k)
   path])

(defn zseq [[obj path]]
  [(seq obj) (conj path [:seq obj])])

(defmethod back :seq [_ old-val new-val]
  (into (empty old-val) new-val))

(defn zroot [[obj path :as zm]]
  (if path
    (recur (zup zm))
    zm))

(defn zpeek [[obj path]]
  obj)

(defn unzip [zm]
  (-> zm zroot zpeek))

(defn zedit [[obj path] f]
  [(f obj) path])

(defn zreplace [[obj path] val]
  [val path])


(defn seqz [[obj path :as zm]]
  (map #(znth zm %) (range (count obj))))
