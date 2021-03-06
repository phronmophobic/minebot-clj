(ns minebot-clj.zipper)

(defn zip [obj]
  [obj nil])

(defn zget [[obj path] k]
  [(get obj k) (conj path [:get obj k])])

(defn znth [[obj path] i]
  [(nth obj i) (conj path [:nth obj i])])


(defn zfn [[obj path] type f & args]
  [(apply f obj args) (conj path (apply vector type obj args))])

(defmacro zdo [zobj f & args]
  `(zfn ~zobj (keyword (quote ~f)) ~f ~@args))


(defmulti back (fn [type & args]
                 type))

(defmulti forward (fn [type & args]
                    type))

(defmethod back :get [_ old-val new-val k]
  (assoc old-val k new-val))

(defmethod forward :get [_ m k]
  (get m k))

(defmethod back :nth [_ old-val new-val i]
  (if (associative? old-val)
    (assoc old-val i new-val)
    (concat (take i old-val)
            [new-val]
            (drop (inc i) old-val))))

(defmethod back :nth [_ coll i]
  (nth coll i))

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

(defmethod forward :seq [_ coll]
  (seq coll))

(defmethod back :first [_ old-val new-val]
  (let [new-list (cons new-val (rest old-val))]
    (if (list? old-val)
      new-list
      (into (empty old-val) new-list))))

(defmethod forward :first [_ coll]
  (first coll))

(defmethod back :rest [_ old-val new-val]
  (if (list? old-val)
    (cons (first old-val) new-val)
    (into (empty old-val) (cons (first old-val) (reverse new-val)))))

(defmethod forward :rest [_ coll]
  (rest coll))

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

;; (require '[clojure.tools.analyzer :as ana])
;; (require '[clojure.tools.analyzer.env :as env])
(require '[clojure.tools.analyzer.jvm :as ana.jvm])
(require '[clojure.tools.analyzer.ast :as ast])

;; for every binding, add meta to what it's derived from
(defn my-analyze [form]
  (ana.jvm/analyze' form
                   (ana.jvm/empty-env)
                   {:passes-opts
                    {:validate/unresolvable-symbol-handler
                     (fn [_ form _]
                       {:op :var
                        :form form})}}))



(defn ret-val [ast]
  (case (:op ast)
    :let
    (ret-val (:body ast))

    :do
    (ret-val (:ret ast))

    ast))

(defn form-deps [ast]
  (->> (ast/nodes ast)
       (filter #(#{:local :var} (:op %)))
       (map #(case (:op %)
               :local (:name %)
               :var (:form %)))))

(defn trace [form]
  (let [ast (my-analyze form)
        bindings (filter #(= (:op %) :binding)
                         (ast/nodes ast))
        dep-map (into {}
                      (for [binding bindings
                            :let [deps (form-deps (:init binding))]
                            :when (seq deps)]
                        [(:name binding)
                         deps]))
        ret-val (ret-val ast)
        all-deps (loop [all-deps (set (form-deps ret-val))]
                   (let [dep-deps (mapcat dep-map all-deps)]
                     (if (not (every? all-deps dep-deps))
                       (recur (into all-deps dep-deps))
                       all-deps)))]
    all-deps))

(defn zzpath [obj]
  (-> obj meta :path))

(defn zzapply [obj path]
  (when obj
    (reduce
     (fn [obj [type _ & args :as sub-path]]
       (when obj
         (when-let [new-obj (apply forward type obj args)]
           (vary-meta new-obj
                      assoc :path (conj (-> obj meta :path) (apply vector type obj args))))))
     (vary-meta obj dissoc :path)
     (reverse path))))


(defn zzrebase [old new]
  (zzapply new (zzpath old)))

(defn zzget [m k]
  (let [val (get m k)]
    (if (instance? clojure.lang.IObj val)
      (with-meta val
        {:path (conj (-> m meta :path)
                     [:get m k])})
      val)))

#_(defmethod back :fn [_ old-val new-val back-fn & args]
  (apply back-fn old-val new-val args)
)

#_(defn zzfn [obj back-fn fn & args]
  (let [val (apply fn obj args)]
    (if (instance? clojure.lang.IObj val)
      (with-meta val
        {:path (conj (-> obj meta :path)
                     (into [key obj] args))})
      val)))

(defmacro defzzfn [name key fn]
  `(let [fn# ~fn]
     (defn ~name [obj# & args#]
       (let[val# (apply fn# obj# args#)]
         (if (instance? clojure.lang.IObj val#)
           (with-meta val#
             {:path (conj (-> obj# meta :path)
                          (into [~key obj#] args#))})
           val#)))
     (defmethod forward ~key [_# obj# & args#]
       (apply fn# obj# args#))))


(defn zznth [coll index]
  (let [val (nth coll index)]
    (if (instance? clojure.lang.IObj val)
      (with-meta val
        {:path (conj (-> coll meta :path)
                     [:nth coll index])})
      val)))

(defn zzfirst [coll]
  (let [val (first coll)]
    (if (instance? clojure.lang.IObj val)
      (with-meta val
        {:path (conj (-> coll meta :path)
                     [:first coll])})
      val)))

(defn zzrest [coll]
  (let [val (rest coll)]
    (with-meta val
      {:path (conj (-> coll meta :path)
                   [:rest coll])})))


(defn zzup [obj]
  (let [[[type old-val & args] & path] (-> obj meta :path)]
    (with-meta (apply back type old-val obj args)
      {:path path})))

(defn zzreplace [obj new-val]
  (with-meta new-val
    (meta obj)))

(defn zzedit [obj f & args]
  (with-meta (apply f obj args)
    (meta obj)))

(defn zzroot? [obj]
  (boolean (-> obj meta :path)))

(defn zzroot [obj]
  (if (zzroot? obj)
    (recur (zzup obj))
    obj))



(defmulti zzremove (fn [obj]
                     (let [[[type & _] & _] (-> obj meta :path)]
                       type)))

(defmulti zzinsert-left
  (fn [obj & args]
    (let [[[type & _] & _] (-> obj meta :path)]
      type)))

(defmulti zzinsert-right
  (fn [obj & args]
    (let [[[type & _] & _] (-> obj meta :path)]
      type)))

(defmethod zzinsert-left :nth [obj val]
  (let [[[type _ i :as op] & _] (-> obj meta :path)]
    (-> obj
        zzup
        (zzedit (fn [xs]
                  (into (empty xs)
                        (concat
                         (take i xs)
                         [val]
                         (drop i xs)))))
        (zznth (inc i)))))

(defmethod zzinsert-right :nth [obj val]
  (let [[[_ _ i :as op] & _] (-> obj meta :path)]
    (-> obj
        zzup
        (zzedit (fn [xs]
                  (into (empty xs)
                        (concat
                         (take (inc i) xs)
                         [val]
                         (drop (inc i) xs)))))
        (zznth i))))

(defmethod zzinsert-left :first [obj val]
  (let [[[_ parent] & _] (-> obj meta :path)]
    (-> obj
        (zzup)
        (zzedit (fn [xs]
                  (cons val
                        xs)))
        (zzrest)
        (zzfirst))))

(defmethod zzinsert-right :first [obj val]
  (let [[[_ parent] & _] (-> obj meta :path)]
    (-> obj
        (zzup)
        (zzedit (fn [xs]
                  (cons (first xs)
                        (cons val
                              (rest xs)))))
        (zzfirst))))

(defmethod zzremove :first [obj]
  (-> obj zzup (zzedit rest)))

(defmethod zzremove :get [obj]
  (let [[[_ old-val k] & path] (-> obj meta :path)
        val (dissoc old-val k)]
    (vary-meta val
               assoc :path path)))

(defn zzunzip [obj]
  (zzroot obj))

(defn zzseq [obj]
  (lazy-seq
   (when-let [s (seq obj)]
     (cons (zzfirst obj)
           (zzseq (zzrest obj))))))

(defn meta-num
  ([n]
     (meta-num n nil))
  ([n m]
     (proxy [Number clojure.lang.IObj clojure.lang.IMeta] []
       (byteValue []
         (byte n))
       (doubleValue []
         (double n))
       (floatValue []
         (float n))
       (intValue []
         (int n))
       (longValue []
         (long n))
       (shortValue []
         (short n))
       (meta []
         m)
       (withMeta [m]
         (meta-num n m)))))
