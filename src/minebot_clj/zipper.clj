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

(defmethod back :first [_ old-val new-val]
  (cons new-val (rest old-val)))

(defmethod back :rest [_ old-val new-val]
  (cons (first old-val)
        new-val))

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


(defn zzget [m k]
  (let [val (get m k)]
    (if (instance? clojure.lang.IObj val)
      (with-meta val
        {:path (conj (-> m meta :path)
                     [:get m k])})
      val)))

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

(defn zzroot [obj]
  (if (-> obj meta :path)
    (recur (zzup obj))
    obj))

(defmulti zzremove (fn [obj]
                     (let [[[type & _] & _] (-> obj meta :path)]
                       type)))


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
