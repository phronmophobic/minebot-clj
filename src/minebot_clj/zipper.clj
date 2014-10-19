(ns minebot-clj.zipper)


(defprotocol IZipper
  (zup [this])
  (zdown [this to from])
  (zpeek [this])
  (zremove [this])
  (zreplace [this val]))

(declare znth zseq)
(deftype Zipper [x path #_ changed?]
  
  clojure.lang.Seqable
  (seq [this]
    (map #(znth (zseq this) %) (range (count x))))

  IZipper
  (zpeek [this]
    x)
  (zremove [this]
    )
  (zreplace [this val]
    (Zipper. val path))
  (zup [this]
    (when path
      (let [[from & path] path]
        (Zipper. (from x) path))))
  (zdown [this to from]
    (Zipper. (to x) (conj path (partial from x)))))

(defn zipper [obj]
  (Zipper. obj nil))

(defn zip [obj]
  (Zipper. obj nil))

(defn zedit [zm f & args]
  (zreplace zm (apply f (zpeek zm) args)))

(defn zroot [zm]
  (loop [zm zm]
    (if-let [up (zup zm)]
      (recur up)
      zm)))

(defn unzip [zm]
  (zpeek (zroot zm)))

(defn zget [zm k]
  (zdown zm #(get % k) #(assoc %1 k %2)))


(defn znth [zm i]
  (zdown zm
         #(nth % i)
         (if (associative? (zpeek zm))
           #(assoc %1 i %2)
           #(concat (take i %1)
                    [%2]
                    (drop (inc i) %1)))))

(defn zseq [zm]
  (zdown zm #(seq %) #(into (empty %1) %2)))

(defn zfirst [zm]
  (zdown zm #(first %) #(cons %2 (rest %1))))






;; (defmacro defnz [name ])

;; (defnz zdouble
;;   ([x]
;;      (map (partial * 2) x))
;;   ([old new]
;;      (into (empty old) (map (partial / 2) new))))

;; (defnz zget
;;   ([x k]
;;      (get x k))
;;   ([x k v]
;;      (assoc x k v)))

;; (defn zdouble [[x path :as zm]]
;;   [(map (partial * 2) x) (conj path
;;                                (fn [new]
;;                                  (into (empty x)
;;                                        (partial / 2))))])
