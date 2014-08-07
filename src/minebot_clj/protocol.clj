(ns minebot-clj.protocol
  (:import (java.net DatagramPacket DatagramSocket InetAddress MulticastSocket))
  (:use [net.cgrand.enlive-html :exclude [move]]
        [clj-http.client :rename {get download}]))


(defn fetch
  "Downloads a document as an html-resource"
  [uri]
  (-> (download uri)
      :body html-snippet))

(def cfetch (memoize fetch))

(def protocol-url "http://wiki.vg/index.php?title=Protocol&oldid=5374")
;; (def protocol-url "http://wiki.vg/Protocol")

(defn get-packets []
  (let [page (cfetch protocol-url)
        tables (select page [:table])
        packet-table? (fn [table]
                        (some #(= (:content %) '(" Packet ID ")) (select table [:th])))
        packet-tables (filter packet-table? tables)
        packet-names (-> page
                         (select [(pred #(or (not-empty (select % [:span.mw-headline]))
                                             (not-empty (select % [:table]))))])
                         reverse
                         (#(map vector % (rest %)))
                         (->> (filter #(= :table (-> % first :tag))))
                         (->> (map (fn [[table span]]
                                     [table (try
                                              (-> span
                                                  :content
                                                  first
                                                  (#(.toLowerCase %))
                                                  (#(.trim %))
                                                  (clojure.string/replace " " "-")
                                                  keyword)
                                              (catch Exception e
                                                nil))])))
                         (->> (into {})))

        packet-direction (-> page
                             (select [(pred #(or (not-empty (select % [:table]))
                                                 (and (= (:tag %) :span)
                                                      (#{" Clientbound " " Serverbound "} (first (:content %))))))])
                             (->> (partition-by #(= (:tag %) :table)))
                             (#(map vector % (rest %)))
                             (->> (filter #(= :span (-> % first first :tag))))
                             (->> (reduce
                                   (fn [m [[direction-title] packet-tables]]
                                     (let [direction (-> direction-title
                                                         :content
                                                         first
                                                         (#(.trim %))
                                                         (#(.toLowerCase %))
                                                         keyword)]
                                       (assoc m direction (into (get m direction #{})
                                                                packet-tables))))
                                   {})))
        packets (for [table packet-tables]
                  (let [packet-id (-> table
                                      (select [:td])
                                      first
                                      :content
                                      first
                                      (#(.trim %))
                                      read-string)
                        body (rest (select table [:tr]))
                        packet-name (get packet-names table)
                        packet-rows (for [row body
                                          :let [raw-row (-> row
                                                            (select [[:td (but-node (attr? :rowspan))]])
                                                            (->> (map text))
                                                            (->> (map #(.trim %))))
                                                [raw-name raw-type raw-notes] raw-row
                                                name (-> raw-name
                                                         (.toLowerCase)
                                                         (clojure.string/replace " " "-")
                                                         keyword)
                                                type (-> raw-type
                                                         (.toLowerCase)
                                                         (clojure.string/replace " " "-")
                                                         keyword)]]
                                      [name type raw-notes])
                        direction (if (contains? (:serverbound packet-direction) table)
                                    :serverbound
                                    :clientbound)]
                    [packet-name direction packet-id packet-rows]))]
    packets))


(defn parse-server-port [s]
  (let [[_ name port] (re-matches #"\[MOTD\](.*)\[/MOTD\]\[AD\](.*)\[/AD\]" s)]
    (read-string port)))

(defn discover-minecraft-server []
  "Listens for 10 seconds and returns [host port] of the first server found."
  (with-open [socket (MulticastSocket. 4445)]
    (doto socket
      (.joinGroup (InetAddress/getByName "224.0.2.60"))
      (.setSoTimeout 10000)
      #_(.setBroadcast true)
      #_(.setReuseAddress true))
    (let [receive-data (byte-array 1024),
          receive-packet (new DatagramPacket receive-data 1024)]
      (.receive socket receive-packet)
      (let [response (String. (.getData receive-packet) 0 (.getLength receive-packet))]
        [(.getHostAddress (.getAddress receive-packet))
         (parse-server-port response)]))))

