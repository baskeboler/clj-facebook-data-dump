(ns playground.facebook
  (:require [clojure.core.async :as async :refer [chan >! <! go go-loop]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.core.reducers :as reducers]
            [pl.danieljanus.tagsoup :as ts]
            [taoensso.timbre :as log]
            [clojure.data.xml :as xml]
            [playground.protocols :as protocols :refer [PText PWords PMessage PMessageThread]]
            [com.rpl.specter :as specter :refer :all])
  (:import [java.io File]
           [com.kennycason.kumo WordFrequency  CollisionMode WordCloud]
           [com.kennycason.kumo.nlp FrequencyAnalyzer]
           [com.kennycason.kumo.palette ColorPalette]
           [com.kennycason.kumo.font.scale LinearFontScalar]
           [java.awt Color Dimension]))
(defn hash-code [obj]
  (.hashCode obj))

(def stop-words
  (-> (io/resource "stopwords.txt")
      slurp
      string/split-lines))

(defn filter-jaja [word]
  (string/includes? word "jaja"))
(defn filter-messages-extras [word]
  (string/includes? word "messages"))

(def word-filters [filter-jaja filter-messages-extras])
(defn filter-words [words]
  (filter (fn [w]
            (not-any? #(% w) word-filters))
          words))

(def freq-analyzer (FrequencyAnalyzer.))
(doto freq-analyzer
  (.setWordFrequenciesToReturn 500)
  (.setMinWordLength 4)
  (.setStopWords stop-words))

(defn create-word-cloud [words outfile]
  (let [filtered (filter-words words)
        freqs (.load  freq-analyzer filtered)
        dims (Dimension. 800 600)
        palette (ColorPalette. (mapv #(Color. %) [0x4055f1 0x408df1 0x40AAF1  0x40c5f1  0x40d3f1  0xFFFFFF]))
        wc (WordCloud. dims CollisionMode/PIXEL_PERFECT)]
    (doto wc
      (.setPadding 2)
      (.setColorPalette palette)
      (.setFontScalar (LinearFontScalar. 10 70))
      (.build freqs)
      (.writeToFile outfile))))

(def facebook-files-path "/home/victor/Documents/fb")

(def f (io/file facebook-files-path))

(def fs (file-seq f))

(defn get-class [n]
  (:class
   (ts/attributes n)))

(defn is-text-node? [node]
  (or
   (empty? (ts/children node))
   (and
    (= 1 (count (ts/children node)))
    (string? (first (ts/children node))))))

(defn is-dom-node? [node]
  (keyword? (ts/tag node)))
(defn get-css-class [node]
  (:class (ts/attributes node)))
(defn select-first-node [pred]
  (fn [node]
    (loop [res       node
           remaining []]
      (if (pred res)
        res
        (let [rem (concat remaining (filter is-dom-node?
                                            (ts/children res)))
              r   (first rem)
              rem (rest rem)]
          (recur r rem))))))
(def get-thread (select-first-node #(= "thread" (get-css-class %))))
(def get-user-node (select-first-node #(and (= :span (ts/tag %))
                                            (= "user" (get-css-class %)))))
(def get-timestamp-node (select-first-node #(and (= :span (ts/tag %))
                                                 (= "meta" (get-css-class %)))))

(defn is-message-node? [node]
  (and (= :div (ts/tag node))
       (= "message" (get-css-class node))))

(defn tag-selector [tag]
  (fn [node]
    (= tag (ts/tag node))))
(defn tag-container [tag]
  (fn [node]
    (cond
      (nil? node) false
      (= tag (ts/tag node)) true
      :otherwise (loop [children (ts/children node)]
                   (if (empty? children)
                     false
                     (let [f (first children)]
                       (if (= tag (ts/tag f))
                         true
                         (recur (rest children)))))))))

(def is-img-node? (tag-selector :img))
(def is-p-node? (tag-selector :p))

(def has-img-node? (tag-container :img))

(defn get-img-path [img-node]
  (assert (is-img-node? img-node))
  (:src (ts/attributes img-node)))

(defn get-node-text [node]
  ;; (assert (is-text-node? node))
  (let [chs (ts/children node)]
    (if-not (empty? chs)
      (first chs)
      "")))

(defrecord Message [sender timestamp text]
  PText
  (get-text [this]
    (->> (:text this)
         (filter is-text-node?)
         (map get-node-text)
         string/join))
  PMessage
  (has-image? [this]
    (some has-img-node? (:text this)))
  (get-images [this]
    (->> (:text this)
         (filter is-img-node?))))

(defn create-message [args]
  (let [header (first args)
        m-nodes (rest args)
        sender (-> (get-user-node header) get-node-text)
        timestamp (-> (get-timestamp-node header) get-node-text)]
    (->Message sender timestamp m-nodes)))

(defn reduce-messages [result node]
  (if (nil? node)
    result
    (if (is-message-node? node)
      (conj result (vector node))
      (conj (vec (butlast result)) (conj (vec (last result)) node)))))
(defn combine-message
  ([] [])
  ([a b] (vec (concat a b))))

(defn partition-nodes [nodes]
  (reducers/fold combine-message   reduce-messages nodes))

(declare ->MessageThread)
(defn file-name [^java.io.File f] (.getAbsolutePath f))

(defn parse-message-file [f]
  (log/debug "Parsing DOM in " (file-name f))
  (let [dom (ts/parse f)]
    (log/debug "Done parsing DOM for " (file-name f))
    (->> dom
         get-thread
         ts/children
         ;; (filter is-dom-node?)
         (drop-while (comp not is-message-node?))
         partition-nodes
         (mapv create-message)
         ->MessageThread)))

(defn is-message-file? [^java.io.File f]
  (let [p (file-name f)]
    (and (string/includes? p "/messages/")
         (string/ends-with? p ".html"))))

(def sample-message-file (->> fs
                              (filter is-message-file?)
                              (map ts/parse)
                              first))

(defn remove-non-letters [word]
  (apply str (filter #(Character/isLetter %) word)))

(extend-protocol PText
  clojure.lang.PersistentVector
  (get-text [this]
    (->> this
         ts/children
         first)))

(defn get-word-list [^java.lang.String the-string]
  (when (string? the-string)
    (->> (string/split the-string #" ")
         (map (comp string/lower-case remove-non-letters))
         (filter #(> (count %) 2)))))
(extend-protocol PWords
  clojure.lang.PersistentVector
  (get-words [this]
    (when (is-text-node? this)
      (->> this
           ts/children
           first
           get-word-list))))

(defrecord MessageThread [messages]
  PMessageThread
  (get-participants [this]
    (if (:participants this)
      (:participants this)
      (->> (:messages this)
           (mapv :sender)
           (filterv (comp not nil?))
           (into #{}))))
  (get-message-count [this]
    (count (:messages this)))
  (get-thread-summary [this]
    (if (:thread-summary this)
      (:thread-summary this)
      {:participants  (protocols/get-participants this)
       :message-count (protocols/get-message-count this)}))
  (get-word-frequencies [this]
    (if (:word-frequencies this)
      (:word-frequencies this)
      (->>
       (for [[word freq] (frequencies (protocols/get-words this))]
         {:word word :frequency freq})
       (sort-by :frequency))))
  (get-images [this]
    (filter is-img-node? (:messages this)))
  PWords
  (get-words [this]
    (if (:words this)
      (:words this)
      (->> (:messages this)
           (map :text)
           (apply concat)
           (map protocols/get-words)
           (apply concat)))))

(def processed-thread-files (atom #{}))
(def threads-by-participants (atom {}))
(defn get-message-file-channel []
  (let [ch (chan 1000)
        files (filterv is-message-file? fs)]
    (go
      (doseq [the-file files
              :let [hc (hash-code the-file)]
              :when  (not (@processed-thread-files hc))
              :while (>! ch the-file)]
        (log/info "Loading " (file-name the-file))
        (swap! processed-thread-files conj hc))
      (log/info "done loading"))
    ch))

(defn get-message-thread-channel [file-chan]
  (let [ch (chan 100)]
    (go-loop [[res the-chan] (async/alts! [file-chan ch])]
      (when      res
        (log/info "Parsing " (file-name res))
        (when (>! ch (parse-message-file res))
          (recur (async/alts! [file-chan ch])))))
    ch))

(defn get-thread-summary-channel [message-chan]
  (let [ch (chan 100)]
    (go-loop [[th the-chan] (async/alts! [message-chan ch])]
      (when      th
        (let [summary (protocols/get-thread-summary th)]
          (log/info summary)
          (swap! threads-by-participants assoc (:participants summary) (-> th
                                                                           (assoc :participants (:participants summary))
                                                                           (assoc :message-count (:message-count summary))))
          (when (>! ch summary)
            (recur (async/alts! [message-chan ch]))))))
    ch))

(defn ingest-message-threads []
  (let [file-chan (get-message-file-channel)
        message-chans (into [] (for [_ (range 6)] (get-message-thread-channel file-chan)))
        message-chan (async/merge message-chans 1000)
        ch (get-thread-summary-channel message-chan)]
    (go-loop [s (<! ch)]
      (if-not (nil? s)
        (do
          ;; (log/info "summary: " s)
          (recur (<! ch)))
        (do
          (async/close! file-chan)
          (doseq [c message-chans] (async/close! c))
          (log/info "done."))))
    ch))
(defn threads-with-participant [name]
  (let [ks (filter #(% name) (keys @threads-by-participants))]
    (->> (for [k ks] [k (get @threads-by-participants k)])
         (into {}))))

(defn read-message-index []
  (let [f (slurp (File. facebook-files-path "html/messages.htm"))
        dom (ts/parse-string f)
        get-item-name (fn [item] (->> item ts/children first ts/children first))
        get-item-path (fn [item] (->> item ts/children first ts/attributes :href))
        ps (->> dom ts/children second ts/children second ts/children rest)]
    (->> (for [i ps] [(get-item-name i) (get-item-path i)])
         (group-by first)
         (map (fn [[a b]] [a (map second b)]))
         (into {}))))

(defn limit-str-length [n]
  (fn [s]
    (if (> (count s) n)
      (str (apply str (take n s)) " ...")
      s)))
(defn print-message-index []
  (let [idx (read-message-index)
        limiter (limit-str-length 60)
        rows (for [[k vs] idx] {:name k :files (limiter (string/join ", " vs)) :file-count (count vs)})]
    (clojure.pprint/print-table (sort-by :name rows))))

(def message-index (read-message-index))

(defn get-message-threads [name]
  (let [paths (get message-index name)]
    (when-let  [abs-paths (map #(str facebook-files-path "/" %) paths)]
      (let [loaded-threads (map (comp parse-message-file #(File. %)) abs-paths)
            loaded-th-idx  (into {} (for [th loaded-threads] [(protocols/get-participants th) th]))]
        (swap! threads-by-participants merge loaded-th-idx)
        loaded-threads))))
(defn print-thread-list [threads]
  (let [rows (map (fn [th]
                    {:participants (->> (protocols/get-participants th)
                                        (string/join ", "))
                     :messages (protocols/get-message-count th)})
                  threads)]
    (clojure.pprint/print-table rows)))
(def thread-leandro (get-message-threads "Leandro Garcia"))

(defn snake-case [the-str]
  (-> the-str
      string/trim
      string/lower-case
      (string/replace #" " "_")))

(defn message-thread-word-cloud [message-thread]
  (let [ppl (protocols/get-participants message-thread)
        file-name (str "wc-" (string/join "-" (map snake-case ppl)) ".png")
        words (protocols/get-words message-thread)]
    (create-word-cloud words file-name)))


(defn get-all-thread-images [thread-list]
  (->> (if (seq? thread-list) thread-list [thread-list])
       (specter/select [ALL :messages ALL protocols/has-image? :text ALL #(not-empty (ts/children %))])
       (specter/transform ALL (fn [o] (->> o ts/children first ts/attributes :src (str facebook-files-path "/"))))))
       ;; (specter/select [ALL #(string/starts-with? % (str facebook-files-path "/messages/photos"))])))

(defn get-thread-image-count)
