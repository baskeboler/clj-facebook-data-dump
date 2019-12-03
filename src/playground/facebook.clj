(ns playground.facebook
  (:require [clojure.core.async :as async :refer [chan >! <! go go-loop]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [pl.danieljanus.tagsoup :as ts]
            [clojure.data.xml :as xml])
  (:import [java.io File]
           [com.kennycason.kumo WordFrequency  CollisionMode WordCloud]
           [com.kennycason.kumo.nlp FrequencyAnalyzer]
           [com.kennycason.kumo.palette ColorPalette]
           [com.kennycason.kumo.font.scale LinearFontScalar]
           [java.awt Color Dimension]))

;; (def stop-words ["para" "como" "estoy" "este" "donde" "dónde" "haces" "hacer" "porque" "andas" "unos" "esta" "está"  "pero" "algo" "bien" "todo" "messagesaudiomp" "messagesphoto"])

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
(defn select-first [pred]
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
(def get-thread (select-first #(= "thread" (get-css-class %))))
(def get-user-node (select-first #(and (= :span (ts/tag %))
                                       (= "user" (get-css-class %)))))
(def get-timestamp-node (select-first #(and (= :span (ts/tag %))
                                            (= "meta" (get-css-class %)))))

(defn is-message-node? [node]
  (and (= :div (ts/tag node))
       (= "message" (get-css-class node))))

(defn get-text [node]
  ;; (assert (is-text-node? node))
  (let [chs  (ts/children node)]
    (if-not (empty? chs)
      (first chs)
      "")))

(defrecord Message [sender timestamp text])

    
(defn create-message [args]
  (let [header (first args)
        m-nodes (rest args)
        sender (-> (get-user-node header) get-text)
        timestamp (-> (get-timestamp-node header) get-text)
        text (apply str (map get-text m-nodes))]
    (->Message sender timestamp text)))

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
  (clojure.core.reducers/fold combine-message   reduce-messages nodes))

(declare ->MessageThread)
         
(defn parse-message-file [f]
  (let [dom (ts/parse f)]
    (->> dom
         get-thread
         ts/children
         ;; (filter is-dom-node?)
         (drop-while (comp not is-message-node?))
         partition-nodes
         (mapv create-message)
         ->MessageThread)))

(defn file-name [^java.io.File f] (.getAbsolutePath f))
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


(defprotocol PMessageThread
  (get-participants [this])
  (message-count [this])
  (thread-summary [this])
  (words [this])
  (word-frequencies [this]))

(defrecord MessageThread [messages]
  PMessageThread
  (get-participants [this]
    (->> (:messages this)
         (mapv :sender)
         (filterv (comp not nil?))
         (into #{})))
  (message-count [this]
    (count (:messages this)))
  (thread-summary [this]
    {:participants (get-participants this)
     :message-count (message-count this)})
  (words [this]
    (->> (:messages this)
         (map (comp #(string/split % #" ") :text))
         (apply concat)
         (map (comp string/lower-case remove-non-letters))
         (filter #(> (count %) 3))))
  (word-frequencies [this]
    (->>
     (for [[word freq] (frequencies (words this))]
       {:word word :frequency freq})
     (sort-by :frequency))))

(defn get-message-file-channel []
  (let [ch (chan 1000)
        files (filterv is-message-file? fs)]
    (go
      (doseq [f files]
        (println "Loading " (file-name f))
        (>! ch f))
      (println "done loading"))
    ch))

(defn get-message-thread-channel []
  (let [ch (chan 100)
        thread-count 4
        file-chan (get-message-file-channel)]
    (doseq [_ (range thread-count)]
      (go-loop [f (<! file-chan)]
        (when f
          (println "Parsing " (file-name f))
          (>! ch (parse-message-file f))
          (recur (<! file-chan)))))
      ;; (async/close! ch))
    ch))

(def threads-by-participants (atom {}))

(defn get-thread-summary-channel [thread-count]
  (let [ch (chan 100)
        message-chan (get-message-thread-channel)]
    (doseq [_ (range thread-count)]
      (go-loop [th (<! message-chan)]
        (when th
          (println "Summarizing ...")
          (let [summary (thread-summary th)]
            (println summary)
            (swap! threads-by-participants assoc (:participants summary) th)
            (>! ch summary))
          (recur (<! message-chan)))))
    ch))

(defn threads-with-participant [name]
  (let [ks (filter #(% name) (keys @threads-by-participants))]
    (->> (for [k ks] [k (get @threads-by-participants k)])
         (into {}))))

(comment
 (def thread-garcia (get @threads-by-participants #{"Victor Gil" "Leandro Garcia"}))
 (def thread-bonnie (get @threads-by-participants #{"Victor Gil" "Victoria Noya"}))
 (def thread-cachete (get @threads-by-participants #{"Victor Gil" "Camilo Falco"}))
 (def thread-biyu (get @threads-by-participants #{"Victor Gil" "Federico O'Neill"}))
 (def thread-paula (get @threads-by-participants #{"Victor Gil" "Paula Lorenzo"})))
