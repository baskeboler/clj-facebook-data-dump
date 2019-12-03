(ns playground.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def odds (iterate (partial + 2) 1))

(def index
  (memoize
   (fn [level]
     (apply + (range level)))))

(def row
  (memoize
   (fn [r]
     (->> odds
          (drop (index r))
          (take r)))))

(def row-sum-odd-numbers
  (memoize
   (fn [n]
     (assert pos? n)
     (apply + (row n)))))

(defn in-array [array1 array2]
  (->> array1
       (filter
        (fn [a]
          (pos?
           (count
            (filter (fn [s]
                      (clojure.string/includes?  s a))
                    array2)))))
       dedupe
       sort))

(def ur ["olp" "love" "string"])
(def vr ["ulove" "alove" "holp" "sholp","vfmstring"])
(def rr ["love" "olp" "string"])

(defn longest-cons [strarr k]
  (loop [remaining strarr
         biggest ""]
    (if (< (count remaining) k)
      biggest
      (let [newstr (apply str (take k remaining))
            newbig (if (> (count newstr) (count biggest)) newstr biggest)]
        (recur (rest remaining) newbig)))))

(defn longest-cons [strarr k])
                                        ; your code
  
