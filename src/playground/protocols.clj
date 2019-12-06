(ns playground.protocols)


(defprotocol PText
  (get-text [this]))

(defprotocol PWords
  (get-words [this]))

(defprotocol PMessageThread
  (get-participants [this])
  (get-message-count [this])
  (get-thread-summary [this])
  (get-images [this])
  (get-word-frequencies [this]))

(defprotocol PMessage
  (has-image? [this])
  (get-images [this]))

