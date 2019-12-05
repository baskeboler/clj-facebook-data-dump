(ns playground.core
  (:require [playground.facebook :as fb]
            [clojure.core.async :as async :refer [>! <! go go-loop chan]]
            [taoensso.timbre :as timbre
             :refer [log trace debug info error fatal report get-env]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (info "Hello, World!")
  (let [sum-chan (fb/get-thread-summary-channel 1)]
    (go-loop [s (<! sum-chan)]
      (if-not (nil? s)
        (do
          (info "---> " s)
          (recur (<! sum-chan)))
        (error "done")))))
