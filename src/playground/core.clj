(ns playground.core
  (:require [playground.facebook :as fb]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.core.async :as async :refer [>! <! go go-loop chan]]
            [taoensso.timbre :as timbre
             :refer [log trace debug info error fatal report get-env]]
            [clojure.tools.cli :as cli :refer [parse-opts]])
  (:import [java.io File])
  (:gen-class))

(def cli-options
  [["-p" "--facebook-path PATH" "Path to unzipped facebook archive"
    :default "."
    :validate [ (fn [path]
                  (let [f (io/file path)]
                    (and
                     (.exists f)
                     (.isDirectory f))))]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Facebook downloaded data digger"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "   print-message-index"
        "   word-cloud "
        "" ]
       (string/join \newline)))
(defn error-msg [errors]
  (str "there were errors:\n\n"
       (string/join \newline errors)))

(defn validate-args
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}
      errors
      {:exit-message (error-msg errors)}
      (and (= (count arguments) 1)
           (#{"print-message-index" "word-cloud"} (first arguments)))
      {:action (first arguments) :options options}
      :else
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "print-message-index"
        (do
          (info "Hello, World!")
          (fb/print-message-index)
          (info (parse-opts args cli-options)))))))
