(ns playground.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [playground.core :refer :all]
            [playground.facebook :as fb]))
(fb/set-facebook-path "resources/test")
(def sample (io/resource "test/html/messages.htm"))
(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest mindex-test
  (testing "message index"
    (is (map? (fb/read-message-index)))))
