(defproject clj-facebook-data-dump "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://github.com/baskeboler/clj-facebook-data-dump.git"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.clojure/tools.cli "0.4.2"]
                 [com.rpl/specter "1.1.3"]
                 [clj-tagsoup/clj-tagsoup "0.3.0"]
                 [org.clojure/core.async "0.5.527"]
                 [com.kennycason/kumo-core "1.22"]]
  :main ^:skip-aot playground.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
