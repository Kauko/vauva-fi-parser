(defproject babycrawl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-http "3.10.3"]
                 [hickory "0.7.1"]
                 [org.clojure/data.csv "1.0.0"]]
  :main ^:skip-aot babycrawl.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})