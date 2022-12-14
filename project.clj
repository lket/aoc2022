(defproject aoc2022 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/core.async "1.6.673"]
                 [com.clojure-goes-fast/clj-async-profiler "1.0.3"]
                 [com.taoensso/tufte "2.4.5"]]
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :repl-options {:init-ns aoc2022.core})
