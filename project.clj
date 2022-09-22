(defproject sicp-exercises "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [nrepl "1.0.0"]
                 ;; 数据可视化
                 [vlaaad/reveal "1.3.276"]]
  :repl-options {:init-ns sicp-exercises.core})
