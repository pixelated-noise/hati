(defproject hati "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [rewrite-clj "0.6.1"]
                 [hiccup "1.0.5"]
                 [enlive "1.1.6"]
                 [venantius/glow "0.1.5"]
                 [org.asciidoctor/asciidoctorj "1.5.6"]
                 [com.vladsch.flexmark/flexmark-all "0.34.48"]]
  :profiles {:dev {:source-paths ["src" "dev" "test"]}})
