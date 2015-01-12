(defproject visi/core "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [instaparse "1.3.3"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/tools.analyzer.jvm "0.6.5"]
                 [http-kit "2.1.18"]
                 [org.clojure/data.json "0.2.5"]
                 ]

  :java-source-paths ["src/java"]

  :java-agents [[dpp.rocks/manifest-destiny "0.2.0"]]

  :target-path "target/%s"

  :aot :all
)
