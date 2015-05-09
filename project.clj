;; (c) Copyright 2014-2015 David Pollak (@dpp, feeder.of.the.bears at gmail)
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defproject visi/core "0.2.1"
  :description "The core parser and runtime for Visi"
  :url "http://visi.works"
  :license {:name "Apache 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.7.0-beta2" ]
                 [instaparse "1.4.0" :exclusions [[org.clojure/clojure]]]
                 [org.clojure/data.codec "0.1.0" :exclusions [[org.clojure/clojure]]]
                 [org.clojure/data.csv "0.1.2" :exclusions [[org.clojure/clojure]]]
                 [org.clojure/tools.analyzer.jvm "0.6.7" :exclusions [[org.clojure/clojure]]]
                 [http-kit "2.1.18" :exclusions [[org.clojure/clojure]]]
                 [org.clojure/data.json "0.2.5" :exclusions [[org.clojure/clojure]]]
                 [com.cemerick/pomegranate "0.3.0" :exclusions [org.codehaus.plexus/plexus-utils]]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha" :exclusions [[org.clojure/clojure]]]
                 ]

  :java-source-paths ["src/java"]

  :java-agents [[dpp.rocks/manifest-destiny "0.2.1"]]

  :target-path "target/%s"

  :aot :all
)
