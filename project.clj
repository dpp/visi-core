;; (c) Copyright 2014-2015 David Pollak (@dpp, feeder.of.the.bears at gmail)
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defproject visi/core "0.1.0"
  :description "The core parser and runtime for Visi"
  :url "http://visi.works"
  :license {:name "Apache 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [instaparse "1.3.5"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/tools.analyzer.jvm "0.6.5"]
                 [http-kit "2.1.18"]
                 [org.clojure/data.json "0.2.5"]
                 [com.cemerick/pomegranate "0.3.0"]
                 ]

  :java-source-paths ["src/java"]

  :java-agents [[dpp.rocks/manifest-destiny "0.2.1"]]

  :target-path "target/%s"

  :aot :all
)
