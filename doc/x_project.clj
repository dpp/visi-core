[[org.clojure/clojure "1.6.0"]

 ;; https://github.com/Engelberg/instaparse
 ;; syntax parser
 [instaparse "1.3.3"]

 ;; http://spark.apache.org/
 [org.apache.spark/spark-core_2.10 "1.1.0"]
 [org.apache.spark/spark-streaming_2.10 "1.1.0"]
 [org.apache.spark/spark-streaming-kafka_2.10  "1.1.0"]
 [org.apache.spark/spark-streaming-mqtt_2.10  "1.1.0"]
 [org.apache.spark/spark-streaming-flume_2.10  "1.1.0"]
 [org.apache.spark/spark-streaming-twitter_2.10 "1.1.0"]

 ;; http://http-kit.org/
 ;; Ring-compatible HTTP client/server for Clojure
 [http-kit "2.1.18"]

 ;; https://github.com/ring-clojure/ring-json
 ;; Standard Ring middleware functions for JSON requests/responses
 [ring/ring-json "0.3.1"]

 ;; https://github.com/dakrone/cheshire
 ;; JSON parser
 [cheshire "5.3.1"]

 [compojure "1.1.8"]
 ;; https://github.com/weavejester/compojure
 ;; Compojure is a small routing lib for Ring
 ;; https://github.com/ring-clojure/ring

 [org.slf4j/slf4j-api "1.7.7"]
 ;; http://www.slf4j.org/
 ;; abstraction for various logging frameworks

 [ch.qos.logback/logback-classic "1.1.2"]
 ;; http://logback.qos.ch/
 ;; logging framework

 [gorilla-renderable "1.0.0"]
 ;; a web-based notebook interface engine for clojure

 [org.clojure/data.codec "0.1.0"]
 ;; https://github.com/clojure/data.codec
 ;; base64 encode/decode

 [javax.servlet/servlet-api "2.5"]
 [grimradical/clj-semver "0.2.0" :exclusions [org.clojure/clojure]]
 ;; https://github.com/grimradical/clj-semver
 ;; version string parser

 [org.clojure/data.json "0.2.5"]
 ;; https://github.com/clojure/data.json
 ;; JSON parser/generator to/from Clojure data structures.

 [org.clojure/data.csv "0.1.2"]
 ;; https://github.com/clojure/data.csv
 ;; CSV (comma separated values) parser

 [cider/cider-nrepl "0.7.0"]
 ;; clojure repl in emacs

 [org.clojure/tools.nrepl "0.2.5"]
 ;; https://github.com/clojure/tools.nrepl
 ;; clojure network repl

 [com.cemerick/pomegranate "0.3.0"]
 ;; https://github.com/cemerick/pomegranate
 ;; Maven repo manipulation
 ;; A sane Clojure API for Sonatype Aether + dynamic runtime modification of the classpath
 ;; http://www.eclipse.org/aether/

 [clojure-complete "0.2.4"]
 ;; Clojure-complete is an symbol completion library for Clojure
 ;; https://github.com/ninjudd/clojure-complete

 [com.twilio.sdk/twilio-java-sdk "3.4.5"]
 ;; twilio is for phone call
 ;; https://github.com/twilio/twilio-java

 [org.clojure/tools.analyzer.jvm "0.6.1"]
 ;; https://github.com/clojure/tools.analyzer.jvm
 ;; An analyzer for Clojure code, written on top of tools.analyzer, providing additional jvm-specific passes.
 ]
