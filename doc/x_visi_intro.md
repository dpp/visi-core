# -*- coding: utf-8; mode: org -*-

visi spreadsheet introduction by david pollak https://vimeo.com/109290307

basically a cloud-based “spreadsheet”, allowing business people to crunch data, for companies.

visi components:
• written in clojure and java

• uses gorilla-repl for interactive web frontend
(gorilla-repl is a rich REPL for Clojure in the notebook style)
 http://gorilla-repl.org/

• uses Apache Spark for backend do computation
https://spark.apache.org/
http://en.wikipedia.org/wiki/Apache_Spark

• backend can be pluggable, for other technology. Anything that runs JavaScript or jvm can be backend. (⁖ mapreduce, apache storm)
(Apache Storm is a free and open source distributed realtime computation system)
https://storm.apache.org/

visi highlights:
dependency tracking
simple formulas
events in, eval, charts, event out
