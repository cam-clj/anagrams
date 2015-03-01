(defproject cam-clj/anagrams "0.1.0-SNAPSHOT"
  :description "Anagrams tutorial for Cambridge NonDysfunctionalProgrammers"
  :url "https://github.com/cam-clj/anagrams"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.apache.commons/commons-lang3 "3.3.2"]]
  :plugins [[lein-gorilla "0.3.4"]]
  :main cam-clj.anagrams
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
