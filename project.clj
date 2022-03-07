(defproject midi "1.0"
  :description "Build MIDI backing tracks from song tabs"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.xerial/sqlite-jdbc "3.32.3.3"]]
  :resource-paths ["resources/android-midi-lib-1.0-SNAPSHOT.jar"]
  :main ^:skip-aot midi.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
