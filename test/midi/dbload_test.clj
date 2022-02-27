(ns midi.dbload-test
  (:require [clojure.test :refer :all]
            [midi.dbload :as db]
            [midi.timlib :as tla]))


(comment
 (remove-ns 'midi.dbload-test)
)

(def sample-edn
  {:id 0, :nm "TEST SONG", :numer 4, :denom 4,
   :ppq 400000, :bb 8, :bpm 90,
   :drum "drums-swing",
   :bass "bass-15-68",
   :tab-score "Am | Am Dm | Am Dm G | Am Dm G C"})

(deftest enhance-song-map-test
  (let [result (db/enhance-song-map sample-edn)
        bars (get result :bars)
        bbcs (get result :bbcs)]
    (is (and bars bbcs))
    (is (= bars [[0 1 1 "Am"]
                 [0 2 1 "Am"] [0 2 3 "Dm"]
                 [0 3 1 "Am"] [0 3 3 "Dm"] [0 3 4 "G"]
                 [0 4 1 "Am"] [0 4 2 "Dm"] [0 4 3 "G"] [0 4 4 "C"]]))
    (is (= bbcs [[0 1 1 "Am"] [0 1 2 "Am"] [0 1 3 "Am"] [0 1 4 "Am"]
                 [0 2 1 "Am"] [0 2 2 "Am"] [0 2 3 "Dm"] [0 2 4 "Dm"]
                 [0 3 1 "Am"] [0 3 2 "Am"] [0 3 3 "Dm"] [0 3 4 "G"]  ;
                 [0 4 1 "Am"] [0 4 2 "Dm"] [0 4 3 "G"]  [0 4 4 "C"]]))))

;((deftest save-song-to-db-test
;  (let [result (-> sample-edn
;                   db/enhance-song-map
;                   db/save-song-to-db)
;        s (->> ["1"]
;               (db/query "select * from song where song_id = :1"))]
;                  (is (= result "TEST SONG"))
;                  (is (= s [0, "TEST SONG1"]))))



