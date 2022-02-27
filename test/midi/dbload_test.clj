(ns midi.dbload-test
  (:require [clojure.test :refer :all]
            [midi.dbload :refer :all]))

(def sample-edn
{:id 11, :nm "BLACK ORPHEUS", :numer 4, :denom 4,
 :ppq 400000, :bb 8, :bpm 90,
 :drum "drums-swing",
 :bass "bass-15-68",
 :tab-score "
Am     | Bm7-5 E7-9 | Am      | Bm7-5 E7-9
Am     | Dm7 G7     | Cmaj7   | C#dim7 
Dm7    | G7         | C6      | Fmaj7
Bm7-5  | E7-9       | Am      | Bm7-5 E7-9 
Am     | Bm7-5 E7-9 | Am      | Bm7-5 E7-9
Em7-5  | A7-9       | Dm      | Dm
Dm Dm7 | Bm7-5 E7-9 | Am Am7  | Fmaj7
Bm7-5  | E7-9       | Am      | Bm7-5 E7-9 
Am     | Dm7 Am7    | Dm7 Am7 | Dm7 Em7
Am     | Am         | Am      | Am
"
})


;  (import-song "resources/tabs/black-orpheus.edn")
;  (exec-dml "delete from bar where song_id = ?" [[11]])
;  (query "select * from bar where song_id = ?" [11])

(deftest import-song-test
  (testing "TEST fn import-song"
    (is (= 1 1))))
