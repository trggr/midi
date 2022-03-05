(ns midi.dbload-test
  (:require [clojure.test :refer [deftest is are]]
            [midi.dbload :as db]
            [midi.timlib :as tla]))

;;-------------------------------------------------------
(deftest tabs->bars-test
  (are [result args] (= result (db/tabs->bars args))
    [["Am"]]                             "Am"
    [["Am"] ["Dm"]]                      "Am | Dm"
    [["Am"] ["Dm" "G"]]                  "Am | Dm G"
    [["Am"] ["Dm" "G"] ["Dm" "G" "C"]]   "Am | Dm G | Dm G C"
    [["Am" "Dm" "G" "C"]]                "Am Dm G C"
    [["Am"] ["Bm"]]                      "Am | 
                                          Bm"
    [["Am"] ["Dm"]]                      "Am \n Dm"))

;;-------------------------------------------------------
(def sample-song
  {:id 0, :nm "TEST SONG", :numer 4, :denom 4,
   :ppq 400000, :bb 8, :bpm 90,
   :drum "drums-swing",
   :bass "bass-15-68",
   :tab-score "Am | Am Dm | Am Dm G | Am Dm G C"})

;;-------------------------------------------------------
(deftest enhance-song-map-test
  (let [result (db/enhance-song-map sample-song)
        bars (get result :bars)
        bbcs (get result :bbcs)]
    (is (result :enhanced?))
    (is (and bars bbcs))
    (is (= 4 (get result :max-bar)))
    (is (= [[0 1 1 "Am"]
            [0 2 1 "Am"] [0 2 3 "Dm"]
            [0 3 1 "Am"] [0 3 3 "Dm"] [0 3 4 "G"]
            [0 4 1 "Am"] [0 4 2 "Dm"] [0 4 3 "G"] [0 4 4 "C"]]
           bars))
    (is (= [[0 1 1 "Am"] [0 1 2 "Am"] [0 1 3 "Am"] [0 1 4 "Am"]
            [0 2 1 "Am"] [0 2 2 "Am"] [0 2 3 "Dm"] [0 2 4 "Dm"]
            [0 3 1 "Am"] [0 3 2 "Am"] [0 3 3 "Dm"] [0 3 4 "G"]  ;
            [0 4 1 "Am"] [0 4 2 "Dm"] [0 4 3 "G"]  [0 4 4 "C"]]
           bbcs))))

;;-------------------------------------------------------
(deftest dense-beats-test
  (are [result args] (= result (db/dense-beats args))
    [[1 :Am][2 :Am][3 :Am][4 :Am]]    [:Am]
    [[1 :Am][2 :Am][3 :Dm][4 :Dm]]    [:Am :Dm]
    [[1 :Am][2 :Am][3 :Dm][4 :G]]     [:Am :Dm :G]
    [[1 :Am][2 :Dm][3 :G] [4 :C]]     [:Am :Dm :G :C]))

;;-------------------------------------------------------
(deftest sparse-beats-test
  (are [result args] (= result (db/sparse-beats args))
    [[1 :Am]]                         [:Am]
    [[1 :Am][3 :Dm]]                  [:Am :Dm]
    [[1 :Am][3 :Dm][4 :G]]            [:Am :Dm :G]
    [[1 :Am] [2 :Dm][3 :G][4 :C]]     [:Am :Dm :G :C]))

;;-------------------------------------------------------
(deftest save-song-to-db-test
  (let [result (-> sample-song
                   db/enhance-song-map
                   db/save-song-to-db)]
    (is (= "TEST SONG"
           result))
    (is (= [0, "TEST SONG" 4 4 400000 8 90 "drums-swing" "bass-15-68" 4]
           (->> [0]
                (db/query "select song_id, song_nm, time_sig_nmrtr_num, time_sig_denom_num,
                                 time_sig_ppq_num, time_sig_bb_num, bpm_num, drum_ptrn_cd,
                                 bass_ty_cd, max_bar
                          from song
                          where song_id = :1")
                second)))
    (is (= [[1	1	"Am"]
            [2	1	"Am"] [2	3	"Dm"]
            [3	1	"Am"] [3	3	"Dm"] [3	4	"G"]
            [4	1	"Am"] [4	2	"Dm"] [4	3	"G"] [4	4	"C"]]
           (->> [0]
                (db/query "select bar_id, beat_id, chord_id
                            from bar
                            where song_id = :1
                            order by 1, 2, 3")
                rest)))
    (is (= [[1 1 "Am"] [1	2	"Am"] [1 3 "Am"] [1	4	"Am"]
            [2 1 "Am"] [2	2	"Am"] [2 3 "Dm"] [2	4	"Dm"]
            [3 1 "Am"] [3	2	"Am"] [3 3 "Dm"] [3	4	"G"]
            [4 1 "Am"] [4	2	"Dm"] [4 3 "G"]  [4	4	"C"]]
           (->> [0]
                (db/query "select bar_id, beat_id, chord_id
                            from song_bar_beat
                            where song_id = :1
                            order by 1, 2, 3")
                rest)))))

;;-------------------------------------------------------
;; C - 60, B - 71
(deftest transpose-note
  (is (= 67 (db/transpose-note "g" 0)))
  (is (= 69 (db/transpose-note "G" 2)))
  (is (= 69 (db/transpose-note "g" 2)))
  (is (= 70 (db/transpose-note "g" 3)))
  (is (= 65 (db/transpose-note "g" -2))))

;;-------------------------------------------------------
(deftest transpose-chord
  (is (= "Am7" (db/transpose-chord "Am7"  0)))
  (is (= "A7"  (db/transpose-chord "G7"   2)))
  (is (= "Fm7" (db/transpose-chord "Gm7" -2))))

;;-------------------------------------------------------
(def sample-bass-line
  {:id "FOO"
   :desc "FROM DOMINANT TO ROOT"
   :tab-score "G7 | C"
   :notes  [[:g3] [:f3] [:e3] [:d3]  [:c3 2] [:g3 2]]})


;;-------------------------------------------------------
(deftest transpose-bass-line
  (let [result (-> sample-bass-line
                   db/enhance-bass-line-map
                   (db/transpose-bass-line 2))
        id "FOO-A7"]
    (is (= id (result :id)))
    (is (= (sample-bass-line :desc) (result :desc)))
    (is (= [[id 1 1 "A7"] [id 1 2 "A7"] [id 1 3 "A7"] [id 1 4 "A7"]
            [id 2 1 "D"]  [id 2 2 "D"] [id 2 3 "D"] [id 2 4 "D"]]
           (result :bbcs)))
    (is (= [[id 1 1 "A7"] [id 2 1 "D"]]
           (result :bars)))
    (is (= [[id 0 69 4] [id 1 67 4] [id 2 66 4] [id 3 64 4]
            [id 4 62 2] [id 5 69 2]]
           (result :midi-notes)))))

;;-------------------------------------------------------
(deftest save-bass-line-to-db-test
  (let [result (-> sample-bass-line
                   db/enhance-bass-line-map
                   db/save-bass-line-to-db)]
    (is (= "FOO" result))
    (is (= ["FOO", "FROM DOMINANT TO ROOT", 2]
           (->> ["FOO"]
                (db/query "select bass_line_id, bass_line_desc, bar_cnt
                           from bass_line
                           where bass_line_id = :1")
                second)))
    (is (= [[1 1 "G7"] [2 1 "C"]]
           (->> ["FOO"]
                (db/query "select bar_id, beat_id, chord_id
                            from bass_line_chord
                            where bass_line_id = :1
                            order by 1, 2")
                rest)))
    (is (= [[67 4] [65 4] [64 4] [62 4] [60 2] [67 2]]
           (->> ["FOO"]
                (db/query "select midi_num, cast(note_dur_num as int)
                            from bass_line_note
                            where bass_line_id = :1
                            order by order_num")
                rest)))))
