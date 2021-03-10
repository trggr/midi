(ns midi.dbload
   (:require [clojure.string :as str])
   (:use [midi.timlib]))


(def conn (connect-sqlite "resources/synth.db"))
; (-> conn .createStatement (.executeUpdate "create table song (song_id number, song_name string);"))
(batch-update conn "insert into song(song_id, song_nm, time_sig_nmrtr_num, time_sig_denom_num, time_sig_ppq_num, time_sig_bb_num, bpm_num) values (?, ?, ?, ?, ?, ?, ?)"
  [["1" "All the Things You Are", "4", "4", "400000", "8", "120"]])
; (cursor conn "select * from person")

(defn bar-to-beats [bar]
  (let [[a b c d] bar]
    (case (count bar)
       1 {1 a}
       2 {1 a, 3 b}
       3 {1 a, 3 b, 4 c}
       4 {1 a, 2 b, 3 c, 4 d}
       :default (throw (Exception. "More than 4 chords per bar!")))))

(defn song [tabs]
  (let [x1 (str/replace tabs #"\n" "|")
        x2 (str/split x1 #"\|")
        x3 (map str/trim x2)
        x4 (map #(str/split % #"\s+") x3)
        x5 (map bar-to-beats x4)
        x6 (map-indexed #(vector (inc %1) %2) x5)
        x7 (for [[barno bar] x6, [beat chord] bar] [barno beat chord])]
     x7))

(defn save-song [conn song-id song]
   (batch-update conn "insert into tab (song_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)"
       (map #(cons song-id %) song)))


  
(def a (song (str
  "Fm7    | Bbm7     | Eb7    | Abmaj7   |"
  "Dbmaj7 | G7       | Cmaj7  |          |"
  "Cm7    | Fm7      | Bb7    | Ebmaj7   |"
  "Abmaj7 | Am7-5 D7 | Gmaj7  | E9       |"
  "Am7    | D7       | Gmaj7  |          |" 
  "F#m7   | B7       | Emaj7  | C7+5     |"
  "Fm7    | Bbm7     | Eb7    | Abmaj7   |"
  "Dbmaj7 | Gb7      | Cm7    | Bdim7    |"
  "Bbm7   | Eb7      | Abmaj7 | Gm7-5  C9")))

