(ns midi.dbload
   (:require [clojure.string :as str])
   (:use [midi.timlib]))

(def conn (connect-sqlite "resources/synth.db"))
; references
; (batch-update conn "insert into all_beat(bar_id, beat_id) values (?, ?)" (for [bar (range 1 101) beat (range 1 5)] [bar beat]))
; (batch-update conn "update bar set chord_id = null where length(chord_id) = 0" [[]])

(defn bar-to-beats [bar]
  (let [[a b c d] bar]
    (case (count bar)
       1 {1 a}
       2 {1 a, 3 b}
       3 {1 a, 3 b, 4 c}
       4 {1 a, 2 b, 3 c, 4 d}
       :default (throw (Exception. "More than 4 chords per bar!")))))

(defn bars [tabs]
  (let [x1 (str/replace tabs #"\n" "|")
        x2 (str/split x1 #"\|")
        x3 (map str/trim x2)
        x4 (map #(str/split % #"\s+") x3)
        x5 (map bar-to-beats x4)
        x6 (map-indexed #(vector (inc %1) %2) x5)
        x7 (for [[barno bar] x6, [beat chord] bar] [barno beat chord])]
     x7))

(defn save-song [conn song]
  (let [{:keys [id nm numer denom ppq bb bpm bars]} song]
     (batch-update conn "insert into song(song_id, song_nm, time_sig_nmrtr_num, time_sig_denom_num, time_sig_ppq_num, time_sig_bb_num, bpm_num) values (?, ?, ?, ?, ?, ?, ?)"
        [[id nm numer denom ppq bb bpm]])
   (batch-update conn "insert into bar (song_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)"
       (map #(cons id %) bars))
   (batch-update conn "update bar set chord_id = null where length(chord_id) = 0" [[]])))

(def songdb [
  {:id 1, :nm "ALL THE THINGS YOU ARE", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (bars (str "Fm7    | Bbm7     | Eb7    | Abmaj7    |"
                    "Dbmaj7 | G7       | Cmaj7  |           |"
                    "Cm7    | Fm7      | Bb7    | Ebmaj7    |"
                    "Abmaj7 | Am7-5 D7 | Gmaj7  | Gmaj7 E9  |"
                    "Am7    | D7       | Gmaj7  |           |" 
                    "F#m7   | B7       | Emaj7  | C7+5      |"
                    "Fm7    | Bbm7     | Eb7    | Abmaj7    |"
                    "Dbmaj7 | Gb7      | Cm7    | Bdim7     |"
                    "Bbm7   | Eb7      | Abmaj7 | Gm7-5  C9 "))}
  {:id 2, :nm "IN A SENTIMENTAL MOOD", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (bars (str "Dm       Dmmaj7 | Dm7    Dm6 | Gm      Gmmaj7 | Gm7      Gm6 A7   |"
                    "Dm              | D7         | Gm7     Gb7    | Fmaj7             |"
                    "Dm       Dmmaj7 | Dm7    Dm6 | Gm      Gmmaj7 | Gm7      Gm6 A7   |"
                    "Dm              | D7         | Gm7     Gb7    | Fmaj7    Ebm7 Ab7 |"
                    "Dbmaj7   Bbm7   | Ebm7   Ab7 | Dbmaj7  Bb7    | Eb7      Ab7      |"
                    "Dbmaj7   Bbm7   | Ebm7   Ab7 | Gm7            | C7                |"
                    "Dm       Dmmaj7 | Dm7    Dm6 | Gm      Gmmaj7 | Gm7      Gm6 A7   |"
                    "Dm              | D7         | Gm7     C11-9  | Fmaj7              "))}
  {:id 3, :nm "ALL OF ME", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (bars (str "C6  |     | E7           |         |"
                    "A7  |     | Dm7          |         |"
                    "E7  |     | Am7          |         |"
                    "D7  |     | Dm7          | G7      |"
                    "C6  |     | E7           |         |"
                    "A7  |     | Dm7          |         |"
                    "F6  | Fm6 | Cmaj7 Em7-5  | A7      |"
                    "Dm7 | G7  | C6    Ebdim7 | Dm7  G7 |"))}
  {:id 4, :nm "AUTUMN LEAVES", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (bars (str "Am7    | D7    | Gmaj7    | Cmaj7    |"
                    "F#m7-5 | B7    | Em       | Em       |"
                    "Am7    | D7    | Gmaj7    | Cmaj7    |"
                    "F#m7-5 | B7    | Em       | Em       |"
                    "F#m7-5 | B7    | Em       | Em       |"
                    "Am7    | D7    | Gmaj7    | Gmaj7    |"
                    "F#m7-5 | B11-9 | Em7   A7 | Dm7    G7|"
                    "F#m7-5 | B11-9 | Em       | Em       |"))}
  {:id 5, :nm "ALL BY MYSELF", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (bars (str "Cmaj7       | C6        | D7           | Am7    D7 |"
                    "G7          | Dm7   G7  | Em7    A7    | Dm     G7 |"
                    "Cmaj7       | C6        | F#m7   B7    | E7        |"
                    "Am7   Am7-5 | D7        | Dm7    Dm7-5 | G7        |"
                    "Cmaj7       | C6        | D7           | Am7    D7 |"
                    "G7          | Dm7    G7 | E7     E7+5  | E7        |"
                    "Fmaj7       | F#dim7    | Cmaj7  B7+5  | Em7-5  A7 |"
                    "Am7   D7    | Dm7    G7 | C6     Am7   | Dm7    G7 |"))}
  {:id 6, :nm "LET IT BE", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (bars (str "C  | G  | Am | F |"
                    "C  | G  | F  | C |"
                    "C  | G  | Am | F |"
                    "C  | G  | F  | C |"
                    "Am | G  | F  | C |"
                    "C  | G  | F  | C |"))}])

; (for [s songdb]
;   (save-song conn s))



