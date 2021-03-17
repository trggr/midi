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

; Middle octave - C3 (also just C for convenience)
(def notedb {:C 60, :C# 61, :Db 61,
             :D 62, :D# 63, :Eb 63,
             :E 64,
             :F 65, :F# 66, :Gb 66,
             :G 67, :G# 68, :Ab 68,
             :A 69, :A# 70, :Bb 70,
             :B 71})

(def notedb (reduce (fn [acc [k v]] (assoc acc k v))
                    notedb
                    (for [octave    [-2 -1 0 1 2 3 4 5 6 7 8]
                          [id freq] notedb]
                        [(keyword (str (name id) octave))
                         (+ freq (* 12 (- octave 3)))])))

(def notedb (reduce (fn [acc [k v]] (assoc acc (keyword (str/lower-case (name k))) v))
                    notedb
                    notedb))

(defn save-notes [conn notes]
   (batch-update conn (str "insert into note (note_id, midi_num) values (?, ?)")
      (for [[k v] notes] [(name k) v])))

(def chord-form {
    :major [[1 5 8]      :Y]    
    :+     [[1 4 9]      :Y]
    :sus4  [[1 6 8]      :Y]
    :6     [[1 5 8 11]   :Y]
    :m6    [[1 4 8 11]   :N]
    :7     [[1 5 8 11]   :Y]
    :m     [[1 4 8]      :N]
    :m7    [[1 4 8 11]   :N]
    :maj7  [[1 5 8 12]   :Y]
    :7sus4 [[1 6 8 11]   :Y]
    :7+5   [[1 5 9 11]   :Y]
    :7-5   [[1 5 7 11]   :Y]
    :dim   [[1 4 7]       :N]
    :dim7  [[1 4 7 11]    :N]
    :m7-5  [[1 4 7 11]    :N]
    :mmaj7 [[1 4 8 12]    :Y]
    :mmaj9 [[1 5 8 12 15] :Y]
    :m9    [[1 4 8 11 15] :N]
    :9     [[1 5 8 11 15] :Y]
    :9+5   [[1 5 9 11 15] :Y]
    :9-5   [[1 5 7 11 15]    :Y]
    :96    [[1 5 8 10 11 15] :Y]
    :maj11 [[1 5 8 12 15 18] :Y]
    :m11   [[1 4 8 11 15 18] :N]
    :11    [[1 5 8 11 15 18] :Y]
    :11-9  [[1 5 8 11 14 18] :Y]
    :maj13 [[-1 3 6 10]      :Y]
    :m13   [[-2 3 6 10]      :N]
    :13    [[-2 3 6 10]      :Y] ; same as m13?
    :13-9  [[-2 2 6 10]      :Y]})

(defn chord-notes [root form]
   (map #(+ (notedb root) % -1) (first (chord-form form))))

(def chorddb
   (for [root [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
         form (keys chord-form)]
      (let [[pattern maj-ind] (chord-form form)
            [a b c d e f] (map #(+ (notedb root) % -1) pattern)
            r             (name root)]
        {:chord      (str (name root) (if (= form :major) "" (name form)))
         :form       (name form)
         :root       r
         :major-ind  (name maj-ind)
         :a    a
         :b    b
         :c    c
         :d    d
         :e    e
         :f    f})))

(defn save-chords [conn chords]
   (batch-update conn (str "insert into chord(chord_id, chord_form_cd, root_cd,"
                           "  major_ind, midi1_num, midi2_num, midi3_num, midi4_num,"
                           "  midi5_num, midi6_num"
                           ") values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
      (map (juxt :chord :form :root :major-ind :a :b :c :d :e :f) chords)))


    
(def chorddb (reduce (fn [acc [k f]]
                        (assoc acc
                               (keyword (str (name k) (if (= f :major) "" (name f))))
                               (chord-notes k f)))
                     {}
                     (for [k [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
                           f (keys chord-form)]
                        [k f])))

; Saving:
;
; (for [s songdb] (save-song conn s))
;
; (save-chords conn chorddb)

(def linea [[:Fm7  [:f0  :ab0  :c1 :eb1]] [:Bbm7 [:bb0 :db1 :f1 :b1]] [:Eb7 [:g1 :eb1 :db1 :bb0]]])
 



