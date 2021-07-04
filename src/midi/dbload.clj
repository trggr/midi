(ns midi.dbload
   (:require [clojure.string :as str]
             [midi.timlib :as tla]))

(def conn (tla/connect-sqlite "resources/synth.db"))
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


(def songdb [
  {:id 1, :nm "ALL THE THINGS YOU ARE", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (bars (str "Fm7    | Bbm7     | Eb7    | Abmaj7    |"
                    "Dbmaj7 | G7       | Cmaj7  | Cmaj7     |"
                    "Cm7    | Fm7      | Bb7    | Ebmaj7    |"
                    "Abmaj7 | Am7-5 D7 | Gmaj7  | Gmaj7 E9  |"
                    "Am7    | D7       | Gmaj7  | Gmaj7     |" 
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
   :bars (bars (str "C6  | C6  | E7           | E7      |"
                    "A7  | A7  | Dm7          | Dm7     |"
                    "E7  | E7  | Am7          | Am7     |"
                    "D7  | D7  | Dm7          | G7      |"
                    "C6  | C6  | E7           | E7      |"
                    "A7  | A7  | Dm7          | Dm7     |"
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
                    "C  | G  | F  | C |"))}
  {:id 7, :nm "MEDIUM BLUES", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
    :bars (bars "C | F7 F#dim | C | C7 | F | F#dim | C | Em7-5 A7 | Dm7 | G7 | Em7-5 A7 | Dm G7")}
  {:id 8, :nm "ALONE TOGETHER", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (bars (str "Dm          | Em7-5 A7  | Dm           | Em7-5  A7 |"
                    "Dm          | Am7-5 D7  | Gm           | Gm7       |"
                    "Bm7     E7  | Gm7   C7  | F      F7    | Em7-5  A7 |"
                    "Dmaj7       | Em7-5 A7  |"
                    "Dm          | Em7-5 A7  | Dm           | Em7-5  A7 |"
                    "Dm          | Am7-5 D7  | Gm           | Gm7       |"
                    "Bm7     E7  | Gm7   C7  | F      F7    | Em7-5  A7 |"
                    "Dmaj7       | Dmaj7     |"
                    "Am7-5       | D7        | Gm           | Gm        |"
                    "Gm7-5       | C7        | F      F7    | Em7-5  A7 |"
                    "Dm          | Em7-5 A7  | Dm           | Em7-5  A7 |"
                    "Dm          | Bb7   A7  | Dm           | Em7-5  A7 |"))}
  {:id 9, :nm "MISTY", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 80, :drum "drums-swing", :bass "patterns",
   :bars (bars (str "Ebmaj7      | Bbm7  Eb7 | Abmaj7       | Abm7   Db7|"
                    "Ebmaj7  Cm7 | Fm7   Bb7 | Gm7    C7    | Fm7    Bb7|"
                    "Ebmaj7      | Bbm7  Eb7 | Abmaj7       | Abm7   Db7|"
                    "Ebmaj7  Cm7 | Fm7   Bb7 | Eb6    Db9   | Ebmaj7    |"
                    "Bbm7        | Eb7-9     | Abmaj7       | Abmaj7    |"
                    "Am7         | D7    F7  | Gm7    C7-9  | Fm7    Bb7|"
                    "Ebmaj7      | Bbm7  Eb7 | Abmaj7       | Abm7   Db7|"
                    "Ebmaj7 Cm7  | Fm7   Bb7 | Eb6    Cm7   | Fm7    Bb7|"))}
])

(defn save-song [conn song]
  (let [{:keys [id nm numer denom ppq bb bpm bars drum bass]} song]
     (tla/batch-update conn
       (str "insert into song(song_id, song_nm, time_sig_nmrtr_num, time_sig_denom_num,"
                             "time_sig_ppq_num, time_sig_bb_num, bpm_num, drum_ptrn_cd, bass_ty_cd) "
            "values (?, ?, ?, ?, ?, ?, ?, ?, ?)")
        [[id nm numer denom ppq bb bpm drum bass]])
   (tla/batch-update conn "insert into bar (song_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)"
       (map #(cons id %) bars))
   (tla/batch-update conn "update bar set chord_id = null where length(chord_id) = 0" [[]])))

; Saving:
;
; (for [s songdb] (save-song conn s))
;

; Middle octave - C3 (also just C for convenience)
(def notedb1 {:C 60, :C# 61, :Db 61,
              :D 62, :D# 63, :Eb 63,
              :E 64,
              :F 65, :F# 66, :Gb 66,
              :G 67, :G# 68, :Ab 68,
              :A 69, :A# 70, :Bb 70,
              :B 71})

(def notedb2 (reduce (fn [acc [k v]] (assoc acc k v))
                    notedb1
                    (for [octave   (range -2 9)
                          [id freq] notedb1]
                        [(keyword (str (name id) octave))
                         (+ freq (* 12 (- octave 3)))])))

(def notedb3 (reduce (fn [acc [k v]] (assoc acc (keyword (str/lower-case (name k))) v))
                    notedb2
                    notedb2))

;; in MIDI each drum is is a certain note, which is added here into notedb
;; for convenience
(def notedb4 (merge notedb3 
                  {:metronome-click 33 :metronome-bell  34 :acoustic-bass-drum  35
                   :bass-drum-1     36 :side-stick      37 :acoustic-snare      38
                   :hand-clap       39 :electric-snare  40 :low-floor-tom       41
                   :closed-hi-hat   42 :high-floor-tom  43 :pedal-hi-hat        44
                   :low-tom         45 :open-hi-hat     46 :low-mid-tom         47
                   :hi-mid-tom      48 :crash-cymbal-1  49 :high-tom            50
                   :ride-cymbal-1   51 :chinese-cymbal  52 :ride-bell           53
                   :tambourine      54 :splash-cymbal   55 :cowbell             56
                   :crash-cymbal-2  57 :vibraslap       58 :ride-cymbal-2       59
                   :hi-bongo        60 :low-bongo       61 :mute-hi-conga       62
                   :open-hi-conga   63 :low-conga       64 :high-timbale        65
                   :low-timbale     66 :high-agogo      67 :low-agogo           68
                   :cabasa          69 :maracas         70 :short-whistle       71
                   :long-whistle    72 :short-guiro     73 :long-guiro          74
                   :claves          75 :hi-wood-block   76 :low-wood-block      77
                   :mute-cuica      78 :open-cuica      79 :mute-triangle       80
                   :open-triangle   81}))

(def notedb (assoc notedb4 :_ 0))  ; silence

(defn save-notes [conn notes]
  (tla/batch-update conn (str "insert into note (note_cd, midi_num) values (?, ?)")
                    (for [[k v] notes] [(name k) v])))

(def chord-form {:major [[1 5 8]      :Y]
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
                 :7-9   [[1 5 8 11 13]    :Y]
                 :maj13 [[-1 3 6 10]      :Y]
                 :m13   [[-2 3 6 10]      :N]
                 :13    [[-2 3 6 10]      :Y] ; same as m13?
                 :13-9  [[-2 2 6 10]      :Y]})

(defn chord-notes [root form]
   (map #(+ -12 (notedb root) % -1) (first (chord-form form))))

(def chord-sqlite
   (for [root [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
         form (keys chord-form)]
      (let [[pattern maj-ind] (chord-form form)
            [a b c d e f] (map #(+ (notedb root) % -1) pattern)
            r             (name root)]
        {:chord_id      (str (name root) (if (= form :major) "" (name form)))
         :root_midi_num (get notedb root)
         :chord_form_cd (name form)
         :root_note_cd  r
         :major-ind     (name maj-ind)
         :a    a
         :b    b
         :c    c
         :d    d
         :e    e
         :f    f})))

(defn save-chords [conn chords]
   (tla/batch-update conn (str "insert into chord(chord_id, root_midi_num, chord_form_cd, root_note_cd,"
                           "  major_ind, midi1_num, midi2_num, midi3_num, midi4_num,"
                           "  midi5_num, midi6_num"
                           ") values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
      (map (juxt :chord_id :root_midi_num :chord_form_cd :root_note_cd :major-ind :a :b :c :d :e :f) chords)))

; (save-chords conn chord-sqlite)
    
(def chorddb (reduce (fn [acc [k f]]
                        (assoc acc
                               (keyword (str (name k) (if (= f :major) "" (name f))))
                               (chord-notes k f)))
                     {}
                     (for [k [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
                           f (keys chord-form)]
                        [k f])))


(defn save-bass-line [conn bass-line]
  (let [{:keys [id desc chords notes]} bass-line
        chords (->> (str/split chords #"\|")
                    (map str/trim)
                    (map #(str/split % #"\s+"))
                    (map bar-to-beats)
                    (map-indexed #(vector (inc %1) %2)))
        chords (for [[barno bar] chords, [beat chord] bar] [id barno beat chord])
        cnt    (str (reduce max (map second chords)))
        notes  (for [i (range (count notes))]
                   (let [[note dur] (nth notes i)]
                       [id (inc i) (name note) (or dur 4)]))]
     (tla/batch-update conn "insert into bass_line(bass_line_id, bar_cnt, bass_line_desc) values (?, ?, ?)"
          [[id cnt desc]])
     (tla/batch-update conn "insert into bass_line_chord (bass_line_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)"
          chords)
     (tla/batch-update conn "insert into bass_line_note (bass_line_id, order_num, note_cd, note_dur_num) values (?, ?, ?, ?)"
          notes)))

(def basslinedb [
   {:id "SMEDBLUES", :desc "Medium blues, Sobolev p. 14",
    :chords "C | F7 F#dim | C | C7 | F | F#dim | C | Em7-5 A7 | Dm7 | G7 | Em7-5 A7 | Dm G7"
    :notes  [[:c4] [:g3] [:e3] [:c3]  
             [:f3] [:e3] [:f3] [:f#3]
             [:g3] [:b3] [:c4] [:b3] 
             [:bb3] [:c3] [:d3] [:e3]

             [:f3] [:a2] [:bb2] [:b2]
             [:c3] [:e3] [:f3] [:f#3] 
             [:g3] [:e3] [:f3] [:d3] 
             [:e3] [:bb3] [:a3] [:c#2] 

             [:d3] [:a3] [:f3] [:f#3]
             [:g3] [:d3] [:g3] [:f3] 
             [:e3] [:bb2] [:a2] [:c#4] 
             [:d4] [:a3] [:b3] [:g3]]}
   {:id "T51", :desc "Walk from dominant to root" :chords "G7 G7 | C C"
    :notes  [[:g3] [:f3] [:e3] [:d3]  [:c3 2] [:g3 2]]}
   {:id "Milk min", :desc "Miliking minor chord, Kaye, p.15" :chords "Dm7 | Dm7"
    :notes  [[:d3] [:e3] [:f3] [:g3]  [:a3] [:f3] [:e3] [:d3]]}
   {:id "Milk maj", :desc "Miliking major chord" :chords "D7 | D7"
    :notes  [[:d3] [:e3] [:f#3] [:g3]  [:a3] [:f#3] [:e3] [:d3]]}
   {:id "S25", :desc "Sobolev p. 14, A1", :chords "Cm7 | F7 "
            :notes  [[:c3]  [:_] [:g3]  [:_]
                     [:f3]  [:_] [:a2]  [:_]]}
   {:id "S251", :desc "Sobolev p. 14, A1", :chords "Cm7 | F7 | Bb7 "
            :notes  [[:c3]  [:_] [:g3]  [:_]
                     [:f3]  [:_] [:a2]  [:_]
                     [:bb2] [:_] [:f3]  [:e3]]}
   {:id "S2514", :desc "Sobolev p. 14, A1", :chords "Cm7 | F7 | Bb7 | Eb7"
            :notes  [[:c3]  [:_] [:g3]  [:_]
                     [:f3]  [:_] [:a2]  [:_]
                     [:bb2] [:_] [:f3]  [:e3]
                     [:eb3] [:_] [:bb2] [:_]]}
   {:id "S25147", :desc "Sobolev p. 14, A1"  :chords "Cm7 | F7 | Bb7 | Eb7 | Am7-5"  
            :notes  [[:c3]  [:_] [:g3]  [:_]
                     [:f3]  [:_] [:a2]  [:_]
                     [:bb2] [:_] [:f3]  [:e3]
                     [:eb3] [:_] [:bb2] [:_]
                     [:a2]  [:_] [:eb3] [:_]]}
   {:id "S251473", :desc "Sobolev p. 14, A1", :chords "Cm7 | F7 | Bb7 | Eb7 | Am7-5 | D7"
            :notes  [[:c3]  [:_] [:g3]  [:_]
                     [:f3]  [:_] [:a2]  [:_]
                     [:bb2] [:_] [:f3]  [:e3]
                     [:eb3] [:_] [:bb2] [:_]
                     [:a2]  [:_] [:eb3] [:_]
                     [:d3]  [:_] [:a3]  [:_]]}
   {:id "S2514736", :desc "Sobolev p. 14, A1", :chords "Cm7 | F7 | Bb7 | Eb7 | Am7-5 | D7 | Gm7"
            :notes  [[:c3]  [:_] [:g3]  [:_]
                     [:f3]  [:_] [:a2]  [:_]
                     [:bb2] [:_] [:f3]  [:e3]
                     [:eb3] [:_] [:bb2] [:_]
                     [:a2]  [:_] [:eb3] [:_]
                     [:d3]  [:_] [:a3]  [:d3]
                     [:g3]  [:_] [:a3]  [:_]]}
   {:id "S25147366", :desc "Sobolev p. 14, A1", :chords "Cm7 | F7 | Bb7 | Eb7 | Am7-5 | D7 | Gm7 | Gm7"
            :notes  [[:c3]  [:_] [:g3]  [:_]
                     [:f3]  [:_] [:a2]  [:_]
                     [:bb2] [:_] [:f3]  [:e3]
                     [:eb3] [:_] [:bb2] [:_]
                     [:a2]  [:_] [:eb3] [:_]
                     [:d3]  [:_] [:a3]  [:d3]
                     [:g3]  [:_] [:a3]  [:_]
                     [:bb3] [:_] [:g3]  [:_]]}
   {:id "Majdown", :desc "Scalewise from root to fifth, Stuart Smith, p. 27", :chords "F7"
            :notes  [[:f3]  [:e3] [:d3]  [:c3]]}
   {:id "Mindown", :desc "Scalewise from root to fifth, Stuart Smith, p. 27", :chords "Fm"
            :notes  [[:f3]  [:eb3] [:db3]  [:c3]]}
])

; Saving
; (map (partial save-bass-line conn) basslinedb)
; (map (partial save-bass-line conn) (drop 4 basslinedb))
; (save-bass-line conn (last basslinedb))

(defn init-chord-based-bass-db
  "Returns a map of functions which can build bass line solely from the chords.
   Each function has three args:
   bar - bar number in a song (starting from 1);
   beat - a beat in a bar;
   chord - a chord that is played on this beat"
 []
  (let [m  {"bass-none" (fn [_ _ _] nil)
            "bass-1"    (fn [_ beat [r _ _]]   (case beat 1 r nil))
            "bass-15"   (fn [_ beat [r _ n5]]  (case beat 1 r 3 n5 nil))
            "bass-1234" (fn [_ beat [r n3 _]]  (case beat 1 r 2 (+ 2 r) 3 n3 4 (inc n3)))
            "bass-1235" (fn [_ beat [r n3 n5]] (case beat 1 r 2 (+ 2 r) 3 n3 4 n5))
            "bass-4321" (fn [_ beat [r n3 _]]  (case beat 1 (inc n3) 2 n3  3 (+ 2 r) 4 r))
            "bass-5321" (fn [_ beat [r n3 n5]] (case beat 1 n5       2 n3  3 (+ 2 r) 4 r))}
        f (fn [pattern bar beat chord]
            (let [k (nth pattern (mod (dec bar) 8))
                  f (m k)]
              (f bar beat chord)))]
    (assoc m
           "bass-15-8"  (partial f ["bass-15" "bass-15" "bass-15" "bass-15"
                                    "bass-15"   "bass-15" "bass-15" "bass-5321"])
           "bass-15-68" (partial f ["bass-15" "bass-15" "bass-15" "bass-15"
                                    "bass-5321" "bass-15" "bass-15" "bass-5321"])
           "bass-ud2"   (partial f ["bass-1234" "bass-1235" "bass-5321" "bass-4321"
                                    "bass-1234" "bass-1235" "bass-5321" "bass-4321"])
           "bass-ud3"   (partial f ["bass-1234" "bass-5321" "bass-1235" "bass-5321"
                                    "bass-1234" "bass-5321" "bass-1235" "bass-5321"]))))
    
(def chord-based-bass-db (init-chord-based-bass-db))

;; Drum patterns. Each drum pattern covers one standard bar (4/4). The pattern
;; can have one or more drums. Each item is [velocity duration], duration can be omitted.
;; Velocity is measured in percents, duration is measured in standard note durations:
;; 1 - whole note, 2 - half, 4 - quarter (default), 8 - eighth, 12 - tripplets
;; basically answer the question: How many of these notes do you need
;; to cover the whole bar?
(def  drum-pattern-db
  {"drums-swing"
   {:ride-cymbal-1      [[70]
                         [70 12] [0 12] [40 12]
                         [70]
                         [70 12] [0 12] [40 12]]
    :closed-hi-hat      [[0] [70] [0]  [70]]
    :acoustic-bass-drum [[90][70] [90] [70]]}
   "drums-intro"
   {:low-wood-block     [[60]
                         [60]
                         [60 8] [60 8]
                         [60 8] [60 8]]}})
