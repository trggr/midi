(ns midi.dbload
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [midi.timlib :as tla]))

(def QUARTER-NOTE  384)      ; Length of quarter note in time code ticks
(def WHOLE-NOTE    (* QUARTER-NOTE 4))
(def CHORD-CHANNEL 2)
(def BASS-CHANNEL  4)
(def DRUMS-CHANNEL 9)
(def BASS-VELOCITY 65)
(def conn          (tla/connect-sqlite "resources/synth.db"))
(def query         (partial tla/cursor conn))
(def exec-dml      (partial tla/batch-update conn))

(defn tabs->bars
  "Split tabs into sequence of bars"
  [tabs]
  (as-> tabs $
    (str/trim $)
    (str/replace $ #"\n" "|")
    (str/split $ #"\|")
    (map str/trim $)
    (remove (fn [x] (zero? (count x))) $)
    (map #(str/split % #"\s+") $)))

(defn save-song-to-db
  "Takes song-map with keys and saves song to the database"
  [song-meta]
  (let [{:keys [id nm numer denom ppq bb bpm bars bbcs drum bass max-bar]} song-meta]
    (exec-dml "delete from song where song_id = ?" [[id]])
    (exec-dml "delete from bar where song_id = ?" [[id]])
    (exec-dml "delete from song_bar_beat where song_id = ?" [[id]])

    (exec-dml (str "insert into song(song_id, song_nm, time_sig_nmrtr_num, time_sig_denom_num,"
                   "time_sig_ppq_num, time_sig_bb_num, bpm_num, drum_ptrn_cd, bass_ty_cd, max_bar) "
                   "values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
              [[id nm numer denom ppq bb bpm drum bass max-bar]])

    (exec-dml "insert into bar (song_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)" bars)
    (exec-dml "insert into song_bar_beat (song_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)" bbcs)
    (exec-dml "update bar set chord_id = null where length(chord_id) = 0" [[]])
    nm))

(defn sparse-beats
  "Sparsely assign chords within a bar.
   Currently only supports 4/4 time signature."
  [chords]
  (let [[a b c d] chords]
    (case (count chords)
      1 [[1 a]]
      2 [[1 a] [3 b]]
      3 [[1 a] [3 b] [4 c]]
      4 [[1 a] [2 b] [3 c] [4 d]]
      :default (throw (Exception. "expecting 1 to 4 chords per bar")))))

(defn dense-beats
  "Assign chords for each beat in a bar.
   Currently only supports 4/4 time signature"
  [chords]
  (let [[a b c d] chords]
    (case (count chords)
      1 [[1 a] [2 a] [3 a] [4 a]]
      2 [[1 a] [2 a] [3 b] [4 b]]
      3 [[1 a] [2 a] [3 b] [4 c]]
      4 [[1 a] [2 b] [3 c] [4 d]]
      :default (throw (Exception. "expecting 1 to 4 chords per bar")))))

(defn read-song-file
  "Reads song info from a map stored in EDN file"
  [song-edn-file]
  (-> song-edn-file
      slurp
      edn/read-string))

(defn enhance-song-map
  "Takes map of song and enhanced it by adding  keys
    :bars - with sparsely assigned chords to beats
    :bbcs - with densely assigned chords to beats
    :max-bar - length of score in bars"
  [song-map]
  (let [bars (-> song-map :tab-score tabs->bars)
        hlp (fn [assign-beats]
              (->> bars
                   (map-indexed (fn [bar tab]
                                  (for [[beat chord] (assign-beats tab)]
                                    [(song-map :id) (inc bar) beat chord])))
                   (apply concat)))]
    (-> song-map
        (assoc :bars (hlp sparse-beats)
               :bbcs (hlp dense-beats)
               :max-bar (count bars)))))

(defn import-song
  "Imports song from a file into internal SQLite database"
  [song-edn-file]
  (-> song-edn-file
      read-song-file
      enhance-song-map
      save-song-to-db))

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

(defn save-notes [notes]
  (exec-dml "insert into note (note_cd, midi_num) values (?, ?)"
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

(defn save-chords [chords]
  (exec-dml (str "insert into chord(chord_id, root_midi_num, chord_form_cd, root_note_cd,"
                 "  major_ind, midi1_num, midi2_num, midi3_num, midi4_num,"
                 "  midi5_num, midi6_num"
                 ") values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
            (map (juxt :chord_id :root_midi_num :chord_form_cd :root_note_cd :major-ind :a :b :c :d :e :f) chords)))

; (save-chords chord-sqlite)

(def chords (reduce (fn [acc [k f]]
                      (assoc acc
                             (keyword (str (name k) (if (= f :major) "" (name f))))
                             (chord-notes k f)))
                    {}
                    (for [k [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
                          f (keys chord-form)]
                      [k f])))


(defn save-bass-line-to-db
  "Takes enhanced map of bass line, and saves it to 
   BASS_LINE, BASS_LINE_CHORD, BASS_LINE_NOTE.
   Returns bass line ID."
  [bass-line]
  (let [{:keys [id desc bars notes max-bar]} bass-line
        notes  (map-indexed (fn [idx [note dur]]
                              [id (inc idx) (name note) (or dur 4)]) notes)]
    (exec-dml "delete from bass_line       where bass_line_id = ?" [[id]])
    (exec-dml "delete from bass_line_chord where bass_line_id = ?" [[id]])
    (exec-dml "delete from bass_line_note  where bass_line_id = ?" [[id]])

    (exec-dml "insert into bass_line(bass_line_id, bar_cnt, bass_line_desc)
               values (?, ?, ?)"
              [[id max-bar desc]])
    (exec-dml "insert into bass_line_chord (bass_line_id, bar_id, beat_id, chord_id)
               values (?, ?, ?, ?)"
              bars)
    (exec-dml "insert into bass_line_note (bass_line_id, order_num, note_cd, note_dur_num)
               values (?, ?, ?, ?)"
              notes)
    id))

(defn import-bass-line
  "Imports bass-line from a file into internal SQLite database"
  [song-edn-file]
  (-> song-edn-file
      read-song-file
      enhance-song-map
      save-bass-line-to-db))

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
(def  drum-patterns
  {"drums-swing"
   {:ride-cymbal-1      [[99]
                         [99 12] [0 12] [90 12]
                         [99]
                         [99 12] [0 12] [90 12]]
    :closed-hi-hat      [[0] [99] [0]  [99]]
    :acoustic-bass-drum [[85] [65] [85] [65]]}
   "drums-fill2"
   {:ride-cymbal-1      [[70]]
    :acoustic-snare     [[0 12] [50 12] [50 12]
                         [50 12] [0 12] [50 12]
                         [0  12] [0 12] [50 12]
                         [50 12] [50 12] [50 12]]
    :low-mid-tom        [[0]
                         [0]
                         [50 12] [0 12] [0 12]
                         [0]]
    :acoustic-bass-drum [[90]]}
   "drums-fill3"
   {:ride-cymbal-1      [[70] [0] [70]]
    :acoustic-snare     [[0 12] [0 12] [50 12]
                         [0 12] [0 12] [50 12]
                         [0]
                         [0 12] [50 12] [50 12]]
    :high-floor-tom     [[0]
                         [0]
                         [50 12] [50 12] [0 12]]
    :low-mid-tom        [[0]
                         [0]
                         [0 12] [0 12] [50 12]
                         [50 12]]
    :acoustic-bass-drum [[90] [0] [90]]},
   "drums-intro"
   {:bass-drum-1 [[100]
                  [100]
                  [100 8] [100 8]
                  [100 8] [100 8]]}})

;; Chord strumming patterns. Each pattern is a collection of velocities,
;; optionally paired with duration. Duration is a fraction of a whole note.
;; 1 - whole note
;; 2 - half note
;; 4 - quarter note, etc.

(def chord-strumming-patterns
  {"things-we-said"   [[70 12] [70 12] [70 12]   [70]  [70 8] [70 8]  [70]]
   "swing"            [[0] [50] [0] [0]]
   "freddie-green"    [[30] [50] [30] [50]]
   "charleston"       [[50 8/3] [50 8] [0 2]]
   "charleston-combo" [[0 8] [50 8/3] [50 8] [0 8/3]]
   "rhythm-2-4"       [[0 8/3] [50 8] [0 8/3] [50 8]]
   "rhythm-3-3-2"     [[50 8/3] [50 8/3] [50]]})
