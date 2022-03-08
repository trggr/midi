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
(def DBFILE        "resources/synth.db")

(defn read-edn-file
  "Returns map stored in extended EDN file, in which the mpty lines and comments
   embedded in strings are ignored."
  [edn-file]
  (as-> edn-file $
    (slurp $)
    (str/split $ #"\n")
    (map #(str/replace % #";.*" "") $)
    (map #(str/trim %) $)
    (remove #(zero? (count %)) $)
    (str/join "\n" $)
    (edn/read-string $)))

(def chord-forms   (read-edn-file "resources/chord-forms.edn"))
(def drum-patterns (read-edn-file "resources/drum-patterns.edn"))
(def chord-strumming-patterns (read-edn-file "resources/chord-strumming-patterns.edn"))

(defn dbhelper [f & args]
  (let [conn (java.sql.DriverManager/getConnection (str "jdbc:sqlite:" DBFILE))
        rc   (apply f conn args)
        rc   (if (sequential? rc) (doall rc) rc)]
    (.close conn)
    rc))

(def dml     (partial dbhelper tla/batch-update))
(def query   (partial dbhelper tla/cursor))

(declare enhance-bass-line-map)
; (declare enhance-song-map)

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
    (dml "delete from song where song_id = ?" [[id]])
    (dml "delete from bar where song_id = ?" [[id]])
    (dml "delete from song_bar_beat where song_id = ?" [[id]])

    (dml (str "insert into song(song_id, song_nm, time_sig_nmrtr_num, time_sig_denom_num,"
                   "time_sig_ppq_num, time_sig_bb_num, bpm_num, drum_ptrn_cd, bass_ty_cd, max_bar) "
                   "values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
              [[id nm numer denom ppq bb bpm drum bass max-bar]])
    (dml "insert into bar (song_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)" bars)
    (dml "insert into song_bar_beat (song_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)" bbcs)
    nm))

(defn sparse-beats
  "Sparsely assign chords within a bar.
   Supports up to four chords per bar"
  [[a b c d :as chords]]
  (case (count chords)
    1 [[1 a]]
    2 [[1 a] [3 b]]
    3 [[1 a] [3 b] [4 c]]
    4 [[1 a] [2 b] [3 c] [4 d]]
    :default (throw (Exception. "expecting 1 to 4 chords per bar"))))

(defn dense-beats
  "Assign chords for each beat in a bar.
   Supports up to four chords per bar"
  [[a b c d :as chords]]
  (mapv (fn [x y] [x y])
        (range 1 5)
        (case (count chords)
          1 [a a a a]
          2 [a a b b]
          3 [a a b c]
          4 [a b c d])))


(def midi->note (zipmap
                 (range 21 128)
                 (cycle [:A :Bb :B :C :Db :D :Eb :E :F :F# :G :Ab])))

; Middle octave - C3 (also just C for convenience)
(def middle-octave->midi {:C 60, :C# 61, :Db 61,
                          :D 62, :D# 63, :Eb 63,
                          :E 64,
                          :F 65, :F# 66, :Gb 66,
                          :G 67, :G# 68, :Ab 68,
                          :A 69, :A# 70, :Bb 70,
                          :B 71})

(def notedb2 (reduce (fn [acc [k v]] (assoc acc k v))
                     middle-octave->midi
                     (for [octave   (range -2 9)
                           [id freq] middle-octave->midi]
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
  (dml "insert into note (note_cd, midi_num) values (?, ?)"
            (for [[k v] notes] [(name k) v])))

(defn enhance-song-map
  "Takes map of song and enhanced it by adding keys
    :bars - with sparsely assigned chords to beats
    :bbcs - with densely assigned chords to beats
    :max-bar - length of score in bars
    :midi-notes - notes converted to MIDI notes"
  [song-map]
  {:pre [(every? song-map [:id :tab-score])]}
  (let [bars (-> song-map :tab-score tabs->bars)
        assign (fn [f coll]
                 (->> coll
                      (map-indexed (fn [bar tab]
                                     (for [[beat chord] (f tab)]
                                       [(song-map :id) (inc bar) beat chord])))
                      (apply concat)
                      (into [])))]
    (-> song-map
        (assoc :enhanced? true
               :bars (assign sparse-beats bars)
               :bbcs (assign dense-beats bars)
               :max-bar (count bars)))))

(def
  ^{:arglists '([edn-file])
    :doc "Imports song from EDN file into database"}
  import-song (comp save-song-to-db enhance-song-map read-edn-file))

(defn enhance-bass-line-map
  "Takes map of bass line and adds keys
    :bars - with sparsely assigned chords to beats
    :bbcs - with densely assigned chords to beats
    :max-bar - length of score in bars
    :midi-notes - notes converted to MIDI notes"
  [song-map]
  {:pre [(every? song-map [:id :tab-score :notes])]}
  (let [id (song-map :id)
        bars (-> song-map :tab-score tabs->bars)
        assign (fn [f coll]
                 (->> coll
                      (map-indexed (fn [bar tab]
                                     (for [[beat chord] (f tab)]
                                       [id (inc bar) beat chord])))
                      (apply concat)
                      (into [])))]
    (-> song-map
        (assoc :enhanced? true
               :bars (assign sparse-beats bars)
               :bbcs (assign dense-beats bars)
               :midi-notes  (map-indexed
                             (fn [idx [note dur]]
                               [id (inc idx) (notedb note) (or dur 4)])
                             (song-map :notes))
               :max-bar (count bars)))))

(defn chord-notes [root form]
  (map #(+ -12 (notedb root) % -1) (first (chord-forms form))))

(def chorddb
  (for [root [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
        form (keys chord-forms)]
    (let [[pattern maj-ind] (chord-forms form)
          [a b c d e f] (map #(+ (notedb root) % -1) pattern)
          r             (name root)]
      {:chord-id      (str (name root) (if (= form :major) "" (name form)))
       :root-midi-num (get notedb root)
       :chord-form-cd (name form)
       :root-note-cd  r
       :major-ind     (name maj-ind)
       :a    a
       :b    b
       :c    c
       :d    d
       :e    e
       :f    f})))

(defn transpose-note
  "Takes note and number of semitones and returns transposed MIDI note. A note
   can be a literal representation of note (such as G, G2, etc.) or a MIDI
   note"
  [note semitones]
  (let [note (cond (keyword? note) (notedb note)
                   (string? note) (-> note keyword notedb)
                   (integer? note) note
                   :else (throw (Exception. "expecting keyword, string or integer")))]
    (+ note semitones)))

(defn transpose-chord
  "Takes chord and semitones and returns a string
   representing a transposed chord"
  [chord semitones]
  (if (zero? semitones)
    chord
    (let [[root-midi-num chord-form] (as-> chord $
                                       (filter #(= (get % :chord-id) $) chorddb)
                                       (first $)
                                       ((juxt :root-midi-num :chord-form-cd) $))
          transposed-root (-> root-midi-num (transpose-note semitones) midi->note name)
          chord-form (if (= "major" chord-form) "" chord-form)]
      (str transposed-root chord-form))))

(defn transpose-bass-line
  "Takes bass line and number of semitones and updates
   keys :id, :bars, and :bbcs. Returns a map representing a
   transposed bass line"
  [bass-line semitones]
  (let [enhanced (if (:enhanced? bass-line)
                   bass-line
                   (enhance-bass-line-map bass-line))
        transposed-id (str (bass-line :id)
                           "-"
                           (-> bass-line
                               :bbcs
                               first
                               tla/fourth
                               (transpose-chord semitones)))
        helper     (fn [coll]
                     (map (fn [[_ bar beat chord]]
                            [transposed-id bar beat (transpose-chord chord semitones)])
                          coll))
        rc (assoc enhanced
                  :id    transposed-id
                  :bbcs  (-> bass-line :bbcs helper)
                  :bars  (-> bass-line :bars helper)
                  :midi-notes (->> bass-line
                                   :notes
                                   (map-indexed (fn [idx [note dur]]
                                                  [transposed-id
                                                   idx
                                                   (transpose-note note semitones)
                                                   (if (nil? dur) 4 dur)]))))]
    rc))

(defn save-chords [chords]
  (dml (str "insert into chord(chord_id, root_midi_num, chord_form_cd, root_note_cd,"
                 "  major_ind, midi1_num, midi2_num, midi3_num, midi4_num,"
                 "  midi5_num, midi6_num"
                 ") values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
            (map (juxt :chord-id :root-midi-num :chord-form-cd :root-note-cd :major-ind :a :b :c :d :e :f) chords)))


(def chords (reduce (fn [acc [k f]]
                      (assoc acc
                             (keyword (str (name k) (if (= f :major) "" (name f))))
                             (chord-notes k f)))
                    {}
                    (for [k [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
                          f (keys chord-forms)]
                      [k f])))

;-----------------------------------------------------------
(defn save-bass-line-to-db
  "Takes enhanced map of bass line, and saves it to 
   BASS_LINE, BASS_LINE_CHORD, BASS_LINE_NOTE.
   Returns bass line ID."
  [bass-line]
  {:pre [(every? bass-line [:id :desc :max-bar :enhanced? :midi-notes])]}
  (let [{:keys [id desc bars midi-notes max-bar]} bass-line]
    (dml "delete from bass_line       where bass_line_id = ?" [[id]])
    (dml "delete from bass_line_chord where bass_line_id = ?" [[id]])
    (dml "delete from bass_line_note  where bass_line_id = ?" [[id]])

    (dml "insert into bass_line(bass_line_id, bar_cnt, bass_line_desc) values (?, ?, ?)"
              [[id max-bar desc]])
    (dml "insert into bass_line_chord (bass_line_id, bar_id, beat_id, chord_id) values (?, ?, ?, ?)"
              bars)
    (dml "insert into bass_line_note (bass_line_id, order_num, midi_num, note_dur_num) values (?, ?, ?, ?)"
              midi-notes)
    id))


(defn import-bass-line
  "Imports bass-line from a file into internal SQLite database"
  [song-edn-file]
  (let [enhanced (-> song-edn-file
                     read-edn-file
                     enhance-bass-line-map)]
    (doseq [semitones [-5 -4 -3 -2 -1 1 2 3 4 5 6 0]]
      (println "Semitones" semitones)
      (let [rc1 (transpose-bass-line enhanced semitones)
            _   (println rc1)]
        (save-bass-line-to-db rc1)))))


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
                                    "bass-15" "bass-15" "bass-15" "bass-1234"])
           "bass-15-68" (partial f ["bass-15" "bass-15" "bass-15" "bass-15"
                                    "bass-5321" "bass-15" "bass-15" "bass-5321"])
           "bass-ud2"   (partial f ["bass-1234" "bass-1235" "bass-5321" "bass-4321"
                                    "bass-1234" "bass-1235" "bass-5321" "bass-4321"])
           "bass-ud3"   (partial f ["bass-1234" "bass-5321" "bass-1235" "bass-5321"
                                    "bass-1234" "bass-5321" "bass-1235" "bass-5321"]))))

(def chord-based-bass-db (init-chord-based-bass-db))
