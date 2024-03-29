(ns midi.core
  (:require [clojure.string :as str]
            [timlib.core :as tla]
            [midi.dbload :as db]
            [midi.midifile2 :as midifile]))

(defn bar->tc
  "Returns beginning on bar in timecode ticks"
  [bar]
  (* bar db/whole-note-tc))

(defn beat->tc
  "Returns beginning of beat in timecode. Beat is a quarter note"
  [beat]
  (* beat db/quarter-note-tc))  

(defn music-dur->tc
  "Returns musical duration of a note in timecode"
  [music-dur]
  (/ db/whole-note-tc music-dur))

(defn tcnext
  "Next timecode after this note"
  [tc dur]
  (+ tc dur))

(defn tcoff
  "Timecode to turn off this timecode"
  [tc dur]
  (dec (tcnext tc dur)))

(defn tc-pair
  "Takes tc, tcoff, chan, note, vel and makes a pair of on/off notes
   suitable to play a note on a MIDI device"
  [tc tcoff chan note vel]
  [[tc chan note vel]
   [tcoff chan note 0]])

(defn ttape->mtape
  "Converts ttape to MIDI tape format"
  ([ttape] (ttape->mtape ttape 1))
  ([ttape tempo-correction]
   (loop [prior 0, ppq 0, tempo 0, acc [], ttape ttape]
     (if-not (seq ttape)
       acc
       (let [[[tc cmd val] & more] ttape]
         (case cmd
           :set-tempo
           (recur tc ppq val acc more)
           :time-signature
           (let [[numer _ clicks _] val]
             (recur tc (* numer clicks tempo-correction) tempo acc more))
           (let [x (/ (* (- tc prior) tempo) (* 1000 ppq))]
             (recur tc ppq tempo (conj acc [x val]) more))))))))

(defn fill-in-beats-2-and-4
  "1. Place the roots of the indicated chords on beats 1 and 3 to create the skeleton
      of the bass line. As far as possible, select the root notes so that the interval
      between them is minimized (e.g., choose a fourth up rather than a fifth down,
      a third down rather than a sixth up, etc.).

   2. Fill in beats 2 and 4 according to the interval between the notes on beats 1 and 3:
      a. If  the  roots  are  separated  by  a  third,  put  a  diatonic  passing  tone  between  them.
         In measure 1 we insert E between F and D, and from measure 2 to measure 3,
         we insert Bb between C and A.
      b. If  the  roots  are  separated  by  a  fourth  or  fifth,  fill  out  the  interval
         with  a  tone drawn from the first chord. In measure 1 we insert F between D and the G
         in the following measure, and in measure 2 we insert D between G and C.
      c. If the roots are separated by a major or minor second, repeat the bass note as shown in measures 3 and 4.
         As can be seen in measure 4, an octave leap can  be  used  instead  of  a  repetition.
         This  move  is  sometimes  used  reposition  a  bass  line  that  is  approaching  either
         the  lower  or  the  upper  extreme of the range of the instrument.
      d. If a chord is held for the entire duration of a measure, the bass line can be filled out
         with a scalewise line from the root of the chord down to the fifth.
         This is done under the FÎ chord in measure 5."
  [chord1 chord2]
  (let [[root n3] chord1
        [root2]   chord2]
    (case (Math/abs (- root root2))
      1 root
      2 root
      3 (+ root 2)
      4 (+ root 2)
      n3)))


(defn tracks->ttape
  "Makes a tick tape from an array of raw notes each of which has a stucture:
   [timecode channel note velocity]
   Tick Tape format:
   Field       Type      Description
   --------------------------------------------------------------------------------------
   1: MIDI tick   integer   Gradually increasing throughout the tape. Treat it as a timecode
   2: event-type  keyword   One of :set-tempo, :time-signature, or :data
   3: data        varies    For :set-tempo is a single integer representing number of microseconds per quarter note
                            For :time-signature array of four matching to MIDI's time signature event
                            For :data - array of notes played during this MIDI tick
                            each note is [timecode, channel, note, velocity].
   Example:
   [0	:set-tempo	434464
   [0	:time-signature	[4 2 24 8]
             ; numer
             ; denom - (power of two)
             ; midi clicks in a single metronome click
             ; number of 32nd notes in midi quarter note 
   [384	:data	        [[384 2 70 50]]
   [477	:data	        [[477 2 98 74]]
   [479	:data	        [[479 2 101 78]]
   [480	:data	        [[480 8 89 61] [480 8 86 55]]
   [489	:data	        [[489 8 86 0] [489 8 89 0]]
   [492	:data	        [[492 8 89 60] [492 8 86 63]]"
  ([tracks]     (tracks->ttape tracks 120))
  ([tracks bpm] (tracks->ttape tracks bpm [4 2 24 8]))
  ([tracks bpm signature]
   (->> tracks
        (group-by first)
        (map (fn [[tc notes]] [tc :data (map rest notes)]))
        (cons [0 :set-tempo (/ 60000000 bpm)])
        (cons [0 :time-signature signature])
        (sort-by first))))


(defn midi-synthesizer
  "Takes a collection of [channel instrument] assignments and
   returns fn which accepts channel, note and velocity
   to play them via MIDI synthesizer"
  [instrument-assignment]
  (let [synth (doto (javax.sound.midi.MidiSystem/getSynthesizer)
                .open)
        channels (-> synth .getChannels)]
    (doseq [[channel assignment] instrument-assignment]
      (let [instrument (-> synth
                           .getDefaultSoundbank
                           .getInstruments
                           (nth assignment))]
        (println "Playing" (.getName instrument) "on channel" channel)
        (.loadInstrument synth instrument)
        (.programChange (nth channels channel) assignment)))
    (fn [c note vol]
      (.noteOn (nth channels c) note vol))))


(defn play-mtape
  "Plays collection of vectors: [timecode, channel, note, velocity]"
  [mtape]
  (let [player (midi-synthesizer
                [[db/chord-chan (db/instruments "Acoustic Guitar (steel)")]
                 [db/bass-chan  (db/instruments "Fretless Bass")]])]
    (doseq [[tc notes] mtape]
      (Thread/sleep tc)
      (doseq [[chan note vel] notes]
        (player chan note vel)))))



(defn paste-bass-line [start-bar bass-line-id transposition vel]
  (->> [bass-line-id]
       (db/query "select n.midi_num                  note,
                         cast(b.note_dur_num as int) note_dur_num
                  from bass_line_note b
                       join note      n on (n.note_cd = b.note_cd)
                  where b.bass_line_id = :1
                  order by order_num")
       rest
       (reduce (fn [[acc tc] [note note-dur]]
                 (let [note (+ note transposition)
                       next-tc (tcnext tc (music-dur->tc note-dur))]
                   [(concat acc (tc-pair tc (dec next-tc) db/bass-chan note vel))
                    next-tc]))
               [[] (bar->tc start-bar)])
       first))


(defn combine-bass-lines
  [[timeline tape :as rc] [bass-line-id start-bar stop-bar transposition]]
  (let [bars (range start-bar (inc stop-bar))]
    (if (some identity (vals (select-keys timeline bars)))
      rc
      [(reduce (fn [acc bar] (assoc acc bar bass-line-id)) timeline bars)
       (concat tape
               (paste-bass-line start-bar bass-line-id transposition db/bass-vel))])))


(defn patterns-bass-track
  [song-id]
  (let [patterns (db/query (str "select bass_line_id, beg_bar_id, end_bar_id, transp_num transp_num "
                                "from bass_line_bar_v "
                                "where song_id = :1 "
                                "order by beg_bar_id")
                           [song-id])
        maxbar (-> (db/query "select max(bar_id) bar from bar where song_id = :1" [song-id])
                   second
                   first)
        [info rc] (reduce combine-bass-lines
                          [(into (sorted-map) (zipmap (range 1 (inc maxbar)) (repeat nil)))
                           []]
                          (rest patterns))]
    (with-meta rc {:bass info})))


(defn apply-drum-pattern
  "Takes pattern (coll of [vel dur]), drum, and bar.
   Applies a drumming pattern to this bar"
  [pattern drum bar]
  (loop [rc [], tc (bar->tc bar), pattern pattern]
    (if-not (seq pattern)
      rc
      (let [[[vel dur] & more] pattern
            dur (or dur 4)
            next-tc (+ tc (music-dur->tc dur))]
        (recur (conj rc
                     [tc db/drum-chan drum vel]
                     [(dec next-tc) db/drum-chan drum 0])
               next-tc
               more)))))


(defn drum-pattern->ttape
  "Takes bar and drum pattern and returns ttape
   for this bar.
   Each drum pattern can have one or more drums"
  [bar pattern]
  (mapcat (fn [drum]
            (apply-drum-pattern (pattern drum)
                                 (db/note-db drum)
                                 bar))
          (keys pattern)))


(defn walking-bass
  "Takes a chord, number of beats of this chord, and the
   next chord and generate a walking bass line between them"
  ([chord nbeats]
   (walking-bass chord nbeats chord))
  ([chord nbeats next-chord]
   (let [a (get-in db/chord-db [chord :notes])
         b (get-in db/chord-db [next-chord :notes])
         [a1 a3 a5 _]    a
         rc     (case nbeats
                  1 [a1]
                  2 [a1 (fill-in-beats-2-and-4 a b)]
                  4 [a1 (dec a1) (- a1 3) (- a1 5)]
                  8 [a1 (+ a1 2) a3 (- a5 2) a5 a3 (+ a1 2) a1]
                  [])]
     rc)))

(defn synthetic-bass-track
  "Takes BBCs, returns a synthesized walking bass track"
  [bbcs]
  (->> bbcs
       (map tla/third)
       tla/compress
       (partition 3 2 nil)
       (mapcat (partial apply walking-bass))
       (map (fn [note] (- note 24)))    ; lower one octave to make jazzier
       (map-indexed (fn [idx note]
                      (let [beat (+ idx 4)  ; compensate for intro in bar 0
                            tc (beat->tc beat)]
                        (tc-pair tc
                                 (tcoff tc db/quarter-note-tc)
                                 db/bass-chan
                                 note
                                 db/bass-vel))))
       (apply concat)))

(defn make-drum-track
  "Gets data stored in table SONG_DRUM"
  [song-id]
  (->> [song-id]
       (db/query "select bar_id, drum_ptrn_cd from song_drum where song_id = ? order by bar_id")
       rest
       (mapcat (fn [[bar pattern]]
                 (drum-pattern->ttape bar (db/drum-pattern-db pattern))))))


(defn strum-chords
  "Applies a strumming pattern to chords in a bar.
   Strumming pattern is a collection of velocities. A velocity can
   optionally be paired with duration, when duration is omitted it
   is assumed to be a quarter note"
  [pattern bar chords]
  (let [begin (bar->tc bar)
        [a b c d] chords]
    (loop [rc []
           tc begin
           pattern pattern]
      (if-not (seq pattern)
        rc
        (let [[[vel dur] & more] pattern
              dur (or dur 4)
              next-tc (tcnext tc (music-dur->tc dur))
              chord (cond
                      (< tc (+ begin (* 1 db/quarter-note-tc))) a
                      (< tc (+ begin (* 2 db/quarter-note-tc))) b
                      (< tc (+ begin (* 3 db/quarter-note-tc))) c
                      :else d)
              notes (get-in db/chord-db [chord :notes])
              ons   (map #(vector tc db/chord-chan % vel) notes)
              offs  (map (fn [[_ c n _]] [(dec next-tc) c n 0]) ons)]
          (recur (concat rc ons offs)
                 next-tc
                 more))))))


(defn strum-chord-track
  "Produce a chord track by strumming pattern over bbcs"
  [pattern-name bbcs]
  (let [pattern (db/chord-strumming-pattern-db pattern-name)]
    (->> bbcs
         (map tla/third)
         (partition 4)
         (map-indexed (fn [bar chords]
                        (strum-chords pattern (inc bar) chords)))
         (apply concat))))


(defn play-song
  "Plays a song"
  [song-name]
  (let [[song-id bpm drum-pattern bass-method]
        (->>  [song-name]
              (db/query "select song_id, bpm_num, drum_ptrn_cd, bass_ty_cd
                         from song
                         where upper(song_nm) = ?")
              second)
        bbcs        (db/get-song-bbcs song-name)
        chord-track (strum-chord-track "rhythm-3-3-2" bbcs)
        ;; TODO - fix this: drum-track  (make-drum-track drum-pattern bbcs)
        bass-track  (if (= bass-method "patterns")
                      (patterns-bass-track song-id)
                      (synthetic-bass-track bbcs))
        m           (meta bass-track)
        info        (if m
                      (m :bass)
                      (apply sorted-map (interleave (range 1 100) (repeat bass-method))))]
    (println (format "song=%s, bpm=%d, drums=%s, bass=%s"
                     song-name bpm drum-pattern bass-method))
    (println (tla/view (map (fn [[bar v] c] [bar v (pr-str c)])
                            info
                            (partition 4 (map (fn [[_ _ c]] c) bbcs)))))
    (-> (concat chord-track bass-track)
        (tracks->ttape bpm)
        ttape->mtape
        play-mtape)))


(defn track->duration-track
  [track]
  (let [sorted (sort-by (juxt tla/third first (comp - tla/fourth)) track)
        dur1  (map (fn [[tc c note vel] [ntc _ _ _]]
                     (if (zero? vel)
                       nil
                       [tc c note vel (- ntc tc)]))
                   sorted
                   (rest sorted))]
    (remove nil? dur1)))

(defn song-name->midi-file-name
  "Returns relative path to the MIDI file corresponding to song name"
  [song-name]
  (as-> song-name $
    (str/lower-case $)
    (str/replace $ #"\s+" "_")
    (str "resources/midi/" $ ".midi")))


(defn export-song
  "Export song to a MIDI file. Song's name should match
   the one in a SONG table. If no file name provided, the MIDI file
   is created in resources/midi."
  ([song]
   (export-song song
                (song-name->midi-file-name song)))
  ([song-name file-name]
   (let [[song-id bpm drum-pattern bass-method strum-pattern]
         (-> (db/query "select song_id, bpm_num, drum_ptrn_cd, bass_ty_cd, strum_ptrn_cd
                        from song
                        where upper(song_nm) = ?" [song-name])
             second)
         bbcs        (db/get-song-bbcs song-id)
         bass-track  (if (= bass-method "patterns")
                       (patterns-bass-track song-id)
                       (synthetic-bass-track bbcs))
         tracks (mapcat track->duration-track
                        [(make-drum-track song-id)
                         bass-track
                         (strum-chord-track strum-pattern bbcs)])]
     (println (format "song=%s, bpm=%d, drums=%s, bass=%s, strum=%s"
                      song-name bpm drum-pattern bass-method strum-pattern))
     (midifile/notes->midi-file tracks bpm file-name))))


(def usage (str "
Usage: lein run
--export [song]               - export song to MIDI file
--export-all                  - export all songs
--import [file.edn]           - import song into db
--import-bass-line [file.edn] - import bass line into db
--list                        - list songs in the db
--play [song]                 - play song  TODO: THIS IS NOT WORKING
--usage                       - this message
"))

(defn -main
  ([] (-main "--usage"))
  ([cmd & [arg & _]]
   (case cmd
     "--export"
     (export-song arg)
     "--export-all"
     (doseq [s (->> (db/list-songs) rest (map second))] (export-song s))
     "--import-bass-line"
     (-> arg db/import-bass-line)
     "--import"
     (->> arg db/import-song export-song)
     "--list"
     (println (tla/view (db/list-songs)))
     "--play"
     (play-song arg)
     "--usage"
     (println usage)
     (println usage))))

