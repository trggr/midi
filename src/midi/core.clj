(ns midi.core
  (:require [clojure.string :as str]
            [midi.timlib :as tla]
            [midi.dbload :as db]
            [midi.midifile2 :as midifile]))

(def bar->drum-fills {0 "drums-intro"
                      8 "drums-fill2"
                      16 "drums-fill3"
                      24 "drums-fill2"
                      32 "drums-fill3"})

(defn bar->timecode
  "Returns beginning on bar in timecode ticks"
  [bar]
  (* bar db/WHOLE-NOTE))

(defn duration->timecode
  "Returns duration of note in time code ticks"
  [duration]
  (/ db/WHOLE-NOTE duration))

(defn wrap-in-timecode
  "Takes beat, note, velocity, and channel and wraps it into timecode 
   suitable to play the note on this channel"
  ([beat note duration velocity channel]
   (let [on  (* duration beat)
         off (+ on duration -1)]
     [[on channel note velocity] [off channel note 0]])))

(defn wrap-in-walking-bass-timecode [beat note]
  (wrap-in-timecode beat
                    note
                    db/QUARTER-NOTE
                    db/BASS-VELOCITY
                    db/BASS-CHANNEL))

(defn ttape->mtape
  "Converts ttape to MIDI tape format"
  ([ttape] (ttape->mtape ttape 1))
  ([ttape tempo-correction]
   (loop [prior 0, ppq 0, tempo 0, acc [], xs ttape]
     (if-not (seq xs)
       acc
       (let [[[tc cmd val] & others] xs]
         (cond (= :set-tempo cmd)
               (recur tc   ppq  val acc others)
               (= :time-signature cmd)
               (let [x (* (first val) (tla/third val) tempo-correction)]
                 (recur tc  x tempo acc others))
               :else
               (let [x (/ (* (- tc prior) tempo) (* 1000 ppq))]
                 (recur tc ppq tempo (conj acc [x val]) others))))))))

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
  [x y]
  (let [[a n3] x
        [b]    y
        sep (Math/abs (- a b))]
    (cond (<= 1 sep 2) a
          (<= 3 sep 4) (+ a 2)
          (<= 5 sep 7) n3
          :else        n3)))

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
  (let [player (midi-synthesizer [[db/CHORD-CHANNEL 26]
                                  [db/BASS-CHANNEL  32]])]
    (doseq [[tc notes] mtape]
      (Thread/sleep tc)
      (doseq [[chan note vel] notes]
        (player chan note vel)))))

(defn get-song-bbcs
  "Returns a collection of BBC (bar-beat-chord) elements for a given song.
   For a typical 4/4, 32-bar song this collection is 128 elements
   long and shows what chord is played on each bar and beat"
  [song-name]
  (let [query "with
               bars  as (select * from bar_flat where upper(song_nm) = :1),
               beats as (select * from all_beat where bar_id <= (select max(bar_id) from bars)),
               fill  as (
                   select a.bar_id, a.beat_id, b.bar_id orig_bar_id, b.beat_id orig_beat_id,
                          b.chord_id,
                          lag(b.chord_id, 1) over (partition by a.bar_id order by a.beat_id) c1,
                          lag(b.chord_id, 2) over (partition by a.bar_id order by a.beat_id) c2,
                          lag(b.chord_id, 3) over (partition by a.bar_id order by a.beat_id) c3,
                          lag(b.chord_id, 4) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c4,
                          lag(b.chord_id, 5) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c5,
                          lag(b.chord_id, 6) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c6,
                          lag(b.chord_id, 7) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c7,
                          lag(b.chord_id, 8) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c8
                   from beats                a
                        left outer join bars b on (a.bar_id = b.bar_id and a.beat_id = b.beat_id)),
               rc as (select bar_id, beat_id, coalesce(chord_id, c1, c2, c3, c4, c5, c6, c7, c8) chord_id
                      from fill)
               select bar_id, beat_id, chord_id
               from rc
               where chord_id is not null
               order by bar_id, beat_id"
        bbcs  (db/query query [(str/upper-case song-name)])]
    (map (fn [[bar beat chord]]
           [bar beat (keyword chord)])
         (rest bbcs))))

(defn paste-bass-line [from-bar bass-line-id transposition vel]
  (println "expand-bass-line" bass-line-id)
  (->> [bass-line-id]
       (db/query "select n.midi_num, b.note_dur_num
                  from bass_line_note b join note n on (n.note_cd = b.note_cd)
                  where b.bass_line_id = :1
                  order by order_num")
       rest
       (reduce (fn [[acc tc] [note dur]]
                 (let [note (+ note transposition)
                       next-tc (+ tc (duration->timecode dur))]
                   [(conj acc
                          [tc db/BASS-CHANNEL note vel]
                          [(dec next-tc) db/BASS-CHANNEL note 0])
                    next-tc]))
               [[]
                (bar->timecode from-bar)])
       first))

(defn combine-bass-lines [[timeline tape :as rc]
                          [bass-line-id from-bar to-bar transposition]]
  (let [bars (range from-bar (inc to-bar))]
    (if (some identity (vals (select-keys timeline bars)))
      rc
      [(reduce (fn [a k] (assoc a k bass-line-id))
               timeline
               bars)
       (concat tape
               (paste-bass-line from-bar bass-line-id transposition db/BASS-VELOCITY))])))

(defn bass-patterns [song-id]
  (let [song-id (str song-id)
        patterns (db/query (str "select bass_line_id, beg_bar_id, end_bar_id, transp_num - 12 transp_num "
                                 "from bass_line_bar_v "
                                 "where song_id = ? "
                                 "order by beg_bar_id")
                            [song-id])
        _ (println patterns)
        maxbar (-> (db/query "select max(bar_id) bar from bar where song_id = ?" [song-id])
                   second
                   first)
        [info rc] (reduce combine-bass-lines
                          [(into (sorted-map) (zipmap (range 1 (inc maxbar)) (repeat nil)))
                           []]
                          (rest patterns))]
    (with-meta rc {:bass info})))


(defn cover-bar-with-drum
  "Takes pattern, drum note, and covers a bar with this pattern.
   Drum pattern is a collection of [velocity duration] elements"
  [pattern drum-note bar]
  (loop [rc [], tc (bar->timecode bar), pattern pattern]
    (if (empty? pattern)
      rc
      (let [[vel dur] (first pattern)
            dur (or dur 4)
            next-tc (+ tc (duration->timecode dur))]
        (recur (conj rc
                     [tc db/DRUMS-CHANNEL drum-note vel]
                     [(dec next-tc) db/DRUMS-CHANNEL drum-note 0])
               next-tc
               (rest pattern))))))

(defn apply-drum-pattern
  "Takes drums pattern and applies it to a bar. Each drum pattern can have one or more drums"
  [pattern bar]
  (mapcat #(cover-bar-with-drum (pattern %) (db/notedb %) bar)
          (keys pattern)))

(defn compress
  "Takes collection and turns it into element, number of occurrences
   coll. E.g. [a a a b b] => [a 3 b 2]"
  [coll]
  (->> coll
       (partition 2 1 nil)
       (reduce (fn [[acc n] [x y]]
                 (if (= x y)
                   [acc (inc n)]
                   [(conj acc x n) 1]))
               [[] 1])
       first))

(comment
  (compress [5 1 1 1 2 2 2 2 4])  ;=> [5 1 1 3 2 4 4 1]
  (compress [2])  ;=> []
  (compress [])  ;=> []
  )

(defn walking-bass
  "Takes a chord, number of beats of this chord, and the
   next chord and generate a walking bass line between them"
  ([chord nbeats]
   (walking-bass chord nbeats chord))
  ([chord nbeats next-chord]
   (let [a (db/chords chord)
         b (db/chords next-chord)
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
       compress
       (partition 3 2 nil)
       (mapcat (partial apply walking-bass))
       (map (fn [note] (- note 12)))    ; lower one octave to make jazzier
       (map-indexed (fn [beat note]
                      (wrap-in-walking-bass-timecode (+ beat 4) ; compensate for zero bar
                                                     note)))
       (apply concat)))

(defn make-drum-track
  "Covers all availble beats with a drum-pattern-nm. Covers zero beat with
   metronom clicks"
  [pattern-nm beats]
  (let [maxbar (inc (reduce max (map first beats)))]
    (mapcat #(apply-drum-pattern
              (db/drum-patterns (get bar->drum-fills % pattern-nm))
              %)
            (range maxbar))))

(comment
  ; (def bbcs (get-song-bbcs db/conn "MISTY"))

  ; chord-track (make-chord-track2 bbcs "things-we-said-today")
  ; (make-chord-track2 "things-we-said-today" bbcs)

  ; drum-track  (make-drum-track drum-pattern bbcs)
  ; (->> bbcs (map (fn [x] (nth x 2))) (partition 4))

  )

(defn strum-chords
  "Using a strumming pattern cover a bar with chords.
   Strumming pattern is a collection of velocities. A velocity can
   optionally paired with duration, if duration is omitted it
   is assumed to be 4 beats"
  [strumming-pattern bar bar-chords]
  (let [begin (bar->timecode bar)
        [a b c d] bar-chords]
  (loop [rc []
         tc begin
         pattern strumming-pattern]
    (if (empty? pattern)
      rc
      (let [[vel dur] (first pattern)
            dur (or dur 4)
            next-tc (+ tc (duration->timecode dur))
            chord-notes (db/chords (cond
                                     (< tc (+ begin (* 1 db/QUARTER-NOTE))) a
                                     (< tc (+ begin (* 2 db/QUARTER-NOTE))) b
                                     (< tc (+ begin (* 3 db/QUARTER-NOTE))) c
                                     :else d))
            ons   (map #(vector tc db/CHORD-CHANNEL % vel) chord-notes)
            offs  (map (fn [[_ c n _]] [(dec next-tc) c n 0]) ons)]
        (recur (concat rc ons offs)
               next-tc
               (rest pattern)))))))

(defn strum-chord-track
  "Produce a chord track by strumming pattern over bbcs"
  [pattern-name bbcs]
  (let [pattern (db/chord-strumming-patterns pattern-name)]
    (->> bbcs
         (map (fn [[_ _ chord]] chord))
         (partition 4)
         (map-indexed (fn [bar chords]
                        (strum-chords pattern (inc bar) chords)))
         (apply concat))))

(defn play-song
  "Plays a song"
  [song-name]
  (let [[song-id bpm drum-pattern bass-method]
        (-> (db/query "select song_id, bpm_num, drum_ptrn_cd, bass_ty_cd
                       from song
                       where upper(song_nm) = ?" [song-name])
            second)
        bbcs        (get-song-bbcs song-name)
        chord-track (strum-chord-track "rhythm-3-3-2" bbcs)
        drum-track  (make-drum-track drum-pattern bbcs)
        bass-track  (if (= bass-method "patterns")
                      (bass-patterns song-id)
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
    (-> (concat chord-track bass-track drum-track)
        (tracks->ttape bpm)
        ttape->mtape
        play-mtape)))

(defn track->duration-track [track]
  (let [sorted (sort-by (juxt tla/third first (comp - tla/fourth)) track)
        dur1  (map (fn [[tc c note vel] [ntc _ _ _]]
                     (if (zero? vel)
                       nil
                       [tc c note vel (- ntc tc)]))
                   sorted
                   (rest sorted))]
    (remove nil? dur1)))

(defn save-song
  "Plays a song"
  [song-name]
  (let [[song-id bpm drum-pattern bass-method]
        (-> (db/query "select song_id, bpm_num, drum_ptrn_cd, bass_ty_cd
                       from song
                       where upper(song_nm) = ?" [song-name])
            second)
        bbcs        (get-song-bbcs song-name)
        chord-track (strum-chord-track "charleston" bbcs)
        drum-track  (make-drum-track drum-pattern bbcs)
        bass-track  (if (= bass-method "patterns")
                      (bass-patterns song-id)
                      (synthetic-bass-track bbcs))
        m           (meta bass-track)
        info        (if m
                      (m :bass)
                      (apply sorted-map (interleave (range 1 100) (repeat bass-method))))
        drum-track-dur  (track->duration-track drum-track)
        bass-track-dur  (track->duration-track bass-track)
        chord-track-dur (track->duration-track chord-track)]
    (println (format "song=%s, bpm=%d, drums=%s, bass=%s"
                     song-name bpm drum-pattern bass-method))
    (println (tla/view (map (fn [[bar v] c] [bar v (pr-str c)])
                            info
                            (partition 4 (map (fn [[_ _ c]] c) bbcs)))))
    ; (midifile/save (str song-name ".midi") (concat chord-track bass-track drum-track) bpm)
    (println "drum track")
    (doseq [s drum-track-dur] (println s))
    (println "bass track")
    (doseq [s bass-track-dur] (println s))
    (midifile/save (str song-name ".midi")
                   (concat drum-track-dur bass-track-dur chord-track-dur)
                   bpm)))

(defn -main [& _]
  (doseq [song ["ALL THE THINGS YOU ARE"
                "ALONE TOGETHER"
                "MISTY"
                "AUTUMN LEAVES" "MEDIUM BLUES"
                "IN A SENTIMENTAL MOOD"
                "ALL OF ME"
                "AUTUMN LEAVES"
                "ALL BY MYSELF"
                "LET IT BE"]]
    (save-song song)))
