(ns midi.core
  (:require [clojure.string :as str]
            [midi.timlib :as tla]
            [midi.dbload :as db]))

; Length of quarter note
(def ^:dynamic *qn*            96)
(def ^:dynamic *chord-channel* 2)
(def ^:dynamic *bass-channel*  4)
(def ^:dynamic *drums-channel* 9)

(defn mtape
  "Converts ttape to MIDI tape format"
  ([ttape] (mtape ttape 1))
  ([ttape tempo-correction]
   (loop [prior 0, ppq 0, tempo 0, acc [], xs ttape]
     (if-not (seq xs)
       acc
       (let [[[tc cmd val] & others] xs]
         (cond (= :set-tempo cmd)
               (recur tc   ppq  val acc others)
               (= :time-signature cmd)
               (let [x (* (first val) (nth val 2) tempo-correction)]
                 (recur tc  x tempo acc others))
               :else
               (let [x (/ (* (- tc prior) tempo) (* 1000 ppq))]
                 (recur tc ppq tempo (conj acc [x val]) others))))))))

(defn bbc-ttc
  "Converts BBC into TC"
  [bbc bassf]
  (let [[bar beat chord] bbc
        chord-vel 50
        bass-vel  80
        tc        (* *qn* (+ (* bar 4) (dec beat)))
        chord     (db/chorddb chord)
        bass      (bassf bar beat chord)
        bass      (when (not (nil? bass))
                    [[tc *bass-channel* (- bass 12) bass-vel]])
        on        (concat (map #(vector tc *chord-channel* % chord-vel)
                               (rest chord))
                          bass)
        off       (map (fn [[t c n _]]
                         [(+ t (* 1 *qn*) -1) c n 0])
                       on)]
    (concat on off)))


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

(defn make-chord-track
  "Takes bbc and bassfn and returns a chord track"
  ([bbcs]
   (make-chord-track bbcs (db/chord-based-bass-db "bass-15")))
  ([bbcs bassfn]
   (mapcat (fn [bbc]
             (bbc-ttc bbc bassfn))
           bbcs)))

(defn combine-tracks-to-ttape
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
  ([tracks]     (combine-tracks-to-ttape tracks 120))
  ([tracks bpm] (combine-tracks-to-ttape tracks bpm [4 2 24 8]))
  ([tracks bpm signature]
   (let [xs (sort-by key (group-by first tracks))
         ys (for [[tc data] xs] [tc :data data])]
     (concat [[0 :set-tempo (/ 60000000 bpm)]
              [0 :time-signature signature]] ys))))

(defn note-player [instruments]
  (let [synth (javax.sound.midi.MidiSystem/getSynthesizer)
        _     (.open synth)
        channels (-> synth .getChannels)]
    (doseq [[ch prog] instruments]
      (let [p (-> synth .getDefaultSoundbank .getInstruments (nth prog))]
        (println "Playing" (.getName p) "on channel" ch)
        (.loadInstrument synth p)
        (.programChange (nth channels ch) prog)))
    (fn [c note vol]
      (.noteOn (nth channels c) note vol))))

(defn play-mtape
  "Takes MIDI tape and plays it"
  [tape]
  (let [f (note-player [[*chord-channel* 26]
                        [*bass-channel*  32]])]
    (doseq [[tc notes] tape]
      (Thread/sleep tc)
      (doseq [[_ ch note vel] notes]
        (f ch note vel)))))

(defn get-song-bbcs
  "Returns a collection of BBC (bar-beat-chord) elements for a given song.
   For a typical 4/4, 32-bar song this collection is 128 elements
   long and shows what chord is played on each bar and beat"
  [conn song-name]
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
        bbcs  (tla/cursor conn query [(str/upper-case song-name)])]
    (map (fn [[bar beat chord]]
           [bar beat (keyword chord)])
         (rest bbcs))))

(defn expand-bass-line [bar bassline transp vel]
  (let [notes (tla/cursor db/conn
                          "select n.midi_num, b.note_dur_num
                           from bass_line_note b join note n on (n.note_cd = b.note_cd)
                           where b.bass_line_id = :1 order by order_num"
                          [bassline])]
    (loop [acc [], tc (+ (* *qn* bar 4)), xs (rest notes)]
      (if (empty? xs)
        acc
        (let [[midi dur] (first xs)
              n      (+ midi transp)
              nxt (+ tc (/ (* 4 *qn*) dur))]
          (recur (conj (conj acc [tc *bass-channel* n vel])
                       [(dec nxt) *bass-channel* n 0])
                 nxt
                 (rest xs)))))))

(defn alloc-bass [[timeline tape :as rc]
                  [bassline begin end transp]]
  (let [bars (range begin (inc end))
        vel  90]
    (if (some identity (vals (select-keys timeline bars)))
      rc
      [(reduce (fn [a k] (assoc a k bassline)) timeline bars)
       (concat tape (expand-bass-line begin bassline transp vel))])))

(defn bass-patterns [id]
  (let [ptrns  (tla/cursor db/conn
                           (str "select bass_line_id, beg_bar_id, end_bar_id, (transp_num - 24) transp_num "
                                "from bass_line_bar_v "
                                "where song_id = ? "
                                "order by beg_bar_id")
                           [(str id)])
        maxbar (-> (tla/cursor db/conn "select max(bar_id) bar
                                        from bar
                                        where song_id = ?" [(str id)]) second first)
        [info rc] (reduce alloc-bass
                          [(into (sorted-map) (zipmap (range 1 (inc maxbar)) (repeat nil)))
                           []]
                          (rest ptrns))]
    (with-meta rc {:bass info})))

(defn cover-single-bar-with-single-drum
  "Takes drum-pattern, drum-note, and cover with them a given bar-num.
   Drum pattern is a collection of [velocity duration] elements"
  [drum-pattern drum-note bar-num]
  (loop [rc []
         tc (+ (* *qn* bar-num 4))
         xs drum-pattern]
    (if (empty? xs)
      rc
      (let [[vel dur] (first xs)
            dur (or dur 4)
            nxt (+ tc (/ (* 4 *qn*) dur))]
        (recur (conj (conj rc
                           [tc *drums-channel* drum-note (/ (* 100 vel) 100)])
                     [(dec nxt) *drums-channel* drum-note 0])
               nxt
               (rest xs))))))

(defn cover-bars-with-drum
  "Takes a single drum pattern, a drum-note, and applies it to given bars"
  [drum-pattern drum-note bars]
  (mapcat #(cover-single-bar-with-single-drum drum-pattern drum-note %)
          bars))

(defn cover-bars-with-drum-pattern
  "Takes drums pattern and applies it to collection of bars. Each drum pattern
   can have one or more drums"
  [pattern bars]
  (let [drums (keys pattern)]
    (mapcat (fn [drum]
              (cover-bars-with-drum (pattern drum)
                                    (db/notedb drum)
                                    bars))
            drums)))

(defn beat-notes-to-timecode [beat note]
  (let [vel 120
        on  (+ (* *qn* 4) (* *qn* beat))
        off (+ on *qn* -1)
        x (- note 12)]
    [[on *bass-channel* x vel]
     [off *bass-channel* x 0]]))

(defn chord-beats-pairs
  "Takes BBCs, and returns an ordered collection where each element is a pair
   [chord nbeats] shows how many beats the chord is played"
  [acc bbc]
  (let [[rc x] acc
        [_ _ chord] bbc]
    (if (nil? x)
      [rc [chord 1]]
      (let [[prior nbeats] x]
        (if (= chord prior)
          [rc [prior (inc nbeats)]]
          [(conj rc x) [chord 1]])))))

(defn walk-between-2-chords [[chord nbeats] [next-chord _]]
  (let [a (db/chorddb chord)
        b (db/chorddb (or next-chord chord))
        [a1 a3 a5 _]    a
        rc     (case nbeats
                 1 [a1]
                 2 [a1 (fill-in-beats-2-and-4 a b)]
                 4 [a1 (dec a1) (- a1 3) (- a1 5)]
                 8 [a1 (+ a1 2) a3 (- a5 2) a5 a3 (+ a1 2) a1]
                 [])]
    rc))

(defn synthesize-bass-track
  "Takes BBCs, returns a synthesized bass track"
  [bbcs]
  (->> bbcs
       (reduce chord-beats-pairs [[] nil])
       first
       (tla/mapcat2 walk-between-2-chords)
       (map-indexed beat-notes-to-timecode)
       (apply concat)))

(defn make-drum-track
  "Covers all availble beats with a drum-pattern-nm. Covers zero beat with
   metronom clicks"
  [pattern-nm beats]
  (let [maxbar (inc (reduce max (map first beats)))]
    (concat (cover-bars-with-drum-pattern
             (db/drum-pattern-db "drums-intro")
             [0])
            (cover-bars-with-drum-pattern
             (db/drum-pattern-db pattern-nm)
             (range 1 maxbar)))))

(defn play-song
  "Plays a song"
  [song]
  (let [[id bpm drum-pattern bass-type]
        (-> (tla/cursor db/conn "select song_id, bpm_num, drum_ptrn_cd, bass_ty_cd
                                 from song
                                 where upper(song_nm) = ?" [song])
            second)
        bbcs        (get-song-bbcs db/conn song)
        chord-track (make-chord-track bbcs
                                      (if-let [f (get db/chord-based-bass-db bass-type)]
                                        f
                                        (db/chord-based-bass-db "bass-none")))
        drum-track  (make-drum-track drum-pattern bbcs)
        _           (println bass-type)
        bass-track  (if (= bass-type "patterns")
                      (bass-patterns id)
                      (synthesize-bass-track bbcs))
        m           (meta bass-track)
        _           (println m)
        info        (if m
                      (m :bass)
                      (apply sorted-map (interleave (range 1 100) (repeat bass-type))))]
    (println (format "song=%s, bpm=%d, drum-pattern-cd=%s, bass-ty-cd=%s"
                     song bpm drum-pattern bass-type))
    (tla/view (map (fn [[bar v] c]
                     [bar v (pr-str c)])
                   info
                   (partition 4 (map (fn [[_ _ c]] c) bbcs))))
    (-> (concat chord-track bass-track drum-track)
        (combine-tracks-to-ttape bpm)
        mtape
        play-mtape)))

(defn main [& _]
  (doseq [song ["MISTY" "ALL THE THINGS YOU ARE" "AUTUMN LEAVES" "MEDIUM BLUES"
                "ALONE TOGETHER"
                "IN A SENTIMENTAL MOOD"
                "ALL OF ME"
                "AUTUMN LEAVES"
                "ALL BY MYSELF"
                "LET IT BE"]]
    (play-song song)))

(main)

;(def synth    (javax.sound.midi.MidiSystem/getSynthesizer))
;#'midi.core/synth
;midi.core=> (.getMaxPolyphony synth)
;64
;midi.core=> (def sb (.getDefaultSoundbank synth))
;#'midi.core/sb
;midi.core=> sb
;#object[com.sun.media.sound.SF2Soundbank 0x53bb91e9 "com.sun.media.sound.SF2Soundbank@53bb91e9"]
;midi.core=> (.getResources sb)
;#object["[Ljavax.sound.midi.SoundbankResource;" 0x78c350f "[Ljavax.sound.midi.SoundbankResource;@78c350f"]
;midi.core=> (.getVendor sb)
;"Generated"
;midi.core=> (.getDescription sb)
;"Emergency generated soundbank"
;
;tim@allocator:/usr/share/sounds$ find . -name *.sf* -exec ls -l {} \;
;lrwxrwxrwx 1 root root 32 Oct 23 17:40 ./sf3/default-GM.sf3 -> /etc/alternatives/default-GM.sf3
;-rw-r--r-- 1 root root 5969788 Jun 17  2015 ./sf2/TimGM6mb.sf2
;lrwxrwxrwx 1 root root 32 Oct 23 17:40 ./sf2/default-GM.sf2 -> /etc/alternatives/default-GM.sf2
;
;(vec (.getMethods (.getClass javax.sound.midi.MidiSystem)))
;
;(use 'clojure.reflect)
;(map println (:members (reflect javax.sound.midi.MidiSystem)))
;
;(->> (reflect javax.sound.midi.MidiSystem)
;     :members
;     (filter #(= (:name %) "getSoundbank")))
;
;
;(def g (java.io.File. "/home/tim/Downloads/GeneralUser_GS_SoftSynth.sf2"))
;#'midi.core/g
;midi.core=> g
;#object[java.io.File 0x6f1a3c9d "/home/tim/Downloads/GeneralUser_GS_SoftSynth.sf2"]
;midi.core=> (javax.sound.midi.MidiSystem/getSoundbank g)
;
; http://www.ronimusic.com/smp_ios_dls_files.htm
;
;(def gsb (javax.sound.midi.MidiSystem/getSoundbank g))
;(.loadAllInstruments synth gsb)
;        g (java.io.File. "/home/tim/Downloads/GeneralUser_GS_SoftSynth.sf2")
;        gsb (javax.sound.midi.MidiSystem/getSoundbank g)
;        _ (.loadAllInstruments synth gsb)
;         (let [p (-> gsb .getInstruments (nth prog))]
