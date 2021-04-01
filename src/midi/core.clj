(ns midi.core
   (:require [clojure.string :as str])
   (:use [midi.timlib])
   (:use [midi.dbload]))

; Length of quarter note
(def ^:dynamic *qn*            96)
(def ^:dynamic *chord-channel* 2)
(def ^:dynamic *bass-channel*  4)
(def ^:dynamic *drums-channel* 9)

; Converts ttape to MIDI tape format
(defn mtape
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


; no bass
(defn bass-none [_ _ _] nil)

(defn bass-1   [_ beat [r _ _]]  (case beat 1 r nil))

; ascending
(defn bass-15   [_ beat [r _ n5]]  (case beat 1 r           3 n5   nil))
(defn bass-1234 [_ beat [r n3 n5]] (case beat 1 r 2 (+ 2 r) 3 n3 4 (inc n3)))
(defn bass-1235 [_ beat [r n3 n5]] (case beat 1 r 2 (+ 2 r) 3 n3 4 n5))

; descending
(defn bass-4321 [_ beat [r n3 n5]] (case beat 1 (inc n3) 2 n3  3 (+ 2 r) 4 r))
(defn bass-5321 [_ beat [r n3 n5]] (case beat 1 n5       2 n3  3 (+ 2 r) 4 r))

; alternating asc and desc
(defn bass-ud2 [bar beat chord] ((nth [bass-1234 bass-1235 bass-5321 bass-4321] (mod bar 4)) bar beat chord))
(defn bass-ud3 [bar beat chord] ((nth [bass-1234 bass-5321 bass-1235 bass-5321] (mod bar 4)) bar beat chord))

(defn expand-chord [[bar beat chord-nm] bassf]
   (let [chord-vel     50
         bass-vel      80
         tc      (* *qn* (+ (* bar 4) (dec beat)))
         vel     (* chord-vel (if (= beat 1) 1.0 0.6))
         chord   (chorddb chord-nm)
         bass    (bassf bar beat chord)
         wobass  (map #(vector tc *chord-channel* % chord-vel) (rest chord))]
      (if (nil? bass)
          wobass
          (cons (vector tc *bass-channel* (- bass 12) bass-vel) wobass))))

(defn raw-chord
  ([beats]  (raw-chord beats bass-15))
  ([beats bassf]
      (let [on  (mapcat #(expand-chord % bassf) beats)
            off (map (fn [[t c n _]] [(+ t *qn* -1) c n 0]) on)]
        (concat on off))))

; Converts raw notes to ttape format:
; Tick Tape format:
;     Field       Type      Description
;     --------------------------------------------------------------------------------------
;  1: MIDI tick   integer   Gradually increasing throughout the tape. Treat it as a timecode
;  2: event-type  keyword   One of :set-tempo, :time-signature, or :data
;  3: data        varies    For :set-tempo is a single integer representing number of microseconds per quarter note
;                           For :time-signature array of four matching to MIDI's time signature event
;                           For :data - array of notes played during this MIDI tick
;                              each note is [timecode, channel, note, velocity]. 
; Example:
;  [0	:set-tempo	434464
;  [0	:time-signature	[4 2 24 8]
;  [384	:data	        [[384 2 70 50]]
;  [477	:data	        [[477 2 98 74]]
;  [479	:data	        [[479 2 101 78]]
;  [480	:data	        [[480 8 89 61] [480 8 86 55]]
;  [489	:data	        [[489 8 86 0] [489 8 89 0]]
;  [492	:data	        [[492 8 89 60] [492 8 86 63]]

(defn ttape
   ([raw]     (ttape raw 120))
   ([raw bpm] (ttape raw bpm [4 2 24 8]))
   ([raw bpm signature]
     (let [xs (sort-by key (group-by first raw))
           ys (for [[tc data] xs] [tc :data data])]
          (concat [[0 :set-tempo (/ 60000000 bpm)][0 :time-signature signature]] ys))))

(defn note-player [instruments]
   (let [synth    (javax.sound.midi.MidiSystem/getSynthesizer)
         _        (.open synth)
         channels (-> synth .getChannels)]
     (doseq [[ch prog] instruments]
         (let [p (-> synth .getDefaultSoundbank .getInstruments (nth prog))]
            (println "Playing" (.getName p) "on channel" ch)
            (.loadInstrument synth p)
            (.programChange (nth channels ch) prog)))
      (fn [c note vol]
          (.noteOn (nth channels c) note vol))))

; Plays MIDI tape
(defn play-mtape [tape]
   (let [f (note-player [[*chord-channel* 26]
                         [*bass-channel*  34]])]
      (doseq [[tc notes] tape]
         (Thread/sleep tc)
         (doseq [[_ ch note vel] notes]
            (f ch note vel)))))

(defn get-beats [conn song-name]
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
        beats  (cursor conn query [(str/upper-case song-name)])]
     (map (fn [[bar beat chord]] [bar beat (keyword chord)]) (rest beats))))
      
(defn expand-bass-line [bar bassline transp vel]
  (let [notes (cursor conn
                      "select n.midi_num, b.note_dur_num
                       from bass_line_note b join note n on (n.note_cd = b.note_cd)
                       where b.bass_line_id = :1 order by order_num"
                      [bassline])]
     (loop [acc [], tc (+ (* *qn* bar 4)), xs (rest notes)]
        (if (empty? xs)
           acc
           (let [[midi dur] (first xs)
                  n      (+ midi transp)
                  nexttc (+ tc (/ (* 4 *qn*) dur))]
               (recur (conj (conj acc [tc *bass-channel* n vel])
                            [(dec nexttc) *bass-channel* n 0])
                      nexttc
                      (rest xs)))))))

(defn alloc-bass [[timeline tape :as rc]
                  [bassline begin end transp]]
  (let [bars (range begin (inc end))
        vel  90]
     (if (some identity (vals (select-keys timeline bars)))
        rc
        [(reduce (fn [a k] (assoc a k bassline)) timeline bars)
         (concat tape (expand-bass-line begin bassline transp vel))])))

(defn raw-bass [songid]
  (let [ptrns  (cursor conn
                       (str "select bass_line_id, beg_bar_id, end_bar_id, (transp_num - 24) transp_num "
                            "from bass_line_bar_v "
                            "where song_id = ? "
                            "order by beg_bar_id")
                        [(str songid)])
        maxbar (-> (cursor conn "select max(bar_id) bar from bar where song_id = ?" [(str songid)]) second first)]
       (reduce alloc-bass
               [(into (sorted-map) (zipmap (range 1 (inc maxbar)) (repeat nil)))
                []]
               (rest ptrns))))

(defn expand-drum [pattern note bar]
   (loop [acc [],
          tc  (+ (* *qn* bar 4)),
          xs pattern]
      (if (empty? xs)
         acc
         (let [[velpct dur] (first xs)
               dur (or dur 4)
               nexttc (+ tc (/ (* 4 *qn*) dur))]
             (recur (conj (conj acc [tc *drums-channel* note (/ (* 70 velpct) 100)])
                          [(dec nexttc) *drums-channel* note 0])
                    nexttc
                    (rest xs))))))

(def swing {:ride-cymbal-1      [[70]  [70 12][0 12][40 12]  [70]  [70 12][0 12][40 12]]
            :closed-hi-hat      [[0]   [70]                  [0]   [70]]
            :acoustic-bass-drum [[90]  [70]                  [90]  [70]]})

(defn single-drum [pattern note bars]
   (mapcat #(expand-drum pattern note %) bars))

(defn raw-drums [pattern bars]
   (mapcat #(single-drum (pattern %) (notedb %) bars) (keys pattern)))

(defn play-song [song-nm]
   (let [[id bpm] (-> (cursor conn "select song_id, bpm_num from song where upper(song_nm) = ?" [song-nm]) second)
         beats    (get-beats conn song-nm)
         bars     (range 1 (inc (reduce max (map first beats))))
         chords   (raw-chord beats bass-none)
         drums    (raw-drums swing bars)
         [info bass] (raw-bass id)]
     (println song-nm)
     (view (map (fn [[k v] c] [k v (pr-str c)])
                info
                (partition 4 (map (fn [[_ _ c]] c) beats))))
     (-> (concat bass drums chords) (ttape bpm) mtape play-mtape)))

(defn -main [& _]
   (doseq [song ["AUTUMN LEAVES" "ALL THE THINGS YOU ARE" "ALL OF ME" "MEDIUM BLUES"
                "IN A SENTIMENTAL MOOD"
                "ALL OF ME"
                "AUTUMN LEAVES"
                "ALL BY MYSELF"
                "LET IT BE"]]
       (play-song song)))
