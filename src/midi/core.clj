(ns midi.core
   (:require [clojure.string :as str])
   (:use [midi.timlib])
   (:use [midi.dbload]))

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

(defn make-tape
  ([ticktape] (make-tape ticktape 1))
  ([ticktape tempo-correction]
   (loop [prior 0, ppq 0, tempo 0, acc [], xs ticktape]
       (if-not (seq xs)
          acc
          (let [[[tc cmd val] & others] xs]
              (cond (= :set-tempo cmd)
                        (do
                           (println "set-tempo" val)
                           (recur tc   ppq  val acc others))
                    (= :time-signature cmd)
                        (let [x (* (first val) (nth val 2))
                              x (* x tempo-correction)
                              _ (println "time-signature" x val)] 
                          (recur tc  x tempo acc others))
                    :else
                        (let [x (/ (* (- tc prior) tempo) (* 1000 ppq))]
                           (recur tc ppq tempo (conj acc [x val]) others))))))))

; Plugin
(defn bass-skelet [beats qn bassf]
   (let [cvel      40   ; chord's velocity
         bvel      80   ; bass velocity
         bchannel  4    ; bass MIDI channel
         cchannel  2]   ; chords' MIDI channel
      (reduce (fn [acc [bar beat chord-nm]]
                 (let [tc           (* qn (+ (* bar 4) (dec beat)))
                       chord        (chorddb chord-nm)
                       bass         (bassf bar beat chord)
                       shell-pretty (map #(vector tc cchannel % cvel) (rest chord))
                       notes        (if (nil? bass)
                                       shell-pretty 
                                       (cons (vector tc bchannel (- bass 24) bvel) shell-pretty))]
                  (assoc-in acc [bar tc] notes)))
              (sorted-map)
              beats)))

; no bass
(defn bass-none [_ _ _] nil)

; ascending
(defn bass-15   [_ beat [r _ n5]]  (case beat 1 r           3 n5   nil))
(defn bass-1234 [_ beat [r n3 n5]] (case beat 1 r 2 (+ 2 r) 3 n3 4 (inc n3)))
(defn bass-1235 [_ beat [r n3 n5]] (case beat 1 r 2 (+ 2 r) 3 n3 4 n5))

; descending
(defn bass-4321 [_ beat [r n3 n5]] (case beat 1 (inc n3) 2 n3  3 (+ 2 r) 4 r))
(defn bass-5321 [_ beat [r n3 n5]] (case beat 1 n5       2 n3  3 (+ 2 r) 4 r))

; alternating asc and desc
(defn bass-ud2 [bar beat chord]
   ((nth [bass-1234 bass-1235 bass-5321 bass-4321] (mod bar 4)) bar beat chord))

(defn bass-ud3 [bar beat chord]
   ((nth [bass-1234 bass-5321 bass-1235 bass-5321] (mod bar 4)) bar beat chord))

(defn chord-ttape
  ([beats]
      (chord-ttape beats bass-15))
  ([beats bassf]
      (let [qn      96   ; quarter's duration
            offpct  0.99 ; notes off events at %
            x2  (bass-skelet beats qn bassf)
            on  (for [[_ bar] x2, [_ chord] bar, note chord] note)
            off (map (fn [[t c n _]] [(+ t (* qn offpct)) c n 0]) on)
            rc  (concat on off)]
         rc)))

(defn assemble-ttape
   ([raw]
      (assemble-ttape raw [[0 :set-tempo 800000][0 :time-signature [4 2 24 8]]]))
   ([raw timing]
     (let [xs (sort-by key (group-by first raw))
           ys (for [[tc data] xs] [tc :data data])]
          (concat timing ys))))

(defn note-player [instr]
   (let [synth    (javax.sound.midi.MidiSystem/getSynthesizer)
         _        (.open synth)
         channels (-> synth .getChannels)
         i        (-> synth .getDefaultSoundbank .getInstruments (nth instr))]
      (println "Playing" (.getName i))
      (.loadInstrument synth i)
;      (.programChange c instr)
      (fn [c note vol]
         (let [ch (nth channels c)]
            (.noteOn ch note vol)))))

(defn play-tape [tape]
   (let [play-note (note-player 1)]
      (doseq [[tc notes] tape]
         (Thread/sleep tc)
         (doseq [[_ ch note vel] notes]
            (play-note ch note vel)))))

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
        beats  (cursor conn query [(str/upper-case song-name)] false)]
     (map (fn [[bar beat chord]] (vector (integer bar) (integer beat) (keyword chord))) (rest beats))))
      

(def qn 96)

(defn make-bass-ttape [bar bassline transp]
  (let [notes (rest (cursor conn
                            (str "select n.midi_num, b.note_dur_num "
                                 "from bass_line_note b join note n on (n.note_cd = b.note_cd) "
                                 "where b.bass_line_id = ? order by order_num")
                            [bassline] false))]
     (loop [acc []
             tc (+ (* qn bar 4))
             xs notes]
        (if (empty? xs)
           acc
           (let [[midi dur] (first xs)
                  midin  (read-string midi)
                  durn   (read-string dur)
                  n      (+ midin transp)
                  nexttc (+ tc (/ (* 4 qn) durn))]
               (recur (conj (conj acc [tc 3 n 80]) [(dec nexttc) 3 n 0])
                      nexttc
                      (rest xs)))))))

(defn alloc_bass [[timeline tape :as rc]
                  [bassline begin end transp-num]]
  (let [b      (integer begin)
        e      (integer end)
        transp (integer transp-num)
        bars (range b e)]
     (if (some identity (vals (select-keys timeline bars)))
        rc
        [(reduce (fn [a k] (assoc a k bassline)) timeline bars)
         (concat tape (make-bass-ttape b bassline transp))])))

(defn raw-bass [songid]
  (let [ptrns  (rest (cursor conn
                             (str "select bass_line_id, beg_bar_id, end_bar_id, transp_num "
                                  "from bass_line_bar_v "
                                  "where song_id = ? "
                                  "order by beg_bar_id")
                              [songid] false))
        maxbar (-> (cursor conn "select max(bar_id) bar from bar where song_id = ?" [songid] true) first :bar integer)]
       (println maxbar ptrns)
       (reduce alloc_bass
               [(into (sorted-map) (zipmap (range 1 (inc maxbar)) (repeat nil)))
                []]
                ptrns)))

(defn -main2 [& _]
   (println "starting")
   (doseq [bassf [bass-ud2 bass-ud3
                  bass-15
                  bass-4321
                  bass-5321
                  bass-1234
                  bass-1235]]
     (doseq [song ["ALL THE THINGS YOU ARE"
;                   "IN A SENTIMENTAL MOOD"
;                   "ALL OF ME"
;                   "AUTUMN LEAVES"
;                   "ALL BY MYSELF"
;                   "LET IT BE"
                  ]]
           (println song bassf)
           (-> (get-beats conn song)
               (chord-ttape bassf)
               assemble-ttape
               make-tape
               play-tape))))

(defn -main [& _]
   (let [song   "ALL THE THINGS YOU ARE"
         songid "1"
         beats  (get-beats conn song)
         rawc   (chord-ttape beats bass-none)
         bass   (raw-bass songid)
         [info rawb] bass]
     (-> (concat rawc rawb) assemble-ttape make-tape play-tape)))



           
