(ns midi.core
   (:require [clojure.string :as str])
   (:use [midi.timlib])
   (:use [midi.dbload]))

;(def notedb {:c  60, :c#  61, :d  62, :d# 63, :e 64, :f 65, :f# 66, :g 67 :g# 68, :a 69, :a# 70, :b 71,
;             :C  60, :C#  61, :D  62, :D# 63, :E 64, :F 65, :F# 66, :G 67 :G# 68, :A 69, :A# 70, :B 71,
;                     :Db  61,         :Eb 63,               :Gb 66,       :Ab 68,        :Bb 70,
;             :c2 72, :c2# 73, :d2 74, :d2# 75, :e2 76})
;
;(def chord-form {
;    :major [1 5 8]
;    :+     [1 4 9]
;    :sus4  [1 6 8]
;    :6     [1 5 8 11]
;    :m6    [1 4 8 11]
;    :7     [1 5 8 11]
;    :m     [1 4 8]
;    :m7    [1 4 8 11]
;    :maj7  [1 5 8 12]
;    :7sus4 [1 6 8 11]
;    :7+5   [1 5 9 11]
;    :7-5   [1 5 7 11]
;    :dim   [1 4 7]
;    :dim7  [1 4 7 11]
;    :m7-5  [1 4 7 11]
;    :mmaj7 [1 4 8 12]
;    :mmaj9 [1 5 8 12 15]
;    :m9    [1 4 8 11 15]
;    :9     [1 5 8 11 15]
;    :9+5   [1 5 9 11 15]
;    :9-5   [1 5 7 11 15]
;    :96    [1 5 8 10 11 15]
;    :maj11 [1 5 8 12 15 18]
;    :m11   [1 4 8 11 15 18]
;    :11    [1 5 8 11 15 18]
;    :11-9  [1 5 8 11 14 18]
;    :maj13 [-1 3 6 10]
;    :m13   [-2 3 6 10]
;    :13    [-2 3 6 10] ; same as m13?
;    :13-9  [-2 2 6 10]})
;
;(defn chord-notes [root form]
;   (map #(+ (notedb root) % -1) (chord-form form)))
    
;(def chorddb (reduce (fn [acc [k f]]
;                        (assoc acc
;                               (keyword (str (name k) (if (= f :major) "" (name f))))
;                               (chord-notes k f)))
;                     {}
;                     (for [k [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
;                           f (keys chord-form)]
;                        [k f])))

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

; ascending
(defn bass-15   [_ beat [r _ n5]]  (case beat 1 r           3 n5   nil))
(defn bass-1234 [_ beat [r n3 n5]] (case beat 1 r 2 (+ 2 r) 3 n3 4 (inc n3)))
(defn bass-1235 [_ beat [r n3 n5]] (case beat 1 r 2 (+ 2 r) 3 n3 4 n5))

; descending
(defn bass-4321 [_ beat [r n3 n5]] (case beat 1 (inc n3) 2 n3  3 (+ 2 r) 4 r))
(defn bass-5321 [_ beat [r n3 n5]] (case beat 1 n5       2 n3  3 (+ 2 r) 4 r))

; alternating asc and desc
; not good
;(defn bass-ud1 [bar beat chord]
;   ((nth [bass-1234 bass-1234 bass-4321 bass-4321] (mod bar 4)) bar beat chord))

(defn bass-ud2 [bar beat chord]
   ((nth [bass-1234 bass-1235 bass-5321 bass-4321] (mod bar 4)) bar beat chord))

(defn bass-ud3 [bar beat chord]
   ((nth [bass-1234 bass-5321 bass-1235 bass-5321] (mod bar 4)) bar beat chord))

(defn chord-ttape
  ([beats]
      (chord-ttape beats bass-15))
  ([beats bassf]
      (chord-ttape beats bassf [[0 :set-tempo 400000][0 :time-signature [4 2 24 8]]]))
  ([beats bassf timing]
      (let [qn      96   ; quarter's duration
            offpct  0.99 ; notes off events at %
            x2  (bass-skelet beats qn bassf)
            on  (for [[_ bar] x2, [_ chord] bar, note chord] note)
            off (map (fn [[t c n _]] [(+ t (* qn offpct)) c n 0]) on)
            x3  (concat on off)
            x4  (sort-by key (group-by first x3))
            x5  (for [[tc data] x4] [tc :data data])]
        (concat timing x5))))

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
     (map (fn [[a b c]] (vector (integer a) (integer b) (keyword c))) (rest beats))))
      

(defn -main [& _]
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
               make-tape
               play-tape))))
