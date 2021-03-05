(ns midi.core)

(defn ascii [n]
   (if (<= 0 n 255) (char n) \?))

(def meta-message-type {
       0x03 [:track-name       #(apply str (map ascii %))]
       0x04 [:instrument-name  #(apply str (map ascii %))]
       0x05 [:lyrics           #(apply str (map ascii %))]
       0x06 [:marker           #(apply str (map ascii %))]
       0x20 [:channel-prefix   #(apply str (map ascii %))]
       0x2F [:end-of-track     #(apply str (map ascii %))]
       0x51 [:set-tempo        (fn [[a b c]] (+ (* a 65536) (* b 256) c))] ; microseconds per quarter note
       0x54 [:smpte-offset     #(apply str (map ascii %))]
       0x58 [:time-signature   (fn [[nn dd ppq bb]] [nn dd ppq bb])]
       0x59 [:key-signature    #(apply str (map ascii %))]})

(def short-message-status {
       0xF1 :midi-time-code
       0xF2 :song-position-pointer
       0xF3 :song-select
       0xF6 :tune-request
       0xF7 :end-of-exclusive
       0xF8 :timing-clock
       0xFA :start
       0xFB :continue
       0xFC :stop
       0xFE :active-sensing
       0xFF :system-reset
       0x80 :note-off
       0x90 :note-on
       0xA0 :poly-pressure
       0xB0 :control-change
       0xC0 :program-change
       0xD0 :channel-pressure
       0xE0 :pitch-bend})

(def division-type {
       javax.sound.midi.Sequence/PPQ          "PPQ"
       javax.sound.midi.Sequence/SMPTE_24     "SMPTE_24"
       javax.sound.midi.Sequence/SMPTE_25     "SMPTE_25"
       javax.sound.midi.Sequence/SMPTE_30DROP "SMPTE_30DROP"
       javax.sound.midi.Sequence/SMPTE_30     "SMPTE_30"})

(defn slice [b start end]
   (for [i (range start end)]
      (nth b i)))

(defn message-data [m]
   (cond (instance? javax.sound.midi.MetaMessage m)
             (let [d       (.getData m)
                   ty      (.getType m)
                   [msg f] (meta-message-type ty)
                   data    (slice d 0 (count d))]
                {:event :meta,
                 :ty    ty,
                 :msg   msg,
                 :data  (map #(keyword (format "%02X" %)) data)
                 :val   (when-not (nil? f) (f data))})
         (instance? javax.sound.midi.ShortMessage m)
                {:event :short,
                 :ch  (.getChannel m),
                 :cmd (short-message-status (.getCommand m))
                 :d1  (.getData1 m)
                 :d2  (.getData2 m)
                }
         (instance? javax.sound.midi.ShortMessage m)
                {:event :sysex}
         :else
                {:event :unknown}))
              
(defn event-info [e]
   (let [tick   (.getTick e)
         msg    (.getMessage e)
         status (keyword (format "%02X" (.getStatus msg)))
         len    (.getLength msg)]
     (merge {:tick tick, :status status :len len}
            (message-data msg))))

(defn get-events [track]
     (map #(.get track %) (range (.size track))))

(defn deu [xs]  
   (println (clojure.string/join \newline
                (for [row xs]
                    (clojure.string/join \tab row)))))

(defn view
  ([coll t d] (deu (take t (drop d coll))))
  ([coll t]   (view coll t 0))
  ([coll]     (view coll 40 0)))

(defn track-info [track]
   (mapv event-info (get-events track)))

(defn note-player [instr]
   (let [synth    (javax.sound.midi.MidiSystem/getSynthesizer)
         _        (.open synth)
         channels (-> synth .getChannels)
         i        (-> synth .getDefaultSoundbank .getInstruments (nth instr))]
;      (println "Playing" (.getName i))
      (.loadInstrument synth i)
;      (.programChange c instr)
      (fn [c note vol]
         (let [ch (nth channels c)]
            (.noteOn ch note vol)))))

(defn play [tape]
   (let [play-note (note-player 1)]
      (doseq [[tc notes] tape]
         (Thread/sleep tc)
;         (print tc)
         (doseq [[tick ch note vel] notes]
;            (print " " tick ch note vel)
            (play-note ch note vel))
;         (println)
       )))

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
(defn make-ticktape [midi]
   (let [ts     (->> (midi :tracks) flatten)
         t1     (concat (->> (filter #(= :note-on (:cmd %))  ts) (map (juxt :tick :ch :d1 :d2)))
                        (->> (filter #(= :note-off (:cmd %)) ts) (map (juxt :tick :ch :d1 (constantly 0)))))
         notes  (->> t1 (group-by first) (map (fn [[t n]] [t :data n])))
         tempos (->> (filter #(contains? #{:set-tempo :time-signature} (:msg %)) (first (midi :tracks))) (map (juxt :tick :msg :val)))
         rc     (sort-by (juxt first second) (concat tempos notes))]
           rc))

(defn make-tape [ticktape tempo-correction]
   (loop [prior 0, ppq 0, tempo 0, acc [], xs ticktape]
       (if-not (seq xs)
          acc
          (let [[[tc cmd val] & others] xs]
              (cond (= :set-tempo cmd)
                        (do
                           (println "set-tempo" val)
                           (recur tc   ppq  val acc others))
                    (= :time-signature cmd)
                        (let [x (* (Math/pow 2 (nth val 1)) (nth val 2))
                              x (* x tempo-correction)
                              _ (println "time-signature" x val)
                              ]
                           (recur tc  x tempo acc others))
                    :else
                        (let [x (/ (* (- tc prior) tempo) (* 1000 ppq))]
                           (recur tc ppq tempo (conj acc [x val]) others)))))))

(defn parse-midi [sq]
  (let [nticks         (.getTickLength sq)                  ; duration of sequence in MIDI ticks
        nmcseconds     (.getMicrosecondLength sq)           ; duration of sequence in microseconds
        tick-duration  (/ (double nmcseconds) nticks 1000)  ; each tick in milliseconds
        tracks         (.getTracks sq)] 
    {:dur-in-ticks             nticks,
     :dur-in-microseconds      nmcseconds,
     :tick-dur-in-milliseconds tick-duration
     :division-cd              (.getDivisionType sq)
     :division-nm              (division-type (.getDivisionType sq))
     :ntracks                  (count tracks)
     :tracks                   (map track-info tracks)}))


(doseq [p [
           ;["alliwant"  4]["nocturne"   4]["days12" 1] 
           ["chesnuts"  1]["bohemian"   2]["santa"  4] 
           ;["sothisisx" 1]["wonderland" 1]
          ]]
  (let [[file correction] p
        mfile    (java.io.File. (str file ".mid"))
        _        (println "Playing file" file)
        sq       (javax.sound.midi.MidiSystem/getSequence mfile)
        midi     (parse-midi sq)
        ticktape (make-ticktape midi)
        tape     (make-tape ticktape correction)]
     (def debug-mfile    mfile)
     (def debug-sq       sq)
     (def debug-midi     midi)
     (def debug-ticktape ticktape)
     (def debug-tape     tape)
     (play tape)))

; (T - PRIOR) - duration of a current note
; TEMPO       - microseconds per quarter note
; PPQ         - number of MIDI ticks in quarter note
;
; (T - PRIOR)     TEMPO
; ------------ *  -----   - milliseconds since last event
; 1000            PPQ

