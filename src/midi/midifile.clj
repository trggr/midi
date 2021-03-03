(ns midi.core)

(def meta-message-type {
       0x03 :track-name
       0x04 :instrument-name
       0x05 :lyrics
       0x06 :marker
       0x20 :channel-prefix
       0x2F :end-of-track
       0x51 :set-tempo
       0x54 :smpte-offset
       0x58 :time-signature
       0x59 :key-signature})

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

;; (def midifile (java.io.File. "days12.mid"))
(def midifile (java.io.File. "chesnuts.mid"))
(def sq (javax.sound.midi.MidiSystem/getSequence midifile))
(def tracks (.getTracks sq))

(defn ascii [n]
   (if (<= 0 n 255) (char n) \?))

(defn message-data [m]
   (cond (instance? javax.sound.midi.MetaMessage m)
             (let [d    (.getData m)
                   ty   (.getType m)
                   data (slice d 0 (count d))]
                {:event :meta,
                 :ty    (keyword (format "%02X" ty)),
                 :msg   (meta-message-type ty),
                 :data  (map #(keyword (format "%02X" %)) data)
                 :txt   (apply str (map ascii data))})
         (instance? javax.sound.midi.ShortMessage m)
                {:event :short,
                 :ch  (.getChannel m),
                 :cmd (short-message-status (.getCommand m))
                 :d1  (.getData1 m) ; (keyword (format "%02X" (.getData1 m))),
                 :d2  (.getData2 m) ; (keyword (format "%02X" (.getData2 m)))
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

(defn de-uglify [xs]  
   (println (clojure.string/join \newline
                (for [row xs]
                    (clojure.string/join \tab row)))))

(def deu de-uglify)

(defn track-info [track]
   (mapv event-info (get-events track)))

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


(defn play [tape]
   (let [play-note (note-player 1)]
      (doseq [[tc notes] tape]
         (Thread/sleep tc)
         (print tc)
         (doseq [[_ ch note vel] notes]
            (print " " ch note vel)
            (play-note ch note vel))
         (println))))

;BPM                = 60,000,000/MicroTempo
;MicrosPerPPQN      = MicroTempo/TimeBase
;MicrosPerMIDIClock = MicroTempo/24
;PPQNPerMIDIClock   = TimeBase/24
;MicrosPerSubFrame  = 1000000 * Frames * SubFrames
;SubFramesPerQuarterNote = MicroTempo/(Frames * SubFrames)
;SubFramesPerPPQN = SubFramesPerQuarterNote/TimeBase
;MicrosPerPPQN    = SubFramesPerPPQN * Frames * SubFrames 


(def db (let [nticks         (.getTickLength sq)                  ; duration of sequence in MIDI ticks
              nmcseconds     (.getMicrosecondLength sq)           ; duration of sequence in microseconds
              tick-duration  (/ (double nmcseconds) nticks 1000)] ; each tick in milliseconds
          {:nticks  nticks,
           :nmicroseconds nmcseconds,
           :tick-duration tick-duration
           :division-cd   (.getDivisionType sq)
           :division-nm   (division-type (.getDivisionType sq))
           :ntracks       (count tracks)
           :tracks        (map track-info tracks)}))

;{:nticks        (.getTickLength sq)           ; Duration of this sequence, expressed in MIDI ticks.
;         :nmicroseconds (.getMicrosecondLength sq)    ; Duration of this sequence, expressed in microseconds.
;         :division-cd   (.getDivisionType sq)
;         :division-nm   (division-type (.getDivisionType sq))
;         :ntracks       (count tracks)
;         :tracks        (map track-info tracks)})
;
;(def tickduration-in-milliseconds  (/ (double (db :nmicroseconds)) (db :nticks) 1000))

;(def db (reduce (fn [acc [k v]] (assoc acc k v))
;                db
;                (map-indexed #(vector (keyword (str "track" %1))
;                                      (track-info %2)) tracks)))

(def tape
   (let [ts    (->> (db :tracks) flatten)
         tape  (concat (->> (filter #(= :note-on (:cmd %)) ts)
                            (map (juxt :tick :ch :d1 :d2)))
                       (->> (filter #(= :note-off (:cmd %)) ts)
                            (map (juxt :tick :ch :d1 (constantly 0)))))
         tape2 (->> tape (group-by first) (sort-by first))
         dur   (db :tick-duration)]
           (loop [prior 0, acc [], xs tape2]
               (if-not (seq xs)
                  acc
                  (let [[tc notes] (first xs)]
                      (recur tc (conj acc [(* dur (- tc prior)) notes]) (rest xs)))))))

(def db2 (dissoc db :tracks))

; midi.core=> (de-uglify (take 20 (sort-by first tape)))
; 360	0	:note-on	:48	:7D
; 360	1	:note-on	:48	:7D
; 360	2	:note-on	:48	:7D
; 419	0	:note-on	:48	:00
; 419	1	:note-on	:48	:00
; 419	2	:note-on	:48	:00
;
;(map (juxt :tick :cmd :d1 :d2) (filter #(contains? #{:note-on :note-off} (:cmd %)) (db :track1)))

;(def y
;       (loop [prior 0, acc [], xs tape2]
;           (if-not (seq xs)
;              acc
;              (let [[tc notes] (first xs)]
;                  (recur tc (conj acc [(- tc prior) notes]) (rest xs))))))

(in-ns 'midi.core)


