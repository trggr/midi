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
       0x51 [:set-tempo        (fn [[a b c]] (/ 60000000. (+ (* a 65536) (* b 256) c)))] ; convert to BPM
;       0x51 [:set-tempo        (fn [[a b c]] (+ (* a 65536) (* b 256) c))] ; convert to BPM
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

; (def midifile (java.io.File. "days12.mid"))
; (def midifile (java.io.File. "chesnuts.mid"))
; (def midifile (java.io.File. "alliwant.mid"))
(def midifile (java.io.File. "bohemian.mid"))

(def sq (javax.sound.midi.MidiSystem/getSequence midifile))
(def tracks (.getTracks sq))

(defn message-data [m]
   (cond (instance? javax.sound.midi.MetaMessage m)
             (let [d       (.getData m)
                   ty      (.getType m)
                   [msg f] (meta-message-type ty)
                   data    (slice d 0 (count d))]
;                  (println "message-data:" data f)
                {:event :meta,
                 :ty    ty,
                 :msg   msg,
                 :data  (map #(keyword (format "%02X" %)) data)
                 :val   (when-not (nil? f) (f data))})
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
         (doseq [[tick ch note vel] notes]
            (print " " tick ch note vel)
            (play-note ch note vel))
         (println))))

(def db (let [nticks         (.getTickLength sq)                  ; duration of sequence in MIDI ticks
              nmcseconds     (.getMicrosecondLength sq)           ; duration of sequence in microseconds
              tick-duration  (/ (double nmcseconds) nticks 1000)] ; each tick in milliseconds
          {:dur-in-ticks             nticks,
           :dur-in-microseconds      nmcseconds,
           :tick-dur-in-milliseconds tick-duration
           :division-cd              (.getDivisionType sq)
           :division-nm              (division-type (.getDivisionType sq))
           :ntracks                  (count tracks)
           :tracks                   (map track-info tracks)}))


;; The formula is 60000 / (BPM * PPQ) (milliseconds).
;; PPQ = 96?

(def bpms (->> db
               :tracks
               first
               (filter #(= :set-tempo (get % :msg)))
               (mapv (juxt :tick :val))))

(def last-real-tick (->> db :tracks flatten (filter #(contains? #{:note-on :note-off} (:cmd %))) (map :tick) (reduce max)))
(def bpms (conj bpms [last-real-tick 0]))

(defn weighted-bpm [acc [[pt pb] & xs]]
   (if-not (seq xs)
      acc
      (let [[t b] (first xs)]
         (recur (+ acc (* pb (- t pt)))
                xs))))

; (def db (assoc db :ppq (/ (get db :dur-in-microseconds) (weighted-bpm 0.0 bpms))))

(def db (assoc db :ppq 96)) ; (/ (get db :dur-in-microseconds) (weighted-bpm 0.0 bpms))))

(def db2 (dissoc db :tracks))

(defn make-tape [db]
   (let [ts     (->> (db :tracks) flatten)
         t1     (concat (->> (filter #(= :note-on (:cmd %))  ts) (map (juxt :tick :ch :d1 :d2)))
                        (->> (filter #(= :note-off (:cmd %)) ts) (map (juxt :tick :ch :d1 (constantly 0)))))
         notes  (->> t1 (group-by first) (map (fn [[t n]] [t :data n])))
         _      (println (deu (take 20 notes)))
         tempos (->> (filter #(contains? #{:set-tempo :time-signature} (:msg %)) ts) (map (juxt :tick :msg :val)))
         _      (println (deu (take 20 tempos)))
         tape2  (sort-by (juxt first second) (concat tempos notes))]
         _      (println (deu (take 20 tape2)))
           (loop [prior 0, ppq 96, bpm 120, acc [], xs tape2]
               (if-not (seq xs)
                  acc
                  (let [[[tc cmd val] & others] xs]
                      (cond (= :set-tempo cmd)
                                (recur tc ppq val acc others)
                            (= :time-signature cmd)
                                (recur tc (* (nth val 1) (nth val 1) (nth val 2)) bpm acc others)
                            :else
                                (recur tc ppq bpm (conj acc [(/ (* 60000. (- tc prior)) bpm ppq) val]) others)))))))

(def tape (make-tape db))

(deu (take 20 tape))
(play (take 20 tape))

(in-ns 'midi.core)

