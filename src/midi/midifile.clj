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

(defn slice [b start end]
   (for [i (range start end)]
      (nth b i)))

(def midifile (java.io.File. "FALL_01.MID"))
(def sq (javax.sound.midi.MidiSystem/getSequence midifile))
(def tracks (.getTracks sq))

(defn message-data [m]
   (cond (instance? javax.sound.midi.MetaMessage m)
             (let [d    (.getData m)
                   ty   (.getType m)
                   data (slice d 0 (count d))]
                {:event :meta,
                 :ty    (keyword (format "%02X" ty)),
                 :msg   (meta-message-type ty),
                 :data  data
                 :txt   (apply str (map char data))})
         (instance? javax.sound.midi.ShortMessage m)
                {:event :short,
                 :ch  (.getChannel m),
                 :cmd (short-message-status (.getCommand m))
                 :d1  (keyword (format "%02X" (.getData1 m))),
                 :d2  (keyword (format "%02X" (.getData2 m)))}
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
   (let [len (.size track)]
     (map #(.get track %) (range len))))

(defn de-uglify [xs]  
   (println (clojure.string/join \newline
                (for [row xs]
                    (clojure.string/join \tab row)))))

(defn track-info [track]
   (mapv event-info (get-events track)))

;(println "Track1")
;(de-uglify (-> (.getTracks sq) first  track-info))
;(println "Track2")
;(de-uglify (-> (.getTracks sq) second track-info))
;
;(println "Track3")
;(de-uglify (-> (.getTracks sq) (nth 2) track-info))
; (de-uglify (map (juxt :tick :status :len :event :ty :msg :cmd :txt :d1 :d2) y))
; (def db {:tracks (mapv track-info (take 1 (.getTracks sq)))})
;(def db (let [tracks (.getTracks sq)]
;           (for [i (range (count tracks))]
;              [i (track-info (nth tracks i))])))

(def db {:ticklen     (.getTickLength sq)
         :mseclen     (.getMicrosecondLength sq)
         :division-ty (.getDivisionType sq)})

(def db (reduce (fn [acc [k v]] (assoc acc k v))
                db
                (map-indexed #(vector (keyword (str "track" %1))
                                      (track-info %2))
                             (.getTracks sq))))


