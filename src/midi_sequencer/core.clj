(ns midi-sequencer.core
  (:gen-class))

(def notedb {:c  60, :c#  61, :d  62, :d# 63, :e 64, :f 65, :f# 66, :g 67 :g# 68, :a 69, :a# 70, :b 71,
             :c2 72, :c2# 73, :d2 74, :d2# 75, :e2 76})

(def c-major [:c :d :e :f :g :a :b])
(def d-major [:d :e :f# :g :a :b :c2#])
(def e-major [:e :f# :g# :a :b :c2# :d2#])
(def f-major [:f :g :a :a# :c2 :d2 :e2])

(defn scale [key]
   (let [durations [250 250 250 250 250 250 500]]
      (map vector (map notedb (concat key (reverse key)))
                  (concat durations durations)
                  (repeat 14 80))))

(defn player [chan instr]
  "Returns player function for a given channel and instrument"
  (let [synth (javax.sound.midi.MidiSystem/getSynthesizer)
        _     (.open synth)
        c     (-> synth .getChannels (nth chan))
        i     (-> synth .getDefaultSoundbank .getInstruments (nth instr))]
     (println "Playing" (.getName i))
     (.loadInstrument synth i)
     (.programChange c instr)
     (fn [note dur vol]
        (.noteOn c note vol)
        (Thread/sleep dur)
        (.noteOff c note))))

(defn -main [& args]
  (let [chan  2
        instr 21
        f     (player chan instr)]
      (doseq [k [c-major d-major e-major f-major]]
        (doseq [[note dur vol] (scale k)]
            (f note dur vol)))))
