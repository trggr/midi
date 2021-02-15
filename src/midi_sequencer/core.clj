(ns midi-sequencer.core
  (:gen-class))

(def notedb {:c  60, :c#  61, :d  62, :d# 63, :e 64, :f 65, :f# 66, :g 67 :g# 68, :a 69, :a# 70, :b 71,
             :C  60, :C#  61, :D  62, :D# 63, :E 64, :F 65, :F# 66, :G 67 :G# 68, :A 69, :A# 70, :B 71,
                     :Db  61,         :Eb 63,               :Gb 66,       :Ab 68,        :Bb 70,
             :c2 72, :c2# 73, :d2 74, :d2# 75, :e2 76})

(def c-major [:c :d  :e  :f  :g  :a   :b])
(def d-major [:d :e  :f# :g  :a  :b   :c2#])
(def e-major [:e :f# :g# :a  :b  :c2# :d2#])
(def f-major [:f :g  :a  :a# :c2 :d2  :e2])

(def chord-form {
    :major [1 5 8]
    :+     [1 4 9]
    :sus4  [1 6 8]
    :6     [1 5 8 11]
    :m6    [1 4 8 11]
    :7     [1 5 8 11]
    :m     [1 4 8]
    :m7    [1 4 8 11]
    :maj7  [1 5 8 12]
    :7sus4 [1 6 8 11]
    :7+5   [1 5 9 11]
    :7-5   [1 5 7 11]
    :dim   [1 4 7]
    :dim7  [1 4 7 11]
    :m7-5  [1 4 7 11]
    :mmaj7 [1 4 8 12]
    :mmaj9 [1 5 8 12 15]
    :m9    [1 4 8 11 15]
    :9     [1 5 8 11 15]
    :9+5   [1 5 9 11 15]
    :9-5   [1 5 7 11 15]
    :96    [1 5 8 10 11 15]
    :maj11 [1 5 8 12 15 18]
    :m11   [1 4 8 11 15 18]
    :11    [1 5 8 11 15 18]
    :11-9  [1 5 8 11 14 18]
    :maj13 [-1 3 6 10]
    :m13   [-2 3 6 10]
    :13    [-2 3 6 10] ; same as m13?
    :13-9  [-2 2 6 10]})

(defn chord-notes [root form]
   (map #(+ (notedb root) % -1) (chord-form form)))
    
(def chorddb (reduce (fn [acc [k f]]
                        (assoc acc
                               (keyword (str (name k) (if (= f :major) "" (name f))))
                               (chord-notes k f)))
                     {}
                     (for [k [:C :C# :Db :D :D# :Eb :E :F :F# :Gb :G :G# :Ab :A :A# :Bb :B]
                           f (keys chord-form)]
                        [k f])))

(defn scale [key]
   (let [durations [250 250 250 250 250 250 500]]
      (map vector (map notedb (concat key (reverse key)))
                  (concat durations durations)
                  (repeat 14 80))))

(defn player-factory [chan instr]
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

(defn chord-player-factory [chan instr]
  "Returns player function for a given channel and instrument"
  (let [synth (javax.sound.midi.MidiSystem/getSynthesizer)
        _     (.open synth)
        c     (-> synth .getChannels (nth chan))
        i     (-> synth .getDefaultSoundbank .getInstruments (nth instr))]
     (println "Playing" (.getName i))
     (.loadInstrument synth i)
     (.programChange c instr)
     (fn [chord dur vol]
        (doseq [n chord]
             (.noteOn c n vol))
        (Thread/sleep dur)
        (doseq [n chord]
             (.noteOff c n)))))

(defn play-song
   ([score] (play-song score {}))
   ([score opts]
       (let [vol   (or (opts :vol)   60)
             tempo (/ 60000 (or (opts :bpm)  120))
             instr (or (opts :instr) 0)
             chan  (or (opts :chan)  2)
             f     (chord-player-factory chan instr)]
       (doseq [c score]
          (print c)
          (flush)
          (f (chorddb c) tempo vol)))))

(defn score-helper [xs]
   (loop [acc [] cur "" ts xs]
      (if (empty? ts)
          acc
          (let [[t & more] ts
                 x (if (= t "/") cur t)]
             (recur (conj acc (keyword x)) x more)))))

(defn score [tabs]
 (-> tabs
     (clojure.string/replace "|" "")
     (clojure.string/split  #"\s+")
     score-helper))

;(defn play-scales [& args]
;  (let [chan  2
;        instr 21
;        f     (player-factory chan instr)]
;      (doseq [k [c-major d-major e-major f-major]]
;        (doseq [[note dur vol] (scale k)]
;            (f note dur vol)))))

(def all-the-things-you-are (score (str
  "Fm7    / / / | Bbm7  / /  / | Eb7    / / / | Abmaj7 /  /  / "
  "Dbmaj7 / / / | G7    / /  / | Cmaj7  / / / | /      /  /  / "
  "Cm7    / / / | Fm7   / /  / | Bb7    / / / | Ebmaj7 /  /  / "
  "Abmaj7 / / / | Am7-5 / D7 / | Gmaj7  / / / | /      /  E9 / "
  "Am7    / / / | D7    / /  / | Gmaj7  / / / | /      /  /  / "
  "F#m7   / / / | B7    / /  / | Emaj7  / / / | C7+5   /  /  / "
  "Fm7    / / / | Bbm7  / /  / | Eb7    / / / | Abmaj7 /  /  / "
  "Dbmaj7 / / / | Gb7   / /  / | Cm7    / / / | Bdim7  /  /  / "
  "Bbm7   / / / | Eb7   / /  / | Abmaj7 / / / | Gm7-5  /  C9 / ")))

(def all-of-me (score (str
  "C6     / / / | /     / /  / | E7     / /      / | /      /  /  / "
  "A7     / / / | /     / /  / | Dm7    / /      / | /      /  /  / "
  "E7     / / / | /     / /  / | Am7    / /      / | /      /  /  / "
  "D7     / / / | /     / /  / | Dm7    / /      / | G7     /  /  / "
  "C6     / / / | /     / /  / | E7     / /      / | /      /  /  / "
  "A7     / / / | /     / /  / | Dm7    / /      / | /      /  /  / "
  "F6     / / / | Fm6   / /  / | Cmaj7  / Em7-5  / | A7     /  /  / "
  "Dm7    / / / | G7    / /  / | C6     / Ebdim7 / | Dm7    /  G7 /")))

(defn -main [& other]
  (map play-song [all-the-things-you-are all-of-me]))

(-main)
