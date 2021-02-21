(ns midi.core)

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

(defn note-player [chan instr]
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

(defn chord-player [chan instr]
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

(defn chord-bass-player [bchan binstr cchan cinstr]
  "Returns player function for a given channel and instrument"
  (let [synth  (javax.sound.midi.MidiSystem/getSynthesizer)
        _      (.open synth)
        chans  (-> synth .getChannels)
        instrs (-> synth .getDefaultSoundbank .getInstruments)
        bch    (nth chans cchan)
        cch    (nth chans cchan)
        bi     (nth instrs binstr)
        ci     (nth instrs cinstr)]
     (println (format "Bass: %s, Chords: %s" (.getName bi) (.getName ci)))
     (.loadInstrument synth bi)
     (.loadInstrument synth ci)
     (.programChange bch binstr)
     (.programChange cch cinstr)
     (fn [notes dur vol]
         (let [{bass :bass, chord :chord} notes]
            (when (not (nil? bass)) (.noteOn bch bass vol))
            (doseq [n chord]        (.noteOn cch n (* vol 0.7)))
            (Thread/sleep dur)
            (when (not (nil? bass)) (.noteOff bch bass))
            (doseq [n chord]        (.noteOff cch n))))))

(defn play-song
   ([score] (play-song score {}))
   ([score opts]
       (let [vol   (or (opts :vol)   60)
             tempo (/ 60000 (or (opts :bpm)  120))
             instr (or (opts :instr) 0)
             chan  (or (opts :chan)  2)
             f     (chord-bass-player 3 33 chan instr)]
       (doseq [bar (partition 4 score)]
          (println bar)
          (doseq [i (range 4)]
             (let [chord (chorddb (nth bar i))
                   [r x3 x5] chord
                   bass (- (case i 0 r
                                   1 (+ 2 r)
                                   2 x3 
                                   3 x5) 24)]
                 (f {:bass bass, :chord chord} tempo vol)))))))

(defn play-song2
   ([score] (play-song2 score {}))
   ([score opts]
       (let [vol   (or (opts :vol)   60)
             tempo (/ 60000 (or (opts :bpm)  120))
             instr (or (opts :instr) 0)
             chan  (or (opts :chan)  2)
             f     (chord-player chan instr)
             bass  (note-player 3 33)] ; acoustic bass
       (doseq [bar (partition 4 score)]
          (println bar)
          (let [[x1 x2 x3 x4] bar]
            (bass (- (first (chorddb x1)) 24) tempo vol)
;            (f (chorddb x1) tempo (* vol 0.7))
            (f (chorddb x2) tempo vol)
            (f (chorddb x3) tempo (* vol 0.7))
            (f (chorddb x4) tempo (* vol 0.9)))))))


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

(def autumn-leaves (score (str
  "Am7    / / / | D7    / / / | Gmaj7  /  /  / | Cmaj7  /  /  / "
  "F#m7-5 / / / | B7    / / / | Em     /  /  / | Em     /  /  / "
  "Am7    / / / | D7    / / / | Gmaj7  /  /  / | Cmaj7  /  /  / "
  "F#m7-5 / / / | B7    / / / | Em     /  /  / | Em     /  /  / "
  "F#m7-5 / / / | B7    / / / | Em     /  /  / | Em     /  /  / "
  "Am7    / / / | D7    / / / | Gmaj7  /  /  / | Gmaj7  /  /  / "
  "F#m7-5 / / / | B11-9 / / / | Em7    /  A7 / | Dm7    /  G7 / "
  "F#m7-5 / / / | B11-9 / / / | Em     /  /  / | Em     /  /  / ")))

(def all-by-myself (score (str
  "Cmaj7  / / /    | C6    / / /   | D7     /  /  /      | Am7   /  D7 / "
  "G7     / / /    | Dm7   / G7 /  | Em7   /  A7  /      | Dm    /  G7 / "
  "Cmaj7  / / /    | C6    / / /   | F#m7  /   B7  /     | E7    /  /  / "
  "Am7   / Am7-5 / | D7    / / /   | Dm7     /  Dm7-5  / | G7    /  /  / "
  "Cmaj7  / / /    | C6    / / /   | D7     /  /  /      | Am7   /  D7 / "
  "G7     / / /    | Dm7   / G7 /  | E7   /  E7+5  /     | E7    /  /  / "
  "Fmaj7  / / /    | F#dim7 / / /  | Cmaj7  / B7+5 /     | Em7-5 /  A7 / "
  "Am7   / D7 /    | Dm7    / G7 / | C6    /  Am7  /     | Dm7   /  G7 / ")))

(def in-a-sentimental-mood (score (str
  "Dm     / Dmmaj7 / | Dm7  / Dm6  /  | Gm   /  Gmmaj7 /  | Gm7   /  Gm6 A7 "
  "Dm     / / /      | D7   / / /     | Gm7   /  Gb7  /   | Fmaj7 /  /  / "
  "Dm     / Dmmaj7 / | Dm7  / Dm6  /  | Gm   /  Gmmaj7 /  | Gm7   /  Gm6 A7 "
  "Dm     / / /      | D7   / / /     | Gm7   /  Gb7  /   | Fmaj7 /  Ebm7 Ab7 "
  "Dbmaj7  / Bbm7 /  | Ebm7  / Ab7  / | Dbmaj7 /  Bb7 /   | Eb7   /  Ab7 / "
  "Dbmaj7  / Bbm7 /  | Ebm7  / Ab7  / | Gm7   /  / /      | C7    /  / / "
  "Dm     / Dmmaj7 / | Dm7  / Dm6  /  | Gm   /  Gmmaj7 /  | Gm7   /  Gm6 A7 "
  "Dm     / / /      | D7   / / /     | Gm7   /  C11-9  / | Fmaj7 /  /  / "
)))

(def let-it-be (score (str
  "C  / / / | G  / / / | Am  / /  / | F  /  /  / "
  "C  / / / | G  / / / | F   / /  / | C  /  /  / "
  "C  / / / | G  / / / | Am  / /  / | F  /  /  / "
  "C  / / / | G  / / / | F   / /  / | C  /  /  / "
  "Am / / / | G  / / / | F   / /  / | C  /  /  / "
  "C  / / / | G  / / / | F   / /  / | C  /  /  / ")))

(defn -main [& args]
   (repeatedly
      (play-song in-a-sentimental-mood {:bpm 62}))
;   (play-song all-by-myself {:bpm 120})
;   (play-song autumn-leaves {:bpm 100})
;   (play-song all-of-me {:bpm 100})
;   (play-song all-the-things-you-are {:instr 26})
;   (play-song let-it-be {:instr 20})
)

