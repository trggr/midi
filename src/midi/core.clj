(ns midi.core)

(def notedb {:c  60, :c#  61, :d  62, :d# 63, :e 64, :f 65, :f# 66, :g 67 :g# 68, :a 69, :a# 70, :b 71,
             :C  60, :C#  61, :D  62, :D# 63, :E 64, :F 65, :F# 66, :G 67 :G# 68, :A 69, :A# 70, :B 71,
                     :Db  61,         :Eb 63,               :Gb 66,       :Ab 68,        :Bb 70,
             :c2 72, :c2# 73, :d2 74, :d2# 75, :e2 76})

(def c-major [:c :d  :e  :f  :g  :a   :b])
(def d-major [:d :e  :f# :g  :a  :b   :c2#])
(def e-major [:e :f# :g# :a  :b  :c2# :d2#])
(def f-major [:f :g  :a  :a# :c2 :d2  :e2])

(def qn 96) ; duration of a quarter note

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

(defn deu [xs]  
   (println (clojure.string/join \newline
                (for [row xs]
                    (clojure.string/join \tab row)))))

(defn view
  ([coll t d] (deu (take t (drop d coll))))
  ([coll t]   (view coll t 0))
  ([coll]     (view coll 40 0)))

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
;(def ttape [
;            [0	 :set-tempo	 434464]
;            [0	 :time-signature [4 2 24 8]]
;            [(* 4 24)  :data	         [[24 2 60 60]]]
;            [(* 4 48)  :data	         [[48 2 64 60]]]
;            [(* 4 62)  :data	         [[62 2 67 60]]]
;            [(* 4 96)  :data	         [[96 2 72 60]]]
;            [(* 4 120) :data	         [[120 2 60 0]]]
;            [(* 4 120) :data	         [[120 2 64 0]]]
;            [(* 4 120) :data	         [[120 2 67 0]]]
;            [(* 4 120) :data	         [[120 2 72 0]]]])

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

;(def x1 let-it-be)
;(def x1a (map-indexed (fn [i x] [i x]) x1))
;(def x2 (reduce (fn [acc [n c]]
;                   (let [beat (mod n 4)
;                         bar  (inc (/ (- n beat) 4))
;                         tc   (* qn (+ (* bar 4) beat))]
;                     (assoc-in acc
;                               [bar tc]
;                               (map #(vector tc 2 % 70) (chorddb c)))))
;                 (sorted-map)
;                 x1a))
;(def x3on  (for [[n bar] x2, [tc chord] bar, note chord] note))
;(def x3off (map (fn [[t c n v]] [(+ t qn) c n 0]) x3on))
;(def x3 (concat x3on x3off))
;(def x4 (sort-by key (group-by first x3)))
;(def x5 (for [[tc data] x4] [tc :data data]))
;(def x6 (concat [[0 :set-tempo 434464][0 :time-signature [4 2 24 8]]] x5))

(defn to-ttape [score]
   (let [x1  score
         x1a (map-indexed (fn [i x] [i x]) x1)
         x2  (reduce (fn [acc [n c]]
                      (let [beat (mod n 4)
                            bar  (inc (/ (- n beat) 4))
                            tc   (* qn (+ (* bar 4) beat))]
                        (assoc-in acc
                                  [bar tc]
                                  (map #(vector tc 2 % 70) (chorddb c)))))
                    (sorted-map)
                    x1a)
         x3on  (for [[n bar] x2, [tc chord] bar, note chord] note)
         x3off (map (fn [[t c n v]] [(+ t qn) c n 0]) x3on)
         x3    (concat x3on x3off)
         x4    (sort-by key (group-by first x3))
         x5    (for [[tc data] x4] [tc :data data])
;         rc (concat [[0 :set-tempo 434464][0 :time-signature [4 2 24 8]]] x5)]
         rc (concat [[0 :set-tempo 434464][0 :time-signature [4 2 24 8]]] x5)]
     rc))

;                   [r x3 x5] chord
;                   bass (- (case i 0 r
;                                   1 (+ 2 r)
;                                   2 x3 
;                                   3 x5) 24)]
;                 (f {:bass bass, :chord chord} tempo vol)))))))

;(def song2 [[:c 4][:e 3][:g 2][:c2 1]])
;
;(defn to-ttape2 [song]
;   (let [qn  96  ; duration of a quarter note
;         t1  (loop [tc (* 4 qn) acc [] xs song]
;                (if-not (seq xs)
;                   acc
;                   (let [[[note dur] & others] xs]
;                      (recur (+ tc quarter-note)
;                             (conj acc [tc 2 (notedb note) 60]
;                                   [(+ tc (* dur qn)) 2 (notedb note) 0])
;                             others))))
;         notes  (->> t1 (group-by first) (map (fn [[t n]] [t :data n])))
;         tempo [[0 :set-tempo 434464][0 :time-signature [4 2 24 8]]]
;         rc    (sort-by (juxt first second) (concat tempo notes))]
;     rc))

;(def song [[ [:c]  [:e :g] [:d]  [:f :a]]
;           [ [:d]  [:f   ] [:a]  [:d2  ]]])
;

;                       (if (sequential? e)
;                          (for [x e] [(+ bar-tc (* i qn)) (notedb x) 60])

;(defn to-ttape [song]
;   (let [t1  (loop [tc (* 4 qn) acc [] xs song]
;                (if-not (seq xs)
;                   acc
;                   (let [[bar & others] xs]
;                      (recur (+ tc (* 4 qn))
;                             (concat acc (process-bar bar tc))
;                             others))))
;         notes  (->> t1 (group-by first) (map (fn [[t n]] [t :data n])))
;         tempo [[0 :set-tempo 434464][0 :time-signature [4 2 24 8]]]
;         rc    (sort-by (juxt first second) (concat tempo notes))]
;     rc))

(defn note-player2 [instr]
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
   (let [play-note (note-player2 1)]
      (doseq [[tc notes] tape]
         (Thread/sleep tc)
;         (print tc)
         (doseq [[tick ch note vel] notes]
;            (print " " tick ch note vel)
            (play-note ch note vel))
;         (println)
       )))

;(def tape (make-tape ttape 2))
;(play (make-tape ttape 2))

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
   (play (make-tape (to-ttape all-by-myself)))
;   (repeatedly
;      (play-song in-a-sentimental-mood {:bpm 62}))
;   (play-song all-by-myself {:bpm 120})
;   (play-song autumn-leaves {:bpm 100})
;   (play-song all-of-me {:bpm 100})
;   (play-song all-the-things-you-are {:instr 26})
;   (play-song let-it-be {:instr 20})
)

