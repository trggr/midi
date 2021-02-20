; http://www.ccarh.org/courses/253/handout/smf/
; https://www.fileformat.info/format/midi/corion.htm
; http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html

(defn slurp-bytes [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn slice [b start end]
   (for [i (range start end)]
      (nth b i)))

(def s (slurp-bytes "FALL_01.MID"))

; (def tracks (clojure.string/split s #"MTrk"))
; (map count tracks)
; (def hdr (first tracks))

(defn bnum [data]
  (reduce bit-or 
         (map-indexed
             (fn [i x]
                (bit-shift-left (bit-and x 0x0FF) (* 8 (- (count data) i 1))))
             data)))

; begin at start, and find the beginning of the next midi track which starts with "Mtrk"
(defn findnexttrk [s start]
   (loop [i start]
        (if (>= i (- (count s) 3))
            nil
            (if (= 1297379947 (bnum (slice s i (+ i 4)))) ; 1297379947 = MTrk
               i
               (recur (inc i))))))

(defn getchunks [s start]
   (loop [p start acc []]
       (println p (count acc))
       (let [n (findnexttrk s (inc p))]
          (if (or (nil? n) (>= (count acc) 12))
             (conj acc (slice s p (count s)))
             (recur n (conj acc (slice s p n)))))))

(defn chunk->track [c]
   {:sig    (apply str (map char (slice c 0 4)))
    :len    (bnum (slice c 4 8))
    :events (slice c 8 (count c))})

(defn chunk->header [c]
   {:mthd     (apply str (map char (slice c 0 4)))
    :hdlen    (bnum (slice c 4 8))
    :format   (bnum (slice c 8 10))
    :ntracks  (bnum (slice c 10 12))
    :division (bnum (slice c 10 12))})

(defn stop [text]
  (throw (Exception. text)))

; read variable integer from Midi file. 
(defn read-varint [s p]
   (println "in read-varint" p)
   (if (>= p (count s))
      (stop "read-varint beyond the buffer")
      (loop [i 0, acc 0]
         (if (>= i (count s))
            [acc i]
            (let [byte    (nth s (+ p i))
                  highbit (bit-and byte 0x80)
                  rc      (+ (bit-and byte 0x7F) (* acc 128))]
               (if (zero? highbit)
                     [rc i]
                     (recur (inc i) rc)))))))

(assert (every? identity (->> [[0x7F] [0x81 0x7F] [0x82 0x80 0x00]]
                              (map #(read-varint % 0))
                              (map first)
                              (map = [127 255 32768]))))

(defn ff-meta-len-txt [name xs p]
   (let [[len i] (read-varint xs p)
         start   (+ p i 1)
         end     (+ start len)
         text    (apply str (map char (slice xs start end)))]
       [name text end]))
;(fn [xs p] (let [[len i] (read-varint xs p)
;                               start   (+ p i 1)
;                               end     (+ start len)
;                               text    (slice xs start end)]
;                            ["Text event" text end]))

(def semantic
  {0xFF {0x01 (partial ff-meta-len-txt "Text event")
         0x02 (partial ff-meta-len-txt "Copyright notice")
         0x03 (partial ff-meta-len-txt "Track Name")
         0x04 (partial ff-meta-len-txt "Instrument Name")
         0x06 (partial ff-meta-len-txt "Marker")
         0x20 (fn [xs p] (let [[_ cc] (slice xs p (+ 2 p))]
                             ["Channel prefix" [cc] (+ 2 p)]))
         0x2F (fn [xs p] ["End of track" "" (inc p)])
         0x51 (fn [xs p] (let [[_ tttttt] (slice xs p (+ 4 p))]
                             ["Set tempo" [tttttt] (+ 4 p)]))
         0x54 (fn [xs p] (let [[_ hr mn se fr ff] (slice xs p (+ 6 p))]
                             ["SMPTE Offset" [hr mn se fr ff] (+ 6 p)]))
         0x58 (fn [xs p] (let [[_ nn dd cc bb] (slice xs p (+ 5 p))]
                             ["Time signature" [nn dd cc bb] (+ 5 p)]))
         0x59 (fn [xs p] (let [[_ sf mi] (slice xs p (+ 3 p))]
                             ["Key signature" [sf mi] (+ 3 p)]))}})

(parsetrack events2)

(defn parsetrack [s]
   (loop [i 0, acc []]
      (if (>= i (count s))
          acc
          (let [[val q] (read-varint s i)
                p       (+ i q 1)
                event   (bnum (vector (nth s p)))
                p       (inc p)
                ty      (bnum (vector (nth s p)))
                p       (inc p)
                f       (get-in semantic [event ty])]
             (if (nil? f)
                 (do 
                    (pprint acc)
                    (println (map #(format "%x" %) (slice s (+ i q 1) (count s))))
                    (stop (format " unsupported: event=%x(%d), ty=%x(%d), val=%d, q=%d, i=%d" event event ty ty val q i)))
                 (let [[desc val pos :as whole] (f s p)]
                    (recur pos (conj acc whole))))))))


(def chunks (getchunks s 0))
(def header (chunk->header (first chunks)))
(def tracks (map chunk->track (rest chunks)))

(every? identity
        (map (fn [{:keys [sig len events]}]
                (and (= sig "MTrk") (= len (count events))))
             tracks))



