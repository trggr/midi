(ns midi.midifile2
  (:require [midi.dbload :as db])
  (:import (com.leff.midi MidiTrack MidiFile)
           (com.leff.midi.event ProgramChange)
           (com.leff.midi.event.meta TimeSignature Tempo)))

;; (def tempo-track
;;   (doto (MidiTrack.)
;;     (.insertEvent
;;      (doto (TimeSignature.)
;;        (.setTimeSignature 4 4 TimeSignature/DEFAULT_METER TimeSignature/DEFAULT_DIVISION)))
;;     (.insertEvent
;;      (doto (Tempo.) (.setBpm 228)))))

;; (def note-track (MidiTrack.))

;; ;; channel, pitch, velocity, tick, duration
;; (dotimes [i 80]
;;   (doto note-track
;;     (.insertNote 0 (inc i) 100 (* i 480) 120)))

;; (doto (MidiFile. MidiFile/DEFAULT_RESOLUTION [tempo-track note-track])
;;   (.writeToFile (java.io.File. "exampleout2.mid")))

  
;;  [timecode channel note velocity]
(defn save [file-name track bpm]
  (let [tempo-track  (doto (MidiTrack.)
                       (.insertEvent
                        (doto (TimeSignature.)
                          (.setTimeSignature 4 4
                                             TimeSignature/DEFAULT_METER
                                             TimeSignature/DEFAULT_DIVISION)))
                       (.insertEvent (doto (Tempo.) (.setBpm bpm)))
                       (.insertEvent (ProgramChange. 0 db/CHORD-CHANNEL 26))
                       (.insertEvent (ProgramChange. 0 db/BASS-CHANNEL 32)))
        note-track (MidiTrack.)
        note-track (reduce (fn [acc [tc c note velocity duration]]
                             (doto acc (.insertNote c note velocity tc duration)))
                           note-track
                           track)]
    (doto (MidiFile. MidiFile/DEFAULT_RESOLUTION [tempo-track note-track])
      (.writeToFile (java.io.File. file-name)))))

