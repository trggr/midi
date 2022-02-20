(ns midi.midifile2
  (:import (com.leff.midi MidiTrack MidiFile)
           (com.leff.midi.event.meta TimeSignature Tempo)))

(def tempo-track
  (doto (MidiTrack.)
    (.insertEvent
     (doto (TimeSignature.)
       (.setTimeSignature 4 4 TimeSignature/DEFAULT_METER TimeSignature/DEFAULT_DIVISION)))
    (.insertEvent
     (doto (Tempo.) (.setBpm 228)))))

(def note-track (MidiTrack.))

;; channel, pitch, velocity, tick, duration
(dotimes [i 80]
  (doto note-track
    (.insertNote 0 (inc i) 100 (* i 480) 120)))

(doto (MidiFile. MidiFile/DEFAULT_RESOLUTION [tempo-track note-track])
  (.writeToFile (java.io.File. "exampleout2.mid")))

