(ns midi.midifile2
  (:require [midi.dbload :as db])
  (:import (com.leff.midi MidiTrack MidiFile)
           (com.leff.midi.event ProgramChange)
           (com.leff.midi.event.meta TimeSignature Tempo)))

(defn notes->midi-file
  "Takes collection of notes (timecode channel note velocity) and BPM,
   saving them as a MIDI file"
  [notes bpm file-name]
  (let [tempo-track  (doto (MidiTrack.)
                       (.insertEvent
                        (doto (TimeSignature.)
                          (.setTimeSignature 4 4
                                             TimeSignature/DEFAULT_METER
                                             TimeSignature/DEFAULT_DIVISION)))
                       (.insertEvent (doto (Tempo.) (.setBpm bpm)))
                       (.insertEvent (ProgramChange. 0 db/CHORD-CHANNEL 26))
                       (.insertEvent (ProgramChange. 0 db/BASS-CHANNEL 32)))
        note-track (reduce (fn [acc [tc c note velocity duration]]
                             (println tc c note velocity duration)
                             (doto acc (.insertNote c note velocity tc duration)))
                           (MidiTrack.)
                           notes)]
    (doto (MidiFile. MidiFile/DEFAULT_RESOLUTION [tempo-track note-track])
      (.writeToFile (java.io.File. file-name)))))

