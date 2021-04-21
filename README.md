# MIDI
                                                            
My own implementation of synthesizer.

To run without lein:

    cd 0atl/midi
    repl2
    (load-file "src/midi/core.clj")
    (in-ns 'midi.core)
    (-main)


To reset MIDI send GM Reset SysEx message (F0 7E 7F 09 01 F7)

    amidi -p hw:1 -S f07e7f0901f7

Find the list of MIDI devices:

    aplaymidi -l

    Port    Client name                      Port name
    14:0    Midi Through                     Midi Through Port-0
    20:0    E-MU XMidi1X1                    E-MU XMidi1X1 MIDI 1

To play on external MIDI 

    aplaymidi --port 20:0 bohemian.mid 

Better MIDI sounds

1. Download FluidR3 soundfont from

    https://member.keymusician.com/Member/FluidR3_GM/index.html

2. cd /home/tim/.gervill/
   cp soundbank-emg.sf2 soundbank-emg.sf2.bak
   cp FluidR3_GM.sf2 soundbank-emg.sf2
                                             
Good resources

    http://tedfelix.com/linux/linux-midi.html
    https://blog.djy.io/making-midi-sound-awesome-in-your-jvm/

