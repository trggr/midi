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

Plan:
1. Determine available MIDI devices
(javax.sound.midi.MidiSystem/getMidiDeviceInfo)
[#object[com.sun.media.sound.SoftSynthesizer$Info 0x3473181a "Gervill"],
 #object[com.sun.media.sound.RealTimeSequencer$RealTimeSequencerInfo 0x2f820baa "Real Time Sequencer"],
 #object[com.sun.media.sound.MidiOutDeviceProvider$MidiOutDeviceInfo 0x21121a51 "XMidi1X1 [hw:1,0,0]"],
 #object[com.sun.media.sound.MidiInDeviceProvider$MidiInDeviceInfo 0x763d22e "XMidi1X1 [hw:1,0,0]"]]


MidiDevice.Info[] MidiDeviceInfos = MidiSystem.getMidiDeviceInfo();
//find the suitable device number here, based on some criteria
MidiDevice MidiOutDevice = MidiSystem.getMidiDevice(MidiDeviceInfos[DEVICE_NUMBER]);
Receiver MidiOutReceiver = MidiOutDevice.getReceiver();
Sequencer MidiOutSequencer = MidiSystem.getSequencer();
//Add the new MIDI out device here.
MidiOutSequencer.getTransmitter().setReceiver(MidiOutReceiver);
MidiOutSequencer.open();
