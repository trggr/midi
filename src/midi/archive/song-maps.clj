(ns midi.song-maps)

(def songdb [
  {:id 1, :nm "ALL THE THINGS YOU ARE", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (str "Fm7    | Bbm7     | Eb7    | Abmaj7    |"
              "Dbmaj7 | G7       | Cmaj7  | Cmaj7     |"
              "Cm7    | Fm7      | Bb7    | Ebmaj7    |"
              "Abmaj7 | Am7-5 D7 | Gmaj7  | Gmaj7 E9  |"
              "Am7    | D7       | Gmaj7  | Gmaj7     |"
              "F#m7   | B7       | Emaj7  | C7+5      |"
              "Fm7    | Bbm7     | Eb7    | Abmaj7    |"
              "Dbmaj7 | Gb7      | Cm7    | Bdim7     |"
              "Bbm7   | Eb7      | Abmaj7 | Gm7-5  C9 ")}
  {:id 2, :nm "IN A SENTIMENTAL MOOD", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (str "Dm       Dmmaj7 | Dm7    Dm6 | Gm      Gmmaj7 | Gm7      Gm6 A7   |"
              "Dm              | D7         | Gm7     Gb7    | Fmaj7             |"
              "Dm       Dmmaj7 | Dm7    Dm6 | Gm      Gmmaj7 | Gm7      Gm6 A7   |"
              "Dm              | D7         | Gm7     Gb7    | Fmaj7    Ebm7 Ab7 |"
              "Dbmaj7   Bbm7   | Ebm7   Ab7 | Dbmaj7  Bb7    | Eb7      Ab7      |"
              "Dbmaj7   Bbm7   | Ebm7   Ab7 | Gm7            | C7                |"
              "Dm       Dmmaj7 | Dm7    Dm6 | Gm      Gmmaj7 | Gm7      Gm6 A7   |"
              "Dm              | D7         | Gm7     C11-9  | Fmaj7              ")}
  {:id 3, :nm "ALL OF ME", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (str "C6  | C6  | E7           | E7      |"
              "A7  | A7  | Dm7          | Dm7     |"
              "E7  | E7  | Am7          | Am7     |"
              "D7  | D7  | Dm7          | G7      |"
              "C6  | C6  | E7           | E7      |"
              "A7  | A7  | Dm7          | Dm7     |"
              "F6  | Fm6 | Cmaj7 Em7-5  | A7      |"
              "Dm7 | G7  | C6    Ebdim7 | Dm7  G7 |")}
  {:id 4, :nm "AUTUMN LEAVES", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (str "Am7    | D7    | Gmaj7    | Cmaj7    |"
              "F#m7-5 | B7    | Em       | Em       |"
              "Am7    | D7    | Gmaj7    | Cmaj7    |"
              "F#m7-5 | B7    | Em       | Em       |"
              "F#m7-5 | B7    | Em       | Em       |"
              "Am7    | D7    | Gmaj7    | Gmaj7    |"
              "F#m7-5 | B11-9 | Em7   A7 | Dm7    G7|"
              "F#m7-5 | B11-9 | Em       | Em       |")}
  {:id 5, :nm "ALL BY MYSELF", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (str "Cmaj7       | C6        | D7           | Am7    D7 |"
              "G7          | Dm7   G7  | Em7    A7    | Dm     G7 |"
              "Cmaj7       | C6        | F#m7   B7    | E7        |"
              "Am7   Am7-5 | D7        | Dm7    Dm7-5 | G7        |"
              "Cmaj7       | C6        | D7           | Am7    D7 |"
              "G7          | Dm7    G7 | E7     E7+5  | E7        |"
              "Fmaj7       | F#dim7    | Cmaj7  B7+5  | Em7-5  A7 |"
              "Am7   D7    | Dm7    G7 | C6     Am7   | Dm7    G7 |")}
  {:id 6, :nm "LET IT BE", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (str "C  | G  | Am | F |"
              "C  | G  | F  | C |"
              "C  | G  | Am | F |"
              "C  | G  | F  | C |"
              "Am | G  | F  | C |"
              "C  | G  | F  | C |")}
  {:id 7, :nm "MEDIUM BLUES", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
    :bars "C | F7 F#dim | C | C7 | F | F#dim | C | Em7-5 A7 | Dm7 | G7 | Em7-5 A7 | Dm G7"}
  {:id 8, :nm "ALONE TOGETHER", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 120,
   :bars (str "Dm          | Em7-5 A7  | Dm           | Em7-5  A7 |"
              "Dm          | Am7-5 D7  | Gm           | Gm7       |"
              "Bm7     E7  | Gm7   C7  | F      F7    | Em7-5  A7 |"
              "Dmaj7       | Em7-5 A7  |"
              "Dm          | Em7-5 A7  | Dm           | Em7-5  A7 |"
              "Dm          | Am7-5 D7  | Gm           | Gm7       |"
              "Bm7     E7  | Gm7   C7  | F      F7    | Em7-5  A7 |"
              "Dmaj7       | Dmaj7     |"
              "Am7-5       | D7        | Gm           | Gm        |"
              "Gm7-5       | C7        | F      F7    | Em7-5  A7 |"
              "Dm          | Em7-5 A7  | Dm           | Em7-5  A7 |"
              "Dm          | Bb7   A7  | Dm           | Em7-5  A7 |")}
  {:id 9, :nm "MISTY", :numer 4, :denom 4, :ppq 400000, :bb 8, :bpm 80, :drum "drums-swing", :bass "patterns",
   :bars (str "Ebmaj7      | Bbm7  Eb7 | Abmaj7       | Abm7   Db7|"
              "Ebmaj7  Cm7 | Fm7   Bb7 | Gm7    C7    | Fm7    Bb7|"
              "Ebmaj7      | Bbm7  Eb7 | Abmaj7       | Abm7   Db7|"
              "Ebmaj7  Cm7 | Fm7   Bb7 | Eb6    Db9   | Ebmaj7    |"
              "Bbm7        | Eb7-9     | Abmaj7       | Abmaj7    |"
              "Am7         | D7    F7  | Gm7    C7-9  | Fm7    Bb7|"
              "Ebmaj7      | Bbm7  Eb7 | Abmaj7       | Abm7   Db7|"
              "Ebmaj7 Cm7  | Fm7   Bb7 | Eb6    Cm7   | Fm7    Bb7|")}])
