# MusAssist
MusAssist is an external DSL devised as a compositional aid for music notation by incorporating concepts from music theory. It attempts to organically model a composer’s flow of thought by modeling its syntax after the musical structures a composer conceives when writing. Users write out musical elements and expressions in MusAssist’s simple and straightforward syntax much in the same way they would when composing. In other words, users describe a composition in MusAssist, and MusAssist writes out the music via these instructions. Users can compose notes (including rests) and custom chords in the octave and key of choice. They can also specify templates for chords (all triads and seventh chords), harmonic sequences (chosen from Ascending Fifths, Descending Fifths, Ascending 5-6, and Descending 5-6) of a desired length, and cadences (chosen from Perfect Auth. The musical expression described by the template will then be written by the MusAssist compiler. The compiler is written in Haskell.

The target language of the MusAssist compiler is MusicXML, an internal DSL that is an extension of XML. MusicXML is interpreted by most major notation software programs (such as MuseScore). Thus, once a user has described a composition in MusAssist, they can open the resulting MusicXML file in MuseScore or another program for further customization and editing. MusAssist does not attempt to replace existing DSLs. Rather, it fills a unique niche in that it assists users in music composition by providing them with a set of easy-to-use instructions that would otherwise be tedious to write out by hand in a musical score. This is why MusAssist is compiled to MusicXML rather than an uneditable PDF format. MusAssist may also be particularly helpful to music students as an educational tool where they can easily see the relationship between a musical expression and its written form, such as a harmonic sequence template and the actual chords that result from it.

In order to use MusAssist, the user need not have any understanding of computing, though they should have a solid knowledge of music theory up through chord and cadence types, as well as harmonic sequences. In order to comprehend this paper, in addition to music theory, users should have a background in basic programming languages theory and compilers.

Example program:
<pre>
SET_KEY A major
(D4 whole) (F#4 quarter) (Ab4 quarter) (G#4 eighth) (rest sixteenth)           // this is a comment
notes1 = (D4 whole) (F#4 quarter) (Ab4 quarter) (G#4 eighth) (rest whole)  // note without b or # is considered to be natural
chords1 = ([Bbb5, Db5, C5] half) ([C#5, E5] half) (D6 minor arpeggio, root inversion, eighth) (F#4 half diminished seventh chord, second inversion, eighth)
(D4 whole) (F#4 quarter) (Ab4 quarter) (G##4 eighth) (rest sixteenth)  // note without b or # is considered to be natural
([Bbb5, Db5, C5] half) ([C#5, E5] half) (C6 minor triad, first inversion, quarter) (F#4 half diminished seventh chord, second inversion, eighth) (rest quarter)
(D#4 diminished seventh arpeggio, root inversion, quarter)
SET_KEY D minor
NEW_MEASURE
(C harmonic minor descending scale, startNote = Eb4, quarter, length=10)
(Descending Fifths Sequence, G5 minor, quarter, length=15) (Perfect Authentic Cadence, Eb5 minor, half)
notes1 chords1 (Ascending Fifths Sequence, G3 minor, quarter, length=5) chords1 (Perfect Authentic Cadence, Eb5 minor, sixteenth) chords1
</pre>

<img width="665" alt="image" src="https://user-images.githubusercontent.com/28958079/201023259-e3faea28-302c-4813-a75a-e405dec7bb16.png">

  
