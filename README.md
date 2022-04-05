# MusAssist
MusAssist is an external DSL devised as a compositional aid for music notation by incorporating concepts from music theory. It attempts to organically model a composer’s flow of thought by modeling its syntax after the musical structures a composer conceives when writing. Users write out musical elements and expressions in MusAssist’s simple and straightforward syntax much in the same way they would when composing. In other words, users describe a composition in MusAssist, and MusAssist writes out the music via these instructions. Users can compose notes (including rests) and custom chords in the octave and key of choice. They can also specify templates for chords (all triads and seventh chords), harmonic sequences (chosen from Ascending Fifths, Descending Fifths, Ascending 5-6, and Descending 5-6) of a desired length, and cadences (chosen from Perfect Auth. The musical expression described by the template will then be written by the MusAssist compiler. The compiler is written in Haskell.

The target language of the MusAssist compiler is MusicXML, an internal DSL that is an extension of XML. MusicXML is interpreted by most major notation software programs (such as MuseScore). Thus, once a user has described a composition in MusAssist, they can open the resulting MusicXML file in MuseScore or another program for further customization and editing. MusAssist does not attempt to replace existing DSLs. Rather, it fills a unique niche in that it assists users in music composition by providing them with a set of easy-to-use instructions that would otherwise be tedious to write out by hand in a musical score. This is why MusAssist is compiled to MusicXML rather than an uneditable PDF format. MusAssist may also be particularly helpful to music students as an educational tool where they can easily see the relationship between a musical expression and its written form, such as a harmonic sequence template and the actual chords that result from it.

In order to use MusAssist, the user need not have any understanding of computing, though they should have a solid knowledge of music theory up through chord and cadence types, as well as harmonic sequences. In order to comprehend this paper, in addition to music theory, users should have a background in basic programming languages theory and compilers.

Example program:
<pre>
SET_KEY Amaj
(D4 whole) (F#4 quarter) (Ab4 quarter) (G#4 eighth) (rest sixteenth)
// this is a comment
notes1 = (D4 whole) (F#4 quarter) (Ab4 quarter) (G#4 eighth) (rest whole)
// note without b or # is considered to be natural
chords1 = ([Bbb5, Db5, C5] half) ([C#5, E5] half) (C6 min triad inv:first quarter)
(F#4 halfdim seventh inv:second eighth)
(D4 whole) (F#4 quarter) (Ab4 quarter) (G##4 eighth) (rest sixteenth)
([Bbb5, Db5, C5] half) ([C#5, E5] half) (C6 min triad inv:first quarter)
(F#4 halfdim seventh inv:second eighth)
SET_KEY Dmin
NEW_MEASURE
(DescFifths G5 min quarter length:15) (PerfAuthCadence Eb5 min half)
notes1 chords1 (AscFifths G3 min quarter length:5) chords1
(PerfAuthCadence Eb5 min sixteenth) chords1
</pre>

<img width="513" alt="Screen Shot 2022-04-04 at 10 37 39 PM" src="https://user-images.githubusercontent.com/28958079/161685974-850be8c6-8439-4db6-81e7-79d01862409e.png">

  
