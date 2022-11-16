# MusAssist

(NOTE: the compiler code and associated MusAssist code examples are all located in the "app" folder of this directory. Instructions for running programs are in app/Compile.hs)

When writing music, composers must manually transition from musical theoretical concepts to notes on a page.
This process can be tedious and slow, requiring the composer to expand complex structures, such as cadences and sequences,
by hand to the notes that they constitute. The level of abstraction of the musical theoretical structure is 
higher than what the composer actually writes. 

Domain specific languages, or DSLs, 
are programming languages highly specialized for a specific application and thus characterized by limited expressiveness.
An $external$ $DSL$ has custom syntax that is separated from the primary language of its application. 
MusAssist is an external, declarative domain specific language (DSL) for music notation that attempts bridge the divide between
music theory and notation. Users describe a composition in MusAssist's straightforward syntax, and 
the MusAssist compiler writes out the music via these instructions. MusAssist's declarative programming 
paradigm was chosen to correspond with the declarative nature of handwritten music. 

Fundamentally, MusAssist supports notes (including rests) and custom chords (i.e. any desired collection of notes)
in the octave and key of choice, as well as commands to change the key signature or start a new measure.
 MusAssist is unique in that users can also write specifications for complex musical templates \textit{at the same level of abstraction
as the musical theoretical structures they describe}. MusAssist supports templates for
\textbf{chords} (all triads and seventh chords in any inversion),
\textbf{scales} (all diatonic scales, as well as chromatic and whole tone),
\textbf{scales} (all triad and seventh arpeggios in any inversion),
\textbf{cadences} (perfect authentic, imperfect authentic, plagal, half, deceptive), and 
\textbf{harmonic sequences} (ascending
fifths, descending fifths, ascending 5-6, descending 5-6) of a desired length. The musical expression 
described by a specification is completely expanded out (i.e. the level of abstraction is
fully lowered to the note level) by the Haskell-based MusAssist compiler.

The target language of the MusAssist compiler is MusicXML, itself a DSL that is an extension of
XML (Extensible Markup Language). MusicXML is accepted by most major notation software programs (such as MuseScore). 
Thus users can open can open the resulting MusicXML file of a compiled MusAssist composition in MuseScore or another
program for further customization and editing, thus bypassing the need to write out complex musical templates by hand at a 
note- and chord-level of abstraction. Beyond a professional music compositional aid, MusAssist may be particularly 
helpful to music theory students as an educational tool, enabling them to visualize the relationship between a theortical musical structure 
and its expanded form, such as a cadence and the chords resulting from its expansion.

Example program (note how note lengths are broken up on the strong beat of the measure in addition to the barlines):
<pre>
SET_KEY A major
SET_KEY A major
SET_KEY G major
(D4 whole) (F#4 quarter) (Ab4 dotted_quarter) (G#4 eighth) (rest sixteenth)
// note without b or # is natural
notes1 = (D4 whole) (F#4 quarter) (Ab4 quarter) (G#4 eighth) (rest whole)  
chords1 = ([Bbb5, Db5, C5] half) ([C#5, E5] half) 
chord = (D6 minor arpeggio, root inversion, eighth) 
(F#4 half diminished seventh chord, second inversion, eighth)
(D4 whole) (F#4 dotted_quarter) (Ab4 quarter) (G##4 eighth) (rest sixteenth) 
NEW_MEASURE
NEW_MEASURE
([Bbb5, Db5, C5] half) 
([C#5, E5] half) (C6 minor triad, first inversion, dotted_eighth) 
(F#4 half diminished seventh chord, second inversion, eighth) 
(D#4 diminished seventh arpeggio, root inversion, quarter)
SET_KEY D minor
SET_KEY C# major
(C harmonic minor descending scale, startNote = Eb4, quarter, length=10)
(Descending Fifths Sequence, G5 minor, quarter, length=15) 
(Perfect Authentic Cadence, D#5 major, half)
notes1 chords1 (Ascending Fifths Sequence, G#3 minor, quarter, length=5) 
chord (Deceptive Cadence, Eb5 minor, sixteenth) chords1
</pre>

![image](https://user-images.githubusercontent.com/28958079/202308303-2c0ef145-49a1-4bdf-acba-ec94817bd3f2.png)
