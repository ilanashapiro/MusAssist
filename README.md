# MusAssist

(NOTE: the compiler code and associated MusAssist code examples are all located in the "app" folder of this directory. Instructions for running programs are in app/Compile.hs. The published paper at TENOR'23 can be accessed <a href="https://www.tenor-conference.org/proceedings/2023/11-TENOR_BOSTON_2023_paper_9804Shapiro.pdf">HERE</a>)

When writing music, composers must manually transition from theoretical musical concepts to notes on a page. This process can be tedious and slow, requiring the composer to expand complex musical structures by hand, such as cadences and harmonic sequences, to the individual notes they define. Therefore, the level of abstraction of the musical structure is higher than what the composer writes. 

Domain specific languages (DSLs) are programming languages highly specialized for a specific application and thus characterized by limited expressiveness. 
An $external$ $DSL$ has custom syntax that is separated from the primary language of its application. 

MusAssist is an external, declarative DSL for music notation that bridges the abstraction divide between music theory and notation. Users describe a composition in MusAssist’s straightforward, high-level syntax, modeled around the musical elements composers organically conceive when writing by hand, and the MusAssist compiler automates the expansion of these elements to their
constituent notes. MusAssist’s declarative programming paradigm was chosen to correspond with the lack of control structures in handwritten music.

MusAssist is unique in that users can encode specifications for complex musical templates at the same level of abstraction as the theoretical musical structures they describe. Specifically,  users can specify high-level templates for 
<b>chords</b> and <b>arpeggios</b> (major, minor, dominant, augmented, half diminished, and diminished triads and seventh chords in any inversion), 
<b>scales</b> (major, natural/harmonic/melodic minor, chromatic, and whole tone), 
<b>cadences</b> (perfect authentic, imperfect authentic, plagal, half, deceptive), and 
<b>harmonic sequences</b> (ascending fifths, descending fifths, ascending 5-6, descending 5-6)of a desired length. MusAssist also supports individual notes, rests, and customized chords consisting of user-defined collections of notes, and enables the user to change the key signature or start a new measure. All high-level templates are expanded, lowering the abstraction level to notes, by the Haskell-based MusAssist compiler.


The target language of the MusAssist compiler is MusicXML, itself a DSL that is an extension of XML (Extensible Markup Language). MusicXML is accepted by most major notation software, such as MuseScore. Thus, users can open the resulting MusicXML file of a compiled MusAssist composition in MuseScore or another program for further customization and editing. Beyond a professional music compositional aid, MusAssist may be particularly helpful to music theory students as an educational tool, enabling them to visualize the relationship between a theoretical musical structure and its expanded form, such as in understanding the chords resulting from the expansion of a cadence. 

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
(F#4 dominant seventh chord, third inversion, eighth)
(D4 whole) (F#4 dotted_quarter) (Ab4 quarter) (G##4 eighth) (rest sixteenth) 
NEW_MEASURE
NEW_MEASURE
([Bbb5, Db5, C5] half) 
([C#5, E5] half) (C6 minor triad, first inversion, dotted_eighth) 
(F#4 half diminished seventh chord, second inversion, eighth) 
(D#4 augmented seventh arpeggio, root inversion, quarter)
SET_KEY D minor
SET_KEY C# major
(C harmonic minor descending scale, startNote = Eb4, quarter, length=10)
(Descending Fifths Sequence, G5 minor, quarter, length=15) 
(Perfect Authentic Cadence, D#5 major, half)
notes1 chords1 (Ascending Fifths Sequence, G#3 minor, quarter, length=5) 
chord (Deceptive Cadence, Eb5 minor, sixteenth) chords1
</pre>

![image](https://user-images.githubusercontent.com/28958079/205985367-090d2348-7a72-4b11-a38c-37e1979a8983.png)

Notice the following:
<ul>
  <li>The key signature can be changed consecutively as many times as desired, but only the last will take effect (as seen m. 1 and m. 12 of the bottom figure). Changing the key signature triggers a new measure.</li>
  <li>Note durations are automatically broken by the compiler both on the strong beat and on the barline (such as in mm. 2-4 of the bottom figure).</li>
  <li>Labeled phrases are not notated until the label is referenced, rather than defined.</li>
  <li>The difference between a customized chord and a chord template is exemplified on line 14 of the top figure.</li>
</ul>
