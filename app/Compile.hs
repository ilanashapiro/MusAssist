{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module       : MusAssist program
Description  : The main program for compiling MusAssist to MusicXML
Maintainer   : Ilana Shapiro

You can load this file into ghci with
    :load compile_and_run.hs
and then run
    run "test1.musassist"
and so on, or you can compile it on the command line with
    ghc --make -O Compile.hs
    ****AS OF OCT 2022: ghc --make -O Compile.hs -package bimap -package parsec
and run
    ./Compile test1.musassist
-}

module Main where

import           Data.Char          (isSpace)
import           Data.IORef         as IORef
import qualified MusAssistAST       as MusAST
import qualified Parser
import qualified MusicXMLgen
import qualified IRConversion
import           System.Environment (getArgs)
import           System.FilePath    (replaceExtension, takeExtension)
import           Data.Map(Map)
import qualified Data.Map as Map

----------
-- Main --
----------
-- | Given a filename, translates the MusAssist program in that file to musicXML,
--   then write the musicXML code to a .musicxml file.

main :: IO ()
main = do
  args <- getArgs
  let fileName = case args of
        [arg] -> arg
        _     -> error "exactly one filename expected"

  -- A mutable counter used throughout the compiler code so that we can
  --    keep track of the beat to generate measures correctly
  ct        <- IORef.newIORef 0.0

  -- Read in the .ast file containing Haskell code
  --   for a list of MusAssistAST values from the parse result
  unprocessedAST <- case takeExtension fileName of 
    ".musassist" -> do
      text <- readFile fileName
      Parser.parseFile fileName
    ext -> error $ "unexpected extension " ++ show ext

  processedAST <- IRConversion.expandIntermediateInstrs unprocessedAST

  -- Translate MusAssistAST code to musicXML code
  putStrLn "Generating musicXML code..."
  code          <- MusicXMLgen.transInstrs processedAST

    -- header code for musicXML file
  let headerCode =
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 3.1 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">",
            "<score-partwise version=\"3.1\">",
            "\t<identification>",
            "\t<encoding>",
                  "\t\t<software>MuseScore 3.6.0</software>",
                  "\t\t<encoding-date>2022-02-24</encoding-date>",
                  "\t\t<supports element=\"accidental\" type=\"yes\"/>",
                  "\t\t<supports element=\"beam\" type=\"no\"/>",
                  "\t\t<supports element=\"print\" attribute=\"new-page\" type=\"no\"/>",
                  "\t\t<supports element=\"print\" attribute=\"new-system\" type=\"no\"/>",
                  "\t\t<supports element=\"stem\" type=\"no\"/>",
                  "\t\t</encoding>",
            "\t</identification>",
            "\t<defaults>",
            "\t<scaling>",
                  "\t\t<millimeters>6.99911</millimeters>",
                  "\t\t<tenths>40</tenths>",
            "\t</scaling>",
            "\t<page-layout>",
                  "\t\t<page-height>1696.94</page-height>",
                  "\t\t<page-width>1200.48</page-width>",
                  "\t\t<page-margins type=\"even\">",
                  "\t\t\t<left-margin>85.7252</left-margin>",
                  "\t\t\t<right-margin>85.7252</right-margin>",
                  "\t\t\t<top-margin>85.7252</top-margin>",
                  "\t\t\t<bottom-margin>85.7252</bottom-margin>",
                  "\t\t</page-margins>",
                  "\t\t<page-margins type=\"odd\">",
                  "\t\t\t<left-margin>85.7252</left-margin>",
                  "\t\t\t<right-margin>85.7252</right-margin>",
                  "\t\t\t<top-margin>85.7252</top-margin>",
                  "\t\t\t<bottom-margin>85.7252</bottom-margin>",
                  "\t\t</page-margins>",
            "\t</page-layout>",
            "\t<word-font font-family=\"Edwin\" font-size=\"10\"/>",
            "\t<lyric-font font-family=\"Edwin\" font-size=\"10\"/>",
            "\t</defaults>",
            "\t<part-list>",
              "\t\t<score-part id=\"P1\">",
                  "\t\t\t<part-name>Piano</part-name>",
                  "\t\t\t<part-abbreviation>Pno.</part-abbreviation>",
                  "\t\t\t<score-instrument id=\"P1-I1\">",
                  "\t\t\t\t<instrument-name>Piano</instrument-name>",
                  "\t\t\t</score-instrument>",
                  "\t\t\t<midi-device id=\"P1-I1\" port=\"1\"></midi-device>",
                  "\t\t\t<midi-instrument id=\"P1-I1\">",
                  "\t\t\t\t<midi-channel>1</midi-channel>",
                  "\t\t\t\t<midi-program>1</midi-program>",
                  "\t\t\t\t<volume>78.7402</volume>",
                  "\t\t\t\t<pan>0</pan>",
                  "\t\t\t</midi-instrument>",
              "\t\t</score-part>",
            "\t</part-list>",
            "\t<part id=\"P1\">",
            "\t<measure number=\"1\">",
            "\t<print>",
                  "\t\t<system-layout>",
                  "\t\t<system-margins>",
                        "\t\t\t<left-margin>65.90</left-margin>",
                        "\t\t\t<right-margin>641.43</right-margin>",
                        "\t\t\t</system-margins>",
                        "\t\t\t<top-system-distance>70.00</top-system-distance>",
                  "\t\t</system-layout>",
                  "\t\t<staff-layout number=\"2\">",
                  "\t\t<staff-distance>65.00</staff-distance>",
                  "\t\t</staff-layout>",
            "\t</print>",
            "\t<direction placement=\"above\">",
            "\t\t<direction-type>",
            "\t\t\t<metronome parentheses=\"no\" default-x=\"-30.68\" relative-y=\"20.00\">",
                  "\t\t\t<beat-unit>quarter</beat-unit>",
                  "\t\t\t<per-minute>80</per-minute>",
                  "\t\t\t</metronome>",
            "\t\t</direction-type>",
            "\t\t<sound tempo=\"90\"/>",
            "\t</direction>"]

      trailerCode = [
            "\t\t\t<barline location=\"right\">",
            "\t\t\t\t\t<bar-style>light-heavy</bar-style>",
            "\t\t\t</barline>",
            "\t\t</measure>",
            "\t</part>", 
            "</score-partwise>"]

  -- Write the resulting musicXML code to a .musicxml file
  let musicXMLCode     = unlines (headerCode ++ code ++ trailerCode)
  let musicXMLFileName = replaceExtension fileName ".musicxml"
  writeFile musicXMLFileName musicXMLCode
  putStrLn $ "Wrote file " ++ musicXMLFileName

