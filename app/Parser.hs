module Parser (parse, parseFile, parseNamed, program) where

import Text.Parsec
import Text.Parsec.Char
import Debug.Trace
import Data.Functor.Identity
import qualified Text.Parsec.Token as Token
import qualified Data.Map as Map 
import           Data.Char as Char
import           Control.Monad.Extra
import           Data.List          (dropWhileEnd)




import MusAssistAST

--------------------------------------------------------------------------------
-- * Helper Functions
--------------------------------------------------------------------------------
lowerCaseToSentenceCase :: String -> String
lowerCaseToSentenceCase s = Char.toUpper (head s) : tail s

-- CREDIT for println and seeNext: https://www.reddit.com/r/haskelltil/comments/3el2d6/a_handy_function_for_debugging_in_parsec_by_what/
println msg = trace (show msg) $ return ()

-- peek next n chars in the parsing stream
seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

--------------------------------------------------------------------------------
-- * Parsing functions
--------------------------------------------------------------------------------

-- | Parse a MusAssist program from a file
parseFile :: FilePath -> IO [IntermediateInstr]
parseFile fileName = do 
  fileContentsStr <- readFile fileName 
  let fileLines = lines (strip fileContentsStr)
  parseNamed fileName fileLines

-- | Parse a MusAssist program, given a name for the source of the program and a string that
--   contains the contents of the programs
parseNamed :: String -> [String] -> IO [IntermediateInstr]
parseNamed name contents = 
  case concatMapM (parse program name) contents of
    Right ast -> return ast
    Left err  -> errorWithoutStackTrace (show err)

--------------------------------------------------------------------------------
-- * Intermediate Instructions
--------------------------------------------------------------------------------

-- | i ∈ IntermediateInstructions ::= SET_KEY 
                                  -- |  NEW_MEASURE
                                  -- |  .......................

instr :: Parsec String () IntermediateInstr
instr =  parseKeySig
         <|> try parseAssign
         <|> (many1 parseExpr >>=: IRWrite) -- write musical expressions
         <|> (keyword "NEW_MEASURE" >>: IRNewMeasure) -- new measure
         <* eof
         <?> "expected instruction"
         
-- | labelName = musical expression
parseAssign :: Parsec String () IntermediateInstr
parseAssign = do 
  label <- identifier
  op "="
  expr <- many1 parseExpr <?> "A label must refer to at least one expression"
  let ast = IRAssign label expr
  return ast

-- | SET_KEY keyword, followed by note name, accidental, quality
parseKeySig :: Parsec String () IntermediateInstr
parseKeySig = do 
  keyword "SET_KEY"
  noteName   <- parseNoteName
  accidental <- parseAccidental -- the maj/min only constriction on key sig gets checked during the IRConversion phase currently
  quality    <- parseQuality
  let ast = IRKeySignature noteName accidental quality
  return ast

parseExpr :: Parsec String () IntermediateExpr
parseExpr = 
-- we HAVE to do parseChordTemplate before parseNote, because parseChordTemplate parses quality "halfdim" first and parseNote parses duration "half" first
-- since "half" is a prefix of "halfdim", this means that the parse will fail if we do parseNote before parseChordTemplate
-- also it's important to do parseLabel first, because we HAVE to have parens after this 
  try parseLabel
  <|> parens parseScale
    -- (try parseChordTemplate -- overlapping prefixes means we need to use "try"
    --   <|> try parseNote
    --   <|> try parseCadence
    --   <|> try parseScale
    --   <|> parseFinalExpr
    --   <|> parseHarmSeq)
    <?> "Expected expression"

parseChordTemplate :: Parsec String () IntermediateExpr
parseChordTemplate = 
    ChordTemplate <$> parseTone <*> parseQuality <*> parseChordTypeBeforeForm <*> parseChordFormAfterType <* comma 
                                <*> parseInversion <* symbol "inversion" <* comma <*> parseDuration <* spaces 

parseScale :: Parsec String () IntermediateExpr
parseScale = Scale <$> parseScaleNoteName <*> parseScaleAccidental <*> parseScaleType 
                                          <*> parseDirection <* symbol "scale" <* comma <*> parseStartNote <* comma
                                          <*> parseDuration <* comma <*> parseLength <* spaces

parseCadence :: Parsec String () IntermediateExpr
parseCadence = Cadence <$> parseCadenceType <* comma <*> parseTone <*> parseQuality <* comma <*> parseDuration <* spaces 

parseHarmSeq :: Parsec String () IntermediateExpr
parseHarmSeq = HarmonicSequence <$> parseHarmSeqType <* comma <*> parseTone <*> parseQuality <* comma <*> parseDuration <* comma <*> parseLength <* spaces

parseNote :: Parsec String () IntermediateExpr 
parseNote = Note <$> parseTone <*> parseDuration <* spaces

parseLabel :: Parsec String () IntermediateExpr
parseLabel = identifier >>=: Label

parseFinalExpr :: Parsec String () IntermediateExpr
parseFinalExpr = FinalExpr <$> (parseRest <|> parseChord)

parseRest :: Parsec String () Expr 
parseRest = do 
  symbol "rest"
  duration <- parseDuration
  let ast = Rest duration
  return ast

parseChord :: Parsec String () Expr
parseChord = do
  tones <- brackets (commaSep1 parseTone <?> "A user-defined chord must have at least one note")
  Chord tones <$> parseDuration

parseTone :: Parsec String () Tone 
parseTone = Tone <$> parseNoteName <*> parseAccidental <*> (natural >>=: \octave -> fromIntegral octave)
  
parseStartNote :: Parsec String () Tone 
parseStartNote = symbol "startNote" >> symbol "=" >> parseTone

parseDuration :: Parsec String () Duration
parseDuration = do
  let parseNonDotted = choice $ map (try . symbol) ["sixteenth", "eighth", "quarter", "half", "whole"]
      parseDotted    = symbol "dotted_" *> parseNonDotted
  durationStr <- (parseNonDotted >>=: lowerCaseToSentenceCase) 
             <|> (parseDotted    >>=: (++) "Dotted" . lowerCaseToSentenceCase)
  let ast = read durationStr :: Duration
  return ast

parseLength :: Parsec String () Int 
parseLength = symbol "length" >> symbol "=" >> natural >>=: fromIntegral

parseDirection :: Parsec String () Direction 
parseDirection = (symbol "ascending"  >>: Ascending) 
             <|> (symbol "descending" >>: Descending)
             <?> "expected direction"

parseScaleType :: Parsec String () ScaleType 
parseScaleType = do
  try (symbol "major"          >>: MajorScale) -- have to "try" here bc "m" is prefix of major, minor, and melodic
  <|> try (symbol "minor"      >>: NaturalMinor)
  <|> (symbol "melodic minor"  >>: MelodicMinor)
  <|> (symbol "harmonic minor" >>: HarmonicMinor)
  <|> (symbol "Chromatic"      >>: Chromatic) -- Chromatic is sentence case in concrete syntax
  <|> (symbol "Whole tone"    >>: WholeTone) -- Whole Tone is sentence case in concrete syntax
  <?> "expected scale type"

lookAheadNonDiatonicScaleType :: ParsecT String () Identity String
lookAheadNonDiatonicScaleType = try $ choice (map (lookAhead . symbol) ["Chromatic", "Whole tone"]) -- have to use "try", or else if start note is C, this will fail on "Chromatic" but still consume the C

parseScaleNoteName :: Parsec String () NoteName 
parseScaleNoteName = do
    try (lookAheadNonDiatonicScaleType >>: C) -- use C as placeholder tonic note name for scales that have no tonic (currently: chromatic, whole tone)
                -- have to use try bc note name "C" and nondiatonic type "Chromatic" share prefix
                -- need to parse note name after nondiatonic scale type bc "Chromatic" is longer than "C", otherwise the parser can succeed incorrectly on prefix "C"
                 <|> try parseNoteName 

parseScaleAccidental :: Parsec String () Accidental 
parseScaleAccidental = do
    try (lookAheadNonDiatonicScaleType >>: Natural) -- use C as placeholder tonic note name for scales that have no tonic (currently: chromatic, whole tone)
                   <|> parseAccidental -- have to parse accidental second since this parse always succeeds (i.e. if there's no valid acc found, it defaults to natural)
                 
parseNoteName :: Parsec String () NoteName
parseNoteName = do
  noteName <- choice $ map (try . string) ["F", "C", "G", "D", "A", "E", "B"] -- we do NOT want to remove trailing space
  let ast = read noteName :: NoteName
  return ast

parseAccidental :: Parsec String () Accidental
parseAccidental = do 
  let parseAccidental = try (string "##" >>: DoubleSharp) 
                              <|> (string "#" >>: Sharp)
                              <|> try (string "bb" >>: DoubleFlat) 
                              <|> (string "b" >>: Flat)
  ast <- option Natural parseAccidental
  spaces >>: ast
    
parseQuality :: Parsec String () Quality 
parseQuality = 
  try (symbol "major"           >>: Major) -- have to "try" here bc "m" is prefix of both maj and min
  <|> (symbol "minor"           >>: Minor)
  <|> (symbol "augmented"       >>: Augmented)
  <|> (symbol "diminished"      >>: Diminished)
  <|> (symbol "half diminished" >>: HalfDiminished)
  <?> "expected quality"

parseInversion :: Parsec String () Inversion
parseInversion =  do
  inversionStr <- choice (map (try . symbol) ["root", "first", "second", "third"]) >>=: lowerCaseToSentenceCase
  let ast = read inversionStr :: Inversion
  return ast

parseChordTypeBeforeForm :: Parsec String () ChordType
parseChordTypeBeforeForm = do 
        (lookAhead $ symbol "triad" >>: Triad)
    <|> (lookAhead $ symbol "seventh" >>: choice (map (lookAhead . symbol) ["chord", "arpeggio"]) >>: Seventh)
    <|> (lookAhead $ symbol "arpeggio" >>: Triad)
    <?> "expecting chord type before form"

parseChordFormAfterType :: Parsec String () ChordForm
parseChordFormAfterType = do 
        (symbol "triad" >>: ClosedChord)
    <|> (symbol "seventh" >> ((symbol "chord" >>: ClosedChord) <|> (symbol "arpeggio" >>: Arpeggio))) -- >> doesn't lift to monad (otherwise we lift twice bc <<: lifts)
    <|> (symbol "arpeggio" >>: Arpeggio)
    <?> "expecting chord form after type"

parseCadenceType :: Parsec String () CadenceType
parseCadenceType = do
  cadenceTypeStr <-  (choice (map (try . symbol) ["Plagal", "Deceptive"]) <* symbol "Cadence")
                      <|> (symbol "Perfect Authentic Cadence" >>: "PerfAuth")
                      <|> (symbol "Imperfect Authentic Cadence" >>: "ImperfAuth")
                      <|> (symbol "Half Cadence" >>: "HalfCad")
  let ast = read cadenceTypeStr :: CadenceType
  return ast

parseHarmSeqType :: Parsec String () HarmonicSequenceType
parseHarmSeqType = 
    let parseSeqType = 
            (symbol "Ascending Fifths" >>: AscFifths) 
                <|> (symbol "Descending Fifths" >>: DescFifths)
                <|> (symbol "Ascending 5-6" >>: Asc56)
                <|> (symbol "Descending 5-6" >>: Desc56)
                <?> "expected harmonic sequence type"
    in parseSeqType <* symbol "Sequence"

-- betweenDelimiters :: String -> String -> Parsec String () a -> Parsec String () a -- replace a with IntermediateExpression
-- betweenDelimiters open close = between (symbol open) (symbol close)
 
--------------------------------------------------------------------------------
-- * Programs
--------------------------------------------------------------------------------

-- | p ∈ Programs ::= [s1, s2, ..., sn]
program :: Parsec String () [IntermediateInstr]
program = do 
  whiteSpace -- do i need this???
  instrs <- many instr
  eof
  return instrs

-----------------------------------------------------------------------------------------
-- Convenience parsers
-----------------------------------------------------------------------------------------
keyword           = Token.reserved lexer
op                = Token.reservedOp lexer
natural           = Token.natural lexer
symbol            = Token.symbol lexer
parens            = Token.parens lexer
brackets          = Token.brackets lexer
comma             = Token.comma lexer
commaSep1         = Token.commaSep1 lexer
identifier        = Token.identifier lexer
whiteSpace        = Token.whiteSpace lexer

infixl 1 >>=:
left >>=: f = f <$> left -- flips order of args in fmap

infixl 1 >>:
left >>: v = left >> return v -- sequentially compose two operations: left (a monad) and v (not a monad). result: v gets lifted to a monad

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------
langDef :: Token.LanguageDef ()
langDef = Token.LanguageDef
  { Token.commentStart    = ""
  , Token.commentEnd      = ""
  , Token.commentLine     = "//"
  , Token.nestedComments  = False
  , Token.identStart      = letter -- identifier must start with a letter
  , Token.identLetter     = alphaNum <|> oneOf "_'" -- identifier can end with alphanum, or with _ or with '
  , Token.opStart         = oneOf "=" 
  , Token.opLetter        = oneOf "="
  , Token.reservedNames   = ["SET_KEY", "NEW_MEASURE"]
  , Token.reservedOpNames = ["="]
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef

-- Helper function to remove whitespace at the beginning and end of a string
strip :: String -> String
strip = dropWhileEnd Char.isSpace . dropWhile Char.isSpace