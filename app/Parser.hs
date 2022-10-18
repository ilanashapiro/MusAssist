module Parser (parse, parseFile, parseNamed, program) where

import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as Token
import qualified Data.Map as Map 
import           Data.Char as Char
import           Control.Monad.Extra
import           Data.List          (dropWhileEnd)

import MusAssistAST

lowerCaseToSentenceCase :: String -> String
lowerCaseToSentenceCase s = (Char.toUpper $ head s) : tail s

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
  <|> parens 
    (try parseChordTemplate -- overlapping prefixes means we need to use "try"
      <|> try parseNote
      <|> try parseCadence
      <|> parseFinalExpr
      <|> parseHarmSeq)
    <?> "Expected expression"

parseChordTemplate :: Parsec String () IntermediateExpr
parseChordTemplate = ChordTemplate <$> parseTone <*> parseQuality <*> parseChordType <*> return ClosedChord <*> parseInversion <*> parseDuration <* spaces 

parseArpeggio :: Parsec String () IntermediateExpr
parseArpeggio = undefined 

parseScale :: Parsec String () IntermediateExpr
parseScale = undefined 

parseCadence :: Parsec String () IntermediateExpr
parseCadence = Cadence <$> parseCadenceType <*> parseTone <*> parseQuality <*> parseDuration <* spaces 

parseHarmSeq :: Parsec String () IntermediateExpr
parseHarmSeq = HarmonicSequence <$> parseHarmSeqType <*> parseTone <*> parseQuality <*> parseDuration <*> (string "length:" *> natural >>=: fromIntegral) <* spaces

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
  tones    <- brackets (commaSep1 parseTone <?> "A user-defined chord must have at least one note")
  duration <- parseDuration
  return $ Chord tones duration

parseTone :: Parsec String () Tone 
parseTone = Tone <$> parseNoteName <*> parseAccidental <*> (natural >>=: \octave -> fromIntegral octave)

parseDuration :: Parsec String () Duration
parseDuration = do
  let parseNonDotted = choice $ map (try . symbol) ["sixteenth", "eighth", "quarter", "half", "whole"]
      parseDotted    = symbol "dotted_" *> parseNonDotted
  durationStr <- (parseNonDotted >>=: lowerCaseToSentenceCase) 
                 <|> (parseDotted >>=: ((++) "Dotted") . lowerCaseToSentenceCase)
  let ast = read durationStr :: Duration
  return ast

parseNoteName :: Parsec String () NoteName
parseNoteName = do
  noteName <- choice $ map (try . string) ["F", "C", "G", "D", "A", "E", "B"] -- we do NOT want to remove trailing space
  let ast = read noteName :: NoteName
  return ast

parseAccidental :: Parsec String () Accidental
parseAccidental = do 
  let parseAlteredAccidental = try (symbol "##" >>: DoubleSharp) 
                              <|> try (symbol "#" >>: Sharp)
                              <|> try (symbol "bb" >>: DoubleFlat) 
                              <|> try (symbol "b" >>: Flat)
  ast <- option Natural $ parseAlteredAccidental
  return ast
    
parseQuality :: Parsec String () Quality 
parseQuality = 
  try (symbol "maj"      >>: Major) -- have to "try" here bc "m" is prefix of both maj and min
  <|> (symbol "min"      >>: Minor)
  <|> (symbol "aug"      >>: Augmented)
  <|> (symbol "dim"      >>: Diminished)
  <|> (symbol "halfdim"  >>: HalfDiminished)
  <?> "expected quality"

parseInversion :: Parsec String () Inversion
parseInversion =  do
  inversionStr <- string "inv:" *> choice (map (try . symbol) ["root", "first", "second", "third"]) >>=: lowerCaseToSentenceCase
  let ast = read inversionStr :: Inversion
  return ast

parseChordType :: Parsec String () ChordType
parseChordType = do
  chordTypeStr <- choice (map (try . symbol) ["triad", "seventh"]) >>=: lowerCaseToSentenceCase
  let ast = read chordTypeStr :: ChordType
  return ast

parseCadenceType :: Parsec String () CadenceType
parseCadenceType = do
  cadenceTypeStr <-  (choice (map (try . symbol) ["PerfAuth", "ImperfAuth", "Plagal", "Deceptive"]) <* symbol "Cadence") 
                      <|> (symbol "HalfCadence" >>: "HalfCad")
  let ast = read cadenceTypeStr :: CadenceType
  return ast

parseHarmSeqType :: Parsec String () HarmonicSequenceType
parseHarmSeqType = do
  harmSeqTypeStr <- choice (map (try . symbol) ["AscFifths", "DescFifths", "Asc56", "Desc56"])
  let ast = read harmSeqTypeStr :: HarmonicSequenceType
  return ast

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
commaSep1         = Token.commaSep1 lexer
identifier        = Token.identifier lexer
whiteSpace        = Token.whiteSpace lexer

infixl 1 >>=:
left >>=: f = f <$> left

infixl 1 >>:
left >>: v = left >> return v

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