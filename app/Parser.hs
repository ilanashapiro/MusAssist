module Parser (parse, parseFile, parseNamed, program) where

import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as Token
import qualified Data.Map as Map 
import           Data.Char as Char
import           Control.Monad.Extra

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
  let fileLines = lines fileContentsStr
  parseNamed fileName fileLines

-- | Parse a MusAssist program, given a name for the source of the program and a string that
--   contains the contents of the programs
parseNamed :: String -> [String] -> IO [IntermediateInstr]
parseNamed name contents = 
  case concatMapM (parse program name) contents of
    Right ast -> return ast
    Left err  -> errorWithoutStackTrace (show err)

-----------------------------------------------------------------------------------------
-- Basic domains: variables, line numbers, and values 
-----------------------------------------------------------------------------------------
identifier = Token.identifier lexer     -- ^  x ∈ Variables
lineNumber = Token.natural lexer        -- ^  l ∈ Line numbers 
number     = Token.integer lexer        -- ^  n ∈ ℤ


--------------------------------------------------------------------------------
-- * Intermediate Instructions
--------------------------------------------------------------------------------

-- | i ∈ IntermediateInstructions ::= SET_KEY 
                                  -- |  NEW_MEASURE
                                  -- |  .......................

instr :: Parsec String () IntermediateInstr
instr = try (parseKeySig
         <|> parseAssign
         <|> (many1 parseExpr >>=: IRWrite)) -- write musical expressions
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
  accidental <- parseAccidental
  quality    <- parseQuality
  let ast = IRKeySignature noteName accidental quality
  return ast

parseExpr :: Parsec String () IntermediateExpr
parseExpr = try ( 
  parens (parseNote 
    <|> parseChordTemplate
    <|> parseCadence
    <|> parseHarmSeq))
  <|> parseFinalExpr
  <?> "Expected expression"

parseChordTemplate :: Parsec String () IntermediateExpr
parseChordTemplate = ChordTemplate <$> parseTone <*> parseQuality <*> parseChordType <*> parseInversion <*> parseDuration <* spaces -- CAN I REMOVE SPACES????

parseCadence :: Parsec String () IntermediateExpr
parseCadence = Cadence <$> parseCadenceType <*> parseTone <*> parseQuality <*> parseDuration <* spaces 

parseHarmSeq :: Parsec String () IntermediateExpr
parseHarmSeq = HarmonicSequence <$> parseHarmSeqType <*> parseTone <*> parseQuality <*> parseDuration <*> (natural >>=: fromIntegral) <* spaces

parseNote :: Parsec String () IntermediateExpr 
parseNote = Note <$> parseTone <*> parseDuration <* spaces

parseFinalExpr :: Parsec String () IntermediateExpr
parseFinalExpr = FinalExpr <$> (try parseLabel <|> parens (parseRest <|> parseChord))

parseRest :: Parsec String () Expr 
parseRest = do 
  string "rest" 
  spaces
  duration <- parseDuration
  spaces
  let ast = Rest duration
  return ast

parseChord :: Parsec String () Expr
parseChord = do
  tones <- brackets (commaSep1 parseTone <?> "A user-defined chord must have at least one note")
  duration <- parseDuration
  return $ Chord tones duration

parseLabel :: Parsec String () Expr
parseLabel = identifier >>=: Label

parseTone :: Parsec String () Tone 
parseTone = do 
  noteName   <- parseNoteName
  accidental <- parseAccidental
  octave     <-  natural
  let ast = Tone noteName accidental (fromIntegral octave)
  return ast

parseDuration :: Parsec String () Duration
parseDuration = do
  let parseNonDotted = choice $ map (try . string) ["sixteenth", "eighth", "quarter", "half", "whole"]
      parseDotted    = string "dotted_" *> parseNonDotted
  durationStr <- (parseNonDotted >>=: lowerCaseToSentenceCase) 
                 <|> (parseDotted >>=: ((++) "Dotted") . lowerCaseToSentenceCase)
  let ast = read durationStr :: Duration
  return ast

parseNoteName :: Parsec String () NoteName
parseNoteName = do
  noteName <- choice $ map (try . string) ["F", "C", "G", "D", "A", "E", "B"]
  let ast = read noteName :: NoteName
  return ast

parseAccidental :: Parsec String () Accidental
parseAccidental = do 
  let parseAlteredAccidental = try (string "##" >>: DoubleSharp) 
                              <|> try (string "#" >>: Sharp)
                              <|> try (string "bb" >>: DoubleFlat) 
                              <|> try (string "b" >>: Flat)
  ast <- option Natural $ parseAlteredAccidental
  return ast
    
parseQuality :: Parsec String () Quality 
parseQuality = try (string "maj" >>: Major)
          <|> (string "min"      >>: Minor)
          <|> (string "aug"      >>: Augmented)
          <|> (string "dim"      >>: Diminished)
          <|> (string "halfdim"  >>: HalfDiminished)
         <?> "expected quality"

parseInversion :: Parsec String () Inversion
parseInversion =  do
  inversionStr <- string "inv:" *> (choice $ map (try . string) ["root", "first", "second", "third"]) >>=: lowerCaseToSentenceCase
  let ast = read inversionStr :: Inversion
  return ast

parseChordType :: Parsec String () ChordType
parseChordType = do
  chordTypeStr <- (choice $ map (try . string) ["triad", "seventh"]) >>=: lowerCaseToSentenceCase
  let ast = read chordTypeStr :: ChordType
  return ast

parseCadenceType :: Parsec String () CadenceType
parseCadenceType = do
  cadenceTypeStr <-  ((choice $ map (try . string) ["PerfAuth", "ImperfAuth, Plagal, Deceptive"]) <* string "Cadence") 
                      <|> (string "Half" >>=: flip (++) "Cad")
  let ast = read cadenceTypeStr :: CadenceType
  return ast

parseHarmSeqType :: Parsec String () HarmonicSequenceType
parseHarmSeqType = do
  harmSeqTypeStr <- (choice $ map (try . string) ["AscFifths", "DescFifths", "Asc56", "Desc56"])
  let ast = read harmSeqTypeStr :: HarmonicSequenceType
  return ast

-- betweenDelimiters :: String -> String -> Parsec String () a -> Parsec String () a -- replace a with IntermediateExpression
-- betweenDelimiters open close = between (symbol open) (symbol close)

--------------------------------------------------------------------------------
-- * Programs
--------------------------------------------------------------------------------

-- | p ∈ Programs ::= [s1, s2, ..., sn]
program :: Parsec String () [IntermediateInstr]
program = do instrs <- many instr
             eof
             return instrs

-----------------------------------------------------------------------------------------
-- Convenience parsers
-----------------------------------------------------------------------------------------
keyword = Token.reserved lexer
op      = Token.reservedOp lexer
natural    = Token.natural lexer
symbol    = Token.symbol lexer
parens        = Token.parens lexer
brackets      = Token.brackets lexer
commaSep1      = Token.commaSep1 lexer

infixl 1 >>=:
left >>=: f = f <$> left

infixl 1 >>:
left >>: v = left >> return v

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------
langDef :: Token.LanguageDef ()
langDef = Token.LanguageDef
  { Token.commentStart    = "//*"
  , Token.commentEnd      = "*//"
  , Token.commentLine     = "//"
  , Token.nestedComments  = True
  , Token.identStart      = letter -- identifier must start with a letter
  , Token.identLetter     = alphaNum <|> oneOf "_'12345678" -- identifier can end with alphanum, or with _ or with ',  or with 1-8
  , Token.opStart         = oneOf "=" 
  , Token.opLetter        = oneOf "="
  , Token.reservedNames   = ["SET_KEY", "NEW_MEASURE"]
  , Token.reservedOpNames = ["="]
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef