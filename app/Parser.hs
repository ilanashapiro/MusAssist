module Parser (parse, parseFile, parseNamed, program) where

import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as Token
import qualified Data.Map as Map 
import           Data.Char as Char

import MusAssistAST

lowerCaseToSentenceCase :: String -> String
lowerCaseToSentenceCase s = (Char.toUpper $ head s) : tail s

--------------------------------------------------------------------------------
-- * Parsing functions
--------------------------------------------------------------------------------

-- | Parse a MusAssist program from a file
parseFile :: FilePath -> IO [IntermediateExpr]
parseFile filename = readFile filename >>= parseNamed filename

-- | Parse a MusAssist program, given a name for the source of the program and a string that
--   contains the contents of the programs
parseNamed :: String -> String -> IO [IntermediateExpr]
parseNamed name contents = 
  case parse program name contents of
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


instr :: Parsec String () IntermediateExpr
instr = try (parens parseNote)
--          <|> assign
--          <|> ifStatement
--          <|> (keyword "input" *> identifier >>=: Input)   -- input x
--          <|> (keyword "print" *> identifier >>=: Print)   -- print x
--          <|> (keyword "goto"  *> lineNumber >>=: Goto)    -- goto l
--          <|> (keyword "skip"                >>:  Skip)    -- skip
--          <|> (keyword "halt"                >>:  Halt)    -- halt
         <?> "expected instruction"


parseDuration :: Parsec String () Duration
parseDuration = do
  let parseNonDotted = choice $ map (try . string) ["sixteenth", "eighth", "quarter", "half", "whole"]
      parseDotted    = string "dotted_" *> parseNonDotted
  durationStr <- (parseNonDotted >>=: lowerCaseToSentenceCase) 
                 <|> (parseDotted >>=: ((++) "Dotted") . lowerCaseToSentenceCase)
  let ast = read durationStr :: Duration
  return ast



parseRest :: Parsec String () Expr 
parseRest = string "rest" *> parseDuration >>=: Rest

parseNote :: Parsec String () IntermediateExpr 
parseNote = Note <$> parseTone <*> parseDuration <* spaces

parens :: Parsec String () a -> Parsec String () a -- replace a with IntermediateExpression
parens = between (symbol "(") (symbol ")")

parseNoteName :: Parsec String () NoteName
parseNoteName = do
  noteName <- choice $ map (try . string) ["F", "C", "G", "D", "A", "E", "B"]
  let ast = read noteName :: NoteName
  return ast

parseAccidental :: Parsec String () Accidental
parseAccidental = do 
  let parseAlteredAccidental = (string "##" >>: DoubleSharp) 
                              <|> (string "#" >>: Sharp)
                              <|> (string "bb" >>: DoubleFlat) 
                              <|> (string "b" >>: Flat)
  ast <- option Natural $ parseAlteredAccidental
  return ast
    
parseTone :: Parsec String () Tone 
parseTone = do 
  noteName   <- parseNoteName
  accidental <- parseAccidental
  octave     <-  natural
  let ast = Tone noteName accidental (fromIntegral octave)
  return ast
    
parseQuality :: Parsec String () Quality 
parseQuality = try (string "maj" >>: Major)
          <|> (string "min"      >>: Minor)
          <|> (string "aug"      >>: Augmented)
          <|> (string "dim"      >>: Diminished)
          <|> (string "halfdim"  >>: HalfDiminished)
         <?> "expected quality"

parseInversion :: Parsec String () Inversion
parseInversion = do
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

--------------------------------------------------------------------------------
-- * Programs
--------------------------------------------------------------------------------

-- | p ∈ Programs ::= [s1, s2, ..., sn]
program :: Parsec String () [IntermediateExpr]
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