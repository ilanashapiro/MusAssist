{-|
Module       : X86gen
Description  : The x86 code emitter for the compiler -- part of the backend
Maintainer   : CS 132
-}

module ToMusicXML where

import qualified MusAssistAST    as MusAST
--import Control.Monad 

---------------------------------------------
-- Beats: to handle measures --
---------------------------------------------

type BeatCounter = Data.IORef.IORef Float
type MeasureCounter = Data.IORef.IORef Int32
type CodeLine = String    -- ^ A line of musicXML code

-- | Update where we're at in a measure and handle new measures
updateBeat :: BeatCounter -> MeasureCounter -> Float -> Float -> (BeatCounter, MeasureCounter, [CodeLine])
updateBeat beatCt measureCt incVal beatsPerMeasure = do
  beatCount <- Data.IORef.readIORef beatCt
  let updatedBeat = beatCount + incVal 
  if updatedBeat == beatsPerMeasure 
    then do
      measureNum <- Data.IORef.readIORef measureCt
      let newMeasureCode = ["\t\t</measure>", "\t\t<measure number=\"" ++ measureNum ++ " width=\"165.43\">"]
      Data.IORef.writeIORef beatCt 0.0                    -- reset beats to 0 bc we're in a new measure
      Data.IORef.writeIORef measureCt (measureNum + 1)    -- increment the measure count
      return (beatCt, measureCt, newMeasureCode)
    else 
      Data.IORef.writeIORef beatCt updatedBeat
      return (beatCt, measureCt, [])

-- | Map note names to keys
type KeyEnv = Map MusAST. Double

-----------------------------------------------------------------------------------------
-- Code Generation for Expressions
-----------------------------------------------------------------------------------------

-- | Returns machine code that puts the value of the given expression into the
--   designated temporary t0.
xNote :: Counter -> MusAST.Note -> IO [A.Instr]
------------------------------------------------
-- Constant values
------------------------------------------------
xNote ct t0 (NT.CONST i) = do
  return [A.OPER ("\tmovq $" ++ show i ++ ", `d0") [] [t0] Nothing]

------------------------------------------------
-- General temps
------------------------------------------------
xExpr _ t0 (NT.TEMP t) = do
  return [A.MOVE "\tmovq `s0, `d0" t t0]

------------------------------------------------
-- TODO: Named temps
------------------------------------------------
xExpr _ t0 (NT.NAME l) = do
  return [A.OPER ("\tleaq " ++ l ++ "(%rip), `d0") [] [t0] Nothing] 

------------------------------------------------
-- Dereference an 8-bit value
------------------------------------------------
xExpr ct t0 (NT.MEM NT.I8 e1) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  return $ code1 ++ [A.OPER "\tmovq (`s0), `d0" [t1] [t0] Nothing]

------------------------------------------------
-- Dereference a 64-bit value
------------------------------------------------
xExpr ct t0 (NT.MEM NT.I1 e1) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  return $
    code1
      ++ [ A.OPER "\tmovb (`s0), %cl" [t1] [rcx] Nothing,
           A.MOVE "\tmovsbq %cl, `d0" rcx t0
         ]

------------------------------------------------
-- leaq Optimizations
------------------------------------------------
-- | Transform e1 + e2 - i into  leaq -i(e1,e2,1), where i is an integer
xExpr ct t0 (NT.BINOP e1 NT.PLUS (NT.BINOP e2 NT.MINUS (NT.CONST i))) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  t2 <- freshTemp ct
  code2 <- xExpr ct t2 e2
  return $ code1 ++ code2 ++ [A.OPER ("\tleaq -" ++ show i ++ "(`s0, `s1, 1), `d0") [t1, t2] [t0] Nothing]

-- | Transform e1 + 8e2 into  leaq (e1,e2,8)
xExpr ct t0 (NT.BINOP e1 NT.PLUS (NT.BINOP (NT.CONST 8) NT.MUL e2)) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  t2 <- freshTemp ct
  code2 <- xExpr ct t2 e2
  return $ code1 ++ code2 ++ [A.OPER "\tleaq (`s0, `s1, 8), `d0" [t1, t2] [t0] Nothing]

------------------------------------------------
-- Binary logical operations
-- You are free to write separate xExpr cases for BINOP + LSHIFT, BINOP + RSHIFT,
--   etc., but I found it simpler to write one case that could handle shifts
------------------------------------------------
xExpr ct t0 (NT.BINOP e1 bop e2)
  | bop `elem` [NT.LSHIFT, NT.RSHIFT, NT.ARSHIFT] = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  code2 <- xExpr ct rcx e2
  let mnemonic = case bop of
        NT.LSHIFT  -> "shlq"
        NT.RSHIFT  -> "shrq"
        NT.ARSHIFT -> "sarq"
        _          -> undefined
  let moveCode2 = A.MOVE "\tmovq `s0, `d0" t1 t0
  let opCode =
        A.OPER ("\t" ++ mnemonic ++ " %cl, `s1") [rcx, t0] [t0] Nothing
  return $ code1 ++ code2 ++ [moveCode2, opCode]

------------------------------------------------
-- TODO: Binary arithmetic operations (except division)
-- You are free to write separate xExpr cases for BINOP + PLUS, BINOP + MINUS,
--   etc., but I found it simpler to write one case that could handle these all.
------------------------------------------------
xExpr ct t0 (NT.BINOP e1 bop e2)
  | bop `elem` [NT.PLUS, NT.MINUS, NT.MUL, NT.AND, NT.OR, NT.XOR] = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  t2 <- freshTemp ct
  code2 <- xExpr ct t2 e2
  let mnemonic = case bop of
        NT.PLUS  -> "addq"
        NT.MINUS -> "subq"
        NT.MUL   -> "imulq"
        NT.AND   -> "andq"
        NT.OR    -> "orq"
        NT.XOR   -> "xorq"
        _        -> undefined
  let moveCode2 = A.MOVE "\tmovq `s0, `d0" t1 t0 -- save result of left side of binop (i.e. what's in t1) to t0 so we are dealing with the correct register to return from
  let opCode =
        A.OPER ("\t" ++ mnemonic ++ " `s0, `d0") [t2, t0] [t0] Nothing
  return $ code1 ++ code2 ++ [moveCode2, opCode]

------------------------------------------------
-- TODO: division
-- Division needs more care than +/-/*,
-- because the rules about idivq needing to use %rdx and %rax
------------------------------------------------
xExpr ct t0 (NT.BINOP e1 NT.DIV e2) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  t2 <- freshTemp ct
  code2 <- xExpr ct t2 e2
  let moveCode = A.MOVE "\tmovq `s0, `d0" t1 rax -- save result of left side of binop (i.e. what's in t1) to rax 
  let extendCode = A.OPER "\tcqto" [] [] Nothing
  let divCode = A.OPER "\tidivq `s0" [t2] [] Nothing
  let setResultCode = A.MOVE "\tmovq `s0, `d0" rax t0 -- save the result of the division to t0
  return $ code1 ++ code2 ++ [moveCode, extendCode, divCode, setResultCode]

------------------------------------------------
-- Anything that remains untranslated prints a warning message If you think
-- you're done, this should probably be replaced by a call to error.
------------------------------------------------
xExpr _ _ e = do
  putStrLn ("   unknown xExpr " ++ show e)
  return []

-----------------------------------------------------------------------------------------
-- Code Generation for Statements
-----------------------------------------------------------------------------------------

-- | Translate a list of statements and concatenate the resulting instructions
xStmts :: Counter -> [NT.Stmt] -> IO [A.Instr]
xStmts ct stmts = do
  instrSeqs <- mapM (xStmt ct) stmts
  return $ concat instrSeqs

-- | Generate machine code for a single statement
xStmt :: Counter -> NT.Stmt -> IO [A.Instr]
------------------------------------------------
-- TODO: Assignment to a general temporary
------------------------------------------------
xStmt ct (NT.ASSIGN (NT.TEMP t) e) = xExpr ct t e



------------------------------------------------
-- Labels
------------------------------------------------
xStmt _ (NT.LABEL label) = do
  return [A.LABEL ("\n" ++ label ++ ":") label]

------------------------------------------------
-- Jump to label
------------------------------------------------
xStmt _ (NT.JUMP label) = do
  return [A.OPER "\tjmp `j0" [] [] (Just [label])]

------------------------------------------------
-- 64-bit store instruction
------------------------------------------------
-- Note that when writing e2 to address e1, both e1 are e2 are
-- read but not written, i.e., they are both sources and no
--  temporary is a destination!
------------------------------------------------
xStmt ct (NT.ASSIGN (NT.MEM NT.I8 e1) e2) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1 --- code to get e1 into some temporary t1
  t2 <- freshTemp ct
  code2 <- xExpr ct t2 e2 --- code to get e2 into some temporary t2
  return (code1 ++ code2 ++ [A.OPER "\tmovq `s1, (`s0)" [t1, t2] [] Nothing])

------------------------------------------------
-- 8-bit store instruction
------------------------------------------------
-- Store the low 8 bits of the 64-bit value e2 at address e1
------------------------------------------------
xStmt ct (NT.ASSIGN (NT.MEM NT.I1 e1) e2) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1 --- code to get e1 into some temporary t1
  code2 <- xExpr ct rcx e2 --- code to get e2 into rcx
  return (code1 ++ code2 ++ [A.OPER "\tmovb %cl, (`s0)" [t1, rcx] [] Nothing])

------------------------------------------------
-- TODO: Compare-and-jump
------------------------------------------------
xStmt ct (NT.CJUMP rop e1 e2 label1 label2) = do
  t1 <- freshTemp ct
  e1code <- xExpr ct t1 e1
  t2 <- freshTemp ct
  e2code <- xExpr ct t2 e2
  let mnemonic = case rop of
        NT.EQUAL      -> "je"
        NT.NEQUAL     -> "jne"
        NT.LESS       -> "jl"
        NT.GREATER    -> "jg"
        NT.LESSEQ     -> "jle"
        NT.GREATEREQ  -> "jge"
        NT.ULESS      -> "jb"
        NT.UGREATER   -> "ja"
        NT.ULESSEQ    -> "jbe"
        NT.UGREATEREQ -> "jae"
  let cmpCode = [A.OPER "\tcmpq `s0, `s1" [t2, t1] [] Nothing]
      jumpCode = [A.OPER ("\t" ++ mnemonic ++ " `j0") [] [] (Just [label1,label2])]
  return $ e2code ++ e1code ++ cmpCode ++ jumpCode

------------------------------------------------
-- TODO: Calls to a named function
------------------------------------------------
-- You could live without this case, at least at first,
-- and just always use the more general next case (making
-- every call an indirect call)
xStmt ct (NT.CALL maybeTemp (NT.NAME functionName) args) = do
  -- XXX: We assume there are no more than 6 arguments!
  -- Find fresh temporaries to hold each argument
  argTemps <- mapM (\_ -> freshTemp ct) args
  argsToTempsCode <- zipWithM (\arg t -> xExpr ct t arg) args argTemps
  let argsToRegsCode = zipWith (\t reg -> A.MOVE "\tmovq `s0, `d0" t reg) argTemps argumentRegisters
      usedArgRegs = take (length args) argumentRegisters
      endCode = case maybeTemp of
        Nothing    -> []
        Just temp  -> [A.MOVE "\tmovq `s0, `d0" rax temp]
  return (concat argsToTempsCode ++ argsToRegsCode ++ [A.OPER ("\tcallq " ++ functionName) usedArgRegs callerSaveRegisters Nothing] ++ endCode)

------------------------------------------------
-- TODO: Calls
------------------------------------------------
xStmt ct (NT.CALL maybeTemp e args) = do
  t1 <- freshTemp ct
  tmpCode <- xExpr ct t1 e 
  argTemps <- mapM (\_ -> freshTemp ct) args
  argsToTempsCode <- zipWithM (\arg t -> xExpr ct t arg) args argTemps
  let argsToRegsCode = zipWith (\t reg -> A.MOVE "\tmovq `s0, `d0" t reg) argTemps argumentRegisters
      usedArgRegs = take (length args) argumentRegisters
      endCode = case maybeTemp of
        Nothing    -> []
        Just temp  -> [A.MOVE "\tmovq `s0, `d0" rax temp]
  return (tmpCode ++ concat argsToTempsCode ++ argsToRegsCode ++ [A.OPER "\tcallq *`s0" (t1:usedArgRegs) callerSaveRegisters Nothing] ++ endCode)

------------------------------------------------
-- Returns
------------------------------------------------
xStmt _ (NT.RETURN Nothing) = return [A.RETURN "\tretq" []]
xStmt ct (NT.RETURN (Just e)) = do
  eCode <- xExpr ct rax e
  return $ eCode ++ [A.RETURN "\tretq" [rax]]

------------------------------------------------
-- Anything that remains untranslated prints a warning message
-- When you think you're done, this should probably be replaced
-- by a call to error.
------------------------------------------------
xStmt _ stmt = do
  putStrLn ("  unknown xStmt " ++ show stmt)
  return []

-----------------------------------------------------------------------------------------
-- Code Generation for Fragments
-----------------------------------------------------------------------------------------

-- | Turns an NT Fragment into the equivalent A.Fragment by just doing
--   instruction-selection on the code parts
xFrag :: Counter -> NT.Fragment -> IO A.Fragment
xFrag ct (NT.FragCode l stmts) = do
  instrs <- xStmts ct stmts
  return $ A.FragCode l instrs
xFrag _ (NT.FragInts l bytes ns) = return $ A.FragInts l bytes ns
xFrag _ (NT.FragLabels l ls) = return $ A.FragLabels l ls

xFrags :: Counter -> [NT.Fragment] -> IO [A.Fragment]
xFrags ct = mapM (xFrag ct)
