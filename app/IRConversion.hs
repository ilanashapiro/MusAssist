{-|
Module       : IRConversion
Description  : The intermediate representation for the compiler -- part of the middle. Expands chord/cadence/harmseq templates, etc
Maintainer   : Ilana Shapiro
-}

module IRConversion where

import qualified MusAssistAST         as MusAST

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Expressions
-----------------------------------------------------------------------------------------
expandIntermediateExpr :: MusAST.IntermediateExpr -> IO MusAST.Instr

expandIntermediateExpr (MusAST.FinalInstr instr) = return instr

expandIntermediateExpr (MusAST.Note tone duration) = return $ Write ??? MusAST.Chord [tone] duration

-- | Predefined chords: these all happen in root position
expandIntermediateExpr (MusAST.ChordTemplate tone quality chordType inversion duration) = undefined

-- | Quality is major/minor ONLY. det the start note and key of the cadence
expandIntermediateExpr (MusAST.Cadence cadenceType tone quality) = undefined

-- | Quality is major/minor ONLY. det the start note and key of the sequence
expandIntermediateExpr (MusAST.HarmonicSequence harmSeqType tone quality duration length) = undefined

expandIntermediateExpr (MusAST.SetKeySignature noteName quality) = undefined

-----------------------------------------------------------------------------------------
-- Expand All the Intermediate Expressions
-----------------------------------------------------------------------------------------
expandIntermediateExprs :: [MusAST.IntermediateExpr] -> IO [MusAST.Instr]
expandIntermediateExprs intermediateExprs = mapM expandIntermediateExpr intermediateExprs
