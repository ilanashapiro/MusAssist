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
expandIntermediateExpr :: MusAST.IntermediateExpr -> IO MusAST.Expr

-- | Notes get expanded to become single-element chords
expandIntermediateExpr (MusAST.Note tone duration) = return $ Write ??? MusAST.Chord [tone] duration

-- | Predefined chords: these all happen in root position
expandIntermediateExpr (MusAST.ChordTemplate tone quality chordType inversion duration) = undefined

-- | Quality is major/minor ONLY. determines the start note and key of the cadence
expandIntermediateExpr (MusAST.Cadence cadenceType tone quality) = undefined

-- | Quality is major/minor ONLY. determines the start note and key of the harmseq
expandIntermediateExpr (MusAST.HarmonicSequence harmSeqType tone quality duration length) = undefined

expandIntermediateExpr (MusAST.FinalExpr expr) = return expr

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstr :: MusAST.IntermediateInstr -> IO MusAST.Instr

-- | Quality is major/minor ONLY
expandIntermediateInstr (MusAST.SetKeySignature noteName quality) = undefined

expandIntermediateInstr MusAST.CreateNewMeasure = return MusAST.NewMeasure

expandIntermediateInstr [MusAST.IRWrite IRExprs] = do
    exprs <- mapM expandIntermediateExpr IRExprs
    return $ MusAST.Write exprs

-----------------------------------------------------------------------------------------
-- Expand All the Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstrs :: [MusAST.IntermediateInstr] -> IO [MusAST.Instr]
expandIntermediateInstrs intermediateInstrs = mapM expandIntermediateExpr intermediateInstrs