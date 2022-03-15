{-|
Module       : IRConversion
Description  : The intermediate representation for the compiler -- part of the middle. Expands chord/cadence/harmseq templates, etc
Maintainer   : Ilana Shapiro
-}

module IRConversion where

import qualified MusAssistAST         as MusAST
import           Data.List

globalOrderOfSharps :: [MusAST.NoteName]
globalOrderOfSharps = [MusAST.F, MusAST.C, MusAST.G, MusAST.D, MusAST.A, MusAST.E, MusAST.B]

globalOrderOfNotesAsc :: [MusAST.NoteName] -- order starting on C (i.e. gives C major)
globalOrderOfNotesAsc = cycle [MusAST.C, MusAST.D, MusAST.E, MusAST.F, MusAST.G, MusAST.A, MusAST.B]

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Expressions
-----------------------------------------------------------------------------------------
expandIntermediateExpr :: MusAST.IntermediateExpr -> IO MusAST.Expr

-- | Notes get expanded to become single-element chords
expandIntermediateExpr (MusAST.Note tone duration) = return $ MusAST.Chord [tone] duration

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
expandIntermediateInstr (MusAST.SetKeySignature noteName accidental quality) = 
    case quality of 
        MusAST.Major -> 
            case accidental of 
                MusAST.Flat -> 
                    if noteName >= MusAST.C
                        then let sharpsIndexMaybe = elemIndex noteName globalOrderOfSharps
                             in case sharpsIndexMaybe of
                                 -- this is a flat key signature
                                 -- seven possible sharps or flats bc 7 possible note names
                                 -- also, we will never have sharpsIndex go below 1 because F major is handled separately
                                 Just sharpsIndex -> return $ MusAST.KeySignature 0 (7 - (sharpsIndex - 1)) 
                                 Nothing          -> return $ error "Cannot convert flat key sig template"
                        else return $ error "Key signature cannot have double flats"
                MusAST.Sharp -> 
                    if noteName <= MusAST.C
                        then convertSharpKeySig noteName
                        else return $ error "Key signature cannot have double flats"
                MusAST.Natural -> 
                    case noteName of
                        MusAST.C -> return $ MusAST.KeySignature 0 0
                        MusAST.F -> return $ MusAST.KeySignature 0 1
                        otherNoteName -> convertSharpKeySig noteName -- this is a sharp key sig by definition
                where convertSharpKeySig noteName = 
                        let numSharpsMaybe = elemIndex (pred noteName) globalOrderOfSharps 
                        in case numSharpsMaybe of
                            Just numSharps -> return $ MusAST.KeySignature (numSharps + 1) 0 -- account for zero-indexing
                            Nothing -> return $ error "Cannot convert sharp key sig template"
        MusAST.Minor -> let majKeyNoteName = (succ . succ) noteName -- we go down 3 semitones in the minor key to get the major key
                            majAccidentalMaybe = 
                                    case accidental of -- default is convert to major, then check if valid there, except when there's no single-accidentals major conversion
                                        MusAST.Sharp -> 
                                            if noteName >= MusAST.D then Just MusAST.Sharp
                                            else Just MusAST.Natural
                                        MusAST.Flat -> 
                                            if noteName >= MusAST.D then Just MusAST.Flat -- technically, Db minor will error in Fb Major as double flats, but we don't error now for consistency 
                                            else Nothing -- no conversion to major due to double flats
                                        MusAST.Natural -> 
                                            if noteName >= MusAST.D then Just MusAST.Natural
                                            else Just MusAST.Flat
                        in case majAccidentalMaybe of 
                            Just majAccidental -> expandIntermediateInstr (MusAST.SetKeySignature majKeyNoteName majAccidental MusAST.Major)
                            Nothing            -> return $ error "Cannot convert minor to major key sig due to double flats" 
        _     -> return $ error "Key signature quality can only be major or minor"

expandIntermediateInstr MusAST.CreateNewMeasure = return MusAST.NewMeasure

expandIntermediateInstr (MusAST.IRWrite intermediateExprs) = do
    exprs <- mapM expandIntermediateExpr intermediateExprs
    return $ MusAST.Write exprs

-----------------------------------------------------------------------------------------
-- Expand All the Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstrs :: [MusAST.IntermediateInstr] -> IO [MusAST.Instr]
expandIntermediateInstrs intermediateInstrs = mapM expandIntermediateInstr intermediateInstrs