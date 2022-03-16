{-|
Module       : IRConversion
Description  : The intermediate representation for the compiler -- part of the middle. Expands chord/cadence/harmseq templates, etc
Maintainer   : Ilana Shapiro
-}

module IRConversion where

import qualified MusAssistAST         as MusAST
import           Data.List
import           Data.Either
globalOrderOfSharps :: [MusAST.NoteName]
globalOrderOfSharps = [MusAST.F, MusAST.C, MusAST.G, MusAST.D, MusAST.A, MusAST.E, MusAST.B]

-- globalOrderOfNotesAsc :: [MusAST.NoteName] -- order starting on C (i.e. gives C major)
-- globalOrderOfNotesAsc = cycle [MusAST.C, MusAST.D, MusAST.E, MusAST.F, MusAST.G, MusAST.A, MusAST.B]

applyN :: (a -> a) -> a -> Int -> a
applyN f x 0 = x
applyN f x n = f (applyN f x (n-1)) 

-- https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n = take (length xs) (drop n (cycle xs))

convertInversionToInt :: MusAST.Inversion -> Int 
convertInversionToInt inversion = 
    case inversion of
        MusAST.Root   -> 0
        MusAST.First  -> 1
        MusAST.Second -> 2
        MusAST.Third  -> 3
        MusAST.Fourth -> 4

-- pred, succ for enums with custom error message
cyclicAccidentalPred :: MusAST.Accidental -> Either MusAST.Accidental String
cyclicAccidentalPred n
  | n == minBound = Right "Cannot lower a double flat"
  | otherwise = Left (pred n)

cyclicAccidentalSucc :: MusAST.Accidental -> Either MusAST.Accidental String
cyclicAccidentalSucc n
  | n == maxBound = Right "Cannot lift a double sharp"
  | otherwise = Left (succ n)

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Expressions
-----------------------------------------------------------------------------------------
expandIntermediateExpr :: MusAST.IntermediateExpr -> IO MusAST.Expr

-- | Notes get expanded to become single-element chords
expandIntermediateExpr (MusAST.Note tone duration) = return $ MusAST.Chord [tone] duration

-- | Predefined chords
expandIntermediateExpr (MusAST.ChordTemplate (MusAST.Tone rootNoteName rootAccidental rootOctave) quality chordType inversion duration) = 
    if rootAccidental == MusAST.DoubleFlat || rootAccidental == MusAST.DoubleSharp then return $ error "Cannot build chord on a double flat or sharp" 
    else if chordType == MusAST.Triad && quality == MusAST.HalfDiminished then return $ error "Cannot have a half diminished triad" else do
    let root = MusAST.Tone rootNoteName rootAccidental rootOctave
        generateMinorThirdAccidental noteName accidental = case accidental of
            MusAST.Natural -> Left (if noteName >= MusAST.D then MusAST.Natural else MusAST.Flat)
            MusAST.Sharp   -> Left (if noteName >= MusAST.D then MusAST.Sharp else MusAST.Natural)
            MusAST.Flat    -> Left (if noteName >= MusAST.D then MusAST.Flat else MusAST.DoubleFlat)
            _              -> Right "Cannot build chord on a double flat or sharp"

        thirdOctave           = if rootNoteName `elem` [MusAST.A, MusAST.B] then rootOctave + 1 else rootOctave
        thirdNoteName         = applyN succ rootNoteName 2
        thirdAccidentalEither = 
            if quality `elem` [MusAST.Minor, MusAST.Diminished, MusAST.HalfDiminished] 
            then minorThirdAccidentalFromRoot
            else case minorThirdAccidentalFromRoot of -- Major and Augmented have major third root-third interval
                (Left accidental) -> cyclicAccidentalSucc accidental
                (Right error)     -> Right error
            where minorThirdAccidentalFromRoot = generateMinorThirdAccidental rootNoteName rootAccidental

    if isRight thirdAccidentalEither then return $ error (fromRight "Cannot generate chordal third accidental" thirdAccidentalEither) else do 
    let thirdAccidental       = head (lefts [thirdAccidentalEither]) 
        chordalThird          =  MusAST.Tone thirdNoteName thirdAccidental thirdOctave
        fifthOctave           = if rootNoteName `elem` [MusAST.C, MusAST.D, MusAST.E] then rootOctave else rootOctave + 1
        fifthNoteName         = applyN succ thirdNoteName 2
        fifthAccidentalEither = 
            if quality `elem` [MusAST.Major, MusAST.Diminished, MusAST.HalfDiminished] 
            then minorThirdAccidentalFromThird
            else case minorThirdAccidentalFromThird of -- Minor and Augmented have major third third-fifth interval
                (Left accidental) -> cyclicAccidentalSucc accidental
                (Right error)     -> Right error
            where minorThirdAccidentalFromThird = generateMinorThirdAccidental thirdNoteName thirdAccidental
    if isRight fifthAccidentalEither then return $ error (fromRight "Cannot generate chordal fifth accidental" fifthAccidentalEither) else do 
    let fifthAccidental = head (lefts [fifthAccidentalEither]) 
        chordalFifth    =  MusAST.Tone fifthNoteName fifthAccidental fifthOctave
        triadTones      = [root, chordalThird, chordalFifth]
        
    if chordType == MusAST.Triad 
        then let inversionVal       = convertInversionToInt inversion
                 invertedTriadTones = rotate triadTones inversionVal
             in if inversionVal > 3 then return $ error "Cannot have fourth inversion triad" 
                else return $ MusAST.Chord invertedTriadTones duration
    else do
    let seventhOctave           = if rootNoteName `elem` [MusAST.C, MusAST.D, MusAST.E] then rootOctave else rootOctave + 1
        seventhNoteName         = applyN succ fifthNoteName 2
        seventhAccidentalEither = 
            if quality `elem` [MusAST.Minor, MusAST.Diminished] 
            then minorThirdAccidentalFromFifth
            else if quality == MusAST.Augmented then 
            case minorThirdAccidentalFromFifth of -- Augmented seventh has major second fifth-seventh interval  
                (Left accidental) -> cyclicAccidentalPred accidental
                (Right error)     -> Right error
            else case minorThirdAccidentalFromFifth of -- Major and Half-Dim seventh have major third fifth-seventh interval
                (Left accidental) -> cyclicAccidentalSucc accidental
                (Right error)     -> Right error
            where minorThirdAccidentalFromFifth = generateMinorThirdAccidental fifthNoteName fifthAccidental
    if isRight seventhAccidentalEither then return $ error (fromRight "Cannot generate chordal seventh accidental" seventhAccidentalEither) else do 
    let seventhAccidental    = head (lefts [seventhAccidentalEither]) 
        chordalSeventh       = MusAST.Tone seventhNoteName seventhAccidental seventhOctave
        seventhTones         = triadTones ++ [chordalSeventh]
        invertedSeventhTones = rotate seventhTones (convertInversionToInt inversion)
    return $ MusAST.Chord invertedSeventhTones duration
       
-- | Quality is major/minor ONLY. tone+quality determines the start note and key of the cadence
expandIntermediateExpr (MusAST.Cadence cadenceType tone quality) = undefined

-- | Quality is major/minor ONLY. tone+quality determines the start note and key of the harmseq
--   Duration is length of each chord, length is number of chords in the sequence
--   The seq chords all happen in root position
expandIntermediateExpr (MusAST.HarmonicSequence harmSeqType tone quality duration length) = undefined

expandIntermediateExpr (MusAST.FinalExpr expr) = return expr

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstr :: MusAST.IntermediateInstr -> IO MusAST.Instr

-- | Quality is major/minor ONLY
expandIntermediateInstr (MusAST.SetKeySignature noteName accidental quality) 
    | quality == MusAST.Major = 
        let convertSharpKeySig noteName = 
                    let numSharpsMaybe = elemIndex (pred noteName) globalOrderOfSharps 
                    in case numSharpsMaybe of
                        Just numSharps -> return $ MusAST.KeySignature (numSharps + 1) 0 -- account for zero-indexing
                        Nothing -> return $ error "Cannot convert sharp key sig template"
        in case accidental of 
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
    | quality == MusAST.Minor =
        let majKeyNoteName = applyN succ noteName 2 -- we go up 3 semitones in the minor key to get the major key
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
    | otherwise = return $ error "Key signature quality can only be major or minor"

expandIntermediateInstr MusAST.CreateNewMeasure = return MusAST.NewMeasure

expandIntermediateInstr (MusAST.IRWrite intermediateExprs) = do
    exprs <- mapM expandIntermediateExpr intermediateExprs
    return $ MusAST.Write exprs

-----------------------------------------------------------------------------------------
-- Expand All the Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstrs :: [MusAST.IntermediateInstr] -> IO [MusAST.Instr]
expandIntermediateInstrs intermediateInstrs = mapM expandIntermediateInstr intermediateInstrs