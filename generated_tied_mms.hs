    --- no longer using since the new scheme of fixed time sig means we'll never have full tied measures
    -- but keeping the code for full tied measures in case this is ever changed in the future
    let fullMeasuresInDuration = noteDuration `div` timePerMeasure
        initialNoteCode = -- the code for the note that fits in the current measure
          ["\t\t\t<note>",
            "\t\t\t\t<rest/>",
            "\t\t\t\t<duration>" ++ show remainingTimeInMeasure ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            -- "\t\t\t\t<tie type=\"start\"/>", -- the note doesn't fit in the measure, so we tie
            "\t\t\t\t<voice>1</voice>"
            -- "\t\t\t\t<notations>",
            --   "\t\t\t\t\t<tie type=\"start\"/>",
            -- "\t\t\t\t</notations>",
          ] ++ noteTypeCode ++
          ["\t\t\t</note>"] 
    newMeasureCode <- updateBeat remainingTimeInMeasure state

    let generateTiedMeasures remainingNoteLength 
          | remainingNoteLength <= timePerMeasure = return
            ["\t\t\t<note>",
              "\t\t\t\t<rest/>",
              "\t\t\t\t<duration>" ++ show remainingNoteLength ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
              -- "\t\t\t\t<tie type=\"stop\"/>", -- last note of ties has end tie only
              "\t\t\t\t<voice>1</voice>",
              -- "\t\t\t\t<notations>",
              --   "\t\t\t\t\t<tie type=\"stop\"/>",
              -- "\t\t\t\t</notations>",
            "\t\t\t</note>"] 
            -- no more tied measures, and so remainingNoteLength fits in this measure
          | otherwise = do
              newMeasureCode <- updateBeat remainingTimeInMeasure state
              tiedMeasures <- generateTiedMeasures (remainingNoteLength - timePerMeasure)
              let noteCode = 
                    ["\t\t\t<note>",
                      "\t\t\t\t<rest measure=\"yes\"/>",
                      "\t\t\t\t<duration>" ++ show timePerMeasure ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
                      -- "\t\t\t\t<tie type=\"start\"/>", -- note in middle of tie has both start and stop ties
                      -- "\t\t\t\t<tie type=\"stop\"/>", 
                      "\t\t\t\t<voice>1</voice>",
                      -- "\t\t\t\t<notations>",
                      --   "\t\t\t\t\t<tie type=\"start\"/>",
                      --   "\t\t\t\t\t<tie type=\"stop\"/>",
                      -- "\t\t\t\t</notations>",
                    "\t\t\t</note>"] 
              return $ newMeasureCode ++ noteCode ++ tiedMeasures
    tiedMeasuresCode <- generateTiedMeasures 
    return $ initialNoteCode ++ newMeasureCode ++ tiedMeasuresCode