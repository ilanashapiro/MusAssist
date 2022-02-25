{-|
Module       : Assem
Description  : Intermediate representation for x86

               Based on Standard ML code by Andrew Appel. Modified by
               Chris Stone for CS 132.
Maintainer   : CS 132
-}

module Assem (
  -- * Instructions
  -- $instructions
     Instr (..)
  -- * Typedefs
  , AssemblyCode , Source , Destination , Jumps
  -- * Fragments
  , Fragment(..)
  -- * Emitting code
  , format

) where

import qualified Data.Char
import qualified NormTarget as NT
import           Temp

-----------------------------------------------------------------------------------------
-- Fragments
-----------------------------------------------------------------------------------------

-- | An assembly fragment is a piece of assembly code or data headed by a label.
data Fragment =
   FragInts   Label NT.Bytes [Integer] -- ^ Labeled sequence of integer constants
 | FragLabels Label [Label]            -- ^ Labeled sequence of (word-sized) addresses
 | FragCode   Label [Instr]            -- ^ Labeled piece of code


-----------------------------------------------------------------------------------------
-- Instructions
-----------------------------------------------------------------------------------------

--  An Instr is a generic representation of instructions.
--  In theory, it could be used for any assembly language. (We'll only
--  need it for x86 assembly, however).
--
--  The advantage over using just plain strings for assembly instructions
--  is that it is easy to extract information about what registers
--  the instruction reads/writes, and how the instruction affects
--  control flow.

-- Useful typedefs

type AssemblyCode = String  -- ^ The type of the x86 templates
type Source = Temp          -- ^ Sources for an instruction
type Destination = Temp     -- ^ Destinations for an instruction
type Jumps = Maybe [Label]  -- ^ Potential jump targets

-- | An assembly-code instruction
data Instr =
   -- | Jump location.
   LABEL
     AssemblyCode  -- ^ ASM code that creates the label (e.g., "foo:\n")
     Label         -- ^ Compiler's representation of the label

   -- | An operation.
   --   See below for more information about what these instructions encode.
 | OPER
     AssemblyCode  -- ^ ASM code for the operation
     [Source]      -- ^ register(s)/variable(s) possibly read by this ASM
                   --   instruction, before the instruction writes to them
     [Destination] -- ^ register(s)/variable(s) possibly overwritten by the ASM
     Jumps         -- ^ Possible jump locations

   -- | Moving data from a source to a destination
 | MOVE   AssemblyCode Source Destination

   -- | Returning from a function.
 | RETURN AssemblyCode [Source]

           deriving (Show)


-- $instructions
-- == Commentary about instructions
--
--  A 'LABEL' contains both the string containing the assembly language line to
--  create the label (e.g., "foo:\n") and the compiler's representation of
--  that label (e.g., "foo").
--
--
--  'MOVE' and 'OPER' contain
--
--    (1) a template for the assembly code [see below]
--    (2) the register(s)/variable(s) used-before-they-were-written-to by the assembly
--    (3) the register(s)/variable(s) possibly overwritten by the assembly
--    (4) for 'OPER', whether the instruction branches/jumps, and if so,
--        whether there are any labels  __in the current function__
--        to which the instruction might jump.
--
--        If a register is read and then overwritten, it should
--        appear on both the destination and source lists
--
--        Tagging register-to-register moves specially will allow a nice
--        optimization next week.
--
--  A "RETURN" is just a wrapper for a retq instruction. If our code is
--  returning a value then the source list will contain the machine
--  register %rax; if no value is being returned, the
--  source list should be empty.  Separating out RETURN's
--  from generic OPER's makes it easy to see where in the
--  code the function is ending, so that we can insert
--  "deallocate the stack frame" code in the
--  right place.
--
-- == Assembly-code templates
--
-- The assembly should contain the opcodes (E.g., addl) but
-- not the operands directly.  Instead, the string should
-- contain `s0, `s1, .... to refer to the first, second, ...
-- source, and `d0, `d1, ... to refer to the first, second, ...
-- target, and `j0, `j1, ... to refer to the first, second, ...
-- jump label.
--
-- For example:
--
-- >   -- movq %t101, %t100
-- >   --   i.e., %t100 <- %t101
-- >   A.MOVE "\tmovq `s0, `d0" t101 t100
--
-- >   -- movq %t103, (%t102)
-- >   --   i.e., *(%t102) <- %t103
-- >   --   Note that in this "store" instruction, both operands are sources!
-- >   A.OPER  "\tmovq `s1, (`s0)"  [t102, t103]  []  Nothing
--
-- >   -- addl %t104, %t105
-- >   --   i.e., %t105 <- %t104 + %t105
-- >   A.OPER  "\taddq `s0, `s1"  [t104, t105]  [t105]  Nothing
--
-- >   -- jmp L37
-- >   A.OPER "\tjmp `j0" [] [] (Just ["L37"])
--
-- >   -- callq *%t106
-- >   --   Remember that function calls might have the effect
-- >   --   of overwriting all caller-save registers before we
-- >   --   get to the following instruction.
-- >   A.OPER "\tcallq *`s0" [t106] callerSaveRegisters Nothing
--
--  In OPER, src and dst should include /all/ registers read or written
--  implicitly by the operation /that we might want to store local variables in/.
-- For example, the x86 has a one-operand multiply that multiplies the given
--  argument (say, t107) by %eax to get a 64-bit result, puts the top 32 bits
--  into %edx, and the low 32 bits into %eax.  Thus, this instruction would be
--  written
--
-- >   A.OPER "\timulq `s0" [t107, eax] [eax, edx] Nothing
--
--  However, pushing temporary t108 on the stack might be
--
-- >   A.OPER "\tpushq `s0" [t108] [] Nothing.
--
--  The instruction does implicitly read and write %rsp, but
--  we don't care because we'd never try to store a
--  local variable in %rsp.
--
-- == Jumps
--
--  The jump information tells us where we will go (in the current
--  function only!) after the current instruction or
--  current function call finishes. It should be:
--
--    * Nothing if we will fall through to the next instruction
--    * (Just [labt,labf]) if we might go to either labels labt or labf
--    * (Just []) if we are jumping out of the function entirely (e.g., a
--                return instruction.)
--



-----------------------------------------------------------------------------------------
-- Emitting code
--
-- Although most of this code is technically generic and would work for any
-- architecture, for simplicity we just define the "show" method on fragments to
-- emit them as x86 assembly.
-----------------------------------------------------------------------------------------

instance Show Fragment where

  -- Labelled code fragments
  show (FragCode l instrs) = region "text" l ++ unlines (map format instrs)

  -- Labelled sequence of addresses
  show (FragLabels l ls) = region "data" l ++ unlines (map ("\t.quad " ++) ls)

  -- Labelled sequence of integer fragments
  show (FragInts l bytes ns) =
    let mkLine n = bytesToDirective bytes ++ show n in
      region "data" l ++ unlines (map mkLine ns)
   where
    bytesToDirective :: NT.Bytes -> String
    bytesToDirective NT.I1 = "\t.byte "
    bytesToDirective NT.I8 = "\t.quad "

-- | The header for a region of code
region :: String -> Label -> String
region kind label =
  unlines ["\n\t." ++ kind, "\t.globl " ++ label, "\t.align 16", label ++ ":"]

-- | Emit a single line of assembly, substituting the source and destination
--   registers
format :: Instr -> String
 -- Possibly we could just define this function to be "show" for Instrs, but for
 -- debugging purposes the default show exposes more information.
format instr =
  let
    myNth :: [a] -> Char -> a
    myNth l c =
      let index = Data.Char.ord c - Data.Char.ord '0'
      in
        if index >= length l
          then error
            "out-of-bounds reference to src/dst/jump in assembly instruction"
          else l !! index

    speak :: AssemblyCode -> [Source] -> [Destination] -> [Label] -> String
    speak asm src dst jump =
      let f ('`' : 's' : c   : rest) = saytemp (myNth src c) ++ f rest
          f ('`' : 'd' : c   : rest) = saytemp (myNth dst c) ++ f rest
          f ('`' : 'j' : c   : rest) = myNth jump c ++ f rest
          f ('`'       : '`' : rest) = '`' : f rest
          f ('`'       : _   : _   ) = error "bad Asm format"
          f (c               : rest) = c : f rest
          f []                       = []
      in  f asm
  in
    case instr of
      OPER asm src dst Nothing  -> speak asm src dst []
      OPER asm src dst (Just j) -> speak asm src dst j
      LABEL asm _               -> asm
      MOVE asm src dst
        | src == dst -> case asm of
          '\t' : rest -> speak ("\t ## " ++ rest) [src] [dst] []
          _           -> speak (" ## " ++ asm) [src] [dst] []
        | otherwise -> speak asm [src] [dst] []
      RETURN asm src -> speak asm src [] []
  where saytemp t = show t
