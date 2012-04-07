{- -----------------------------------------------------------------------------
ZDCPU16 is a DCPU-16 emulator.
Copyright (C) 2012  Luis Cabellos

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------- -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ZDCpu16.ZDCpu16( Emulator, runEmulator, stepEmulator, mkState ) where

-- -----------------------------------------------------------------------------
import Control.Monad.Identity( Identity, runIdentity )
import Control.Monad.State( StateT, MonadState(..), runStateT )
import Data.Array.Unboxed( (!), (//) )
import Data.Bits( xor, (.&.), (.|.) );
import Data.Word( Word16 )
import ZDCpu16.Hardware( DCPU_16(..), initialDCPU )
import ZDCpu16.Inst( 
  OpCode(..), opcode, basicA, basicB, nonbasicA, instructionLength, addOverflow, 
  subUnderflow, mulOverflow, divUnderflow, modChecked, shlOverflow, shrUnderflow )

-- -----------------------------------------------------------------------------
data EmulatorState = EmulatorState
                     { eCpu :: ! DCPU_16
                     , cycles :: ! Integer }
                   deriving( Show )

-- -----------------------------------------------------------------------------
newtype Emulator a = Emulator
                     { runEmul :: StateT EmulatorState Identity a }
                   deriving( Functor, Monad, MonadState EmulatorState )

-- -----------------------------------------------------------------------------
mkState :: EmulatorState
mkState = EmulatorState initialDCPU 0

-- -----------------------------------------------------------------------------

runEmulator :: Emulator a -> EmulatorState -> (a, EmulatorState)
runEmulator emulator st = runIdentity (runStateT (runEmul emulator) st)

-- -----------------------------------------------------------------------------
incCycles :: Integer -> Emulator ()
incCycles d = do
  st <- get
  put st{ cycles = (cycles st) + d }

-- -----------------------------------------------------------------------------
getRegister :: Word16 -> Emulator Word16
getRegister v = get >>= return . (! (fromIntegral v)) . registers . eCpu

setRegister :: Int -> Word16 -> Emulator ()
setRegister v val = do
  st <- get
  let newcpu = eCpu st
      oldregisters = registers newcpu
  put st{ eCpu = newcpu{ registers = oldregisters // [(v,val)] } }

-- -----------------------------------------------------------------------------
getPC :: Emulator Word16
getPC = get >>= return . programCounter . eCpu

setPC :: Word16 -> Emulator ()
setPC val = do
  st <- get
  let newcpu = eCpu st
  put st{ eCpu = newcpu{ programCounter = val } }

-- -----------------------------------------------------------------------------
incPC :: Emulator ()
incPC = do
  pc <- getPC
  setPC (pc + 1)

-- -----------------------------------------------------------------------------
getSP :: Emulator Word16
getSP = get >>= return . stackPointer . eCpu

setSP :: Word16 -> Emulator ()
setSP val = do
  st <- get
  let newcpu = eCpu st
  put st{ eCpu = newcpu{ stackPointer = val } }

-- -----------------------------------------------------------------------------
incSP :: Emulator ()
incSP = do
  sp <- getSP
  setPC (sp + 1)

decSP :: Emulator ()
decSP = do
  sp <- getSP
  setPC (sp - 1)

-- -----------------------------------------------------------------------------
getOverflow :: Emulator Word16
getOverflow = get >>= return . overflow . eCpu

setOverflow :: Word16 -> Emulator ()
setOverflow val = do
  st <- get
  let newcpu = eCpu st
  put st{ eCpu = newcpu{ overflow = val } }

-- -----------------------------------------------------------------------------
getMem :: Word16 -> Emulator Word16
getMem dir = get >>= return . (! (fromIntegral dir)) . ram . eCpu

setMem :: Word16 -> Word16 -> Emulator ()
setMem dir val = do
  st <- get
  let newcpu = eCpu st
      oldram = ram newcpu
  put st{ eCpu = newcpu{ ram = oldram // [(fromIntegral dir,val)] } }

-- -----------------------------------------------------------------------------
data LVal = LRegister ! Int
          | LMem Word16
          | LPC | LSP | LO
          | LLiteral Word16
          deriving( Show )

-- -----------------------------------------------------------------------------
getRVal :: Word16 -> Emulator Word16
getRVal v
  -- 0x00-0x07: register (A, B, C, X, Y, Z, I or J, in that order)
  | v >= 0 && v <= 0x07 = getRegister v
  -- 0x08-0x0f: [register]
  | v >= 0x8 && v <= 0x0f = getRegister (v - 0x08) >>= getMem
  -- 0x10-0x17: [next word + register]
  | v >= 0x10 && v <= 0x17 = do
    incCycles 1
    reg <- getRegister (v - 0x10)
    pc <- getPC
    incPC
    dir <- getMem pc
    getMem (dir + reg)
  -- 0x18: POP / [SP++]
  | v == 0x18 = do
    sp <- getSP
    incSP
    getMem sp
  -- 0x19: PEEK / [SP]
  | v == 0x19 = getSP >>= getMem
  -- 0x1a: PUSH / [--SP]
  | v == 0x1a = decSP >> getSP >>= getMem
  -- 0x1b: SP
  | v == 0x1b = getSP
  -- 0x1c: PC
  | v == 0x1c = getPC
  -- 0x1d: O
  | v == 0x1d = getOverflow
  -- 0x1e: [next word]
  | v == 0x1e = do
    incCycles 1
    pc <- getPC
    incPC
    dir <- getMem pc
    getMem dir
  -- 0x1f: next word (literal)
  | v == 0x1f = do
    incCycles 1
    pc <- getPC
    incPC
    getMem pc
  -- 0x20-0x3f: literal value 0x00-0x1f (literal)
  | v >= 0x20 && v <= 0x3f = return $! (v - 0x20)
  | otherwise = error $ "invalid R val " ++ show v

-- -----------------------------------------------------------------------------
getLValRef :: Word16 -> Emulator LVal
getLValRef v
  -- 0x00-0x07: register (A, B, C, X, Y, Z, I or J, in that order)
  | v >= 0 && v <= 0x07 = return $! LRegister (fromIntegral v)
  -- 0x08-0x0f: [register]
  | v >= 0x8 && v <= 0x0f = getRegister (v - 0x08) >>= return . LMem
  -- 0x10-0x17: [next word + register]
  | v >= 0x10 && v <= 0x17 = do
    incCycles 1
    reg <- getRegister (v - 0x10)
    pc <- getPC
    incPC
    dir <- getMem pc
    return $! LMem (dir + reg)
  -- 0x18: POP / [SP++]
  | v == 0x18 = do
    sp <- getSP
    incSP
    return $ LMem sp
  -- 0x19: PEEK / [SP]
  | v == 0x19 = getSP >>= return . LMem
  -- 0x1a: PUSH / [--SP]
  | v == 0x1a = decSP >> getSP >>= return . LMem
  -- 0x1b: SP
  | v == 0x1b = return LSP
  -- 0x1c: PC
  | v == 0x1c = return LPC
  -- 0x1d: O
  | v == 0x1d = return LO
  -- 0x1e: [next word]
  | v == 0x1e = do
    incCycles 1
    pc <- getPC
    incPC
    dir <- getMem pc
    return $ LMem dir
  -- 0x1f: next word (literal)
  | v == 0x1f = do
    incCycles 1
    pc <- getPC
    incPC
    getMem pc >>= return . LLiteral
  -- 0x20-0x3f: literal value 0x00-0x1f (literal)
  | v >= 0x20 && v <= 0x3f = return . LLiteral $! (v - 0x20)
  | otherwise = error $ "invalid L val " ++ show v

-- -----------------------------------------------------------------------------
setLVal :: LVal -> Word16 -> Emulator ()
setLVal (LRegister v) val = setRegister v val
setLVal (LMem dir) val = setMem dir val
setLVal LSP val = setSP val
setLVal LPC val = setPC val
setLVal LO val = setOverflow val
setLVal (LLiteral _) _ = return ()

getLVal :: LVal -> Emulator Word16
getLVal (LRegister v) = getRegister (fromIntegral v)
getLVal (LMem dir) = getMem dir
getLVal LSP = getSP
getLVal LPC = getPC
getLVal LO = getOverflow
getLVal (LLiteral v) = return $ v

-- -----------------------------------------------------------------------------
stepEmulator :: Emulator ()
stepEmulator = do
  pc <- getPC
  op <- getMem pc
  incPC
  execInstruction op

-- -----------------------------------------------------------------------------
execInstruction :: Word16 -> Emulator ()
execInstruction ins = case opcode ins of
  -- SET a, b - sets a to b
  SET -> setLValIns ins 1 (\_ b -> b)
  -- ADD a, b - sets a to a+b,
  -- sets O to 0x0001 if there's an overflow, 0x0 otherwise
  ADD -> setLValOIns ins 2 addOverflow
  -- SUB a, b - sets a to a-b,
  -- sets O to 0xffff if there's an underflow, 0x0 otherwise
  SUB -> setLValOIns ins 2 subUnderflow
  -- MUL a, b - sets a to a*b, sets O to ((a*b)>>16)&0xffff
  MUL -> setLValOIns ins 2 mulOverflow
  -- DIV a, b - sets a to a/b, sets O to ((a<<16)/b)&0xffff.
  -- if b==0, sets a and O to 0 instead.
  DIV -> setLValOIns ins 3 divUnderflow
  -- MOD a, b - sets a to a%b. if b==0, sets a to 0 instead.
  MOD -> setLValIns ins 3 modChecked
  -- SHL a, b - sets a to a<<b, sets O to ((a<<b)>>16)&0xffff
  SHL -> setLValOIns ins 2 shlOverflow
  -- SHR a, b - sets a to a>>b, sets O to ((a<<16)>>b)&0xffff
  SHR -> setLValOIns ins 2 shrUnderflow
  -- AND a, b - sets a to a&b
  AND -> setLValIns ins 1 (.&.)
  -- BOR a, b - sets a to a|b
  BOR -> setLValIns ins 1 (.|.)
  -- XOR a, b - sets a to a^b
  XOR -> setLValIns ins 1 xor
  -- IFE a, b - performs next instruction only if a==b
  IFE -> ifIns ins (==)
  -- IFN a, b - performs next instruction only if a!=b
  IFN -> ifIns ins (/=)
  -- IFG a, b - performs next instruction only if a>b
  IFG -> ifIns ins (>)
  -- IFB a, b - performs next instruction only if (a&b)!=0
  IFB -> ifIns ins (\a b -> (a .&. b) /= 0)
  -- JSR a - pushes the address of the next instruction to the stack,
  -- then sets PC to a
  JSR -> do
    incCycles 2
    a <- getRVal (nonbasicA ins)
    pc <- getPC
    decSP
    sp <- getSP
    setMem sp pc
    setPC a

  RESERV -> return ()

-- -----------------------------------------------------------------------------
skipNextInstruction :: Emulator ()
skipNextInstruction = do
  pc <- getPC
  op <- getMem pc
  setPC (pc + instructionLength op)

-- -----------------------------------------------------------------------------
setLValIns :: Word16 -> Integer -> (Word16 -> Word16 -> Word16) -> Emulator ()
setLValIns ins cost f = do
  incCycles cost
  aref <- getLValRef (basicA ins)
  b <- getRVal (basicB ins)
  a <- getLVal aref
  setLVal aref $! (a `f` b)

-- -----------------------------------------------------------------------------
setLValOIns :: Word16 -> Integer -> (Word16 -> Word16 -> (Word16, Word16))
               -> Emulator ()
setLValOIns ins cost f = do
  incCycles cost
  aref <- getLValRef (basicA ins)
  b <- getRVal (basicB ins)
  a <- getLVal aref
  let (val, o) = f a b
  setLVal aref val
  setOverflow o

-- -----------------------------------------------------------------------------
ifIns :: Word16 -> (Word16 -> Word16 -> Bool) -> Emulator ()
ifIns ins f = do
  a <- getLValRef (basicA ins) >>= getLVal
  b <- getRVal (basicB ins)
  if f a b
    then incCycles 2
    else incCycles 3 >> skipNextInstruction

-- -----------------------------------------------------------------------------
testProgram :: [Word16]
testProgram = [0x7c01, 0x0030, 0x7de1, 0x1000, 0x0020, 0x7803, 0x1000, 0xc00d, 0x7dc1, 0x001a, 0xa861, 0x7c01, 0x2000, 0x2161, 0x2000, 0x8463, 0x806d, 0x7dc1, 0x000d, 0x9031, 0x7c10, 0x0018, 0x7dc1, 0x001a, 0x9037, 0x61c1, 0x7dc1, 0x001a, 0x0000, 0x0000, 0x0000, 0x0000]

-- -----------------------------------------------------------------------------
