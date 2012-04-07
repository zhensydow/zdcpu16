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
module ZDCpu16.ZDCpu16( Emulator, runEmulator, stepEmulator ) where

-- -----------------------------------------------------------------------------
import Control.Monad.Identity( Identity, runIdentity )
import Control.Monad.State( StateT, MonadState(..), runStateT )
import Data.Array.Unboxed( (!), (//) )
import Data.Bits( xor, (.&.), (.|.) );
import Data.Word( Word16 )
import ZDCpu16.Hardware( DCPU_16(..) )
import ZDCpu16.Inst(
  OpCode(..), OpVal(..), opcode, opval, basicA, basicB, nonbasicA,
  instructionLength, addOverflow, subUnderflow, mulOverflow, divUnderflow,
  modChecked, shlOverflow, shrUnderflow )
import ZDCpu16.EmuState( EmuState(..) )

-- -----------------------------------------------------------------------------
newtype Emulator a = Emulator
                     { runEmul :: StateT EmuState Identity a }
                   deriving( Functor, Monad, MonadState EmuState )

-- -----------------------------------------------------------------------------
runEmulator :: Emulator a -> EmuState -> (a, EmuState)
runEmulator emulator st = runIdentity (runStateT (runEmul emulator) st)

-- -----------------------------------------------------------------------------
incCycles :: Integer -> Emulator ()
incCycles d = do
  st <- get
  put st{ cycles = (cycles st) + d }

-- -----------------------------------------------------------------------------
getRegister :: Word16 -> Emulator Word16
getRegister v = get >>= return . (! (fromIntegral v)) . registers . emuCpu

setRegister :: Int -> Word16 -> Emulator ()
setRegister v val = do
  st <- get
  let newcpu = emuCpu st
      oldregisters = registers newcpu
  put st{ emuCpu = newcpu{ registers = oldregisters // [(v,val)] } }

-- -----------------------------------------------------------------------------
getPC :: Emulator Word16
getPC = get >>= return . programCounter . emuCpu

setPC :: Word16 -> Emulator ()
setPC val = do
  st <- get
  let newcpu = emuCpu st
  put st{ emuCpu = newcpu{ programCounter = val } }

-- -----------------------------------------------------------------------------
incPC :: Emulator ()
incPC = do
  pc <- getPC
  setPC (pc + 1)

-- -----------------------------------------------------------------------------
getSP :: Emulator Word16
getSP = get >>= return . stackPointer . emuCpu

setSP :: Word16 -> Emulator ()
setSP val = do
  st <- get
  let newcpu = emuCpu st
  put st{ emuCpu = newcpu{ stackPointer = val } }

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
getOverflow = get >>= return . overflow . emuCpu

setOverflow :: Word16 -> Emulator ()
setOverflow val = do
  st <- get
  let newcpu = emuCpu st
  put st{ emuCpu = newcpu{ overflow = val } }

-- -----------------------------------------------------------------------------
getMem :: Word16 -> Emulator Word16
getMem dir = get >>= return . (! (fromIntegral dir)) . ram . emuCpu

setMem :: Word16 -> Word16 -> Emulator ()
setMem dir val = do
  st <- get
  let newcpu = emuCpu st
      oldram = ram newcpu
  put st{ emuCpu = newcpu{ ram = oldram // [(fromIntegral dir,val)] } }

-- -----------------------------------------------------------------------------
data LVal = LRegister ! Int
          | LMem Word16
          | LPC | LSP | LO
          | LLiteral Word16
          deriving( Show )

-- -----------------------------------------------------------------------------
getRVal :: Word16 -> Emulator Word16
getRVal v = case opval v of
  -- register (A, B, C, X, Y, Z, I or J, in that order)
  VReg r -> getRegister r
  -- [register]
  VMemReg r -> getRegister r >>= getMem
  -- [next word + register]
  VMemWordReg r _ -> do
    incCycles 1
    reg <- getRegister r
    pc <- getPC
    incPC
    dir <- getMem pc
    getMem (dir + reg)
  -- POP / [SP++]
  VPop -> do
    sp <- getSP
    incSP
    getMem sp
  -- PEEK / [SP]
  VPeek -> getSP >>= getMem
  -- PUSH / [--SP]
  VPush -> decSP >> getSP >>= getMem
  -- SP
  VSP -> getSP
  -- PC
  VPC -> getPC
  -- O
  VO -> getOverflow
  -- [next word]
  VMemWord _ -> do
    incCycles 1
    pc <- getPC
    incPC
    dir <- getMem pc
    getMem dir
  -- next word (literal)
  VWord _ -> do
    incCycles 1
    pc <- getPC
    incPC
    getMem pc
  -- literal value 0x00-0x1f (literal)
  VLiteral l -> return l

-- -----------------------------------------------------------------------------
getLValRef :: Word16 -> Emulator LVal
getLValRef v = case opval v of
  -- register (A, B, C, X, Y, Z, I or J, in that order)
  VReg r -> return $! LRegister (fromIntegral r)
  -- [register]
  VMemReg r -> getRegister r >>= return . LMem
  -- [next word + register]
  VMemWordReg r _ -> do
    incCycles 1
    reg <- getRegister r
    pc <- getPC
    incPC
    dir <- getMem pc
    return $! LMem (dir + reg)
  -- POP / [SP++]
  VPop -> do
    sp <- getSP
    incSP
    return $ LMem sp
  -- PEEK / [SP]
  VPeek -> getSP >>= return . LMem
  -- PUSH / [--SP]
  VPush -> decSP >> getSP >>= return . LMem
  -- SP
  VSP -> return LSP
  -- PC
  VPC -> return LPC
  -- O
  VO -> return LO
  -- [next word]
  VMemWord _ -> do
    incCycles 1
    pc <- getPC
    incPC
    dir <- getMem pc
    return $ LMem dir
  -- next word (literal)
  VWord _ -> do
    incCycles 1
    pc <- getPC
    incPC
    getMem pc >>= return . LLiteral
  -- literal value 0x00-0x1f (literal)
  VLiteral l -> return . LLiteral $ l

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
