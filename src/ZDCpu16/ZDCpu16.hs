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
import Data.Bits( shiftR, (.&.) );
import Data.Word( Word16 )
import ZDCpu16.Hardware( DCPU_16(..) )

-- -----------------------------------------------------------------------------
data OpCode = SET | ADD | SUB | MUL | DIV | MOD
	    | SHL | SHR | AND | BOR | XOR
	    | IFE | IFN | IFG | IFB | JSR | RESERV
	    deriving( Show )


-- -----------------------------------------------------------------------------
opcode :: Word16 -> OpCode
opcode val
  | basicCode == 0 = nonBasicOp ((val `shiftR` 4) .&. 0x3f)
  | otherwise = basicOp basicCode
    where
      basicCode = val .&. 0xf

basicOp :: Word16 -> OpCode
basicOp 0x01 = SET
basicOp 0x02 = ADD
basicOp 0x03 = SUB
basicOp 0x04 = MUL
basicOp 0x05 = DIV
basicOp 0x06 = MOD
basicOp 0x07 = SHL
basicOp 0x08 = SHR
basicOp 0x09 = AND
basicOp 0x0a = BOR
basicOp 0x0b = XOR
basicOp 0x0c = IFE
basicOp 0x0d = IFN
basicOp 0x0e = IFG
basicOp 0x0f = IFB
basicOp _ = error "basic op"

nonBasicOp :: Word16 -> OpCode
nonBasicOp 0x00 = RESERV
nonBasicOp 0x01 = JSR
nonBasicOp _ = error "non basic op"

-- -----------------------------------------------------------------------------
basicA :: Word16 -> Word16
basicA val = (val `shiftR` 4) .&. 0x3f

basicB :: Word16 -> Word16
basicB val = (val `shiftR` 10) .&. 0x3f

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
runEmulator :: Emulator a -> DCPU_16 -> (a, EmulatorState)
runEmulator emulator dcpu = runIdentity (runStateT (runEmul emulator) (EmulatorState dcpu 0))

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
data LVal = LRegister ! Int
	  | LMem Word16
	  | LPC
	  | LSP
	  | LO
	  | LLiteral
	  deriving( Show )

-- -----------------------------------------------------------------------------
getLVal :: Word16 -> Emulator LVal
getLVal v
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
  | v == 0x1f = incCycles 1 >> incPC >> return LLiteral
  -- 0x20-0x3f: literal value 0x00-0x1f (literal)
  | v >= 0x20 && v <= 0x3f = return LLiteral
  | otherwise = error $ "invalid L val " ++ show v
                
-- -----------------------------------------------------------------------------
setLVal :: LVal -> Word16 -> Emulator ()
setLVal (LRegister v) val = setRegister v val
setLVal (LMem dir) val = setMem dir val
setLVal LSP val = setSP val
setLVal LPC val = setPC val
setLVal LO val = setOverflow val
setLVal LLiteral _ = return ()

-- -----------------------------------------------------------------------------
stepEmulator :: Emulator ()
stepEmulator = do
  pc <- getPC
  op <- getMem pc
  incPC
  execInstruction op

-- -----------------------------------------------------------------------------
execInstruction :: Word16 -> Emulator ()
execInstruction sp = case opcode sp of
  SET -> do
    incCycles 1
    a <- getLVal (basicA sp)
    b <- getRVal (basicB sp)
    setLVal a b
  _ -> error $ "invalid opcode " ++ show (opcode sp) ++ " " ++ show sp

-- -----------------------------------------------------------------------------
