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
import Data.Array.Unboxed( UArray, listArray, elems, (!), (//) )
import Data.Bits( shiftR, (.&.) );
import Data.Word( Word16 )

-- -----------------------------------------------------------------------------
type RAM = UArray Int Word16

data DCPU_16 = DCPU_16
	       { ram :: ! RAM
	       , registers :: UArray Int Word16
	       , programCounter :: ! Word16
	       , stackPointer :: ! Word16
	       , overflow :: ! Word16 }

instance Show DCPU_16 where
  show dcpu = concat [ "DCPU-16 { ram = [..]"
		     , ", registers = ", show . elems $ registers dcpu
		     , ", PC = ", show $ programCounter dcpu
		     , ", SP = ", show $ stackPointer dcpu
		     , ", O = ", show $ overflow dcpu
		     , "}"]

-- -----------------------------------------------------------------------------
initialRAM = listArray (0,0xffff) $ repeat 0
initialRegisters = listArray (0,8) $ repeat 0
initialDCPU = DCPU_16 initialRAM initialRegisters 0 0 0

-- -----------------------------------------------------------------------------
reg_A :: DCPU_16 -> Word16
reg_A = (!0) . registers

reg_B :: DCPU_16 -> Word16
reg_B = (!1) . registers

reg_C :: DCPU_16 -> Word16
reg_C = (!2) . registers

reg_X :: DCPU_16 -> Word16
reg_X = (!3) . registers

reg_Y :: DCPU_16 -> Word16
reg_Y = (!4) . registers

reg_Z :: DCPU_16 -> Word16
reg_Z = (!5) . registers

reg_I :: DCPU_16 -> Word16
reg_I = (!6) . registers

reg_J :: DCPU_16 -> Word16
reg_J = (!7) . registers

-- -----------------------------------------------------------------------------
load :: Int -> Word16 -> DCPU_16 -> DCPU_16
load dir val dcpu = dcpu{ ram = newram }
  where
    newram = (ram dcpu) // [(dir,val)]

-- -----------------------------------------------------------------------------
loads :: Int -> [Word16] -> DCPU_16 -> DCPU_16
loads dir vals dcpu = dcpu{ ram = newram }
  where
    newram = (ram dcpu) // zip [dir..] vals

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
		     { eCpu :: ! DCPU_16 }
		     deriving( Show )

-- -----------------------------------------------------------------------------
newtype Emulator a = Emulator
		     { runEmul :: StateT EmulatorState Identity a }
		    deriving( Functor, Monad, MonadState EmulatorState )

-- -----------------------------------------------------------------------------
runEmulator :: Emulator a -> DCPU_16 -> (a, EmulatorState)
runEmulator emulator dcpu = runIdentity (runStateT (runEmul emulator) (EmulatorState dcpu))

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
  | v >= 0 && v <= 0x07 = getRegister v

  | v >= 0x8 && v <= 0x0f = do
    dir <- getRegister (v - 0x08)
    getMem dir

  | v >= 0x10 && v <= 0x17 = do
    reg <- getRegister (v - 0x10)
    pc <- getPC
    incPC
    dir <- getMem pc
    getMem (dir + reg)

  | v == 0x18 = do
    sp <- getSP
    incSP
    getMem sp

  | v == 0x19 = do
    sp <- getSP
    getMem sp

  | v == 0x1a = do
    decSP
    sp <- getSP
    getMem sp

  | v == 0x1b = getSP

  | v == 0x1c = getPC

  | v == 0x1d = getOverflow

  | v == 0x1e = do
    pc <- getPC
    incPC
    dir <- getMem pc
    getMem dir

  | v == 0x1f = do
    pc <- getPC
    incPC
    getMem pc

  | otherwise = error $ "get RVal " ++ show v

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
  | v >= 0 && v <= 0x07 = return $ LRegister (fromIntegral v)

  | v >= 0x8 && v <= 0x0f = do
    dir <- getRegister (v - 0x08)
    return $ LMem dir

  | v >= 0x10 && v <= 0x17 = do
    reg <- getRegister (v - 0x10)
    pc <- getPC
    incPC
    dir <- getMem pc
    return $ LMem (dir + reg)

  | v == 0x18 = do
    sp <- getSP
    incSP
    return $ LMem sp

  | v == 0x19 = do
    sp <- getSP
    return $ LMem sp

  | v == 0x1a = do
    decSP
    sp <- getSP
    return $ LMem sp

  | v == 0x1b = return $ LSP

  | v == 0x1c = return $ LPC

  | v == 0x1d = return $ LO

  | v == 0x1e = do
    pc <- getPC
    incPC
    dir <- getMem pc
    return $ LMem dir

  | v == 0x1f = do
    incPC
    return $ LLiteral

  | otherwise = return $ LLiteral

setLVal :: LVal -> Word16 -> Emulator ()
setLVal (LRegister v) val = setRegister v val
setLVal (LMem dir) val = setMem dir val
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
    a <- getLVal (basicA sp)
    b <- getRVal (basicB sp)
    setLVal a b
  _ -> error $ "invalid opcode " ++ show (opcode sp) ++ " " ++ show sp

-- -----------------------------------------------------------------------------
