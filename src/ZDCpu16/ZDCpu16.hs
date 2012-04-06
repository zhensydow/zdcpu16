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
import Data.Bits( shiftR, shiftL, xor, (.&.), (.|.) );
import Data.Word( Word16, Word32 )
import ZDCpu16.Hardware( DCPU_16(..), initialDCPU )

-- -----------------------------------------------------------------------------
data OpCode = SET | ADD | SUB | MUL | DIV | MOD
            | SHL | SHR | AND | BOR | XOR
            | IFE | IFN | IFG | IFB | JSR | RESERV
            deriving( Show )


-- -----------------------------------------------------------------------------
isBasicInstruction :: Word16 -> Bool
isBasicInstruction val = (val .&. 0xf) /= 0

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

nonbasicA :: Word16 -> Word16
nonbasicA = basicB

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
instructionLength :: Word16 -> Word16
instructionLength v
  | isBasicInstruction v = 1 + (valLength $ basicA v) + (valLength $ basicB v)
  | otherwise = 1 + (valLength $ nonbasicA v)

-- -----------------------------------------------------------------------------
valLength :: Word16 -> Word16
valLength v
  -- 0x10-0x17: [next word + register]
  -- 0x1e: [next word]
  -- 0x1f: next word (literal)
  | (v >= 0x10 && v <= 0x17) || (v == 0x1e) || (v == 0x1f) = 1
  | otherwise = 0

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
addOverflow :: Word16 -> Word16 -> (Word16, Word16)
addOverflow a b = (fromIntegral sum32, overf)
  where
    overf = if sum32 > 0xffff then 0x0001 else 0x0
    sum32 = (fromIntegral a + fromIntegral b) :: Word32

-- -----------------------------------------------------------------------------
subUnderflow :: Word16 -> Word16 -> (Word16, Word16)
subUnderflow a b = (fromIntegral subInt, overf)
  where
    overf = if subInt < 0 then 0xffff else 0x0
    subInt = (fromIntegral a - fromIntegral b) :: Int

mulOverflow :: Word16 -> Word16 -> (Word16, Word16)
mulOverflow a b = (fromIntegral val32, fromIntegral overf)
  where
    val32 = (fromIntegral a * fromIntegral b) :: Word32
    overf = (val32 `shiftR` 16) .&. 0xffff

-- -----------------------------------------------------------------------------
divUnderflow :: Word16 -> Word16 -> (Word16, Word16)
divUnderflow a b
  | b == 0 = (0, 0)
  | otherwise = (fromIntegral div32, fromIntegral overf)
  where
    overf = ((a32 `shiftL` 16) `div` b32) .&. 0xffff
    div32 = a32 `div` b32
    a32 = fromIntegral a :: Word32
    b32 = fromIntegral b :: Word32

-- -----------------------------------------------------------------------------
shlOverflow :: Word16 -> Word16 -> (Word16, Word16)
shlOverflow a b = (fromIntegral val32, fromIntegral overf)
  where
    overf = (val32 `shiftR` 16) .&. 0xffff
    val32 = (fromIntegral a `shiftL` fromIntegral b) :: Word32

-- -----------------------------------------------------------------------------
shrUnderflow :: Word16 -> Word16 -> (Word16, Word16)
shrUnderflow a b = (a `shiftR` fromIntegral b, fromIntegral overf)
  where
    overf = ((a32 `shiftL` 16) `shiftR` fromIntegral b) .&. 0xffff
    a32 = fromIntegral a :: Word32

-- -----------------------------------------------------------------------------
modChecked :: Word16 -> Word16 -> Word16
modChecked a b
  | b == 0 = a
  | otherwise = a `mod` b

-- -----------------------------------------------------------------------------
