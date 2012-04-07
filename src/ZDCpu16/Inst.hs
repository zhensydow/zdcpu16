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
module ZDCpu16.Inst(
  -- * Types
  OpCode(..), OpVal(..),
  -- * Functions
  opcode, opval, basicA, basicB, nonbasicA, isBasicInstruction,
  instructionLength, numValues,
  -- * Operations
  addOverflow, subUnderflow, mulOverflow, divUnderflow, modChecked,
  shlOverflow, shrUnderflow
  ) where

-- -----------------------------------------------------------------------------
import Data.Bits( shiftR, shiftL, (.&.) )
import Data.Word( Word16, Word32 )

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
data OpVal = VReg ! Word16
           | VMemReg ! Word16
           | VMemWordReg ! Word16 ! Word16
           | VPop | VPeek | VPush | VSP | VPC | VO
           | VMemWord ! Word16
           | VWord ! Word16
           | VLiteral ! Word16
           deriving( Show )

-- -----------------------------------------------------------------------------
opval :: Word16 -> OpVal
opval v
  -- 0x00-0x07: register (A, B, C, X, Y, Z, I or J, in that order)
  | v >= 0 && v <= 0x07 = VReg v
  -- 0x08-0x0f: [register]
  | v >= 0x8 && v <= 0x0f = VMemReg (v - 0x08)
  -- 0x10-0x17: [next word + register]
  | v >= 0x10 && v <= 0x17 = VMemWordReg (v - 0x10) 0
  -- 0x18: POP / [SP++]
  | v == 0x18 = VPop
  -- 0x19: PEEK / [SP]
  | v == 0x19 = VPeek
  -- 0x1a: PUSH / [--SP]
  | v == 0x1a = VPush
  -- 0x1b: SP
  | v == 0x1b = VSP
  -- 0x1c: PC
  | v == 0x1c = VPC
  -- 0x1d: O
  | v == 0x1d = VO
  -- 0x1e: [next word]
  | v == 0x1e = VMemWord 0
  -- 0x1f: next word (literal)
  | v == 0x1f = VWord 0
  -- 0x20-0x3f: literal value 0x00-0x1f (literal)
  | v >= 0x20 && v <= 0x3f = VLiteral (v - 0x20)
  | otherwise = error $ "invalid opval " ++ show v

-- -----------------------------------------------------------------------------
basicA :: Word16 -> Word16
basicA val = (val `shiftR` 4) .&. 0x3f
{-# INLINE basicA #-}

basicB :: Word16 -> Word16
basicB val = (val `shiftR` 10) .&. 0x3f
{-# INLINE basicB #-}

nonbasicA :: Word16 -> Word16
nonbasicA = basicB
{-# INLINE nonbasicA #-}

-- -----------------------------------------------------------------------------
isBasicInstruction :: Word16 -> Bool
isBasicInstruction val = (val .&. 0xf) /= 0
{-# INLINE isBasicInstruction #-}

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
numValues :: OpCode -> Int
numValues JSR = 1
numValues RESERV = 0
numValues _ = 2

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
