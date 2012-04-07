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
module ZDCpu16.Hardware(
  -- * Types
  DCPU_16(..),
  -- * Functions
  initialDCPU,
  reg_A, reg_B, reg_C, reg_X, reg_Y, reg_Z, reg_I, reg_J, showReg, load, loads,
  dumps
  ) where

-- -----------------------------------------------------------------------------
import Data.Array.Unboxed( UArray, listArray, elems, (!), (//) )
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
initialRAM :: UArray Int Word16
initialRAM = listArray (0,0xffff) $ repeat 0
initialRegisters :: UArray Int Word16
initialRegisters = listArray (0,8) $ repeat 0
initialDCPU :: DCPU_16
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
showReg :: Word16 -> String
showReg r
  | idx >= 0 && idx < (length table) = table !! idx
  | otherwise = "?"
    where
      idx = fromIntegral r
      table = ["A","B","C","X","Y","Z","I","J"]

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
dumps :: Int -> DCPU_16 -> [Word16]
dumps dir = drop dir . elems . ram

-- -----------------------------------------------------------------------------
