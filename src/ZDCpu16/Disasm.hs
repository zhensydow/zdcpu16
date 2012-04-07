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
module ZDCpu16.Disasm( disasm ) where

-- -----------------------------------------------------------------------------
import Data.Word( Word16 )
import ZDCpu16.Inst(
  OpCode(..), OpVal(..), opcode, opval, nonbasicA, basicA, basicB, numValues )

-- -----------------------------------------------------------------------------
data DIns = DOpAB ! OpCode ! OpVal ! OpVal
          | DOpA ! OpCode ! OpVal
          | DOp ! OpCode
          | DUnknown
          deriving( Show )

-- -----------------------------------------------------------------------------
disasm :: [Word16] -> [DIns]
disasm [] = []
disasm (x:xs) = case numValues opc of
  2 -> case extractAB x xs of
    Nothing -> [DUnknown]
    Just (a,b,ys) -> DOpAB opc a b : disasm ys
  1 -> case extractA x xs of
    Nothing -> [DUnknown]
    Just (a,ys) -> DOpA opc a : disasm ys
  0 -> DOp opc : disasm xs
  _ -> [DUnknown]
  where
    opc = opcode x

extractA :: Word16 -> [Word16] -> Maybe (OpVal, [Word16])
extractA v xs = extractVal (opval $ nonbasicA v) xs

extractAB :: Word16 -> [Word16] -> Maybe (OpVal, OpVal, [Word16])
extractAB v xs = do
  resta <- extractVal (opval $ basicA v) xs
  restb <- extractVal (opval $ basicB v) (snd resta)
  return $ (fst resta, fst restb, snd restb)

extractVal :: OpVal -> [Word16] -> Maybe (OpVal, [Word16])
extractVal v xs = case v of
  VMemWordReg r _ -> do
    w <- safeHead xs
    ys <- safeTail xs
    return $! (VMemWordReg r w, ys)
  VMemWord _ -> do
    w <- safeHead xs
    ys <- safeTail xs
    return $! (VMemWord w, ys)
  VWord _ -> do
    w <- safeHead xs
    ys <- safeTail xs
    return $! (VWord w, ys)
  _ -> Just (v, xs)

-- -----------------------------------------------------------------------------
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- -----------------------------------------------------------------------------
