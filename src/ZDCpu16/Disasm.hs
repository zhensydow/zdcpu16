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
module ZDCpu16.Disasm( disasm, disasm', showDIns ) where

-- -----------------------------------------------------------------------------
import Data.Word( Word16 )
import ZDCpu16.Inst(
  OpCode(..), OpVal(..), opcode, opval, nonbasicA, basicA, basicB, numValues,
  showOpVal )

-- -----------------------------------------------------------------------------
data DIns = DOpAB ! OpCode ! OpVal ! OpVal
          | DOpA ! OpCode ! OpVal
          | DOp ! OpCode
          | DUnknown
          deriving( Show )

-- -----------------------------------------------------------------------------
showDIns :: DIns -> String
showDIns (DOpAB cod a b) = show cod ++ " " ++ showOpVal a ++ ", " ++ showOpVal b
showDIns (DOpA cod a) = show cod ++ " " ++ showOpVal a
showDIns (DOp cod) = show cod
showDIns DUnknown = "unknown"

-- -----------------------------------------------------------------------------
disasm :: [Word16] -> [DIns]
disasm = map snd . disasm' . zip [0 :: Int ..]

-- -----------------------------------------------------------------------------
extractA :: Word16 -> [(a,Word16)] -> Maybe (OpVal, [(a,Word16)])
extractA = extractVal . opval . nonbasicA

extractAB :: Word16 -> [(a,Word16)] -> Maybe (OpVal, OpVal, [(a,Word16)])
extractAB v xs = do
  resta <- extractVal (opval $ basicA v) xs
  restb <- extractVal (opval $ basicB v) (snd resta)
  return (fst resta, fst restb, snd restb)

extractVal :: OpVal -> [(a,Word16)] -> Maybe (OpVal, [(a,Word16)])
extractVal v xs = case v of
  VMemWordReg r _ -> do
    w <- safeHead xs
    ys <- safeTail xs
    return $! (VMemWordReg r (snd w), ys)
  VMemWord _ -> do
    w <- safeHead xs
    ys <- safeTail xs
    return $! (VMemWord (snd w), ys)
  VWord _ -> do
    w <- safeHead xs
    ys <- safeTail xs
    return $! (VWord (snd w), ys)
  _ -> Just (v, xs)

-- -----------------------------------------------------------------------------
disasm' :: [(a,Word16)] -> [(a,DIns)]
disasm' [] = []
disasm' ((z,x):xs) = case numValues opc of
  2 -> case extractAB x xs of
    Nothing -> [(z,DUnknown)]
    Just (a,b,ys) -> (z,DOpAB opc a b) : disasm' ys
  1 -> case extractA x xs of
    Nothing -> [(z,DUnknown)]
    Just (a,ys) -> (z,DOpA opc a) : disasm' ys
  0 -> (z,DOp opc) : disasm' xs
  _ -> [(z,DUnknown)]
  where
    opc = opcode x

-- -----------------------------------------------------------------------------
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- -----------------------------------------------------------------------------
