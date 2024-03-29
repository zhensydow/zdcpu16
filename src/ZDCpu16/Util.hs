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
module ZDCpu16.Util(
  showWord, byteStringToWord16, word16ToByteString ) where

-- -----------------------------------------------------------------------------
import qualified Data.ByteString as BS( ByteString, unpack, pack )
import Data.Bits( shiftR, shiftL, (.|.), (.&.) )
import Data.Word( Word8, Word16 )
import Numeric( showHex )

-- -----------------------------------------------------------------------------
showWord :: Word16 -> String
showWord v = replicate filln '0' ++ cad
  where
    cad = showHex v ""
    filln = 4 - length cad

-- -----------------------------------------------------------------------------
byteStringToWord16 :: BS.ByteString -> [Word16]
byteStringToWord16 = packW8ToW16 . BS.unpack

word16ToByteString :: [Word16] -> BS.ByteString
word16ToByteString = BS.pack . packW16ToW8

-- -----------------------------------------------------------------------------
packW8ToW16 :: [Word8] -> [Word16]
packW8ToW16 [] = []
packW8ToW16 (_:[]) = []
packW8ToW16 (x:y:xs) = word : packW8ToW16 xs
  where
    ah = fromIntegral x `shiftL` 8
    al = fromIntegral y
    word =  ah .|. al
-- -----------------------------------------------------------------------------
packW16ToW8 :: [Word16] -> [Word8]
packW16ToW8 [] = []
packW16ToW8 (x:xs) = ah : al : packW16ToW8 xs
  where
    ah = fromIntegral $ (x `shiftR` 8) .&. 0xff
    al = fromIntegral $ x .&. 0xff

-- -----------------------------------------------------------------------------
