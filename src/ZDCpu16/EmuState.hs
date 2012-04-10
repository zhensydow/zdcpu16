
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
module ZDCpu16.EmuState( EmuState(..), mkEmuState ) where

-- -----------------------------------------------------------------------------
import Network.MessagePackRpc.Client( Connection )
import ZDCpu16.Hardware( DCPU_16(..), initialDCPU )

-- -----------------------------------------------------------------------------
data EmuState = EmuState
                { emuCpu :: ! DCPU_16
                , cycles :: ! Integer 
                , runMode :: ! Bool
                , conComm :: ! Connection }

-- -----------------------------------------------------------------------------
mkEmuState :: Connection -> EmuState
mkEmuState = EmuState initialDCPU 0 False

-- -----------------------------------------------------------------------------
