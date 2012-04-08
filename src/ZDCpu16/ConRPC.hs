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
module ZDCpu16.ConRPC( serverRPCMethods, clQuit, clWriteVRAM ) where

-- -----------------------------------------------------------------------------
import Control.Concurrent.MVar( MVar, modifyMVar_ )
import qualified Network.MessagePackRpc.Server as MsgSRV( RpcMethod, fun )
import Network.MessagePackRpc.Client( RpcMethod, method )
import ZDCpu16.ConState( ConState(..), writeVRAM )

-- -----------------------------------------------------------------------------
_quit :: MVar ConState -> IO ()
_quit csRef = do
  putStrLn "executed QUIT"
  modifyMVar_ csRef $ \cs -> return cs{ csEnd = True }
  return ()

-- -----------------------------------------------------------------------------
_writeVRAM :: MVar ConState -> Int -> Int -> IO ()
_writeVRAM csRef dir val = do
  putStrLn "executed WRITEVRAM"
  modifyMVar_ csRef (return . writeVRAM dir (fromIntegral val))
  return ()

-- -----------------------------------------------------------------------------
serverRPCMethods :: MVar ConState -> [(String, MsgSRV.RpcMethod)]
serverRPCMethods csRef = [ ("con_quit", MsgSRV.fun $ _quit csRef)
			 , ("con_writeVRAM", MsgSRV.fun $ _writeVRAM csRef)
		   ]

-- -----------------------------------------------------------------------------
clQuit :: RpcMethod (IO ())
clQuit = method "con_quit"

-- -----------------------------------------------------------------------------
clWriteVRAM :: RpcMethod (Int -> Int -> IO ())
clWriteVRAM = method "con_writeVRAM"

-- -----------------------------------------------------------------------------
