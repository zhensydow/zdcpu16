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
module ZDCpu16.ConRPC( serverRPCMethods, clAdd, clQuit ) where

-- -----------------------------------------------------------------------------
import Control.Concurrent.MVar( MVar, modifyMVar_ )
import qualified Network.MessagePackRpc.Server as MsgSRV( RpcMethod, fun )
import Network.MessagePackRpc.Client( RpcMethod, method )
import ZDCpu16.ConState( ConState(..) )

-- -----------------------------------------------------------------------------
add :: Int -> Int -> IO Int
add x y = do
  putStrLn "executed ADD"
  return $ x + y

-- -----------------------------------------------------------------------------
quit :: MVar ConState -> IO ()
quit csRef = do
  putStrLn "executed QUIT"
  
  modifyMVar_ csRef $ \cs -> return cs{ csEnd = True }
  
  return ()
  
-- -----------------------------------------------------------------------------
serverRPCMethods :: MVar ConState -> [(String, MsgSRV.RpcMethod)]
serverRPCMethods csRef = [ ("add", MsgSRV.fun add)
                     , ("con_quit", MsgSRV.fun $ quit csRef)
                   --, ("writeVidMem", fun writeVidMem)
                   ]

-- -----------------------------------------------------------------------------
clAdd :: RpcMethod (Int -> Int -> IO Int)
clAdd = method "add"

-- -----------------------------------------------------------------------------
clQuit :: RpcMethod (IO ())
clQuit = method "con_quit"

-- -----------------------------------------------------------------------------
