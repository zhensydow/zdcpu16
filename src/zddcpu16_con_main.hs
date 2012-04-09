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
module Main where

-- -----------------------------------------------------------------------------
import Control.Concurrent( forkIO, killThread )
import Control.Concurrent.MVar( MVar, newMVar, readMVar )
import qualified Graphics.UI.SDL as SDL( Event(..), pollEvent )
import Network.MessagePackRpc.Server( serve )
import System.IO( stdout, hFlush )
import ZDCpu16.Render( runRender, clearScreen )
import ZDCpu16.ConRPC( serverRPCMethods )
import ZDCpu16.ConRender( RenderState, mkRenderState, renderConsole )
import ZDCpu16.ConState( ConState(..), mkConState )

-- -----------------------------------------------------------------------------
isEnded :: MVar ConState -> IO Bool
isEnded csRef = do
  cs <- readMVar csRef
  return . csEnd $ cs
  
-- -----------------------------------------------------------------------------
mainLoop :: RenderState -> MVar ConState -> IO ()
mainLoop rst csRef = do
  est <- readMVar csRef 
  _ <- runRender (clearScreen >> renderConsole est) rst
  e <- SDL.pollEvent
  case e of
    SDL.Quit -> return ()
    SDL.NoEvent -> do
      quit <- isEnded csRef
      if quit 
        then return ()
        else mainLoop rst csRef
    _ -> do
      mainLoop rst csRef

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  csRef <- newMVar $ mkConState
  
  msgTID <- forkIO $ serve 1234 $ serverRPCMethods csRef

  rst <- mkRenderState
  putStrLn "console started"
  hFlush stdout
  mainLoop rst csRef

  killThread msgTID
  putStrLn "console ended"

-- -----------------------------------------------------------------------------
