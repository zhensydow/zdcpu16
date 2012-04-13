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
module Main( main ) where

-- -----------------------------------------------------------------------------
import qualified Data.ByteString as BS( readFile )
import qualified Graphics.UI.SDL as SDL(
  Event(..), SDLKey(..), Keysym(..), pollEvent )
import System.Environment( getArgs, getProgName )
import System.Exit( exitSuccess, exitFailure )
import ZDCpu16.DebugRender(
  RenderState, runRender, mkRenderState, clearScreen, renderEmuState )
import ZDCpu16.EmuState( EmuState(..), mkEmuState )
import ZDCpu16.Hardware( loads )
import ZDCpu16.ConRPC( startConsole, clQuit, clWriteVRAM )
import ZDCpu16.Util( byteStringToWord16 )
import ZDCpu16.ZDCpu16( runEmulator, stepEmulator )

-- -----------------------------------------------------------------------------
getInput :: EmuState -> IO (EmuState, Bool)
getInput est = do
  e <- SDL.pollEvent
  case e of
    SDL.Quit -> return $! (est, True)
    SDL.KeyUp key -> do
      case SDL.symKey key of
        SDL.SDLK_ESCAPE -> return $! (est, True)
        SDL.SDLK_q -> return $! (est, True)
        SDL.SDLK_s -> do
          if runMode est
            then getInput est
            else do
              (_,newEst) <- runEmulator stepEmulator est
              getInput newEst
        SDL.SDLK_r -> do
          if runMode est
            then getInput est
            else getInput est{ runMode = True }
        SDL.SDLK_h -> do  
          if runMode est
            then getInput est{ runMode = False }
            else getInput est
        _ -> getInput est
    SDL.NoEvent -> return $! (est, False)      
    _ -> getInput est
    
-- -----------------------------------------------------------------------------
mainLoop :: RenderState -> EmuState -> IO ()
mainLoop rst est = do
  _ <- runRender (clearScreen >> renderEmuState est) rst
  (newEst, quit) <- getInput est
  if quit
    then return ()
    else do
      if runMode newEst
        then do
          (_,newEst2) <- runEmulator stepEmulator newEst
          mainLoop rst newEst2
        else mainLoop rst newEst

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      program <- fmap byteStringToWord16 . BS.readFile $ filename
      conn <- startConsole
      rst <- mkRenderState
      let emptyState = mkEmuState (clWriteVRAM conn)
          initialEmuState = emptyState {
            emuCpu = loads 0 program $ emuCpu emptyState }
      mainLoop rst initialEmuState
      clQuit conn
      exitSuccess

    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ "BIN_FILE"
      exitFailure

-- -----------------------------------------------------------------------------
