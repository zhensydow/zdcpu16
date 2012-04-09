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
  Event(..), SDLKey(..), Keysym(..), waitEvent )
import System.Environment( getArgs, getProgName )
import System.Exit( exitSuccess, exitFailure )
import ZDCpu16.DebugRender(
  RenderState, runRender, mkRenderState, clearScreen, renderEmuState )
import ZDCpu16.EmuState( EmuState(..), mkEmuState )
import ZDCpu16.Hardware( loads )
import ZDCpu16.ConRPC( startConsole, clQuit )
import ZDCpu16.Util( byteStringToWord16 )
import ZDCpu16.ZDCpu16( runEmulator, stepEmulator )

-- -----------------------------------------------------------------------------
mainLoop :: RenderState -> EmuState -> IO ()
mainLoop rst est = do
  _ <- runRender (clearScreen >> renderEmuState est) rst
  e <- SDL.waitEvent
  case e of
    SDL.Quit -> return ()
    SDL.KeyUp key -> do
      case SDL.symKey key of
        SDL.SDLK_ESCAPE -> return ()
        SDL.SDLK_q -> return ()
        SDL.SDLK_s -> do
          (_,newEst) <- runEmulator stepEmulator est
          mainLoop rst newEst
        _ -> mainLoop rst est
    _ -> mainLoop rst est

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      program <- fmap byteStringToWord16 . BS.readFile $ filename
      conn <- startConsole
      rst <- mkRenderState
      let emptyState = mkEmuState conn
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
