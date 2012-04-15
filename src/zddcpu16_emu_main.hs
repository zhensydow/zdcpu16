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
import Control.Monad( unless )
import qualified Data.ByteString as BS( readFile )
import Data.Word( Word32 )
import qualified Graphics.UI.SDL as SDL(
  Event(..), SDLKey(..), Keysym(..), pollEvent, getTicks )
import System.Environment( getArgs, getProgName )
import System.Exit( exitSuccess, exitFailure )
import ZDCpu16.DebugRender(
  RenderState, runRender, mkRenderState, clearScreen, renderEmuState )
import ZDCpu16.EmuState( EmuState(..), mkEmuState )
import ZDCpu16.Hardware( loads )
import ZDCpu16.ConRPC( startConsole, clQuit, clWriteVRAM )
import ZDCpu16.Util( byteStringToWord16 )
import ZDCpu16.ZDCpu16( runEmulator, stepEmulator, stepNCycles )

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
        SDL.SDLK_PLUS -> if (speed est < 1000)
                         then getInput est{ speed = speed est + 10 }
                         else getInput est
        SDL.SDLK_MINUS -> if (speed est > 10)
                          then getInput est{ speed = speed est - 10 }
                          else getInput est
        _ -> getInput est
    SDL.NoEvent -> return $! (est, False)
    _ -> getInput est

-- -----------------------------------------------------------------------------
mainLoop :: RenderState -> EmuState -> Int -> Word32 -> IO ()
mainLoop rst est lastd lastt = do
  _ <- runRender (clearScreen >> renderEmuState est) rst
  (newEst, quit) <- getInput est
  unless quit $ do
    newt <- SDL.getTicks
    if runMode newEst
      then do
        let dt = newt - lastt
            cycles = fromIntegral $ dt * (fromIntegral $ speed newEst)
        (newd,newEst2) <- runEmulator (stepNCycles lastd cycles) newEst
        mainLoop rst newEst2 newd newt
      else mainLoop rst newEst lastd newt

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      program <- fmap byteStringToWord16 . BS.readFile $ filename
      conn <- startConsole
      rst <- mkRenderState
      emptyState <- mkEmuState (clWriteVRAM conn)
      loads 0 program $ emuCpu emptyState
      lastTicks <- SDL.getTicks
      mainLoop rst emptyState 0 lastTicks
      clQuit conn
      exitSuccess

    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ "BIN_FILE"
      exitFailure

-- -----------------------------------------------------------------------------
