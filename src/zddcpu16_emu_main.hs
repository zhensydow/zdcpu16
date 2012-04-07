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
import Data.Word( Word16 )
import qualified Graphics.UI.SDL as SDL(
  Event(..), SDLKey(..), Keysym(..), waitEvent )
import ZDCpu16.EmuRender(
  RenderState, runRender, mkRenderState, clearScreen, renderEmuState )
import ZDCpu16.EmuState( EmuState(..), mkEmuState )
import ZDCpu16.Hardware( loads )
import ZDCpu16.ZDCpu16( runEmulator, stepEmulator )

-- -----------------------------------------------------------------------------
testProgram :: [Word16]
testProgram = [ 0x7c01, 0x0030, 0x7de1, 0x1000, 0x0020
              , 0x7803, 0x1000, 0xc00d, 0x7dc1, 0x001a
              , 0xa861, 0x7c01, 0x2000, 0x2161, 0x2000
              , 0x8463, 0x806d, 0x7dc1, 0x000d, 0x9031
              , 0x7c10, 0x0018, 0x7dc1, 0x001a, 0x9037
              , 0x61c1, 0x7dc1, 0x001a, 0x0000, 0x0000 ]

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
          let (_,newEst) = runEmulator stepEmulator est
          mainLoop rst newEst
        _ -> do
          print key
          mainLoop rst est
    _ -> mainLoop rst est

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  rst <- mkRenderState
  let initialEmuState = mkEmuState {
        emuCpu = loads 0 testProgram $ emuCpu mkEmuState }
  mainLoop rst initialEmuState
  putStrLn "Exit"

-- -----------------------------------------------------------------------------
