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
{-# LANGUAGE OverloadedStrings #-}
module ZDCpu16.DebugRender(
  RenderState, runRender, mkRenderState,  clearScreen, renderText,
  renderEmuState,
  ) where

-- -----------------------------------------------------------------------------
import Control.Monad( forM_ )
import Data.Text( pack )
import Data.Version( showVersion )
import qualified Graphics.UI.SDL as SDL(
  InitFlag(..), init, setVideoMode, setCaption )
import qualified Graphics.UI.SDL.TTF as SDLTTF( init, openFont )
import ZDCpu16.Render(
  RenderState, Render, TextSpan(..), newRenderState, runRender, clearScreen,
  renderText, white, red, lightblue )
import ZDCpu16.Disasm( disasm', showDIns )
import ZDCpu16.EmuState( EmuState(..) )
import ZDCpu16.Hardware(
  DCPU_16(..), regA, regB, regC, regX, regY, regZ, regI, regJ, dumps )
import ZDCpu16.Util( showWord )
import Paths_zdcpu16( version, getDataFileName )

-- -----------------------------------------------------------------------------
mkRenderState :: IO RenderState
mkRenderState = do
  _ <- SDL.init [SDL.InitVideo]
  _ <- SDLTTF.init
  _ <- SDL.setVideoMode 640 480 32 []
  SDL.setCaption "Zhen DCPU-16" ""
  filename <- getDataFileName "ProggyCleanSZ.ttf"
  font <- SDLTTF.openFont filename 16
  return $! newRenderState font

-- -----------------------------------------------------------------------------
renderEmuState :: EmuState -> Render ()
renderEmuState st = do
  let vrs = showVersion version
  renderText (TextSpan 10 10 (255,255,0) (pack $ "Zhen DCPU-16 " ++ vrs))
  let valA = regA . emuCpu $ st
      valB = regB . emuCpu $ st
      valC = regC . emuCpu $ st
      valX = regX . emuCpu $ st
      valY = regY . emuCpu $ st
      valZ = regZ . emuCpu $ st
      valI = regI . emuCpu $ st
      valJ = regJ . emuCpu $ st
  renderText (TextSpan 500 30 white (pack $ "A:  0x" ++ showWord valA))
  renderText (TextSpan 500 40 white (pack $ "B:  0x" ++ showWord valB))
  renderText (TextSpan 500 50 white (pack $ "C:  0x" ++ showWord valC))
  renderText (TextSpan 500 60 white (pack $ "X:  0x" ++ showWord valX))
  renderText (TextSpan 500 70 white (pack $ "Y:  0x" ++ showWord valY))
  renderText (TextSpan 500 80 white (pack $ "Z:  0x" ++ showWord valZ))
  renderText (TextSpan 500 90 white (pack $ "I:  0x" ++ showWord valI))
  renderText (TextSpan 500 100 white (pack $ "J:  0x" ++ showWord valJ))

  let valPC = programCounter . emuCpu $ st
      valSP = stackPointer . emuCpu $ st
      valO = overflow . emuCpu $ st

  renderText (TextSpan 500 120 white (pack $ "PC: 0x" ++ showWord valPC))
  renderText (TextSpan 500 130 white (pack $ "SP: 0x" ++ showWord valSP))
  renderText (TextSpan 500 140 white (pack $ "O:  0x" ++ showWord valO))

  let cys = totalCycles st
  renderText (TextSpan 500 160 red (pack $ "CPU cycles " ++ show cys))

  let pcnums = [valPC ..]
      pcdir = fromIntegral valPC
      pcdisasm = take 20 $ disasm' . zip pcnums . dumps pcdir . emuCpu $ st

  forM_ (zip [0..] pcdisasm) $ \(i,(mdir,inst)) -> do
    renderText (TextSpan 20 (50 + (i*10)) white
                (pack $ showWord mdir ++ ": " ++ showDIns inst))

  renderText (TextSpan 20 35 lightblue "PC Disassembly")

  let spd = speed st
  if runMode st
    then renderText (TextSpan 10 460 white . pack
                     $ "[H] Halt, [Q] Quit, [+/-] " ++ show spd ++ " MHz")
    else renderText (TextSpan 10 460 white . pack
                     $ "[S] Step, [R] Run, [Q] Quit, [+/-] " ++ show spd ++ " MHz")

  return ()

-- -----------------------------------------------------------------------------
