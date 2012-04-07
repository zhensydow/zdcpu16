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
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module ZDCpu16.EmuRender(
  -- * Render Monad
  RenderState, runRender, mkRenderState,  clearScreen, renderText,
  renderRectangle, renderEmuState,
  -- * Types
  TextSpan(..), Rectangle(..),
  -- * Colors
  black, white, red, green, blue
  ) where

-- -----------------------------------------------------------------------------
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.State( MonadState, StateT, runStateT, get )
import Data.Text( Text, unpack, pack )
import Data.Word( Word8 )
import qualified Graphics.UI.SDL as SDL(
  Surface, InitFlag(..), Color(..), Rect(..), init, setVideoMode, setCaption,
  flip, blitSurface, mapRGB, fillRect, surfaceGetPixelFormat, getVideoSurface )
import qualified Graphics.UI.SDL.TTF as SDLTTF(
  Font, init, openFont, renderTextBlended, textSize )
import ZDCpu16.EmuState( EmuState(..) )
import ZDCpu16.Hardware(
  DCPU_16(..), reg_A, reg_B, reg_C, reg_X, reg_Y, reg_Z, reg_I, reg_J )
import Paths_zdcpu16( getDataFileName )

-- -----------------------------------------------------------------------------
black, white, red, green, blue :: (Word8, Word8, Word8)
black = (0,0,0)
white = (255,255,255)
red = (255,0,0)
green = (0,255,0)
blue = (0,0,255)

-- -----------------------------------------------------------------------------
data TextSpan = TextSpan
                { txtX :: ! Int
                , txtY :: ! Int
                , txtColor :: !(Word8, Word8, Word8)
                , txtData :: ! Text }

-- -----------------------------------------------------------------------------
data Rectangle = Rectangle
                 { rectX :: ! Int
                 , rectY :: ! Int
                 , rectW :: ! Int
                 , rectH :: ! Int
                 , rectColor :: !(Word8, Word8, Word8) }

-- -----------------------------------------------------------------------------
data RenderState = RS { renderFont :: !SDLTTF.Font }

-- -----------------------------------------------------------------------------
mkRenderState :: IO RenderState
mkRenderState = do
  _ <- SDL.init [SDL.InitVideo]
  _ <- SDLTTF.init
  _ <- SDL.setVideoMode 640 480 32 []
  SDL.setCaption "DCPU-16" ""
  filename <- getDataFileName "GentiumPlus-R.ttf"
  font <- SDLTTF.openFont filename 14
  return $! RS font

-- -----------------------------------------------------------------------------
newtype Render a = Render
                      { runR :: StateT RenderState IO a }
                    deriving( Functor, Monad, MonadIO
                            , MonadState RenderState )

-- -----------------------------------------------------------------------------
runRender :: MonadIO m => Render a -> RenderState -> m (a, RenderState)
runRender renderf rs = liftIO $ do
  v <- runStateT (runR renderf) rs
  screen <- SDL.getVideoSurface
  SDL.flip screen
  return v

-- -----------------------------------------------------------------------------
io :: IO a -> Render a
io = liftIO

-- -----------------------------------------------------------------------------
getMainBuffer :: Render (SDL.Surface)
getMainBuffer = io SDL.getVideoSurface

-- -----------------------------------------------------------------------------
getMainFont :: Render (SDLTTF.Font)
getMainFont = fmap renderFont $ get

-- -----------------------------------------------------------------------------
clearScreen :: Render ()
clearScreen = do
  screen <- getMainBuffer
  pixel <- io $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 50 0
  _ <- io $ SDL.fillRect screen Nothing pixel
  return ()

-- -----------------------------------------------------------------------------
renderText :: TextSpan -> Render ()
renderText (TextSpan x y (r,g,b) txt) = do
  screen <- getMainBuffer
  font <- getMainFont
  let str = unpack txt
  (w,h) <- io $ SDLTTF.textSize font str
  txtBuff <- io $ SDLTTF.renderTextBlended font str (SDL.Color r g b)
  _ <- io $ SDL.blitSurface txtBuff Nothing screen (Just $ SDL.Rect x y w h)
  return ()

-- -----------------------------------------------------------------------------
renderRectangle :: Rectangle -> Render ()
renderRectangle (Rectangle x y w h (r,g,b)) = do
  screen <- getMainBuffer
  pixel <- io $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b
  _ <- io $ SDL.fillRect screen (Just $ SDL.Rect x y w h) pixel
  return ()

-- -----------------------------------------------------------------------------
renderEmuState :: EmuState -> Render ()
renderEmuState st = do
  renderText (TextSpan 10 10 (255,255,0) "Prueba")
  let valA = reg_A . emuCpu $ st
      valB = reg_B . emuCpu $ st
      valC = reg_C . emuCpu $ st
      valX = reg_X . emuCpu $ st
      valY = reg_Y . emuCpu $ st
      valZ = reg_Z . emuCpu $ st
      valI = reg_I . emuCpu $ st
      valJ = reg_J . emuCpu $ st
  renderText (TextSpan 200 20 white (pack $ "A: " ++ show valA))
  renderText (TextSpan 200 30 white (pack $ "B: " ++ show valB))
  renderText (TextSpan 200 40 white (pack $ "C: " ++ show valC))
  renderText (TextSpan 200 50 white (pack $ "X: " ++ show valX))
  renderText (TextSpan 200 60 white (pack $ "Y: " ++ show valY))
  renderText (TextSpan 200 70 white (pack $ "Z: " ++ show valZ))
  renderText (TextSpan 200 80 white (pack $ "I: " ++ show valI))
  renderText (TextSpan 200 90 white (pack $ "J: " ++ show valJ))

  let valPC = programCounter . emuCpu $ st
      valSP = stackPointer . emuCpu $ st
      valO = overflow . emuCpu $ st

  renderText (TextSpan 200 110 white (pack $ "PC: " ++ show valPC))
  renderText (TextSpan 200 120 white (pack $ "SP: " ++ show valSP))
  renderText (TextSpan 200 130 white (pack $ "O: " ++ show valO))

  return ()

-- -----------------------------------------------------------------------------
