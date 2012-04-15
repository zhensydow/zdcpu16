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
module ZDCpu16.Render(
  -- * Types
  RenderState, Render, TextSpan(..), Rectangle(..),
  -- * Functions
  io, newRenderState, runRender, clearScreen, renderRectangle, renderText,
  -- * Colors
  black, white, red, green, blue, lightblue
  ) where

-- -----------------------------------------------------------------------------
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.State( MonadState, StateT, runStateT, get )
import Data.Text( Text, unpack )
import Data.Word( Word8 )
import qualified Graphics.UI.SDL as SDL(
  Surface, Color(..), Rect(..), flip, blitSurface, mapRGB, fillRect, 
  surfaceGetPixelFormat, getVideoSurface )
import qualified Graphics.UI.SDL.TTF as SDLTTF(
  Font, renderTextBlended, textSize )

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
black, white, red, green, blue, lightblue :: (Word8, Word8, Word8)
black = (0,0,0)
white = (255,255,255)
red = (255,0,0)
green = (0,255,0)
blue = (0,0,255)
lightblue = (173,216,230)

-- -----------------------------------------------------------------------------
data RenderState = RS { renderFont :: !SDLTTF.Font }

-- -----------------------------------------------------------------------------
newRenderState :: SDLTTF.Font -> RenderState
newRenderState = RS

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
getMainFont = fmap renderFont get

-- -----------------------------------------------------------------------------
clearScreen :: Render ()
clearScreen = do
  screen <- getMainBuffer
  pixel <- io $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 20 0
  _ <- io $ SDL.fillRect screen Nothing pixel
  return ()

-- -----------------------------------------------------------------------------
renderRectangle :: Rectangle -> Render ()
renderRectangle (Rectangle x y w h (r,g,b)) = do
  screen <- getMainBuffer
  pixel <- io $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b
  _ <- io $ SDL.fillRect screen (Just $ SDL.Rect x y w h) pixel
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
