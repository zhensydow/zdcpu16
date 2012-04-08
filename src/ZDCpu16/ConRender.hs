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
module ZDCpu16.ConRender( 
  RenderState, mkRenderState, renderConsole ) where

-- -----------------------------------------------------------------------------
import Control.Monad( forM_ )
import Data.Array.Unboxed( assocs )
import Data.Bits( (.&.) )
import Data.Char( chr )
import Data.Text( pack )
import qualified Graphics.UI.SDL as SDL( 
  InitFlag(..), init, setVideoMode, setCaption )
import qualified Graphics.UI.SDL.TTF as SDLTTF( init, openFont )
import ZDCpu16.Render( 
  RenderState, Render, TextSpan(..), newRenderState, renderText, white )
import ZDCpu16.ConState( ConState(..) )
import Paths_zdcpu16( getDataFileName )

-- -----------------------------------------------------------------------------
mkRenderState :: IO RenderState
mkRenderState = do
  _ <- SDL.init [SDL.InitVideo]
  _ <- SDLTTF.init
  _ <- SDL.setVideoMode 640 480 32 []
  SDL.setCaption "Zhen DCPU-16 Console" ""
  filename <- getDataFileName "Inconsolata.ttf"
  font <- SDLTTF.openFont filename 24
  return $! newRenderState font

-- -----------------------------------------------------------------------------
renderConsole :: ConState -> Render ()
renderConsole cs = do
  forM_ (assocs . csVRAM $ cs) $ \(i,w) -> do
    let (y,x) = i `divMod` 32
        c = pack [chr . fromIntegral $ w .&. 0xff]
    renderText (TextSpan (x*20) (y*30) white c)
    return ()
  
  return ()
  
-- -----------------------------------------------------------------------------
