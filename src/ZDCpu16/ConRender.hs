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
module ZDCpu16.ConRender( 
  RenderState, mkRenderState, renderConsole ) where

-- -----------------------------------------------------------------------------
import Control.Monad( forM_ )
import Data.Text( pack )
import qualified Graphics.UI.SDL as SDL( 
  InitFlag(..), init, setVideoMode, setCaption )
import qualified Graphics.UI.SDL.TTF as SDLTTF( init, openFont )
import ZDCpu16.Render( 
  RenderState, Render, TextSpan(..), newRenderState, renderText )
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
  renderText (TextSpan 0 0 (255,255,0) (pack $ "          1         2         3 "))
  forM_ [1..12] $ \i -> do
    renderText (TextSpan 0 (25*i) (255,255,0) (pack $ "12345678901234567890123456789012"))
  return ()
  
-- -----------------------------------------------------------------------------
