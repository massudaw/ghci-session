-- | A small terminal UI library on libghostty-vt: see "Tui.App" for the loop, "Tui.Buffer" for the frame,
-- "Tui.Terminal" for keys and events, "Tui.Pane" for a program running in a part of the screen.
module Tui
  ( module Tui.Types, module Tui.Buffer, module Tui.Terminal, module Tui.Pane, module Tui.App
  ) where

import Tui.App
import Tui.Buffer
import Tui.Pane
import Tui.Terminal
import Tui.Types
