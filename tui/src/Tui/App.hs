-- | The loop: draw, wait for a key, a resize, a tick or a wake, let the application handle it, draw again.
module Tui.App
  ( App (..), runApp
  ) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Data.IORef
import Data.Maybe (fromMaybe)
import System.IO

import Tui.Buffer
import Tui.Terminal

-- | An application over a state @s@.
data App s = App
  { appTick :: Double                                         -- ^ seconds between ticks (0: none)
  , appDraw :: (Int, Int) -> s -> IO ([Put], Maybe (Int, Int))  -- ^ the frame for this size, and the cursor to show
  , appEvent :: Event -> s -> IO (Maybe s)                   -- ^ 'Nothing' ends the loop
  }

-- | Run the application in the raw terminal; @start@ makes the first state given the events (so threads
-- it starts can 'wake' the loop). The last state is returned.
runApp :: App s -> (Events -> IO s) -> IO s
runApp app start = withRawTerminal $ bracket startEvents stopEvents $ \ev -> do
  s0 <- start ev
  lastFrame <- newIORef Nothing
  let draw s = do
        size <- fromMaybe (80, 24) <$> termSize
        (puts, cur) <- appDraw app size s
        new <- pure (frame size puts)
        old <- readIORef lastFrame
        let out = diff old new
            place = case cur of
              Just (x, y) -> "\ESC[" ++ show (y + 1) ++ ";" ++ show (x + 1) ++ "H\ESC[?25h"
              Nothing -> "\ESC[?25l"
        hPutStr stdout ("\ESC[?25l" ++ out ++ "\ESC[0m" ++ place)
        hFlush stdout
        writeIORef lastFrame (Just new)
      loop s = do
        -- (the wakes seen are counted BEFORE the draw: a wake that comes in while the frame is drawn -- a key's echo,
        -- ~100 us after the key -- would otherwise be counted as seen, and shown only at the next tick)
        seen <- readTVarIO (evWakes ev)
        draw s
        tv <- if appTick app > 0 then registerDelay (round (appTick app * 1e6)) else newTVarIO False
        e <- atomically $ readTQueue (evQueue ev)
               `orElse` (readTVar (evWakes ev) >>= \n -> check (n /= seen) >> pure EvWake)
               `orElse` (readTVar tv >>= check >> pure EvTick)
        r <- appEvent app e s
        case (r, e) of
          (Nothing, _) -> pure s
          (Just s', EvResize) -> writeIORef lastFrame Nothing >> hPutStr stdout "\ESC[2J" >> loop s'
          (Just s', _) -> loop s'
  loop s0
