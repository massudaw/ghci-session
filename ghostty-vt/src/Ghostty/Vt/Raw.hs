{-# LANGUAGE ForeignFunctionInterface #-}
-- | The flat C functions of @cbits/gvt.c@ and @cbits/gvt_pty.c@, as imported. "Ghostty.Vt" is the API.
--
-- A write (and a resize, which may make the terminal report its size) can call back into Haskell through
-- the write-to-program callback, so those two are @safe@ imports; the rest never call back.
module Ghostty.Vt.Raw where

import Data.Word (Word32, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar (..), CInt (..), CLong (..), CSize (..), CUInt (..))
import Foreign.Ptr (FunPtr, Ptr)

type WriteFn = Ptr () -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "gvt_load" c_load :: CString -> IO CInt
foreign import ccall unsafe "gvt_error" c_error :: IO CString
foreign import ccall unsafe "gvt_loaded" c_loaded :: IO CInt
foreign import ccall unsafe "gvt_terminal_new" c_terminal_new :: CInt -> CInt -> IO (Ptr ())
foreign import ccall unsafe "gvt_terminal_free" c_terminal_free :: Ptr () -> IO ()
foreign import ccall safe "gvt_terminal_write" c_terminal_write :: Ptr () -> Ptr Word8 -> CSize -> IO ()
foreign import ccall safe "gvt_terminal_resize" c_terminal_resize :: Ptr () -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "gvt_terminal_on_write" c_terminal_on_write :: Ptr () -> FunPtr WriteFn -> Ptr () -> IO ()
foreign import ccall unsafe "gvt_terminal_int" c_terminal_int :: Ptr () -> CInt -> IO CLong
foreign import ccall unsafe "gvt_terminal_string" c_terminal_string :: Ptr () -> CInt -> Ptr CChar -> CSize -> IO CInt
foreign import ccall unsafe "gvt_terminal_scroll" c_terminal_scroll :: Ptr () -> CInt -> CLong -> IO ()
foreign import ccall unsafe "gvt_render_new" c_render_new :: IO (Ptr ())
foreign import ccall unsafe "gvt_render_free" c_render_free :: Ptr () -> IO ()
foreign import ccall unsafe "gvt_render_update" c_render_update :: Ptr () -> Ptr () -> IO CInt
foreign import ccall unsafe "gvt_render_clean" c_render_clean :: Ptr () -> IO CInt
foreign import ccall unsafe "gvt_render_int" c_render_int :: Ptr () -> CInt -> IO CLong
foreign import ccall unsafe "gvt_render_colors" c_render_colors :: Ptr () -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "gvt_render_rows_begin" c_render_rows_begin :: Ptr () -> IO CInt
foreign import ccall unsafe "gvt_render_row_next" c_render_row_next :: Ptr () -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "gvt_render_cell_next" c_render_cell_next :: Ptr () -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr Word8 -> Ptr CInt -> Ptr Word8 -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall unsafe "gvt_pty_spawn" c_pty_spawn :: CString -> CString -> CString -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "gvt_pty_resize" c_pty_resize :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "gvt_pty_wait" c_pty_wait :: CInt -> IO CInt
foreign import ccall "wrapper" mkWriteFn :: WriteFn -> IO (FunPtr WriteFn)
foreign import ccall unsafe "gvt_key_encoder_new" c_key_encoder_new :: IO (Ptr ())
foreign import ccall unsafe "gvt_key_encoder_free" c_key_encoder_free :: Ptr () -> IO ()
foreign import ccall unsafe "gvt_key_encode" c_key_encode :: Ptr () -> Ptr () -> CInt -> CString -> CUInt -> Ptr CChar -> CSize -> Word32 -> Ptr CChar -> CSize -> IO CInt
