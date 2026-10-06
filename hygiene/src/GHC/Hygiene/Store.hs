{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | __State that outlives a @:reload@__, by name.
--
-- A reload reverts every CAF of the modules it links again, so a cache a module keeps in a top-level
-- 'IORef' starts empty after every edit. The session's engine is not reloaded: it holds named slots
-- (@hygiene/c/store.c@), and 'storeRef' hands back the SAME 'IORef' under a name for as long as the
-- process lives:
--
-- > {-# NOINLINE cache #-}
-- > cache :: IORef (Map Key Value)
-- > cache = unsafePerformIO (storeRef "myproject.cache.v1" Map.empty)
--
-- Two rules, both the caller's:
--
-- * __A slot is untyped.__ What it holds must only be read by code compiled against the same layout
--   of its type: put a version in the name and change it with the type (or key what is stored on the
--   source of the modules that define its types).
-- * __What is stored should be evaluated.__ A thunk holds the code and the CAFs of the generation
--   that built it.
--
-- Outside the engine (a compiled program, another GHCi) there are no slots: 'storeRef' is then a ref
-- per name for the life of the process, which is all a program without reloads needs.
--
-- @ghci-session store@ lists a session's slots; @ghci-session census --kept@ says what each retains.
module GHC.Hygiene.Store
  ( storeRef
  , storeNames
  , storeDrop
  , storeRoots
  ) where

import Control.Monad (forM)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as M
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Exts (Any)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import GHC.Hygiene (engineSymbol)

foreign import ccall unsafe "dynamic" callGet :: FunPtr (CString -> IO (Ptr ())) -> CString -> IO (Ptr ())
foreign import ccall unsafe "dynamic" callPut :: FunPtr (CString -> Ptr () -> IO (Ptr ())) -> CString -> Ptr () -> IO (Ptr ())
foreign import ccall unsafe "dynamic" callCount :: FunPtr (IO CInt) -> IO CInt
foreign import ccall unsafe "dynamic" callName :: FunPtr (CInt -> IO CString) -> CInt -> IO CString
foreign import ccall unsafe "dynamic" callPtr :: FunPtr (CInt -> IO (Ptr ())) -> CInt -> IO (Ptr ())

-- | The ref kept under @name@, made with @initial@ the first time it is asked for.
storeRef :: forall a. String -> a -> IO (IORef a)
storeRef name initial = do
  get <- engineSymbol "ghs_store_get"
  put <- engineSymbol "ghs_store_put_new"
  case (get, put) of
    (Just g, Just p) -> withCString name $ \cn -> do
      have <- callGet g cn
      if have /= nullPtr then deRefStablePtr (castPtrToStablePtr have :: StablePtr (IORef a)) else do
        r <- newIORef initial
        sp <- newStablePtr r
        got <- callPut p cn (castStablePtrToPtr sp)
        -- another thread made the slot first: its ref is the one
        if got == castStablePtrToPtr sp then pure r else freeStablePtr sp >> deRefStablePtr (castPtrToStablePtr got :: StablePtr (IORef a))
    _ -> do
      r <- newIORef initial
      x <- atomicModifyIORef' local $ \m -> case M.lookup name m of
        Just old -> (m, old)
        Nothing -> let v = unsafeCoerce r :: Any in (M.insert name v m, v)
      pure (unsafeCoerce x)

-- | The slots of a process that is not the engine.
{-# NOINLINE local #-}
local :: IORef (M.Map String Any)
local = unsafePerformIO (newIORef M.empty)

-- | Every slot's name and the address it holds (a 'StablePtr'): what a heap census starts from.
storeRoots :: IO [(String, Ptr ())]
storeRoots = do
  fs <- (,,) <$> engineSymbol "ghs_store_count" <*> engineSymbol "ghs_store_name" <*> engineSymbol "ghs_store_ptr"
  case fs of
    (Just c, Just n, Just p) -> do
      k <- callCount c
      forM [0 .. k - 1] $ \i -> do
        nm <- callName n i >>= \s -> if s == nullPtr then pure "" else peekCString s
        ptr <- callPtr p i
        pure (nm, castPtr ptr)
    _ -> pure []

storeNames :: IO [String]
storeNames = map fst <$> storeRoots

-- | Forget a slot: the next 'storeRef' of that name starts from its initial value. Code already holding
-- the ref (a module linked since the slot was made) keeps it until it is linked again, so this takes
-- effect at the next reload of its owner. False: no such slot.
storeDrop :: String -> IO Bool
storeDrop name = do
  take' <- engineSymbol "ghs_store_take"
  case take' of
    Just t -> withCString name $ \cn -> do
      p <- callGet t cn
      if p == nullPtr then pure False else freeStablePtr (castPtrToStablePtr p :: StablePtr ()) >> pure True
    Nothing -> atomicModifyIORef' local (\m -> (M.delete name m, M.member name m))
