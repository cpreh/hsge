{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Input (
	KeyboardPtr,
	KeyCallback,
	KeyRepeatCallback,
	KeyboardKey(..),
	KeyState(..),
	RawKeyboardPtr,
	withKeyCallback,
	withKeyRepeatCallback
)

#include <sgec/input/keyboard/device.h>
#include <sgec/input/keyboard/key_code.h>
#include <sgec/input/keyboard/key_state.h>

where

import Control.Exception ( bracket )
import Data.Eq ( Eq )
import Data.Function ( ($) )
import Data.List ( (++) )
import Data.Maybe ( Maybe )
import Data.Ord ( compare, Ordering(EQ, LT, GT))
import Foreign ( ForeignPtr, withForeignPtr )
import Foreign.C ( CInt(..) )
import Foreign.Marshal.Utils ( maybePeek )
import Foreign.Ptr ( FunPtr, Ptr, nullPtr )
import Prelude ( Enum (enumFromTo, enumFrom, fromEnum, pred, succ, toEnum),  error )
import qualified SGE.Signal ( ConnectionPtr, RawConnectionPtr, destroyCallback, makeCallbackState )
import SGE.Utils ( failMaybe, fromCInt )
import System.IO ( IO )
import Text.Show ( Show, show )

data KeyboardStruct
type RawKeyboardPtr = Ptr KeyboardStruct
type KeyboardPtr = ForeignPtr KeyboardStruct

type RawKeyCallback = FunPtr (CInt -> CInt -> Ptr () -> IO ())
type WrappedKeyCallback = CInt -> CInt -> Ptr () -> IO ()

type RawKeyRepeatCallback = FunPtr (CInt -> Ptr () -> IO ())
type WrappedKeyRepeatCallback = CInt -> Ptr () -> IO ()

{#enum sgec_input_keyboard_key_code as KeyboardKey {underscoreToCase} with prefix = "sgec_input_keyboard_key_code" add prefix = "KeyboardKey" deriving (Eq, Show)#}

{#enum sgec_input_keyboard_key_state as KeyState {underscoreToCase} with prefix = "sgec_input_keyboard_key_state" add prefix = "KeyState" deriving (Eq, Show)#}

foreign import ccall unsafe "sgec_input_keyboard_device_connect_key_callback" sgeConnectKey :: RawKeyboardPtr -> RawKeyCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)

foreign import ccall unsafe "wrapper" wrapKeyCallback :: WrappedKeyCallback -> IO RawKeyCallback

type KeyCallback = KeyboardKey -> KeyState -> IO ()

connectKeyCallback :: KeyboardPtr -> KeyCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawKeyCallback))
connectKeyCallback keyboard callback =
	withForeignPtr keyboard $ \kp -> do
		       rawCallback <- wrapKeyCallback $ \key -> \value -> \_ -> callback (toEnum $ fromCInt key) (toEnum $ fromCInt value)
		       wrappedCallback <- sgeConnectKey kp rawCallback nullPtr
		       maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectKeyCallbackExn :: KeyboardPtr -> KeyCallback -> IO (SGE.Signal.ConnectionPtr, RawKeyCallback)
connectKeyCallbackExn keyboard callback =
	failMaybe "connect key callback" (connectKeyCallback keyboard callback)

withKeyCallback :: KeyboardPtr -> KeyCallback -> IO a -> IO a
withKeyCallback keyboard callback function =
	bracket (connectKeyCallbackExn keyboard callback) SGE.Signal.destroyCallback (\_ -> function)


foreign import ccall unsafe "sgec_input_keyboard_device_connect_key_repeat_callback" sgeConnectRepeatKey :: RawKeyboardPtr -> RawKeyRepeatCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)

foreign import ccall unsafe "wrapper" wrapKeyRepeatCallback :: WrappedKeyRepeatCallback -> IO RawKeyRepeatCallback

type KeyRepeatCallback = KeyboardKey -> IO ()

connectKeyRepeatCallback :: KeyboardPtr -> KeyRepeatCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawKeyRepeatCallback))
connectKeyRepeatCallback keyboard callback =
	withForeignPtr keyboard $ \kp -> do
		       rawCallback <- wrapKeyRepeatCallback $ \key -> \_ -> callback (toEnum $ fromCInt key)
		       wrappedCallback <- sgeConnectRepeatKey kp rawCallback nullPtr
		       maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectKeyRepeatCallbackExn :: KeyboardPtr -> KeyRepeatCallback -> IO (SGE.Signal.ConnectionPtr, RawKeyRepeatCallback)
connectKeyRepeatCallbackExn keyboard callback =
	failMaybe "connect key callback" (connectKeyRepeatCallback keyboard callback)

withKeyRepeatCallback :: KeyboardPtr -> KeyRepeatCallback -> IO a -> IO a
withKeyRepeatCallback keyboard callback function =
	bracket (connectKeyRepeatCallbackExn keyboard callback) SGE.Signal.destroyCallback (\_ -> function)