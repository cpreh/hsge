{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Input (
	KeyboardPtr,
	RawKeyboardPtr,
	KeyboardKey(..),
	KeyboardKeyStatus(..),
	connectKeyCallback,
	connectKeyCallbackExn
)

#include <sgec/input/keyboard/device.h>

where

import Control.Monad ( (>>=) )

import Data.Eq ( Eq )

import Data.Function ( ($) )

import Data.Maybe ( Maybe )

import Foreign ( ForeignPtr, newForeignPtr, withForeignPtr )

import Foreign.C ( CInt(..) )

import Foreign.Marshal.Utils ( maybePeek )

import Foreign.Ptr ( FunPtr, Ptr, nullPtr )

import Prelude ( Enum (fromEnum, toEnum), error )

import SGE.Signal ( ConnectionPtr, RawConnectionPtr, sgeDestroyConnection )

import SGE.Utils ( failMaybe, fromCInt )

import System.IO ( IO )

data KeyboardStruct = KeyboardStruct

type RawKeyboardPtr = Ptr KeyboardStruct

type KeyboardPtr = ForeignPtr KeyboardStruct

type RawKeyCallback = FunPtr (CInt -> CInt -> Ptr () -> IO ())

type WrappedKeyCallback = CInt -> CInt -> Ptr () -> IO ()

data KeyboardKey =
	KeyEscape
	| KeyLeft
	| KeyRight
	| KeyUp
	| KeyDown
	deriving(Eq)

instance Enum KeyboardKey where
	fromEnum KeyEscape = 0
	fromEnum KeyLeft = 18
	fromEnum KeyRight = 19
	fromEnum KeyUp = 20
	fromEnum KeyDown = 21

	toEnum 0 = KeyEscape
	toEnum 18 = KeyLeft
	toEnum 19 = KeyRight
	toEnum 20 = KeyUp
	toEnum 21 = KeyDown
	toEnum _ = error "Invalid key"

data KeyboardKeyStatus =
	KeyReleased
	| KeyPressed

instance Enum KeyboardKeyStatus where
	fromEnum KeyReleased = 0
	fromEnum KeyPressed = 1

	toEnum 0 = KeyReleased
	toEnum 1 = KeyPressed
	toEnum _ = error "Invalid key status"


foreign import ccall unsafe "sgec_input_keyboard_device_connect_key_callback" sgeConnectKey :: RawKeyboardPtr -> RawKeyCallback -> Ptr () -> IO (RawConnectionPtr)

foreign import ccall unsafe "wrapper" wrapKeyCallback :: WrappedKeyCallback -> IO RawKeyCallback

type KeyCallback = KeyboardKey -> KeyboardKeyStatus -> IO ()

-- TODO: We need to free the wrapped key callback
connectKeyCallback :: KeyboardPtr -> KeyCallback -> IO (Maybe ConnectionPtr)
connectKeyCallback keyboard callback =
	withForeignPtr keyboard $ \kp ->
	(wrapKeyCallback $ \key -> \value -> \_ -> callback (toEnum $ fromCInt key) (toEnum $ fromCInt value))
	>>= \wrappedCallback -> sgeConnectKey kp wrappedCallback nullPtr
	>>= maybePeek (newForeignPtr sgeDestroyConnection)

connectKeyCallbackExn :: KeyboardPtr -> KeyCallback -> IO ConnectionPtr
connectKeyCallbackExn keyboard callback =
	failMaybe "connect key callback" (connectKeyCallback keyboard callback)
