module SGE.Input (
	KeyboardPtr,
	RawKeyboardPtr,
	connectKeyCallback
)

#include <sgec/input/keyboard/device.h>

where

import Foreign ( ForeignPtr )

import Foreign.Ptr ( Ptr )

data KeyboardStruct = KeyboardStruct

type RawKeyboardPtr = Ptr KeyboardStruct

type KeyboardPtr = ForeignPtr KeyboardStruct

type RawKeyCallback = FunPtr ( RawKeyEventPtr -> VoidPtr -> IO () )

foreign import ccall unsafe "sgec_input_keyboard_connect_key_callback" sgeConnectKey :: RawKeyboardPtr -> RawKeyCallback -> IO (RawConnectionPtr)

connectKeyCallback :: KeyboardPtr -> KeyCallback -> IO ConnectionPtr
