module SGE.Input (
       CursorButtonCallback,
       CursorButtonCode(..),
       CursorButtonState(..),
       CursorPtr,
       KeyboardPtr,
       KeyCallback,
       KeyRepeatCallback,
       KeyboardKey(..),
       KeyState(..),
       RawKeyboardPtr,
       RawCursorPtr,
       withCursorButtonCallback,
       withKeyCallback,
       withKeyRepeatCallback
)

#include <sgec/input/cursor/button_code.h>
#include <sgec/input/cursor/button_state.h>
#include <sgec/input/cursor/object.h>
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
import System.IO ( IO )
import Text.Show ( Show, show )

import qualified SGE.Pos ( Pos(..) )
import qualified SGE.Signal ( ConnectionPtr, RawConnectionPtr, destroyCallback, makeCallbackState )
import SGE.Utils ( failMaybe, fromCInt )

data CursorStruct
type RawCursorPtr = Ptr CursorStruct
type CursorPtr = ForeignPtr CursorStruct

type WrappedCursorButtonCallback = CInt -> CInt -> CInt -> CInt -> Ptr() -> IO ()
type RawCursorButtonCallback = FunPtr WrappedCursorButtonCallback
type CursorButtonCallback = CursorButtonCode -> CursorButtonState -> SGE.Pos.Pos -> IO ()

data KeyboardStruct
type RawKeyboardPtr = Ptr KeyboardStruct
type KeyboardPtr = ForeignPtr KeyboardStruct

type WrappedKeyCallback = CInt -> CInt -> Ptr () -> IO ()
type RawKeyCallback = FunPtr WrappedKeyCallback
type KeyCallback = KeyboardKey -> KeyState -> IO ()

type WrappedKeyRepeatCallback = CInt -> Ptr () -> IO ()
type RawKeyRepeatCallback = FunPtr WrappedKeyRepeatCallback
type KeyRepeatCallback = KeyboardKey -> IO ()


{#enum sgec_input_cursor_button_code as CursorButtonCode {underscoreToCase} with prefix = "sgec_input_cursor_button_code" add prefix = "CursorButtonCode" deriving (Eq, Show)#}
{#enum sgec_input_cursor_button_state as CursorButtonState {underscoreToCase} with prefix = "sgec_input_cursor_button_state" add prefix = "CursorButtonState" deriving (Eq, Show)#}

{#enum sgec_input_keyboard_key_code as KeyboardKey {underscoreToCase} with prefix = "sgec_input_keyboard_key_code" add prefix = "KeyboardKey" deriving (Eq, Show)#}
{#enum sgec_input_keyboard_key_state as KeyState {underscoreToCase} with prefix = "sgec_input_keyboard_key_state" add prefix = "KeyState" deriving (Eq, Show)#}

foreign import ccall unsafe "sgec_input_cursor_object_connect_button_callback" sgeConnectCursorButton :: RawCursorPtr -> RawCursorButtonCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapCursorButtonCallback :: WrappedCursorButtonCallback -> IO RawCursorButtonCallback

connectCursorButtonCallback :: CursorPtr -> CursorButtonCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawCursorButtonCallback))
connectCursorButtonCallback cursor callback =
                            withForeignPtr cursor $ \cp -> do
                                           rawCallback <- wrapCursorButtonCallback $ \button -> \value -> \px -> \py -> \_ -> callback (toEnum $ fromCInt button) (toEnum $ fromCInt value) (SGE.Pos.Pos (fromCInt px, fromCInt py))
                                           wrappedCallback <- sgeConnectCursorButton cp rawCallback nullPtr
                                           maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectCursorButtonCallbackExn :: CursorPtr -> CursorButtonCallback -> IO (SGE.Signal.ConnectionPtr, RawCursorButtonCallback)
connectCursorButtonCallbackExn cursor callback =
                               failMaybe "connect cursor button callback" (connectCursorButtonCallback cursor callback)

withCursorButtonCallback :: CursorPtr -> CursorButtonCallback -> IO a -> IO a
withCursorButtonCallback cursor callback function =
                         bracket (connectCursorButtonCallbackExn cursor callback) SGE.Signal.destroyCallback (\_ -> function)

foreign import ccall unsafe "sgec_input_keyboard_device_connect_key_callback" sgeConnectKey :: RawKeyboardPtr -> RawKeyCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapKeyCallback :: WrappedKeyCallback -> IO RawKeyCallback

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
