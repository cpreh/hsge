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
       MouseAxisCode(..),
       MouseButtonCode(..),
       MousePtr,
       RawCursorPtr,
       RawKeyboardPtr,
       RawMousePtr,
       connectCursorButtonCallback,
       connectCursorButtonCallbackExn,
       connectCursorMoveCallback,
       connectCursorMoveCallbackExn,
       connectMouseAxisCallback,
       connectMouseAxisCallbackExn,
       connectMouseButtonCallback,
       connectMouseButtonCallbackExn,
       connectKeyCallback,
       connectKeyCallbackExn,
       connectKeyRepeatCallback,
       connectKeyRepeatCallbackExn,
       withCursorButtonCallback,
       withCursorMoveCallback,
       withKeyCallback,
       withKeyRepeatCallback,
       withMouseAxisCallback,
       withMouseButtonCallback
)

#include <sgec/input/cursor/button_code.h>
#include <sgec/input/cursor/button_state.h>
#include <sgec/input/cursor/object.h>
#include <sgec/input/mouse/axis_code.h>
#include <sgec/input/mouse/button_code.h>
#include <sgec/input/mouse/button_state.h>
#include <sgec/input/mouse/device.h>
#include <sgec/input/keyboard/device.h>
#include <sgec/input/keyboard/key_code.h>
#include <sgec/input/keyboard/key_state.h>

where

import Control.Exception ( bracket )
import Data.Eq ( Eq )
import Data.Function ( ($) )
import Data.Int ( Int )
import Data.List ( (++) )
import Data.Maybe ( Maybe )
import Data.Ord ( compare, Ordering(EQ, LT, GT))
import Foreign ( ForeignPtr, withForeignPtr )
import Foreign.C ( CInt(..), CLong(..) )
import Foreign.Marshal.Utils ( maybePeek )
import Foreign.Ptr ( FunPtr, Ptr, nullPtr )
import Prelude ( Enum (enumFromTo, enumFrom, fromEnum, pred, succ, toEnum),  error )
import System.IO ( IO )
import Text.Show ( Show, show )

import qualified SGE.Pos ( Pos(..) )
import qualified SGE.Signal ( ConnectionPtr, RawConnectionPtr, destroyCallback, makeCallbackState )
import SGE.Utils ( failMaybe, fromCInt, fromCLong )


{#enum sgec_input_cursor_button_code as CursorButtonCode {underscoreToCase} with prefix = "sgec_input_cursor_button_code" add prefix = "CursorButtonCode" deriving (Eq, Show)#}
{#enum sgec_input_cursor_button_state as CursorButtonState {underscoreToCase} with prefix = "sgec_input_cursor_button_state" add prefix = "CursorButtonState" deriving (Eq, Show)#}

{#enum sgec_input_keyboard_key_code as KeyboardKey {underscoreToCase} with prefix = "sgec_input_keyboard_key_code" add prefix = "KeyboardKey" deriving (Eq, Show)#}
{#enum sgec_input_keyboard_key_state as KeyState {underscoreToCase} with prefix = "sgec_input_keyboard_key_state" add prefix = "KeyState" deriving (Eq, Show)#}

{#enum sgec_input_mouse_axis_code as MouseAxisCode {underscoreToCase} with prefix = "sgec_input_mouse_axis_code" add prefix = "MouseAxisCode" deriving (Eq, Show)#}
{#enum sgec_input_mouse_button_code as MouseButtonCode {underscoreToCase} with prefix = "sgec_input_mouse_button_code" add prefix = "MouseButtonCode" deriving (Eq, Show)#}
{#enum sgec_input_mouse_button_state as MouseButtonState {underscoreToCase} with prefix = "sgec_input_mouse_button_state" add prefix = "MouseButtonState" deriving (Eq, Show)#}


data CursorStruct
type RawCursorPtr = Ptr CursorStruct
type CursorPtr = ForeignPtr CursorStruct

type WrappedCursorButtonCallback = CInt -> CInt -> CInt -> CInt -> Ptr() -> IO ()
type RawCursorButtonCallback = FunPtr WrappedCursorButtonCallback
type CursorButtonCallback = CursorButtonCode -> CursorButtonState -> SGE.Pos.Pos -> IO ()

type WrappedCursorMoveCallback = CInt -> CInt -> Ptr() -> IO ()
type RawCursorMoveCallback = FunPtr WrappedCursorMoveCallback
type CursorMoveCallback = SGE.Pos.Pos -> IO ()


data KeyboardStruct
type RawKeyboardPtr = Ptr KeyboardStruct
type KeyboardPtr = ForeignPtr KeyboardStruct

type WrappedKeyCallback = CInt -> CInt -> Ptr () -> IO ()
type RawKeyCallback = FunPtr WrappedKeyCallback
type KeyCallback = KeyboardKey -> KeyState -> IO ()

type WrappedKeyRepeatCallback = CInt -> Ptr () -> IO ()
type RawKeyRepeatCallback = FunPtr WrappedKeyRepeatCallback
type KeyRepeatCallback = KeyboardKey -> IO ()


data MouseStruct
type RawMousePtr = Ptr MouseStruct
type MousePtr = ForeignPtr MouseStruct

type WrappedMouseAxisCallback = CInt -> CLong -> Ptr () -> IO ()
type RawMouseAxisCallback = FunPtr WrappedMouseAxisCallback
type MouseAxisCallback = MouseAxisCode -> Int -> IO ()

type WrappedMouseButtonCallback = CInt -> CInt -> Ptr () -> IO ()
type RawMouseButtonCallback = FunPtr WrappedMouseButtonCallback
type MouseButtonCallback = MouseButtonCode -> MouseButtonState -> IO ()


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

foreign import ccall unsafe "sgec_input_cursor_object_connect_move_callback" sgeConnectCursorMove :: RawCursorPtr -> RawCursorMoveCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapCursorMoveCallback :: WrappedCursorMoveCallback -> IO RawCursorMoveCallback

connectCursorMoveCallback :: CursorPtr -> CursorMoveCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawCursorMoveCallback))
connectCursorMoveCallback cursor callback =
                            withForeignPtr cursor $ \cp -> do
                                           rawCallback <- wrapCursorMoveCallback $ \px -> \py -> \_ -> callback (SGE.Pos.Pos (fromCInt px, fromCInt py))
                                           wrappedCallback <- sgeConnectCursorMove cp rawCallback nullPtr
                                           maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectCursorMoveCallbackExn :: CursorPtr -> CursorMoveCallback -> IO (SGE.Signal.ConnectionPtr, RawCursorMoveCallback)
connectCursorMoveCallbackExn cursor callback =
                               failMaybe "connect cursor move callback" (connectCursorMoveCallback cursor callback)

withCursorMoveCallback :: CursorPtr -> CursorMoveCallback -> IO a -> IO a
withCursorMoveCallback cursor callback function =
                         bracket (connectCursorMoveCallbackExn cursor callback) SGE.Signal.destroyCallback (\_ -> function)


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


foreign import ccall unsafe "sgec_input_mouse_device_connect_axis_callback" sgeConnectMouseAxis :: RawMousePtr -> RawMouseAxisCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapMouseAxisCallback :: WrappedMouseAxisCallback -> IO RawMouseAxisCallback

connectMouseAxisCallback :: MousePtr -> MouseAxisCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawMouseAxisCallback))
connectMouseAxisCallback mouse callback =
                            withForeignPtr mouse $ \cp -> do
                                           rawCallback <- wrapMouseAxisCallback $ \axis -> \value -> \_ -> callback (toEnum $ fromCInt axis) (toEnum $ fromCLong value)
                                           wrappedCallback <- sgeConnectMouseAxis cp rawCallback nullPtr
                                           maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectMouseAxisCallbackExn :: MousePtr -> MouseAxisCallback -> IO (SGE.Signal.ConnectionPtr, RawMouseAxisCallback)
connectMouseAxisCallbackExn mouse callback =
                               failMaybe "connect mouse axis callback" (connectMouseAxisCallback mouse callback)

withMouseAxisCallback :: MousePtr -> MouseAxisCallback -> IO a -> IO a
withMouseAxisCallback mouse callback function =
                         bracket (connectMouseAxisCallbackExn mouse callback) SGE.Signal.destroyCallback (\_ -> function)

foreign import ccall unsafe "sgec_input_mouse_device_connect_button_callback" sgeConnectMouseButton :: RawMousePtr -> RawMouseButtonCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapMouseButtonCallback :: WrappedMouseButtonCallback -> IO RawMouseButtonCallback

connectMouseButtonCallback :: MousePtr -> MouseButtonCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawMouseButtonCallback))
connectMouseButtonCallback mouse callback =
                            withForeignPtr mouse $ \cp -> do
                                           rawCallback <- wrapMouseButtonCallback $ \button -> \value -> \_ -> callback (toEnum $ fromCInt button) (toEnum $ fromCInt value)
                                           wrappedCallback <- sgeConnectMouseButton cp rawCallback nullPtr
                                           maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectMouseButtonCallbackExn :: MousePtr -> MouseButtonCallback -> IO (SGE.Signal.ConnectionPtr, RawMouseButtonCallback)
connectMouseButtonCallbackExn mouse callback =
                               failMaybe "connect mouse button callback" (connectMouseButtonCallback mouse callback)

withMouseButtonCallback :: MousePtr -> MouseButtonCallback -> IO a -> IO a
withMouseButtonCallback mouse callback function =
                         bracket (connectMouseButtonCallbackExn mouse callback) SGE.Signal.destroyCallback (\_ -> function)
