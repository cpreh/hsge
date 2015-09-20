module SGE.Input (
       CursorButtonCallback,
       CursorButtonCode(..),
       CursorButtonState(..),
       CursorPtr,
       CursorScrollCode(..),
       FocusPtr,
       FocusKeyCallback,
       KeyboardPtr,
       KeyRepeatCallback,
       KeyboardKeyCallback,
       KeyCode(..),
       KeyState(..),
       MouseAxisCode(..),
       MouseButtonCode(..),
       MousePtr,
       RawCursorPtr,
       RawFocusPtr,
       RawKeyboardPtr,
       RawMousePtr,
       connectCursorButtonCallback,
       connectCursorButtonCallbackExn,
       connectCursorMoveCallback,
       connectCursorMoveCallbackExn,
       connectCursorScrollCallback,
       connectCursorScrollCallbackExn,
       connectFocusKeyCallback,
       connectFocusKeyCallbackExn,
       connectMouseAxisCallback,
       connectMouseAxisCallbackExn,
       connectMouseButtonCallback,
       connectMouseButtonCallbackExn,
       connectKeyRepeatCallback,
       connectKeyRepeatCallbackExn,
       connectKeyboardKeyCallback,
       connectKeyboardKeyCallbackExn,
       withCursorButtonCallback,
       withCursorMoveCallback,
       withCursorScrollCallback,
       withFocusKeyCallback,
       withKeyRepeatCallback,
       withKeyboardKeyCallback,
       withMouseAxisCallback,
       withMouseButtonCallback
)

#include <sgec/input/cursor/button_code.h>
#include <sgec/input/cursor/button_state.h>
#include <sgec/input/cursor/object.h>
#include <sgec/input/cursor/scroll_code.h>
#include <sgec/input/focus/object.h>
#include <sgec/input/mouse/axis_code.h>
#include <sgec/input/mouse/button_code.h>
#include <sgec/input/mouse/button_state.h>
#include <sgec/input/mouse/device.h>
#include <sgec/input/key/code.h>
#include <sgec/input/key/state.h>
#include <sgec/input/keyboard/device.h>

where

import Control.Exception ( bracket )
import Data.Eq ( Eq )
import Data.Function ( ($) )
import Data.Int ( Int )
import Data.List ( (++) )
import Data.Maybe ( Maybe )
import Data.Ord ( compare, Ordering(EQ, LT, GT))
import Foreign ( ForeignPtr, withForeignPtr )
import Foreign.C ( CInt(..), CLong(..), CUInt(..) )
import Foreign.Marshal.Utils ( maybePeek )
import Foreign.Ptr ( FunPtr, Ptr, nullPtr )
import Prelude ( Enum (enumFromTo, enumFrom, fromEnum, pred, succ, toEnum),  error )
import System.IO ( IO )
import Text.Show ( Show, show )

import qualified SGE.Pos ( Pos(..) )
import qualified SGE.Signal ( ConnectionPtr, RawConnectionPtr, destroyCallback, makeCallbackState )
import SGE.Utils ( failMaybe, fromCInt, fromCLong, fromCUInt )


{#enum sgec_input_cursor_button_code as CursorButtonCode {underscoreToCase} with prefix = "sgec_input_cursor_button_code" add prefix = "CursorButtonCode" deriving (Eq, Show)#}
{#enum sgec_input_cursor_button_state as CursorButtonState {underscoreToCase} with prefix = "sgec_input_cursor_button_state" add prefix = "CursorButtonState" deriving (Eq, Show)#}
{#enum sgec_input_cursor_scroll_code as CursorScrollCode {underscoreToCase} with prefix = "sgec_input_cursor_scroll_code" add prefix = "CursorScrollCode" deriving (Eq, Show)#}

{#enum sgec_input_key_code as KeyCode {underscoreToCase} with prefix = "sgec_input_key_code" add prefix = "KeyCode" deriving (Eq, Show)#}
{#enum sgec_input_key_state as KeyState {underscoreToCase} with prefix = "sgec_input_key_state" add prefix = "KeyState" deriving (Eq, Show)#}

{#enum sgec_input_mouse_axis_code as MouseAxisCode {underscoreToCase} with prefix = "sgec_input_mouse_axis_code" add prefix = "MouseAxisCode" deriving (Eq, Show)#}
{#enum sgec_input_mouse_button_code as MouseButtonCode {underscoreToCase} with prefix = "sgec_input_mouse_button_code" add prefix = "MouseButtonCode" deriving (Eq, Show)#}
{#enum sgec_input_mouse_button_state as MouseButtonState {underscoreToCase} with prefix = "sgec_input_mouse_button_state" add prefix = "MouseButtonState" deriving (Eq, Show)#}


-- Cursor
data CursorStruct
type RawCursorPtr = Ptr CursorStruct
type CursorPtr = ForeignPtr CursorStruct

type WrappedCursorButtonCallback = CInt -> CInt -> CInt -> CInt -> Ptr() -> IO ()
type RawCursorButtonCallback = FunPtr WrappedCursorButtonCallback
type CursorButtonCallback = CursorButtonCode -> CursorButtonState -> SGE.Pos.Pos -> IO ()

type WrappedCursorMoveCallback = CInt -> CInt -> Ptr() -> IO ()
type RawCursorMoveCallback = FunPtr WrappedCursorMoveCallback
type CursorMoveCallback = SGE.Pos.Pos -> IO ()

type WrappedCursorScrollCallback = CInt -> CLong -> Ptr() -> IO ()
type RawCursorScrollCallback = FunPtr WrappedCursorScrollCallback
type CursorScrollCallback = CursorScrollCode -> Int -> IO ()


-- Focus
data FocusStruct
type RawFocusPtr = Ptr FocusStruct
type FocusPtr = ForeignPtr FocusStruct

type WrappedFocusKeyCallback = CInt -> CInt -> Ptr () -> IO ()
type RawFocusKeyCallback = FunPtr WrappedFocusKeyCallback
type FocusKeyCallback = KeyCode -> KeyState -> IO ()

type WrappedKeyRepeatCallback = CInt -> Ptr () -> IO ()
type RawKeyRepeatCallback = FunPtr WrappedKeyRepeatCallback
type KeyRepeatCallback = KeyCode -> IO ()


-- Keyboard
data KeyboardStruct
type RawKeyboardPtr = Ptr KeyboardStruct
type KeyboardPtr = ForeignPtr KeyboardStruct

type WrappedKeyboardKeyCallback = CInt -> CInt -> CUInt -> Ptr () -> IO ()
type RawKeyboardKeyCallback = FunPtr WrappedKeyboardKeyCallback
type KeyboardKeyCallback = KeyCode -> KeyState -> Int -> IO ()


-- Mouse
data MouseStruct
type RawMousePtr = Ptr MouseStruct
type MousePtr = ForeignPtr MouseStruct

type WrappedMouseAxisCallback = CInt -> CLong -> Ptr () -> IO ()
type RawMouseAxisCallback = FunPtr WrappedMouseAxisCallback
type MouseAxisCallback = MouseAxisCode -> Int -> IO ()

type WrappedMouseButtonCallback = CInt -> CInt -> Ptr () -> IO ()
type RawMouseButtonCallback = FunPtr WrappedMouseButtonCallback
type MouseButtonCallback = MouseButtonCode -> MouseButtonState -> IO ()


-- Cursor
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

foreign import ccall unsafe "sgec_input_cursor_object_connect_scroll_callback" sgeConnectCursorScroll :: RawCursorPtr -> RawCursorScrollCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapCursorScrollCallback :: WrappedCursorScrollCallback -> IO RawCursorScrollCallback

connectCursorScrollCallback :: CursorPtr -> CursorScrollCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawCursorScrollCallback))
connectCursorScrollCallback cursor callback =
                            withForeignPtr cursor $ \cp -> do
                                           rawCallback <- wrapCursorScrollCallback $ \code -> \value -> \_ -> callback (toEnum $ fromCInt code) (fromCLong value)
                                           wrappedCallback <- sgeConnectCursorScroll cp rawCallback nullPtr
                                           maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectCursorScrollCallbackExn :: CursorPtr -> CursorScrollCallback -> IO (SGE.Signal.ConnectionPtr, RawCursorScrollCallback)
connectCursorScrollCallbackExn cursor callback =
                               failMaybe "connect cursor move callback" (connectCursorScrollCallback cursor callback)

withCursorScrollCallback :: CursorPtr -> CursorScrollCallback -> IO a -> IO a
withCursorScrollCallback cursor callback function =
                         bracket (connectCursorScrollCallbackExn cursor callback) SGE.Signal.destroyCallback (\_ -> function)


-- Focus
foreign import ccall unsafe "sgec_input_focus_object_connect_key_callback" sgeConnectFocusKey :: RawFocusPtr -> RawFocusKeyCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapFocusKeyCallback :: WrappedFocusKeyCallback -> IO RawFocusKeyCallback

connectFocusKeyCallback :: FocusPtr -> FocusKeyCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawFocusKeyCallback))
connectFocusKeyCallback focus callback =
                        withForeignPtr focus $ \kp -> do
                                       rawCallback <- wrapFocusKeyCallback $ \key -> \value -> \_ -> callback (toEnum $ fromCInt key) (toEnum $ fromCInt value)
                                       wrappedCallback <- sgeConnectFocusKey kp rawCallback nullPtr
                                       maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectFocusKeyCallbackExn :: FocusPtr -> FocusKeyCallback -> IO (SGE.Signal.ConnectionPtr, RawFocusKeyCallback)
connectFocusKeyCallbackExn focus callback =
                              failMaybe "connect focus key callback" (connectFocusKeyCallback focus callback)

withFocusKeyCallback :: FocusPtr -> FocusKeyCallback -> IO a -> IO a
withFocusKeyCallback focus callback function =
                        bracket (connectFocusKeyCallbackExn focus callback) SGE.Signal.destroyCallback (\_ -> function)


foreign import ccall unsafe "sgec_input_focus_object_connect_key_repeat_callback" sgeConnectFocusKeyRepeat :: RawFocusPtr -> RawKeyRepeatCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapKeyRepeatCallback :: WrappedKeyRepeatCallback -> IO RawKeyRepeatCallback

connectKeyRepeatCallback :: FocusPtr -> KeyRepeatCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawKeyRepeatCallback))
connectKeyRepeatCallback focus callback =
                         withForeignPtr focus $ \kp -> do
                                        rawCallback <- wrapKeyRepeatCallback $ \key -> \_ -> callback (toEnum $ fromCInt key)
                                        wrappedCallback <- sgeConnectFocusKeyRepeat kp rawCallback nullPtr
                                        maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectKeyRepeatCallbackExn :: FocusPtr -> KeyRepeatCallback -> IO (SGE.Signal.ConnectionPtr, RawKeyRepeatCallback)
connectKeyRepeatCallbackExn focus callback =
                            failMaybe "connect key callback" (connectKeyRepeatCallback focus callback)

withKeyRepeatCallback :: FocusPtr -> KeyRepeatCallback -> IO a -> IO a
withKeyRepeatCallback focus callback function =
                      bracket (connectKeyRepeatCallbackExn focus callback) SGE.Signal.destroyCallback (\_ -> function)


-- Keyboard
foreign import ccall unsafe "sgec_input_keyboard_device_connect_key_callback" sgeConnectKeyboardKey :: RawKeyboardPtr -> RawKeyboardKeyCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapKeyboardKeyCallback :: WrappedKeyboardKeyCallback -> IO RawKeyboardKeyCallback

connectKeyboardKeyCallback :: KeyboardPtr -> KeyboardKeyCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawKeyboardKeyCallback))
connectKeyboardKeyCallback keyboard callback =
                           withForeignPtr keyboard $ \kp -> do
                                          rawCallback <- wrapKeyboardKeyCallback $ \key -> \value -> \id -> \_ -> callback (toEnum $ fromCInt key) (toEnum $ fromCInt value) (fromCUInt id)
                                          wrappedCallback <- sgeConnectKeyboardKey kp rawCallback nullPtr
                                          maybePeek (SGE.Signal.makeCallbackState rawCallback) wrappedCallback

connectKeyboardKeyCallbackExn :: KeyboardPtr -> KeyboardKeyCallback -> IO (SGE.Signal.ConnectionPtr, RawKeyboardKeyCallback)
connectKeyboardKeyCallbackExn keyboard callback =
                              failMaybe "connect keyboard key callback" (connectKeyboardKeyCallback keyboard callback)

withKeyboardKeyCallback :: KeyboardPtr -> KeyboardKeyCallback -> IO a -> IO a
withKeyboardKeyCallback keyboard callback function =
                        bracket (connectKeyboardKeyCallbackExn keyboard callback) SGE.Signal.destroyCallback (\_ -> function)


-- Mouse
foreign import ccall unsafe "sgec_input_mouse_device_connect_axis_callback" sgeConnectMouseAxis :: RawMousePtr -> RawMouseAxisCallback -> Ptr () -> IO (SGE.Signal.RawConnectionPtr)
foreign import ccall unsafe "wrapper" wrapMouseAxisCallback :: WrappedMouseAxisCallback -> IO RawMouseAxisCallback

connectMouseAxisCallback :: MousePtr -> MouseAxisCallback -> IO (Maybe (SGE.Signal.ConnectionPtr, RawMouseAxisCallback))
connectMouseAxisCallback mouse callback =
                            withForeignPtr mouse $ \cp -> do
                                           rawCallback <- wrapMouseAxisCallback $ \axis -> \value -> \_ -> callback (toEnum $ fromCInt axis) (fromCLong value)
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
