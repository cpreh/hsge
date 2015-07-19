{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Systems (
       CursorOption(..),
       InstancePtr,
       audioLoader,
       audioPlayer,
       cursor,
       fontSystem,
       imageSystem,
       keyboard,
       mouse,
       renderer,
       windowSystem,
       with
)

#include <sgec/systems/cursor_option.h>
#include <sgec/systems/instance.h>

where

import Control.Exception( bracket )
import Control.Monad ( (>>=) )
import Data.Eq ( Eq )
import Data.Function ( ($) )
import Data.Maybe ( Maybe, fromMaybe )
import Data.String ( String )
import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.C ( CString, withCString )
import Foreign.C.Types ( CInt(..), CUInt(..) )
import Foreign.Marshal.Utils ( maybePeek )
import Foreign.Ptr ( Ptr )
import Prelude ( Enum (fromEnum)  )
import System.IO ( IO )
import System.IO.Unsafe ( unsafeDupablePerformIO )
import Text.Show ( Show )

import qualified SGE.Audio ( LoaderPtr, PlayerPtr, RawLoaderPtr, RawPlayerPtr )
import qualified SGE.Font ( RawSystemPtr, SystemPtr )
import qualified SGE.Image2D ( RawSystemPtr, SystemPtr )
import qualified SGE.Input ( CursorPtr, KeyboardPtr, MousePtr, RawCursorPtr, RawKeyboardPtr, RawMousePtr )
import qualified SGE.Renderer ( RawDevicePtr, DevicePtr )
import SGE.Dim ( Dim(..), dimW, dimH )
import SGE.Utils ( failMaybe, toCInt, toCUInt )
import qualified SGE.Window ( RawSystemPtr, SystemPtr )

data InstanceStruct
type RawInstancePtr = Ptr InstanceStruct
type InstancePtr = ForeignPtr InstanceStruct

{#enum sgec_systems_cursor_option as CursorOption {underscoreToCase} with prefix = "sgec_systems_cursor_option" add prefix = "CursorOption" deriving (Eq, Show)#}

foreign import ccall unsafe "sgec_systems_instance_create" sgeSystemsCreate :: CString -> CUInt -> CUInt -> CInt -> IO RawInstancePtr

foreign import ccall unsafe "sgec_systems_instance_destroy" sgeSystemsDestroy :: RawInstancePtr -> IO ()

create :: String -> Maybe Dim -> CursorOption -> IO (Maybe InstancePtr)
create title dim cursorOption =
       let realDim = fromMaybe (Dim (0,0)) dim in
       withCString title $ \titlePtr ->
       sgeSystemsCreate titlePtr (toCUInt (dimW realDim)) (toCUInt (dimH realDim)) (toCInt (fromEnum cursorOption))
       >>= maybePeek newForeignPtr_

createExn :: String -> Maybe Dim -> CursorOption -> IO (InstancePtr)
createExn title dim cursorOption = failMaybe "create system instance" (create title dim cursorOption)

destroy :: InstancePtr -> IO ()
destroy ptr = withForeignPtr ptr sgeSystemsDestroy

with :: String -> Maybe Dim -> CursorOption -> (InstancePtr -> IO a) -> IO a
with title dim cursorOption func =
     bracket (createExn title dim cursorOption) destroy func

extractSystem :: (RawInstancePtr -> Ptr a) -> InstancePtr -> ForeignPtr a
extractSystem func inst =
              unsafeDupablePerformIO $ withForeignPtr inst $ \ptr -> newForeignPtr_ (func ptr)

foreign import ccall unsafe "sgec_systems_instance_renderer" sgeSystemsRenderer :: RawInstancePtr -> SGE.Renderer.RawDevicePtr

renderer :: InstancePtr -> SGE.Renderer.DevicePtr
renderer = extractSystem sgeSystemsRenderer

foreign import ccall unsafe "sgec_systems_instance_window_system" sgeSystemsWindowSystem :: RawInstancePtr -> SGE.Window.RawSystemPtr

windowSystem :: InstancePtr -> SGE.Window.SystemPtr
windowSystem = extractSystem sgeSystemsWindowSystem

foreign import ccall unsafe "sgec_systems_instance_cursor" sgeSystemsCursor :: RawInstancePtr -> SGE.Input.RawCursorPtr

cursor :: InstancePtr -> SGE.Input.CursorPtr
cursor = extractSystem sgeSystemsCursor

foreign import ccall unsafe "sgec_systems_instance_keyboard" sgeSystemsKeyboard :: RawInstancePtr -> SGE.Input.RawKeyboardPtr

keyboard :: InstancePtr -> SGE.Input.KeyboardPtr
keyboard = extractSystem sgeSystemsKeyboard

foreign import ccall unsafe "sgec_systems_instance_mouse" sgeSystemsMouse :: RawInstancePtr -> SGE.Input.RawMousePtr

mouse :: InstancePtr -> SGE.Input.MousePtr
mouse = extractSystem sgeSystemsMouse

foreign import ccall unsafe "sgec_systems_instance_image2d_system" sgeSystemsImageSystem :: RawInstancePtr -> SGE.Image2D.RawSystemPtr

imageSystem :: InstancePtr -> SGE.Image2D.SystemPtr
imageSystem = extractSystem sgeSystemsImageSystem

foreign import ccall unsafe "sgec_systems_instance_font_system" sgeSystemsFontSystem :: RawInstancePtr -> SGE.Font.RawSystemPtr

fontSystem :: InstancePtr -> SGE.Font.SystemPtr
fontSystem = extractSystem sgeSystemsFontSystem

foreign import ccall unsafe "sgec_systems_instance_audio_loader" sgeSystemsAudioLoader :: RawInstancePtr -> SGE.Audio.RawLoaderPtr

audioLoader :: InstancePtr -> SGE.Audio.LoaderPtr
audioLoader = extractSystem sgeSystemsAudioLoader

foreign import ccall unsafe "sgec_systems_instance_audio_player" sgeSystemsAudioPlayer :: RawInstancePtr -> SGE.Audio.RawPlayerPtr

audioPlayer :: InstancePtr -> SGE.Audio.PlayerPtr
audioPlayer = extractSystem sgeSystemsAudioPlayer
