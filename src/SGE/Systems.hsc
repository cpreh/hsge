{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Systems (
       InstancePtr,
       audioLoader,
       fontSystem,
       imageSystem,
       keyboard,
       renderer,
       windowSystem,
       with
)

#include <sgec/systems/instance.h>

where

import Control.Exception( bracket )
import Control.Monad ( (>>=) )
import Data.Function ( ($) )
import Data.Maybe ( Maybe, fromMaybe )
import Data.String ( String )
import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )
import Foreign.C ( CString, withCString )
import Foreign.C.Types ( CUInt(..) )
import Foreign.Marshal.Utils ( maybePeek )
import Foreign.Ptr ( Ptr )
import System.IO ( IO )
import System.IO.Unsafe ( unsafeDupablePerformIO )

import qualified SGE.Audio ( RawLoaderPtr, LoaderPtr )
import qualified SGE.Font ( RawSystemPtr, SystemPtr )
import qualified SGE.Image2D ( RawSystemPtr, SystemPtr )
import qualified SGE.Input ( RawKeyboardPtr, KeyboardPtr )
import qualified SGE.Renderer ( RawDevicePtr, DevicePtr )
import SGE.Dim ( Dim(..), dimW, dimH )
import SGE.Utils ( failMaybe, toCUInt )
import qualified SGE.Window ( RawSystemPtr, SystemPtr )

data InstanceStruct

type RawInstancePtr = Ptr InstanceStruct

type InstancePtr = ForeignPtr InstanceStruct

foreign import ccall unsafe "sgec_systems_instance_create" sgeSystemsCreate :: CString -> CUInt -> CUInt -> IO RawInstancePtr

foreign import ccall unsafe "sgec_systems_instance_destroy" sgeSystemsDestroy :: RawInstancePtr -> IO ()

create :: String -> Maybe Dim -> IO (Maybe InstancePtr)
create title dim =
       let realDim = fromMaybe (Dim (0,0)) dim in
       withCString title $ \titlePtr ->
       sgeSystemsCreate titlePtr (toCUInt (dimW realDim)) (toCUInt (dimH realDim))
       >>= maybePeek newForeignPtr_

createExn :: String -> Maybe Dim -> IO (InstancePtr)
createExn title dim = failMaybe "create system instance" (create title dim)

destroy :: InstancePtr -> IO ()
destroy ptr = withForeignPtr ptr sgeSystemsDestroy

with :: String -> Maybe Dim -> (InstancePtr -> IO a) -> IO a
with title dim func =
     bracket (createExn title dim) destroy func

extractSystem :: (RawInstancePtr -> Ptr a) -> InstancePtr -> ForeignPtr a
extractSystem func inst =
              unsafeDupablePerformIO $ withForeignPtr inst $ \ptr -> newForeignPtr_ (func ptr)

foreign import ccall unsafe "sgec_systems_instance_renderer" sgeSystemsRenderer :: RawInstancePtr -> SGE.Renderer.RawDevicePtr

renderer :: InstancePtr -> SGE.Renderer.DevicePtr
renderer = extractSystem sgeSystemsRenderer

foreign import ccall unsafe "sgec_systems_instance_window_system" sgeSystemsWindowSystem :: RawInstancePtr -> SGE.Window.RawSystemPtr

windowSystem :: InstancePtr -> SGE.Window.SystemPtr
windowSystem = extractSystem sgeSystemsWindowSystem

foreign import ccall unsafe "sgec_systems_instance_keyboard" sgeSystemsKeyboard :: RawInstancePtr -> SGE.Input.RawKeyboardPtr

keyboard :: InstancePtr -> SGE.Input.KeyboardPtr
keyboard = extractSystem sgeSystemsKeyboard

foreign import ccall unsafe "sgec_systems_instance_image2d_system" sgeSystemsImageSystem :: RawInstancePtr -> SGE.Image2D.RawSystemPtr

imageSystem :: InstancePtr -> SGE.Image2D.SystemPtr
imageSystem = extractSystem sgeSystemsImageSystem

foreign import ccall unsafe "sgec_systems_instance_font_system" sgeSystemsFontSystem :: RawInstancePtr -> SGE.Font.RawSystemPtr

fontSystem :: InstancePtr -> SGE.Font.SystemPtr
fontSystem = extractSystem sgeSystemsFontSystem

foreign import ccall unsafe "sgec_systems_instance_audio_loader" sgeSystemsAudioLoader :: RawInstancePtr -> SGE.Audio.RawLoaderPtr

audioLoader :: InstancePtr -> SGE.Audio.LoaderPtr
audioLoader = extractSystem sgeSystemsAudioLoader
