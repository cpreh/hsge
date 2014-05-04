{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Systems (
	InstancePtr,
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

import Data.Int ( Int )

import Data.Maybe ( Maybe )

import Data.String ( String )

import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )

import Foreign.C ( CUInt(..), CString, withCString )

import Foreign.Marshal.Utils ( maybePeek )

import Foreign.Ptr ( Ptr )

import System.IO.Unsafe ( unsafePerformIO )

import qualified SGE.Image2D( RawSystemPtr, SystemPtr )

import qualified SGE.Input ( RawKeyboardPtr, KeyboardPtr )

import qualified SGE.Renderer ( RawDevicePtr, DevicePtr )

import SGE.Utils ( failMaybe, toCUInt )

import qualified SGE.Window ( RawSystemPtr, SystemPtr )

import System.IO ( IO )

data InstanceStruct

type RawInstancePtr = Ptr InstanceStruct

type InstancePtr = ForeignPtr InstanceStruct

foreign import ccall unsafe "sgec_systems_instance_create" sgeSystemsCreate :: CString -> CUInt -> CUInt -> IO RawInstancePtr

foreign import ccall unsafe "sgec_systems_instance_destroy" sgeSystemsDestroy :: RawInstancePtr -> IO ()

create :: String -> Int -> Int -> IO (Maybe InstancePtr)
create title w h =
	(withCString title $ \titlePtr ->
	sgeSystemsCreate titlePtr (toCUInt w) (toCUInt h))
	>>= maybePeek newForeignPtr_

createExn :: String -> Int -> Int -> IO (InstancePtr)
createExn title w h = failMaybe "create system instance" (create title w h)

destroy :: InstancePtr -> IO ()
destroy ptr = withForeignPtr ptr sgeSystemsDestroy

with :: String -> Int -> Int -> (InstancePtr -> IO a) -> IO a
with title w h func =
	bracket (createExn title w h) destroy func

extractSystem :: (RawInstancePtr -> Ptr a) -> InstancePtr -> ForeignPtr a
extractSystem func inst =
	unsafePerformIO $ withForeignPtr inst $ \ptr -> newForeignPtr_ (func ptr)

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
