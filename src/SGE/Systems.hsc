module SGE.Systems (
	InstancePtr,
	create,
	createExn,
	renderer
)

#include <sgec/systems/instance.h>

where

import Foreign ( ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr )

import Foreign.C ( CInt(..), CUInt(..), CString(..), withCString )

import Foreign.Marshal.Utils ( maybePeek )

import Foreign.Ptr ( FunPtr, Ptr )

import System.IO.Unsafe ( unsafePerformIO )

import SGE.Renderer ( RawDevicePtr, DevicePtr )

import SGE.Utils ( failMaybe, toCUInt )

import SGE.Window ( RawSystemPtr, SystemPtr )

data InstanceStruct = InstanceStruct

type RawInstancePtr = Ptr InstanceStruct

type InstancePtr = ForeignPtr InstanceStruct

foreign import ccall unsafe "sgec_systems_instance_create" sgeSystemsCreate :: CString -> CUInt -> CUInt -> IO RawInstancePtr

foreign import ccall unsafe "&sgec_systems_instance_destroy" sgeSystemsDestroy :: FunPtr (RawInstancePtr -> IO ())

create :: String -> Int -> Int -> IO (Maybe InstancePtr)
create title w h =
	(withCString title $ \titlePtr ->
	sgeSystemsCreate titlePtr (toCUInt w) (toCUInt h))
	>>= maybePeek (newForeignPtr sgeSystemsDestroy)

createExn :: String -> Int -> Int -> IO (InstancePtr)
createExn title w h = failMaybe "create system instance" (create title w h)

foreign import ccall unsafe "sgec_systems_instance_renderer" sgeSystemsRenderer :: RawInstancePtr -> RawDevicePtr

renderer :: InstancePtr -> DevicePtr
renderer inst =
	unsafePerformIO $ withForeignPtr inst $ \ptr -> newForeignPtr_ (sgeSystemsRenderer ptr)

foreign import ccall unsafe "sgec_systems_instance_window_system" sgeSystemsWindowSystem :: RawInstancePtr -> RawSystemPtr

windowSystem :: InstancePtr -> SystemPtr
windowSystem inst =
	unsafePerformIO $ withForeignPtr inst $ \ptr -> newForeignPtr_ (sgeSystemsWindowSystem ptr)
