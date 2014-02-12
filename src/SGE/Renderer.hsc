module SGE.Renderer (
	DevicePtr,
	RawDevicePtr,
	beginRendering,
	beginRenderingExn,
	endRendering
)

#include <sgec/renderer/context/ffp.h>
#include <sgec/renderer/device/ffp.h>

where

import Foreign ( ForeignPtr, newForeignPtr, withForeignPtr )

import Foreign.Marshal.Utils ( maybePeek )

import Foreign.C ( CInt(..) )

import Foreign.Ptr ( FunPtr, Ptr )

import SGE.Utils ( failMaybe, failResultIO )

data DeviceStruct = DeviceStruct

type RawDevicePtr = Ptr DeviceStruct

type DevicePtr = ForeignPtr DeviceStruct

data ContextStruct = ContextStruct

type RawContextPtr = Ptr ContextStruct

type ContextPtr = ForeignPtr ContextStruct

foreign import ccall unsafe "sgec_renderer_device_ffp_begin_rendering" sgeRendererBegin :: RawDevicePtr -> IO (RawContextPtr)

foreign import ccall unsafe "&sgec_renderer_context_ffp_destroy" sgeRendererDestroyContext :: FunPtr (RawContextPtr -> IO ())

beginRendering :: DevicePtr -> IO (Maybe ContextPtr)
beginRendering renderer =
	withForeignPtr renderer $ \ptr ->
	sgeRendererBegin ptr >>= maybePeek (newForeignPtr sgeRendererDestroyContext)

beginRenderingExn :: DevicePtr -> IO ContextPtr
beginRenderingExn renderer =
	failMaybe "begin rendering" (beginRendering renderer)

foreign import ccall unsafe "sgec_renderer_device_ffp_end_rendering" sgeRendererEnd :: RawDevicePtr -> RawContextPtr -> IO (CInt)

endRendering :: DevicePtr -> ContextPtr -> IO ()
endRendering renderer context =
	withForeignPtr renderer $ \rp ->
	withForeignPtr context $ \cp ->
	failResultIO "end rendering" $ sgeRendererEnd rp cp

foreign import ccall unsafe "sgec_renderer_context_ffp_clear" sgeRendererClear :: RawContextPtr -> IO (CInt)

clear :: ContextPtr -> IO ()
clear context =
	withForeignPtr context $ \cp ->
	failResultIO "renderer clear" $ sgeRendererClear cp
