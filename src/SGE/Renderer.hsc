{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Renderer (
	ContextPtr,
	DevicePtr,
	PlanarTexturePtr,
	RawContextPtr,
	RawDevicePtr,
	RawPlanarTexturePtr,
	beginRendering,
	beginRenderingExn,
	endRendering,
	planarTextureFromPath,
	planarTextureFromPathExn
)

#include <sgec/renderer/context/ffp.h>
#include <sgec/renderer/device/ffp.h>
#include <sgec/renderer/texture/create_planar_from_path.h>
#include <sgec/renderer/texture/planar.h>

where

import Control.Monad ( (>>=) )

import Data.Function ( ($) )

import Data.Maybe ( Maybe )

import Data.String ( String )

import Foreign ( ForeignPtr, newForeignPtr, withForeignPtr )

import Foreign.Marshal.Utils ( maybePeek )

import Foreign.C ( CInt(..), CString(..), withCString )

import Foreign.Ptr ( FunPtr, Ptr )

import SGE.Image2D ( RawSystemPtr, SystemPtr )

import SGE.Utils ( failMaybe, failResultIO )

import System.IO ( IO )

data DeviceStruct = DeviceStruct
type RawDevicePtr = Ptr DeviceStruct
type DevicePtr = ForeignPtr DeviceStruct

data ContextStruct = ContextStruct
type RawContextPtr = Ptr ContextStruct
type ContextPtr = ForeignPtr ContextStruct

data PlanarTextureStruct = PlanarTextureStruct
type RawPlanarTexturePtr = Ptr PlanarTextureStruct
type PlanarTexturePtr = ForeignPtr PlanarTextureStruct

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

foreign import ccall unsafe "sgec_renderer_texture_create_planar_from_path" sgeCreatePlanarTextureFromPath :: RawDevicePtr -> SGE.Image2D.RawSystemPtr -> CString -> IO RawPlanarTexturePtr

foreign import ccall unsafe "&sgec_renderer_texture_planar_destroy" sgeDestroyPlanarTexture :: FunPtr (RawPlanarTexturePtr -> IO ())

planarTextureFromPath :: DevicePtr -> SGE.Image2D.SystemPtr -> String -> IO (Maybe PlanarTexturePtr)
planarTextureFromPath device imagesys path =
	withForeignPtr device $ \dp ->
	withForeignPtr imagesys $ \sp ->
	withCString path $ \pp ->
	sgeCreatePlanarTextureFromPath dp sp pp
	>>= maybePeek (newForeignPtr sgeDestroyPlanarTexture)

planarTextureFromPathExn :: DevicePtr -> SGE.Image2D.SystemPtr -> String -> IO PlanarTexturePtr
planarTextureFromPathExn device imagesys path =
	failMaybe "loading a planar texture" (planarTextureFromPath device imagesys path)
