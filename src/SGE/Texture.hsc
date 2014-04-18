{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Texture (
	PartPtr,
	RawPartPtr,
	destroyPart,
	partRaw,
	partRawExn,
	withPartRaw
)

#include <sgec/texture/part.h>
#include <sgec/texture/part_raw.h>

where

import Control.Exception( bracket )

import Control.Monad ( (>>=) )

import Data.Function ( ($) )

import Data.Maybe ( Maybe )

import Foreign ( ForeignPtr, newForeignPtr_, withForeignPtr )

import Foreign.Ptr ( Ptr )

import Foreign.Marshal.Utils ( maybePeek )

import SGE.Renderer ( PlanarTexturePtr, RawPlanarTexturePtr )

import SGE.Utils ( failMaybe )

import System.IO ( IO )

data PartStruct = PartStruct
type RawPartPtr = Ptr PartStruct
type PartPtr = ForeignPtr PartStruct

foreign import ccall unsafe "sgec_texture_part_raw" sgeTexturePartRaw :: RawPlanarTexturePtr -> IO RawPartPtr

partRaw :: PlanarTexturePtr -> IO (Maybe PartPtr)
partRaw texture =
	withForeignPtr texture $ \ptex ->
	sgeTexturePartRaw ptex
	>>= maybePeek newForeignPtr_

partRawExn :: PlanarTexturePtr -> IO PartPtr
partRawExn texture =
	failMaybe "partRaw" (partRaw texture)

foreign import ccall unsafe "sgec_texture_part_destroy" sgeDestroyTexturePart :: RawPartPtr -> IO ()

destroyPart :: PartPtr -> IO ()
destroyPart texture =
	withForeignPtr texture sgeDestroyTexturePart

withPartRaw :: PlanarTexturePtr -> (PartPtr -> IO a) -> IO a
withPartRaw texture function =
	bracket (partRawExn texture) destroyPart function
