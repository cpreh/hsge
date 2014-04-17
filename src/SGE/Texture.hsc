{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Texture (
	PartPtr,
	RawPartPtr,
	partRaw,
	partRawExn
)

#include <sgec/texture/part.h>
#include <sgec/texture/part_raw.h>

where

import Control.Monad ( (>>=) )

import Data.Function ( ($) )

import Data.Maybe ( Maybe )

import Foreign ( ForeignPtr, newForeignPtr, withForeignPtr )

import Foreign.Ptr ( FunPtr, Ptr )

import Foreign.Marshal.Utils ( maybePeek )

import SGE.Renderer ( PlanarTexturePtr, RawPlanarTexturePtr )

import SGE.Utils ( failMaybe )

import System.IO ( IO )

data PartStruct = PartStruct
type RawPartPtr = Ptr PartStruct
type PartPtr = ForeignPtr PartStruct

foreign import ccall unsafe "sgec_texture_part_raw" sgeTexturePartRaw :: RawPlanarTexturePtr -> IO RawPartPtr

foreign import ccall unsafe "&sgec_texture_part_destroy" sgeDestroyTexturePart :: FunPtr (RawPartPtr -> IO ())

partRaw :: PlanarTexturePtr -> IO (Maybe PartPtr)
partRaw texture =
	withForeignPtr texture $ \ptex ->
	sgeTexturePartRaw ptex
	>>= maybePeek (newForeignPtr sgeDestroyTexturePart)

partRawExn :: PlanarTexturePtr -> IO PartPtr
partRawExn texture =
	failMaybe "partRaw" (partRaw texture)
