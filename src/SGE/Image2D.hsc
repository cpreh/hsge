{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Image2D (
	RawSystemPtr,
	SystemPtr
)

#include <sgec/image2d/system_fwd.h>

where

import Foreign ( ForeignPtr )

import Foreign.Ptr ( Ptr )

data SystemStruct = SystemeStruct
type RawSystemPtr = Ptr SystemStruct
type SystemPtr = ForeignPtr SystemStruct
