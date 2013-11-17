module SGE.Systems (
	InstancePtr,
	create
)

#include <sgec/systems/instance.h>

where

import Foreign ( FunPtr(..), FinalizerPtr, ForeignPtr, addForeignPtrFinalizer, newForeignPtr )

import Foreign.C ( CUInt(..), CString(..), withCString )

import Foreign.Ptr ( FunPtr, Ptr )

import Foreign.Marshal.Utils ( maybePeek )

toCUInt :: Integral a => a -> CUInt
toCUInt = fromIntegral

data InstanceStruct = InstanceStruct

type InstancePtr = ForeignPtr InstanceStruct

foreign import ccall unsafe "sgec_systems_create" sgeSystemsCreate :: CString -> CUInt -> CUInt -> IO (Ptr InstanceStruct)

foreign import ccall unsafe "&sgec_systems_destroy" sgeSystemsDestroy :: FunPtr ( Ptr InstanceStruct -> IO ())

create :: String -> Int -> Int -> IO (Maybe InstancePtr)
create title w h =
	(withCString title $ \titlePtr ->
	sgeSystemsCreate titlePtr (toCUInt w) (toCUInt h))
	>>= maybePeek (newForeignPtr sgeSystemsDestroy)
