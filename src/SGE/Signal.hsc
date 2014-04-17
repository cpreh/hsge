{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Signal (
	ConnectionPtr,
	RawConnectionPtr,
	sgeDestroyConnection
)

#include <sgec/signal/connection.h>

where

import Foreign ( ForeignPtr )

import Foreign.Ptr ( Ptr, FunPtr )

import System.IO ( IO )

data ConnectionStruct = ConnectionStruct

type RawConnectionPtr = Ptr ConnectionStruct

type ConnectionPtr = ForeignPtr ConnectionStruct

foreign import ccall unsafe "&sgec_signal_connection_destroy" sgeDestroyConnection :: FunPtr (RawConnectionPtr -> IO ())
