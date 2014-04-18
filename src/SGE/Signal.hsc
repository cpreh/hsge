{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Signal (
	ConnectionPtr,
	RawConnectionPtr,
	destroy
)

#include <sgec/signal/connection.h>

where

import Foreign ( ForeignPtr, withForeignPtr )

import Foreign.Ptr ( Ptr )

import System.IO ( IO )

data ConnectionStruct = ConnectionStruct

type RawConnectionPtr = Ptr ConnectionStruct

type ConnectionPtr = ForeignPtr ConnectionStruct

foreign import ccall unsafe "sgec_signal_connection_destroy" sgeDestroyConnection :: RawConnectionPtr -> IO ()

destroy :: ConnectionPtr -> IO ()
destroy sig =
	withForeignPtr sig sgeDestroyConnection
