{-# LANGUAGE NoImplicitPrelude #-}

module SGE.Utils (
	failMaybe,
	failResult,
	failResultIO,
	failWithMessage,
	fromCInt,
	toCFloat,
	toCInt,
        toCUChar,
	toCUInt,
	toCSize,
	toResult
)

where

import Control.Monad ( liftM, return )

import Data.Function ( ($) )

import Data.Int ( Int )

import Data.Maybe ( Maybe(Just, Nothing) )

import Data.Word ( Word8 )

import Data.String ( String )

import Foreign.C ( CFloat, CInt, CUChar, CUInt, CSize )

import Prelude ( Enum(toEnum), Float, Integral, fromIntegral, realToFrac )

import SGE.Types ( Result(..) )

import System.IO ( IO )

import System.IO.Error ( ioError, userError )

failWithMessage :: String -> IO a
failWithMessage message =
	ioError $ userError $ message

failMaybe :: String -> IO (Maybe a) -> IO a
failMaybe message action = do
	val <- action
	case val of
		Just a -> return a
		Nothing -> failWithMessage message

failResult :: String -> IO Result -> IO ()
failResult message action = do
	val <- action
	case val of
		ResultOk -> return ()
		ResultError -> failWithMessage message

failResultIO :: String -> IO CInt -> IO ()
failResultIO message action =
	failResult message $ liftM toResult $ liftM fromCInt $ action

fromCInt :: Integral a => CInt -> a
fromCInt = fromIntegral

toCUInt :: Integral a => a -> CUInt
toCUInt = fromIntegral

toCFloat :: Float -> CFloat
toCFloat = realToFrac

toCInt :: Integral a => a -> CInt
toCInt = fromIntegral

toCUChar :: Word8 -> CUChar
toCUChar = fromIntegral

toCSize :: Integral a => a -> CSize
toCSize = fromIntegral

toResult :: Int -> Result
toResult = toEnum
