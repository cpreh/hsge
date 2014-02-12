module SGE.Utils (
	failMaybe,
	failResult,
	failResultIO,
	failWithMessage,
	fromCInt,
	toCInt,
	toCUInt,
	toResult
)

where

import Control.Monad ( liftM )

import Foreign.C ( CInt(..), CUInt(..) )

import SGE.Types ( Result(..) )

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

toCInt :: Integral a => a -> CInt
toCInt = fromIntegral

toResult :: Int -> Result
toResult = toEnum
