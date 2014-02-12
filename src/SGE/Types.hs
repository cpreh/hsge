module SGE.Types (
	Result(..)
)

where

data Result =
	ResultOk
	| ResultError
	deriving(Eq)

instance Enum Result where
	fromEnum ResultOk = 0
	fromEnum ResultError = 1

	toEnum 0 = ResultOk
	toEnum 1 = ResultError
	toEnum _ = error "Invalid result"
