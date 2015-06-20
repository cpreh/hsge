module SGE.Dim (
       Dim(..),
       dimW,
       dimH
)

where

import Data.Eq ( Eq )
import Data.Int ( Int )

data Dim = Dim (Int, Int)
     deriving(Eq)

dimW :: Dim -> Int
dimW (Dim (w, _)) = w

dimH :: Dim -> Int
dimH (Dim (_, h)) = h
