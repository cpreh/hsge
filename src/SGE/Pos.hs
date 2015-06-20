module SGE.Pos (
       Pos(..),
       posX,
       posY
)

where

import Data.Eq ( Eq )
import Data.Int ( Int )

data Pos = Pos (Int, Int)
     deriving(Eq)

posX :: Pos -> Int
posX (Pos (x, _)) = x

posY :: Pos -> Int
posY (Pos (_, y)) = y
