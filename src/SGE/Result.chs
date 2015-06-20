module SGE.Result (
       Result(..)
)

#include <sgec/result.h>

where

import Data.Eq ( Eq )
import Prelude ( Enum )
import Text.Show ( Show )

{#enum sgec_result as Result {underscoreToCase} with prefix = "sgec_result" add prefix = "Result" deriving (Eq, Show)#}
