{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}
module SpecHelper (module X) where

import BasicPrelude as X hiding ((<.>))
import Control.Lens as X
import Data.Thyme.Calendar as X
import Data.Thyme.Clock as X
import Test.Hspec      as X
import Text.InterpolatedString.Perl6 as X (q)
