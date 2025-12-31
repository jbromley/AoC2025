{-|
Module      : Advent
Description : Solution helper library
Copyright   : (c) Jay Bromley, 2025
License     : ISC
Maintainer  : jbromley@gmail.com

This module re-exports the most commonly used modules.

* "Advent.Prelude" is full of useful helper functions
* "Advent.RangeList" maintains a list of ranges

-}
module Advent
  ( module Advent.Prelude
  , module Advent.RangeList
  ) where

import Advent.Prelude
import Advent.RangeList
