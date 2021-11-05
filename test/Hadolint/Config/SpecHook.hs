module Hadolint.Config.SpecHook (hook) where

import System.Environment
import Test.Hspec


hook :: SpecWith () -> SpecWith ()
hook = before_ (setEnv "BAR" "bar")
