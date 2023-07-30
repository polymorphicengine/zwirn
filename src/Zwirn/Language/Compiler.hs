module Zwirn.Language.Compiler
    (
    ) where

import Zwirn.Language.Simple
import Zwirn.Language.Rotate
import Zwirn.Language.Generator
import Zwirn.Language.TypeCheck.Env
import Zwirn.Language.TypeCheck.Constraint

import Control.Monad.Except
import Control.Monad.State

data Environment = Environment { typeEnv :: TypeEnv } deriving (Show, Eq)

data CompilationError
  = ParserE String
  | FixityE RotationError
  | TypeE TypeError

type Compile a = StateT
                  Environment                 -- keeps track of changing type/fixity environment
                  (Except CompilationError)   -- Error
                  a                           -- Result
