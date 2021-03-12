module Hadolint.Rule where

import Control.DeepSeq (NFData)
import qualified Control.Foldl as Foldl
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Language.Docker.Syntax

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

data DLSeverity
  = DLErrorC
  | DLWarningC
  | DLInfoC
  | DLStyleC
  | DLIgnoreC
  deriving (Show, Eq, Ord, Generic, NFData)

newtype RuleCode = RuleCode {unRuleCode :: Text.Text}
  deriving (Show, Eq, Ord)

instance IsString RuleCode where
  fromString = RuleCode . Text.pack

data CheckFailure = CheckFailure
  { code :: RuleCode,
    severity :: DLSeverity,
    message :: Text.Text,
    line :: Linenumber
  }
  deriving (Show, Eq)

instance Ord CheckFailure where
  a `compare` b = line a `compare` line b

type Failures = Seq.Seq CheckFailure

data State a = State
  { failures :: Failures,
    state :: a
  }
  deriving (Show)

withLineNumber ::
  (Linenumber -> t1 -> Instruction args -> t2) ->
  t1 ->
  InstructionPos args ->
  t2
withLineNumber f state InstructionPos {instruction, lineNumber} =
  f lineNumber state instruction

addFail :: CheckFailure -> State a -> State a
addFail failure state@(State fails _) =
  state
    { failures =
        fails
          Seq.|> failure
    }

emptyState :: a -> State a
emptyState st = State Seq.empty st

simpleState :: State ()
simpleState = State Seq.empty ()

modify :: (a -> a) -> State a -> State a
modify f s@(State _ st) = s {state = f st}

replaceWith :: a -> State a -> State a
replaceWith newState s = s {state = newState}

type Rule args = Foldl.Fold (InstructionPos args) Failures

simpleRule :: RuleCode -> DLSeverity -> Text.Text -> (Instruction args -> Bool) -> Rule args
simpleRule code severity message checker =
  Foldl.Fold (withLineNumber step) simpleState failures
  where
    step line s instr
      | checker instr = s
      | otherwise = s |> addFail (CheckFailure code severity message line)

customRule ::
  (Linenumber -> State a -> Instruction args -> State a) ->
  State a ->
  Rule args
customRule step initial = Foldl.Fold (withLineNumber step) initial failures

veryCustomRule ::
  (Linenumber -> State a -> Instruction args -> State a) ->
  State a ->
  (State a -> Failures) ->
  Rule args
veryCustomRule step initial extract = Foldl.Fold (withLineNumber step) initial extract

foldArguments :: (a -> b) -> Arguments a -> b
foldArguments applyRule args =
  case args of
    ArgumentsText as -> applyRule as
    ArgumentsList as -> applyRule as

-- | Returns the result of running the check function on the image alias
--   name, if the passed instruction is a FROM instruction with a stage alias.
--   Otherwise, returns True.
aliasMustBe :: (Text.Text -> Bool) -> Instruction a -> Bool
aliasMustBe predicate fromInstr =
  case fromInstr of
    From BaseImage {alias = Just (ImageAlias as)} -> predicate as
    _ -> True
