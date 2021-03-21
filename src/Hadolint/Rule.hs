module Hadolint.Rule where

import Control.DeepSeq (NFData)
import qualified Control.Foldl as Foldl
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import qualified Data.Text as Text
import qualified Data.YAML as Yaml
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

type LabelName = Text.Text

data LabelType
  = RawText
  | Url
  | Spdx
  | GitHash
  | Rfc3339
  | SemVer
  deriving (Eq, Read, Show)

read :: Text.Text -> Either Text.Text LabelType
read "url"     = Right Url
read "spdx"    = Right Spdx
read "hash"    = Right GitHash
read "rfc3339" = Right Rfc3339
read "semver"  = Right SemVer
read "text"    = Right RawText
read ""        = Right RawText
read t         = Left ("Invalid label type: " <> t)

instance Yaml.FromYAML LabelType where
  parseYAML = withLabelType pure

withLabelType :: (LabelType -> Yaml.Parser a) -> Yaml.Node Yaml.Pos -> Yaml.Parser a
withLabelType f v@(Yaml.Scalar _ (Yaml.SStr b)) =
    case Hadolint.Rule.read b of
      Right lt -> f lt
      Left _ -> Yaml.typeMismatch "labeltype" v
withLabelType _ v = Yaml.typeMismatch "labeltype" v

type LabelSchema = Map.Map LabelName LabelType


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
emptyState = State Seq.empty

simpleState :: State ()
simpleState = State Seq.empty ()

modify :: (a -> a) -> State a -> State a
modify f s@(State _ st) = s {state = f st}

replaceWith :: a -> State a -> State a
replaceWith newState s = s {state = newState}

type Rule args = Foldl.Fold (InstructionPos args) Failures

-- | A simple rule that can be implemented in terms of returning True or False for each instruction
-- If you need to calculate some state to decide upon past information, use 'customRule'
simpleRule ::
  -- | rule code
  RuleCode ->
  -- | severity for the rule
  DLSeverity ->
  -- | failure message for the rule
  Text.Text ->
  -- | step calculation for the rule. Returns True or False for each line in the dockerfile depending on its validity.
  (Instruction args -> Bool) ->
  Rule args
simpleRule code severity message checker = customRule step simpleState
  where
    step line s instr
      | checker instr = s
      | otherwise = s |> addFail (CheckFailure code severity message line)

-- | A rule that accumulates a State a. The state contains the collection of failed lines and a custom data
-- type that can be used to track properties for the rule. Each step always returns the new State, which offers
-- the ability to both accumulate properties and mark failures for every given instruction.
customRule ::
  (Linenumber -> State a -> Instruction args -> State a) ->
  State a ->
  Rule args
customRule step initial = veryCustomRule step initial failures

-- | Similarly to 'customRule', it returns a State a for each step, but it has the ability to run a
-- done callback as the last step of the rule. The done callback can be used to transform the state
-- and mark failures for any arbitrary line in the input. This helper is meant for rules that need
-- to do lookahead. Instead of looking ahead, the state should store the facts and make a decision about
-- them once the input is finished.
veryCustomRule ::
  -- | step calculation for the rule. Called for each instruction in the docker file
  -- it must return the state after being modified by the rule
  (Linenumber -> State a -> Instruction args -> State a) ->
  -- | initial state
  State a ->
  -- | done callaback. It is passed the final accumulated state and it should return all failures
  -- found by the rule
  (State a -> Failures) ->
  Rule args
veryCustomRule step = Foldl.Fold (withLineNumber step)

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
