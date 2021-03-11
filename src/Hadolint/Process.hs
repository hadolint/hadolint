module Hadolint.Process (run, RulesConfig (..)) where

import qualified Control.Foldl as Foldl
import qualified Data.IntMap.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Hadolint.Ignore
import Hadolint.Rule (CheckFailure (..), Failures, RuleCode)
import qualified Hadolint.Rule.DL3000
import qualified Hadolint.Rule.DL3001
import qualified Hadolint.Rule.DL3002
import qualified Hadolint.Rule.DL3003
import qualified Hadolint.Rule.DL3004
import qualified Hadolint.Rule.DL3005
import qualified Hadolint.Rule.DL3006
import qualified Hadolint.Rule.DL3007
import qualified Hadolint.Rule.DL3008
import qualified Hadolint.Rule.DL3009
import qualified Hadolint.Rule.DL3010
import qualified Hadolint.Rule.DL3011
import qualified Hadolint.Rule.DL3013
import qualified Hadolint.Rule.DL3014
import qualified Hadolint.Rule.DL3015
import qualified Hadolint.Rule.DL3016
import qualified Hadolint.Rule.DL3017
import qualified Hadolint.Rule.DL3018
import qualified Hadolint.Rule.DL3019
import qualified Hadolint.Rule.DL3020
import qualified Hadolint.Rule.DL3021
import qualified Hadolint.Rule.DL3022
import qualified Hadolint.Rule.DL3023
import qualified Hadolint.Rule.DL3024
import qualified Hadolint.Rule.DL3025
import qualified Hadolint.Rule.DL3026
import qualified Hadolint.Rule.DL3027
import qualified Hadolint.Rule.DL3028
import qualified Hadolint.Rule.DL3029
import qualified Hadolint.Rule.DL3030
import qualified Hadolint.Rule.DL3031
import qualified Hadolint.Rule.DL3032
import qualified Hadolint.Rule.DL3033
import qualified Hadolint.Rule.DL3034
import qualified Hadolint.Rule.DL3035
import qualified Hadolint.Rule.DL3036
import qualified Hadolint.Rule.DL3037
import qualified Hadolint.Rule.DL3038
import qualified Hadolint.Rule.DL3039
import qualified Hadolint.Rule.DL3040
import qualified Hadolint.Rule.DL3041
import qualified Hadolint.Rule.DL3042
import qualified Hadolint.Rule.DL3043
import qualified Hadolint.Rule.DL3044
import qualified Hadolint.Rule.DL3045
import qualified Hadolint.Rule.DL3046
import qualified Hadolint.Rule.DL4000
import qualified Hadolint.Rule.DL4001
import qualified Hadolint.Rule.DL4003
import qualified Hadolint.Rule.DL4004
import qualified Hadolint.Rule.DL4005
import qualified Hadolint.Rule.DL4006
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

-- | Contains the required parameters for optional rules
newtype RulesConfig = RulesConfig
  { -- | The docker registries that are allowed in FROM
    allowedRegistries :: Set.Set Registry
  }
  deriving (Show, Eq)

instance Semigroup RulesConfig where
  RulesConfig a <> RulesConfig b = RulesConfig (a <> b)

instance Monoid RulesConfig where
  mempty = RulesConfig mempty

data AnalisisResult = AnalisisResult
  { -- | The set of ignored rules per line
    ignored :: Map.IntMap (Set.Set RuleCode),
    -- | A set of failures collected for reach rule
    failed :: Seq.Seq Failures
  }

run :: RulesConfig -> [InstructionPos Text.Text] -> Failures
run config dockerfile = Seq.filter shouldKeep (Foldl.fold Foldl.mconcat failed)
  where
    AnalisisResult {..} = Foldl.fold (analyze config) dockerfile

    shouldKeep CheckFailure {line, code} = fromMaybe True $ do
      ignoreList <- Map.lookup line ignored
      return $ not $ code `Set.member` ignoreList

analyze :: RulesConfig -> Foldl.Fold (InstructionPos Text.Text) AnalisisResult
analyze config =
  AnalisisResult
    <$> Hadolint.Ignore.ignored
    <*> Foldl.premap parseShell (failures config <> onBuildFailures config)

parseShell :: InstructionPos Text.Text -> InstructionPos Shell.ParsedShell
parseShell = fmap Shell.parseShell

onBuildFailures :: RulesConfig -> Foldl.Fold (InstructionPos Shell.ParsedShell) (Seq.Seq Failures)
onBuildFailures config =
  Foldl.prefilter
    isOnBuild
    (Foldl.premap unwrapOnbuild (failures config))
  where
    isOnBuild InstructionPos {instruction = OnBuild {}} = True
    isOnBuild _ = False

    unwrapOnbuild inst@InstructionPos {instruction = OnBuild i} = inst {instruction = i}
    unwrapOnbuild inst = inst

failures :: RulesConfig -> Foldl.Fold (InstructionPos Shell.ParsedShell) (Seq.Seq Failures)
failures RulesConfig {allowedRegistries} =
  sequenceA $
    Seq.fromList
      [ Hadolint.Rule.DL3000.rule,
        Hadolint.Rule.DL3001.rule,
        Hadolint.Rule.DL3002.rule,
        Hadolint.Rule.DL3003.rule,
        Hadolint.Rule.DL3004.rule,
        Hadolint.Rule.DL3005.rule,
        Hadolint.Rule.DL3006.rule,
        Hadolint.Rule.DL3007.rule,
        Hadolint.Rule.DL3008.rule,
        Hadolint.Rule.DL3009.rule,
        Hadolint.Rule.DL3010.rule,
        Hadolint.Rule.DL3011.rule,
        Hadolint.Rule.DL3013.rule,
        Hadolint.Rule.DL3014.rule,
        Hadolint.Rule.DL3015.rule,
        Hadolint.Rule.DL3016.rule,
        Hadolint.Rule.DL3017.rule,
        Hadolint.Rule.DL3018.rule,
        Hadolint.Rule.DL3019.rule,
        Hadolint.Rule.DL3020.rule,
        Hadolint.Rule.DL3021.rule,
        Hadolint.Rule.DL3022.rule,
        Hadolint.Rule.DL3023.rule,
        Hadolint.Rule.DL3024.rule,
        Hadolint.Rule.DL3025.rule,
        Hadolint.Rule.DL3026.rule allowedRegistries,
        Hadolint.Rule.DL3027.rule,
        Hadolint.Rule.DL3028.rule,
        Hadolint.Rule.DL3029.rule,
        Hadolint.Rule.DL3030.rule,
        Hadolint.Rule.DL3031.rule,
        Hadolint.Rule.DL3032.rule,
        Hadolint.Rule.DL3033.rule,
        Hadolint.Rule.DL3034.rule,
        Hadolint.Rule.DL3035.rule,
        Hadolint.Rule.DL3036.rule,
        Hadolint.Rule.DL3037.rule,
        Hadolint.Rule.DL3038.rule,
        Hadolint.Rule.DL3039.rule,
        Hadolint.Rule.DL3040.rule,
        Hadolint.Rule.DL3041.rule,
        Hadolint.Rule.DL3042.rule,
        Hadolint.Rule.DL3043.rule,
        Hadolint.Rule.DL3044.rule,
        Hadolint.Rule.DL3045.rule,
        Hadolint.Rule.DL3046.rule,
        Hadolint.Rule.DL4000.rule,
        Hadolint.Rule.DL4001.rule,
        Hadolint.Rule.DL4003.rule,
        Hadolint.Rule.DL4004.rule,
        Hadolint.Rule.DL4005.rule,
        Hadolint.Rule.DL4006.rule
      ]
