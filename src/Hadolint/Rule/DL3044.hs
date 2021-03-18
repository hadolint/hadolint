module Hadolint.Rule.DL3044 (rule) where

import Data.List.Index (indexed)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = customRule check (emptyState Set.empty)
  where
    code = "DL3044"
    severity = DLErrorC
    message = "Do not refer to an environment variable within the same `ENV` statement where it is defined."

    check line st (Env pairs) =
      let newState = st |> modify (Set.union (Set.fromList (map fst pairs)))
       in if null [env | env <- listOfReferences pairs, env `Set.notMember` state st]
            then newState
            else newState |> addFail CheckFailure {..}
    check _ st (Arg arg _) = st |> modify (Set.insert arg)
    check _ st _ = st
{-# INLINEABLE rule #-}

-- | generates a list of references to variable names referenced on the right
-- hand side of a variable definition, except when the variable is
-- referenced on its own right hand side.
listOfReferences :: Pairs -> [Text.Text]
listOfReferences prs =
  [ var
    | (idx, (var, _)) <- indexed prs,
      var `isSubstringOfAny` map (snd . snd) (filter ((/= idx) . fst) (indexed prs))
  ]

-- | is a reference of a variable substring of any text?
-- matches ${var_name} and $var_name, but not $var_nameblafoo
isSubstringOfAny :: Text.Text -> [Text.Text] -> Bool
isSubstringOfAny t l =
  not $
    null
      [ v
        | v <- l,
          (Text.pack "${" <> t <> Text.pack "}") `Text.isInfixOf` v
            || (t `bareVariableInText` v)
      ]

-- | we find a 'bare' variable with name v in a text, if
-- '$v' is in the text at any place and any text following after that
-- occurence would terminate a variable name. To determine that, the text t
-- is split at every occurence of var, check if '$v' is in the text and if
-- any part of the split text would terminate a variable name.
bareVariableInText :: Text.Text -> Text.Text -> Bool
bareVariableInText v t =
  let var = "$" <> v
      rest = drop 1 $ Text.splitOn var t
   in var `Text.isInfixOf` t && any terminatesVarName rest
  where
    -- x would terminate a variable name if it was appended directly to
    -- that name
    terminatesVarName :: Text.Text -> Bool
    terminatesVarName x = Text.null x || not (beginsWithAnyOf x varChar)

    -- txt begins with any character of String
    beginsWithAnyOf :: Text.Text -> Set.Set Char -> Bool
    beginsWithAnyOf txt str = any (`Text.isPrefixOf` txt) (Set.map Text.singleton str)

    -- all characters valid in the inner of a shell variable name
    varChar :: Set.Set Char
    varChar = Set.fromList (['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'])
