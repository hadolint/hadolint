module Hadolint.Rule.DL3006 (rule) where

import Data.Char (isAlphaNum)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = customRule check (emptyState Set.empty)
  where
    code = "DL3006"
    severity = DLWarningC
    message = "Always tag the version of an image explicitly"

    check line st (From from) =
      let newState = st |> modify (insertFromAlias from)
       in case from of
            BaseImage {image = (Image _ "scratch")} -> newState
            BaseImage {digest = Just _} -> newState
            BaseImage {image = (Image _ i), tag = Nothing} ->
              -- When the image being used is a previously defined FROM alias,
              -- then we can safely ignore that the image is not tagged. Otherwise
              -- we marked it as a failure
              if isPreviousStage (dropQuotes i) (state st)
                then newState
                else newState |> addFail (CheckFailure {..})
            _ -> newState
    check _ st _ = st
{-# INLINEABLE rule #-}

-- | Whether the image refers to a previously defined stage. An image that
-- starts with a variable can't be resolved, so it is assumed to be one.
-- Variables elsewhere in the name (e.g. @base-${TARGETARCH}@) are treated as
-- wildcards and matched against the known stage aliases.
isPreviousStage :: Text -> Set.Set Text -> Bool
isPreviousStage i aliases
  | "$" `Text.isPrefixOf` i = True
  | "$" `Text.isInfixOf` i = any (matchesLiterals (literalParts i)) aliases
  | otherwise = Set.member i aliases

-- | The literal parts of a name around its variables, e.g.
-- @base-${ARCH}-slim@ gives @["base-", "-slim"]@.
literalParts :: Text -> [Text]
literalParts t =
  case Text.breakOn "$" t of
    (lit, rest)
      | Text.null rest -> [lit]
      | otherwise -> lit : literalParts (dropVariable (Text.drop 1 rest))
  where
    dropVariable r
      | "{" `Text.isPrefixOf` r = Text.drop 1 (Text.dropWhile (/= '}') r)
      | otherwise = Text.dropWhile (\c -> isAlphaNum c || c == '_') r

-- | Whether the text consists of the literals in order, with anything in
-- between them.
matchesLiterals :: [Text] -> Text -> Bool
matchesLiterals [] t = Text.null t
matchesLiterals (first : rest) t =
  maybe False (go rest) (Text.stripPrefix first t)
  where
    go [] r = Text.null r
    go [lastLit] r = lastLit `Text.isSuffixOf` r
    go (lit : lits) r =
      case Text.breakOn lit r of
        (_, match)
          | Text.null match -> False
          | otherwise -> go lits (Text.drop (Text.length lit) match)

insertFromAlias :: BaseImage -> Set.Set Text -> Set.Set Text
insertFromAlias BaseImage {alias = Just a} st = st |> Set.insert (unImageAlias a)
insertFromAlias _ st = st
