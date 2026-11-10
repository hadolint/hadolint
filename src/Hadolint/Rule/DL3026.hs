module Hadolint.Rule.DL3026 (rule) where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.String ( IsString(..) )
import qualified Data.Text as Text
import Data.Text (Text, pack, unpack, drop, dropEnd, isSuffixOf, isPrefixOf, breakOn)
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Set.Set Registry -> Rule args
rule allowed = customRule check (emptyState Set.empty)
  where
    code = "DL3026"
    severity = DLErrorC
    message = "Use only an allowed registry in the FROM image"

    check line st (From BaseImage {image, alias}) =
      let newState = st |> modify (Set.insert alias)
       in if doCheck (state st) image
            then newState
            else newState |> addFail CheckFailure {..}
    check line st (Copy _ (CopyFlags _ _ _ _ (CopySource src) _)) =
      let img = fromString ( Data.Text.unpack src )
       in if doCheck (state st) img
            then st
            else st |> addFail CheckFailure {..}
    check line st (Run (RunArgs _ (RunFlags m _ _))) =
       if null $ Set.filter (not . checkMount st) m
         then st
         else st |> addFail CheckFailure {..}
    check _ st _ = st

    checkMount st (CacheMount (CacheOpts _ _ _ _ fi _ _ _ _)) =
      let img = fromString $ unpack $ Maybe.fromMaybe "scratch" fi
       in doCheck (state st) img
    checkMount st (BindMount (BindOpts _ _ fi _ _)) =
      let img = fromString $ unpack $ Maybe.fromMaybe "scratch" fi
       in doCheck (state st) img
    checkMount _ _ = True

    doCheck st img = Set.member (toImageAlias img) st || Set.null allowed || isAllowed img

    toImageAlias = Just . ImageAlias . imageName
    isAllowed Image {registryName = Just registry, imageName} =
      isRegistryAllowed (unRegistry registry) imageName
    isAllowed Image {registryName = Nothing, imageName} =
      imageName == "scratch"
        || isRegistryAllowed "docker.io" imageName
        || isRegistryAllowed "hub.docker.com" imageName

    isRegistryAllowed registry image = any (matchesImage . unRegistry) allowed
      -- COPY and mount sources can include a tag or digest in imageName.
      -- Neither is part of the registry/namespace being trusted.
      where
        imagePath = Text.takeWhile (`notElem` (":@" :: String)) image
        matchesImage allowedImage =
          case breakOn "/" allowedImage of
            (host, path)
              | not (Text.null path) ->
                  matchRegistry host registry
                    && (matchRegistry (Data.Text.drop 1 path) imagePath
                          || (Data.Text.drop 1 path <> "/") `isPrefixOf` imagePath)
            _ -> matchRegistry allowedImage registry
{-# INLINEABLE rule #-}

matchRegistry :: Text -> Text -> Bool
matchRegistry allow registry
  | allow == star = True
  | star `isPrefixOf` allow = Data.Text.drop 1 allow `isSuffixOf` registry
  | star `isSuffixOf` allow = Data.Text.dropEnd 1 allow `isPrefixOf` registry
  | otherwise = registry == allow
  where
      star = pack "*"
