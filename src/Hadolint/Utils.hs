module Hadolint.Utils
  ( hasCacheOrTmpfsMount,
    hasCacheOrTmpfsMountWith
  )
where

import qualified Data.Text as Text
import qualified Data.Set as Set
import Language.Docker.Syntax


-- Returns true if the given RunFlags contain a mount of type cache or tmpfs at
-- a location that contains the path fragment `frag`.
hasCacheOrTmpfsMountWith :: Text.Text -> RunFlags -> Bool
hasCacheOrTmpfsMountWith frag RunFlags { mount } =
  not ( null $ Set.filter (isCacheOrTmpfsMountWith frag) mount)

isCacheOrTmpfsMountWith :: Text.Text -> RunMount -> Bool
isCacheOrTmpfsMountWith frag (CacheMount CacheOpts {cTarget = TargetPath {unTargetPath = t}}) = frag `Text.isInfixOf` t
isCacheOrTmpfsMountWith frag (TmpfsMount TmpOpts {tTarget = TargetPath {unTargetPath = t}}) = frag `Text.isInfixOf` t
isCacheOrTmpfsMountWith _ _ = False

-- Returns true if the given RunFlags contain a mount of type cache or tmpfs
hasCacheOrTmpfsMount :: RunFlags -> Bool
hasCacheOrTmpfsMount RunFlags { mount } =
  not ( null $ Set.filter isCacheOrTmpfsMount mount)

isCacheOrTmpfsMount :: RunMount -> Bool
isCacheOrTmpfsMount (CacheMount _) = True
isCacheOrTmpfsMount (TmpfsMount _) = True
isCacheOrTmpfsMount _ = False
