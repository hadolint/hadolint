module Syntax where

import Data.ByteString.Char8 (ByteString)

type Image = String
type Tag = String
type Port = Integer
type Directory = String

data BaseImage
  = LatestImage Image
  | TaggedImage Image Tag
  | DigestedImage Image ByteString
  deriving (Eq, Ord, Show)

type Dockerfile = [Instruction]

data Instruction
  = From BaseImage
  | Add String
  | User String
  | Label String
  | Stopsignal String
  | Copy String
  | Run String
  | Cmd String
  | Workdir Directory
  | Expose Port
  | Volume String
  | Entrypoint String
  | Maintainer String
  | Env String String
  deriving (Eq, Ord, Show)
