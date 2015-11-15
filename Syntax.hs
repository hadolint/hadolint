module Syntax where

import Data.ByteString.Char8 (ByteString)

type Image = String
type Tag = String
type Port = Integer

data BaseImage
  = LatestImage Image
  | TaggedImage Image Tag
  | DigestedImage Image ByteString
  deriving (Eq, Ord, Show)

type Arguments = String
type Dockerfile = [Instruction]

data Instruction
  = From BaseImage
  | Add Arguments
  | Copy Arguments
  | Run Arguments
  | Workdir Arguments
  | Expose Port
  | Volume Arguments
  | Entrypoint Arguments
  | Maintainer String
  | Env String String
  deriving (Eq, Ord, Show)
