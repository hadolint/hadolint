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
type Source = String
type Destination = String
type Arguments = [String]
type Pairs = [(String, String)]

data Instruction
  = From BaseImage
  | Add Source Destination
  | User String
  | Label Pairs
  | Stopsignal String
  | Copy Source Destination
  | Run Arguments
  | Cmd Arguments
  | Workdir Directory
  | Expose Port
  | Volume String
  | Entrypoint Arguments
  | Maintainer String
  | Env Pairs
  | Comment String
  deriving (Eq, Ord, Show)
