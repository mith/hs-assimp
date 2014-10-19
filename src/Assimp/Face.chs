{-# LANGUAGE ForeignFunctionInterface #-}
module Assimp.Face where

#include <assimp/mesh.h>

data Face = Face deriving (Show)
