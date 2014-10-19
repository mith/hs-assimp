{-# LANGUAGE ForeignFunctionInterface #-}
module Assimp.Internal.Defines where

#include <assimp/scene.h>

aiMaxNumberOfTexturecoords :: Int
aiMaxNumberOfTexturecoords = fromIntegral $ #const AI_MAX_NUMBER_OF_TEXTURECOORDS
