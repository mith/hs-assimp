{-# LANGUAGE ForeignFunctionInterface #-}
module Assimp.Scene 
   ( Scene
   , flags
   , meshes
   , materials
   ) where

import Foreign.Marshal.Safe
import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Control.Applicative

import Assimp.Mesh
import Assimp.Material

#include <assimp/scene.h>
#include "typedefs.h"

{#enum define SceneFlag { AI_SCENE_FLAGS_INCOMPLETE as SceneIncomplete
       	      		, AI_SCENE_FLAGS_VALIDATED as SceneValidated
			, AI_SCENE_FLAGS_VALIDATION_WARNING as SceneValidationWarning
			, AI_SCENE_FLAGS_NON_VERBOSE_FORMAT as SceneNonVerboseFormat
			, AI_SCENE_FLAGS_TERRAIN as SceneTerrain
			} deriving (Show) #}

data Scene = Scene 
           { flags :: [SceneFlag]
           , meshes :: [Mesh]
           , materials :: [Material]
           } deriving Show

instance Storable Scene where
    sizeOf _ = {#sizeof aiScene #}
    alignment _ = {#alignof aiScene #}
    peek ptr = Scene <$> pure []
    	       	     <*> getMeshes ptr
		     <*> pure []
    poke ptr s = return ()
    
getMeshes :: Ptr Scene -> IO [Mesh]
getMeshes scp = do numMeshes <- {# get aiScene->mNumMeshes #} scp
    	       	   meshesPtr <- {# get aiScene->mMeshes #} scp
		   meshesPtrs <- peekArray (fromIntegral numMeshes) (castPtr meshesPtr)
		   mapM (peek) meshesPtrs


    

