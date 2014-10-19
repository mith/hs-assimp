{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Assimp 
    ( loadSceneFile 
    , PostProcessStep(..)
    ) where

import Filesystem.Path.CurrentOS as FP
import Foreign.C
import Foreign.Safe
import Data.Bits
import Control.Applicative

import Assimp.Scene

#include <assimp/cimport.h>
#include <assimp/postprocess.h>
#include "typedefs.h"

{#enum aiPostProcessSteps as PostProcessStep {} with prefix = "aiProcess_" deriving (Show) #}

loadSceneFile :: FP.FilePath -> [PostProcessStep] -> IO (Either String Scene)
loadSceneFile fp pps = withCString (FP.encodeString fp) importFile
    where
	flagsToBits :: CUInt
	flagsToBits = foldr (.|.) 0 . map (fromIntegral . fromEnum) $ pps
        importFile :: CString -> IO (Either String Scene)
        importFile cs = do sp <- {# call aiImportFile #} cs flagsToBits
		      	   if sp == nullPtr
			     then return $ Left "fail"
			     else Right <$> do sc <- peek (castPtr sp)
			                       {# call aiReleaseImport #} sp
					       return sc
                           
