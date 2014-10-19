{-# LANGUAGE OverloadedLists #-}
module Assimp.Mesh
    ( Mesh
    , name
    , materialIndex
    , normals
    , textureCoords
    , vertices
    ) where

import Foreign.C
import Foreign.Safe
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Data.Vector.Storable.Mutable as MV
import Linear
import Control.Applicative
import Control.Monad

import Assimp.Internal.Defines

#include <assimp/mesh.h>
#include "typedefs.h"

data Mesh = Mesh
          { materialIndex :: Int
          , name :: String
          , normals :: Vector (V3 CFloat)
          , textureCoords :: [(Int, Vector (V3 CFloat))]
          , vertices :: Vector (V3 CFloat)
          } deriving (Show)

instance Storable Mesh where
    sizeOf _ = {#sizeof aiMesh#}
    alignment _ = {#alignof aiMesh#}
    peek ptr = Mesh <$> (fromIntegral <$> {# get aiMesh->mMaterialIndex #} ptr)
    	       	    <*> getName ptr
		    <*> getNormals ptr
		    <*> getTextureCoords ptr
		    <*> getVertices ptr
    poke ptr m = return ()
    
getName :: Ptr Mesh -> IO String
getName mp = do let offset = {# offsetof aiMesh->mName.data #}
	     	strlen <- {# get aiMesh->mName.length #} mp
		peekCStringLen (plusPtr mp offset, fromIntegral strlen)
		
getNormals :: Ptr Mesh -> IO (Vector (V3 CFloat))
getNormals mp = do np <- castPtr <$> {#get aiMesh->mNormals #} mp
	      	   numVert <- fromIntegral <$> {#get aiMesh->mNumVertices #} mp
	      	   newForeignPtr_ np >>= \fptr ->
		       V.freeze $ MV.unsafeFromForeignPtr0 fptr numVert
		       
getTextureCoords :: Ptr Mesh -> IO [(Int, Vector (V3 CFloat))]
getTextureCoords mp = do let offsetNum = {# offsetof aiMesh->mNumUVComponents #}
		      	     offsetCoords = {# offsetof aiMesh->mTextureCoords #}
		      	 numUVcomp <- peekArray aiMaxNumberOfTexturecoords
			 	      		(plusPtr mp offsetNum)
			 texCoordPtrs <- peekArray aiMaxNumberOfTexturecoords
			 	      	 	   (plusPtr mp offsetCoords)
			 numVert <- fromIntegral <$> {#get aiMesh->mNumVertices #} mp
			 texCoordVecs <- forM (filter (/= nullPtr) texCoordPtrs) $ \tcp ->
			     newForeignPtr_ tcp >>= \fptr ->
			         V.freeze $ MV.unsafeFromForeignPtr0 fptr numVert
		         return $ zip numUVcomp texCoordVecs
			 
getVertices :: Ptr Mesh -> IO (Vector (V3 CFloat))
getVertices mp = do numVert <- fromIntegral <$> {# get aiMesh->mNumVertices #} mp
                    vertPtr <- castPtr <$> {#get aiMesh->mVertices #} mp
		    newForeignPtr_ vertPtr >>= \fptr ->
		        V.freeze $ MV.unsafeFromForeignPtr0 fptr numVert
	      	   