{-# OPTIONS_GHC -Wno-unused-imports #-}

module Effectful.FileSystem.Path.IO.ByteString.Builder (
  -- * Executing Builders
  writeFile,
) where

import Prelude hiding (writeFile)

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BS.Builder
import Effectful (Eff, type (:>))
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString.Builder qualified as EBSB
import Path (File, Path)
import Path qualified as P

----------------------------------------
-- Executing Builders

-- | Lifted 'BS.Builder.writeFile'.
writeFile :: (FileSystem :> es) => Path b File -> Builder -> Eff es ()
writeFile = EBSB.writeFile . P.toFilePath
