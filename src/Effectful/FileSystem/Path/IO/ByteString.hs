{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Effectful.FileSystem.Path.IO.ByteString (
  fromFilePath,

  -- * Files
  readFile,
  writeFile,
  appendFile,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Effectful (Eff, type (:>))
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as EBS
import Path (File, Path)
import Path qualified as P
import Prelude hiding (appendFile, readFile, writeFile)

----------------------------------------
-- Introducing and eliminating ByteStrings

-- | Lifted 'BS.fromFilePath'.
fromFilePath :: (FileSystem :> es) => Path b File -> Eff es ByteString
fromFilePath = EBS.fromFilePath . P.toFilePath

----------------------------------------
-- Files

-- | Lifted 'BS8.readFile'.
readFile :: (FileSystem :> es) => Path b File -> Eff es ByteString
readFile = EBS.readFile . P.toFilePath

-- | Lifted 'BS8.writeFile'.
writeFile :: (FileSystem :> es) => Path b File -> ByteString -> Eff es ()
writeFile = EBS.writeFile . P.toFilePath

-- | Lifted 'BS8.appendFile'.
appendFile :: (FileSystem :> es) => Path b File -> ByteString -> Eff es ()
appendFile = EBS.appendFile . P.toFilePath
