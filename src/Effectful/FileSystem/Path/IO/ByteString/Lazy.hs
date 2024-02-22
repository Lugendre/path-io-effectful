{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Effectful.FileSystem.Path.IO.ByteString.Lazy (
  -- * Files
  readFile,
  writeFile,
  appendFile,
) where

import Prelude hiding (
  appendFile,
  readFile,
  writeFile,
 )

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Effectful (Eff, type (:>))
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString.Lazy qualified as ELBS
import Path (File, Path)
import Path qualified as P

----------------------------------------
-- Files

-- | Lifted 'LBS8.readFile'.
readFile :: (FileSystem :> es) => Path b File -> Eff es ByteString
readFile = ELBS.readFile . P.toFilePath

-- | Lifted 'LBS8.writeFile'.
writeFile :: (FileSystem :> es) => Path b File -> ByteString -> Eff es ()
writeFile = ELBS.writeFile . P.toFilePath

-- | Lifted 'LBS8.appendFile'.
appendFile :: (FileSystem :> es) => Path b File -> ByteString -> Eff es ()
appendFile = ELBS.appendFile . P.toFilePath
