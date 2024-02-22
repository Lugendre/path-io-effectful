{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Lifted "UnliftIO.IO.File".
module Effectful.FileSystem.Path.IO.File (
  writeBinaryFile,
  writeBinaryFileAtomic,
  writeBinaryFileDurable,
  writeBinaryFileDurableAtomic,
  withBinaryFile,
  withBinaryFileAtomic,
  withBinaryFileDurable,
  withBinaryFileDurableAtomic,
  ensureFileDurable,
) where

import Data.ByteString (ByteString)
import Effectful (Eff, type (:>))
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.File qualified as EF
import Path (File, Path)
import Path qualified as P
import System.IO (Handle, IOMode (..))
import UnliftIO.IO.File qualified as U

-- | Lifted 'U.writeBinaryFile'.
writeBinaryFile :: (FileSystem :> es) => Path b File -> ByteString -> Eff es ()
writeBinaryFile path = EF.writeBinaryFile (P.toFilePath path)

-- | Lifted 'U.writeBinaryFileAtomic'.
writeBinaryFileAtomic :: (FileSystem :> es) => Path b File -> ByteString -> Eff es ()
writeBinaryFileAtomic path = EF.writeBinaryFileAtomic (P.toFilePath path)

-- | Lifted 'U.writeBinaryFileDurable'.
writeBinaryFileDurable :: (FileSystem :> es) => Path b File -> ByteString -> Eff es ()
writeBinaryFileDurable path = EF.writeBinaryFileDurable (P.toFilePath path)

-- | Lifted 'U.writeBinaryFileDurableAtomic'.
writeBinaryFileDurableAtomic :: (FileSystem :> es) => Path b File -> ByteString -> Eff es ()
writeBinaryFileDurableAtomic path = EF.writeBinaryFileDurableAtomic (P.toFilePath path)

----------------------------------------

-- | Lifted 'U.withBinaryFile'.
withBinaryFile ::
  (FileSystem :> es) => Path b File -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFile path = EF.withBinaryFile (P.toFilePath path)

-- | Lifted 'U.withBinaryFileAtomic'.
withBinaryFileAtomic ::
  (FileSystem :> es) => Path b File -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFileAtomic path = EF.withBinaryFileAtomic (P.toFilePath path)

-- | Lifted 'U.withBinaryFileDurable'.
withBinaryFileDurable ::
  (FileSystem :> es) => Path b File -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFileDurable path = EF.withBinaryFileDurable (P.toFilePath path)

-- | Lifted 'U.withBinaryFileDurableAtomic'.
withBinaryFileDurableAtomic ::
  (FileSystem :> es) => Path b File -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFileDurableAtomic path = EF.withBinaryFileDurableAtomic (P.toFilePath path)

----------------------------------------

-- | Lifted 'U.ensureFileDurable'.
ensureFileDurable :: (FileSystem :> es) => Path b File -> Eff es ()
ensureFileDurable = EF.ensureFileDurable . P.toFilePath