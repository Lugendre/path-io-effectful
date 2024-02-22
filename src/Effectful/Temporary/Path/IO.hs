{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Temporary.Path.IO (
  -- * Effect
  Temporary,

  -- ** Handlers
  runTemporary,

  -- ** Temporary files and directories
  withTempFile,
  withTempDir,
  withSystemTempFile,
  withSystemTempDir,
  openTempFile,
  openBinaryTempFile,
  createTempDir,
)
where

import Effectful (Eff, type (:>))
import Effectful.Dispatch.Static (unsafeEff_, unsafeSeqUnliftIO)
import Effectful.Temporary (Temporary, runTemporary)
import Path (Abs, Dir, File, Path)
import Path.IO qualified as PIO
import System.IO (Handle)

----------------------------------------------------------------------------
-- Temporary files and directories

-- | Lifted 'PIO.withTempFile'.
withTempFile ::
  (Temporary :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  String ->
  -- | Callback that can use the file
  (Path Abs File -> Handle -> Eff es a) ->
  Eff es a
withTempFile path t action = unsafeSeqUnliftIO $ \unlift ->
  PIO.withTempFile path t (\p -> unlift . action p)

-- | Lifted 'PIO.withTempDir'.
withTempDir ::
  (Temporary :> es) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  String ->
  -- | Callback that can use the directory
  (Path Abs Dir -> Eff es a) ->
  Eff es a
withTempDir path t action = unsafeSeqUnliftIO $ \unlift ->
  PIO.withTempDir path t (unlift . action)

-- | Lifted 'PIO.withSystemTempFile'.
withSystemTempFile ::
  (Temporary :> es) =>
  -- | File name template, see 'openTempFile'
  String ->
  -- | Callback that can use the file
  (Path Abs File -> Handle -> Eff es a) ->
  Eff es a
withSystemTempFile t action = unsafeSeqUnliftIO $ \unlift ->
  PIO.withSystemTempFile t (\p -> unlift . action p)

-- | Lifted 'PIO.withSystemTempDir'.
withSystemTempDir ::
  (Temporary :> es) =>
  -- | Directory name template, see 'openTempFile'
  String ->
  -- | Callback that can use the directory
  (Path Abs Dir -> Eff es a) ->
  Eff es a
withSystemTempDir t action = unsafeSeqUnliftIO $ \unlift ->
  PIO.withSystemTempDir t (unlift . action)

-- | Lifted 'PIO.openTempFile'.
openTempFile ::
  (Temporary :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template; if the template is "foo.ext" then the created
  -- file will be @\"fooXXX.ext\"@ where @XXX@ is some random number
  String ->
  -- | Name of created file and its 'Handle'
  Eff es (Path Abs File, Handle)
openTempFile path t = unsafeEff_ $ PIO.openTempFile path t

-- | Lifted 'PIO.openBinaryTempFile'.
openBinaryTempFile ::
  (Temporary :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  String ->
  -- | Name of created file and its 'Handle'
  Eff es (Path Abs File, Handle)
openBinaryTempFile path t = unsafeEff_ $ PIO.openBinaryTempFile path t

-- | Lifted 'PIO.createTempDir'.
createTempDir ::
  (Temporary :> es) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  String ->
  -- | Name of created temporary directory
  Eff es (Path Abs Dir)
createTempDir path t = unsafeEff_ $ PIO.createTempDir path t
