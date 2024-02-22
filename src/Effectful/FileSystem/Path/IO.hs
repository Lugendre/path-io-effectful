{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.FileSystem.Path.IO (
  -- * Effect
  FileSystem,

  -- ** Handlers
  runFileSystem,

  -- * Actions on directories
  createDir,
  createDirIfMissing,
  ensureDir,
  removeDir,
  removeDirRecur,
  removePathForcibly,
  renameDir,
  renamePath,
  listDir,
  listDirRel,
  listDirRecur,
  listDirRecurRel,
  copyDirRecur,
  copyDirRecur',

  -- ** Walking directory trees
  WalkAction (..),
  walkDir,
  walkDirRel,
  walkDirAccum,
  walkDirAccumRel,

  -- ** Current working directory
  getCurrentDir,
  setCurrentDir,
  withCurrentDir,

  -- * Pre-defined directories
  getHomeDir,
  getAppUserDataDir,
  getUserDocsDir,
  getTempDir,
  D.XdgDirectory (..),
  getXdgDir,
  D.XdgDirectoryList (..),
  getXdgDirList,

  -- * Path transformation
  AnyPath (..),
  resolveFile,
  resolveFile',
  resolveDir,
  resolveDir',

  -- * Actions on files
  removeFile,
  renameFile,
  copyFile,
  getFileSize,
  findExecutable,
  findFile,
  findFiles,
  findFilesWith,

  -- * Symbolic links
  createFileLink,
  createDirLink,
  removeDirLink,
  getSymlinkTarget,
  isSymlink,

  -- * Existence tests
  doesPathExist,
  doesFileExist,
  doesDirExist,
  isLocationOccupied,
  forgivingAbsence,
  ignoringAbsence,

  -- * Permissions
  D.Permissions,
  D.emptyPermissions,
  D.readable,
  D.writable,
  D.executable,
  D.searchable,
  D.setOwnerReadable,
  D.setOwnerWritable,
  D.setOwnerExecutable,
  D.setOwnerSearchable,
  getPermissions,
  setPermissions,
  copyPermissions,

  -- * Timestamps
  getAccessTime,
  setAccessTime,
  setModificationTime,
  getModificationTime,
)
where

import Data.Time (UTCTime)
import Effectful
import Effectful.Dispatch.Static (unsafeEff_, unsafeSeqUnliftIO)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Path
import Path.IO (AnyPath, WalkAction)
import Path.IO qualified as PIO
import System.Directory qualified as D

----------------------------------------
-- Actions on directories

-- | Lifted 'PIO.createDir'.
createDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
createDir = unsafeEff_ . PIO.createDir

-- | Lifted 'PIO.createDirIfMissing'.
createDirIfMissing ::
  (FileSystem :> es) =>
  -- | Create its parents too?
  Bool ->
  -- | The path to the directory you want to make
  Path b Dir ->
  Eff es ()
createDirIfMissing p = unsafeEff_ . PIO.createDirIfMissing p

-- | Lifted 'PIO.ensureDir'
ensureDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
ensureDir = unsafeEff_ . PIO.ensureDir

-- | Lifted 'PIO.removeDir'.
removeDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
removeDir = unsafeEff_ . PIO.removeDir

-- | Lifted 'PIO.removeDirRecur'.
removeDirRecur :: (FileSystem :> es) => Path b Dir -> Eff es ()
removeDirRecur = unsafeEff_ . PIO.removeDirRecur

-- | Lifted 'PIO.removePathForcibly'.
removePathForcibly :: (FileSystem :> es) => Path b t -> Eff es ()
removePathForcibly = unsafeEff_ . PIO.removePathForcibly

-- | Lifted 'PIO.renameDir'.
renameDir ::
  (FileSystem :> es) =>
  -- | Old name
  Path b0 Dir ->
  -- | New name
  Path b1 Dir ->
  Eff es ()
renameDir p = unsafeEff_ . PIO.renameDir p

-- | Lifted 'PIO.renamePath'.
renamePath :: (FileSystem :> es) => Path b0 t -> Path b1 t -> Eff es ()
renamePath p = unsafeEff_ . PIO.renamePath p

-- | Lifted 'PIO.listDir'.
listDir ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Abs Dir], [Path Abs File])
listDir = unsafeEff_ . PIO.listDir

-- | Lifted 'PIO.listDirRel'.
listDirRel ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRel = unsafeEff_ . PIO.listDirRel

-- | Lifted 'PIO.listDirRecur'.
listDirRecur ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Abs Dir], [Path Abs File])
listDirRecur = unsafeEff_ . PIO.listDirRecur

-- | Lifted 'PIO.listDirRecurRel'.
listDirRecurRel ::
  (FileSystem :> es) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  Eff es ([Path Rel Dir], [Path Rel File])
listDirRecurRel = unsafeEff_ . PIO.listDirRecurRel

-- | Lifted 'PIO.copyDirRecur'.
copyDirRecur ::
  (FileSystem :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur p = unsafeEff_ . PIO.copyDirRecur p

-- | Lifted 'PIO.copyDirRecur''.
copyDirRecur' ::
  (FileSystem :> es) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  Eff es ()
copyDirRecur' p = unsafeEff_ . PIO.copyDirRecur' p

----------------------------------------------------------------------------
-- Walking directory trees

-- | Lifted 'PIO.WalkAction'.
walkDir ::
  (FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> Eff es (WalkAction Abs)) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDir handler topdir = unsafeSeqUnliftIO $ \unlift ->
  PIO.walkDir (\p d -> unlift . handler p d) topdir

-- | Lifted 'PIO.walkDirRel'.
walkDirRel ::
  (FileSystem :> es) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  ( Path Rel Dir ->
    [Path Rel Dir] ->
    [Path Rel File] ->
    Eff es (WalkAction Rel)
  ) ->
  -- | Directory where traversal begins
  Path b Dir ->
  Eff es ()
walkDirRel handler topdir' = unsafeSeqUnliftIO $ \unlift ->
  PIO.walkDirRel (\p d -> unlift . handler p d) topdir'

-- | Lifted 'PIO.walkDirAccum'.
walkDirAccum ::
  (FileSystem :> es, Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> Eff es (WalkAction Abs)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> Eff es o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  Eff es o
walkDirAccum h1 h2 path = unsafeSeqUnliftIO $ \unlift ->
  PIO.walkDirAccum
    ((\h p d -> unlift . h p d) <$> h1)
    (\p d -> unlift . h2 p d)
    path

-- | Lifted 'PIO.walkDirAccumRel'.
walkDirAccumRel ::
  (FileSystem :> es, Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> Eff es (WalkAction Rel)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> Eff es o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  Eff es o
walkDirAccumRel h1 h2 path = unsafeSeqUnliftIO $ \unlift ->
  PIO.walkDirAccumRel
    ((\h p d -> unlift . h p d) <$> h1)
    (\p d -> unlift . h2 p d)
    path

----------------------------------------------------------------------------
-- Current working directory

-- | Lifted 'PIO.getCurrentDir'.
getCurrentDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getCurrentDir = unsafeEff_ PIO.getCurrentDir

-- | Lifted 'PIO.setCurrentDir'.
setCurrentDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
setCurrentDir = unsafeEff_ . PIO.setCurrentDir

-- | Lifted 'PIO.withCurrentDir'.
withCurrentDir ::
  (FileSystem :> es) =>
  -- | Directory to execute in
  Path b Dir ->
  -- | Action to be executed
  Eff es a ->
  Eff es a
withCurrentDir dir action = unsafeSeqUnliftIO $ \unlift ->
  PIO.withCurrentDir dir (unlift action)

----------------------------------------------------------------------------
-- Pre-defined directories

-- | Lifted 'PIO.getHomeDir'.
getHomeDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getHomeDir = unsafeEff_ PIO.getHomeDir

-- | Lifted 'PIO.getAppUserDataDir'.
getAppUserDataDir ::
  (FileSystem :> es) =>
  -- | Name of application (used in path construction)
  String ->
  Eff es (Path Abs Dir)
getAppUserDataDir = unsafeEff_ . PIO.getAppUserDataDir

-- | Lifted 'PIO.getUserDocsDir'.
getUserDocsDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getUserDocsDir = unsafeEff_ PIO.getUserDocsDir

-- | Lifted 'PIO.getTempDir'.
getTempDir :: (FileSystem :> es) => Eff es (Path Abs Dir)
getTempDir = unsafeEff_ PIO.getTempDir

-- | Lifted 'PIO.getXdgDir'.
getXdgDir ::
  (FileSystem :> es) =>
  -- | Which special directory
  D.XdgDirectory ->
  -- | A relative path that is appended to the path; if 'Nothing', the
  -- base path is returned
  Maybe (Path Rel Dir) ->
  Eff es (Path Abs Dir)
getXdgDir xdgDir suffix = unsafeEff_ $ PIO.getXdgDir xdgDir suffix

-- | Lifted 'PIO.getXdgDirList'.
getXdgDirList ::
  (FileSystem :> es) =>
  -- | Which special directory list
  D.XdgDirectoryList ->
  Eff es [Path Abs Dir]
getXdgDirList xdgDirList = unsafeEff_ $ PIO.getXdgDirList xdgDirList

----------------------------------------------------------------------------
-- Path transformation

-- | Lifted 'PIO.resolveFile'.
resolveFile ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  FilePath ->
  Eff es (Path Abs File)
resolveFile b = unsafeEff_ . PIO.resolveFile b

-- | Lifted 'PIO.resolveFile''.
resolveFile' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  FilePath ->
  Eff es (Path Abs File)
resolveFile' = unsafeEff_ . PIO.resolveFile'

-- | Lifted 'PIO.resolveDir'.
resolveDir ::
  (FileSystem :> es) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  FilePath ->
  Eff es (Path Abs Dir)
resolveDir b = unsafeEff_ . PIO.resolveDir b

-- | Lifted 'PIO.resolveDir''.
resolveDir' ::
  (FileSystem :> es) =>
  -- | Path to resolve
  FilePath ->
  Eff es (Path Abs Dir)
resolveDir' = unsafeEff_ . PIO.resolveDir'

----------------------------------------------------------------------------
-- Actions on files

-- | Lifted 'PIO.removeFile'.
removeFile :: (FileSystem :> es) => Path b File -> Eff es ()
removeFile = unsafeEff_ . PIO.removeFile

-- | Lifted 'PIO.renameFile'.
renameFile ::
  (FileSystem :> es) =>
  -- | Original location
  Path b0 File ->
  -- | New location
  Path b1 File ->
  Eff es ()
renameFile p = unsafeEff_ . PIO.renameFile p

-- | Lifted 'PIO.copyFile'.
copyFile ::
  (FileSystem :> es) =>
  -- | Original location
  Path b0 File ->
  -- | Where to put copy
  Path b1 File ->
  Eff es ()
copyFile p = unsafeEff_ . PIO.copyFile p

-- | Lifted 'PIO.getFileSize'.
getFileSize :: (FileSystem :> es) => Path b File -> Eff es Integer
getFileSize = unsafeEff_ . PIO.getFileSize

-- | Lifted 'PIO.findExecutable'.
findExecutable ::
  (FileSystem :> es) =>
  -- | Executable file name
  Path Rel File ->
  -- | Path to found executable
  Eff es (Maybe (Path Abs File))
findExecutable = unsafeEff_ . PIO.findExecutable

-- | Lifted 'PIO.findFile'.
findFile ::
  (FileSystem :> es) =>
  -- | Set of directories to search in
  [Path b Dir] ->
  -- | Filename of interest
  Path Rel File ->
  -- | Absolute path to file (if found)
  Eff es (Maybe (Path Abs File))
findFile ds = unsafeEff_ . PIO.findFile ds

-- | Lifted 'PIO.findFiles'.
findFiles ::
  (FileSystem :> es) =>
  -- | Set of directories to search in
  [Path b Dir] ->
  -- | Filename of interest
  Path Rel File ->
  -- | Absolute paths to all found files
  Eff es [Path Abs File]
findFiles ds = unsafeEff_ . PIO.findFiles ds

-- | Lifted 'PIO.findFilesWith'.
findFilesWith ::
  (FileSystem :> es) =>
  -- | How to test the files
  (Path Abs File -> Eff es Bool) ->
  -- | Set of directories to search in
  [Path b Dir] ->
  -- | Filename of interest
  Path Rel File ->
  -- | Absolute paths to all found files
  Eff es [Path Abs File]
findFilesWith h ds path = unsafeSeqUnliftIO $ \unlift ->
  PIO.findFilesWith (unlift . h) ds path

----------------------------------------------------------------------------
-- Symbolic links

-- | Lifted 'PIO.createFileLink'.
createFileLink ::
  (FileSystem :> es) =>
  -- | Path to the target file
  Path b0 File ->
  -- | Path to the link to be created
  Path b1 File ->
  Eff es ()
createFileLink p = unsafeEff_ . PIO.createFileLink p

-- | Lifted 'PIO.createDirLink'.
createDirLink ::
  (FileSystem :> es) =>
  -- | Path to the target directory
  Path b0 Dir ->
  -- | Path to the link to be created
  Path b1 Dir ->
  Eff es ()
createDirLink target' = unsafeEff_ . PIO.createDirLink target'

-- | Lifted 'PIO.removeDirLink'.
removeDirLink ::
  (FileSystem :> es) =>
  -- | Path to the link to be removed
  Path b Dir ->
  Eff es ()
removeDirLink = unsafeEff_ . PIO.removeDirLink

-- | Lifted 'PIO.getSymlinkTarget'.
getSymlinkTarget ::
  (FileSystem :> es) =>
  -- | Symlink path
  Path b t ->
  Eff es FilePath
getSymlinkTarget = unsafeEff_ . PIO.getSymlinkTarget

-- | Lifted 'PIO.isSymlink'.
isSymlink :: (FileSystem :> es) => Path b t -> Eff es Bool
isSymlink = unsafeEff_ . PIO.isSymlink

----------------------------------------------------------------------------
-- Existence tests

-- | Lifted 'PIO.doesPathExist'.
doesPathExist :: (FileSystem :> es) => Path b t -> Eff es Bool
doesPathExist = unsafeEff_ . PIO.doesPathExist

-- | Lifted 'PIO.doesFileExist'.
doesFileExist :: (FileSystem :> es) => Path b File -> Eff es Bool
doesFileExist = unsafeEff_ . PIO.doesFileExist

-- | Lifted 'PIO.doesDirExist'.
doesDirExist :: (FileSystem :> es) => Path b Dir -> Eff es Bool
doesDirExist = unsafeEff_ . PIO.doesDirExist

-- | Lifted 'PIO.isLocationOccupied'.
isLocationOccupied :: (FileSystem :> es) => Path b t -> Eff es Bool
isLocationOccupied = unsafeEff_ . PIO.isLocationOccupied

-- | Lifted 'PIO.forgivingAbsence'.
forgivingAbsence :: (FileSystem :> es) => Eff es a -> Eff es (Maybe a)
forgivingAbsence h = unsafeSeqUnliftIO $ \unlift ->
  PIO.forgivingAbsence (unlift h)

-- | Lifted 'PIO.ignoringAbsence'.
ignoringAbsence :: (FileSystem :> es) => Eff es a -> Eff es ()
ignoringAbsence h = unsafeSeqUnliftIO $ \unlift ->
  PIO.ignoringAbsence (unlift h)

----------------------------------------------------------------------------
-- Permissions

-- | Lifted 'PIO.getPermissions'.
getPermissions :: (FileSystem :> es) => Path b t -> Eff es D.Permissions
getPermissions = unsafeEff_ . PIO.getPermissions

-- | Lifted 'PIO.setPermissions'.
setPermissions :: (FileSystem :> es) => Path b t -> D.Permissions -> Eff es ()
setPermissions p = unsafeEff_ . PIO.setPermissions p

-- | Lifted 'PIO.copyPermissions'.
copyPermissions ::
  (FileSystem :> es) =>
  -- | From where to copy
  Path b0 t0 ->
  -- | What to modify
  Path b1 t1 ->
  Eff es ()
copyPermissions p = unsafeEff_ . PIO.copyPermissions p

----------------------------------------------------------------------------
-- Timestamps

-- | Lifted 'PIO.getAccessTime'.
getAccessTime :: (FileSystem :> es) => Path b t -> Eff es UTCTime
getAccessTime = unsafeEff_ . PIO.getAccessTime

-- | Lifted 'PIO.setAccessTime'.
setAccessTime :: (FileSystem :> es) => Path b t -> UTCTime -> Eff es ()
setAccessTime p = unsafeEff_ . PIO.setAccessTime p

-- | Lifted 'PIO.setModificationTime'.
setModificationTime :: (FileSystem :> es) => Path b t -> UTCTime -> Eff es ()
setModificationTime p = unsafeEff_ . PIO.setModificationTime p

-- | Lifted 'PIO.getModificationTime'.
getModificationTime :: (FileSystem :> es) => Path b t -> Eff es UTCTime
getModificationTime = unsafeEff_ . PIO.getModificationTime
