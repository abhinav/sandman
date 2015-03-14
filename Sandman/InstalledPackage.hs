{-# LANGUAGE NamedFieldPuns #-}
module Sandman.InstalledPackage (
      InstalledPackage
    , installedPackageInfo
    , installedPackageInfoPath
    , installedPackageId
    , installedPackageName
    , installedPackageVersion

    , getInstalledPackage
    ) where

import Data.Text            (Text)
import Distribution.Version (Version)
import System.Directory     (doesFileExist)

import qualified Data.Text                         as Text
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Package              as Cabal
import qualified Distribution.Text                 as Cabal

-- | Represents a Cabal package installed somewhere in the system.
data InstalledPackage = InstalledPackage {
    installedPackageInfo     :: Cabal.InstalledPackageInfo
  -- ^ 'Cabal.InstalledPackageInfo' for the package.
  , installedPackageInfoPath :: FilePath
  -- ^ Path where the 'Cabal.InstalledPackageInfo' file is stored.
  , cabalPackageId           :: Cabal.PackageId
  -- ^ Cabal package ID
  }

-- | Get the package ID for the given InstalledPackage
installedPackageId :: InstalledPackage -> Text
installedPackageId = Text.pack . Cabal.display . cabalPackageId

-- | Get the package name
installedPackageName :: InstalledPackage -> Text
installedPackageName =
    Text.pack . Cabal.display . Cabal.pkgName . cabalPackageId

-- | Get the package version
installedPackageVersion :: InstalledPackage -> Version
installedPackageVersion = Cabal.pkgVersion . cabalPackageId

-- | Build a 'InstalledPackage' object from the given path.
--
-- Returns either an error or the 'InstalledPackage' object.
getInstalledPackage :: FilePath -> IO (Either String InstalledPackage)
getInstalledPackage path =
  doesFileExist path >>= \exists ->
    if not exists
      then return (Left $ "File does not exist: " ++ path)
      else getInstalledPackage_ path

getInstalledPackage_ :: FilePath -> IO (Either String InstalledPackage)
getInstalledPackage_ installedPackageInfoPath =
  readFile installedPackageInfoPath >>= \contents ->
    case Cabal.parseInstalledPackageInfo contents of
      Cabal.ParseFailed e -> return . Left $
          "Failed to parse " ++ installedPackageInfoPath ++ ": " ++ show e
      Cabal.ParseOk _ installedPackageInfo -> return . Right $
          InstalledPackage{
              installedPackageInfo
            , installedPackageInfoPath
            , cabalPackageId = Cabal.sourcePackageId installedPackageInfo
            }
