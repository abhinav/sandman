module Sandman.PackageDb (
      PackageDb
    , packageDbRoot
    , packageDbInstalledPackages
    , getPackageDb
    ) where

import Control.Applicative
import Data.Either
import Data.List           (isSuffixOf)
import System.Directory    (doesDirectoryExist)

import Sandman.InstalledPackage (InstalledPackage, getInstalledPackage)
import Sandman.Util

-- | Represents a Cabal package database.
data PackageDb = PackageDb {
    packageDbRoot              :: FilePath
  -- ^ Root directory of the package database.
  , packageDbInstalledPackages :: [InstalledPackage]
  -- ^ List of packages installed inside the database.
  }

-- | Get the package database for the given root directory.
getPackageDb :: FilePath -> IO (Either String PackageDb)
getPackageDb root = doesDirectoryExist root >>= \exists ->
  if not exists
    then return $ Left (root ++ " is not a package DB")
    else getPackageDb_ root

getPackageDb_ :: FilePath -> IO (Either String PackageDb)
getPackageDb_ root = do
    confFiles <- listDirectory root <&> filter (".conf" `isSuffixOf`)
    result <- partitionEithers <$> mapM getInstalledPackage confFiles
    return $ case result of
      ([], installedPackages) -> Right $
          PackageDb root installedPackages
      (errs, _) -> Left . unlines $
          ("Failed to read package DB at " ++ root) : errs
