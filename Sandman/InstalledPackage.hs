{-# LANGUAGE NamedFieldPuns #-}
module Sandman.InstalledPackage (
      InstalledPackage
    , installedPackageInfo
    , installedPackageInfoPath
    , installedPackageId

    , getInstalledPackage
    ) where

import Data.Text        (Text)
import System.Directory (doesFileExist)

import qualified Data.Text                         as Text
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Text                 as Cabal

-- | Represents a Cabal package installed somewhere in the system.
data InstalledPackage = InstalledPackage {
    installedPackageInfo     :: Cabal.InstalledPackageInfo
  -- ^ 'Cabal.InstalledPackageInfo' for the package.
  , installedPackageInfoPath :: FilePath
  -- ^ Path where the 'Cabal.InstalledPackageInfo' file is stored.
  , installedPackageId       :: Text
  -- ^ Package ID
  }

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
      Cabal.ParseOk _ installedPackageInfo ->
          let installedPackageId = getPackageId installedPackageInfo
          in return . Right $
              InstalledPackage{
                  installedPackageInfo
                , installedPackageInfoPath
                , installedPackageId
                }
  where
    getPackageId = Text.pack . Cabal.display . Cabal.sourcePackageId
