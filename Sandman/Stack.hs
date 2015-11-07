module Sandman.Stack
    ( getSnapshotPackageDb
    , getGlobalPackageDb
    , getGhcPath
    , Resolver
    ) where

import Data.Char (isSpace)

import qualified Data.List      as L
import qualified System.Process as Proc

import Sandman.PackageDb (PackageDb, getPackageDb)
import Sandman.Util

type Resolver = String

-- | Gets the package database for a stack snapshot.
--
-- A resolver may be optionally specified.
getSnapshotPackageDb :: Maybe Resolver -> IO (Either String PackageDb)
getSnapshotPackageDb resolver =
    Proc.readProcess "stack" args ""
        <&> L.dropWhileEnd isSpace . L.dropWhile isSpace
        >>= getPackageDb
  where
    resolverArgs = case resolver of
        Nothing -> []
        Just re -> ["--resolver", re]
    args = resolverArgs ++ ["path", "--snapshot-pkg-db"]


getGlobalPackageDb :: IO (Either String PackageDb)
getGlobalPackageDb =
    Proc.readProcess "stack" ["path", "--global-pkg-db"] ""
        <&> L.dropWhileEnd isSpace . L.dropWhile isSpace
        >>= getPackageDb

getGhcPath :: IO FilePath
getGhcPath =
    Proc.readProcess "stack" ["exec", "which", "ghc"] ""
        <&> L.dropWhileEnd isSpace . L.dropWhile isSpace
