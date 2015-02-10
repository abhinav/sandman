{-# LANGUAGE OverloadedStrings #-}
module Sandman.Util (
      (<&>)
    , whenM
    , tshow
    , listDirectory
    , removeTree
    , die
    , dieHappy
    , warn
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid              ((<>))
import Data.Text                (Text)
import System.Directory         (getDirectoryContents, removeDirectory,
                                 removeFile)
import System.Exit              (exitFailure, exitSuccess)
import System.FilePath          ((</>))
import System.IO                (stderr)
import System.PosixCompat.Files (getSymbolicLinkStatus, isDirectory)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- | '<$>' with the arguments flipped.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 1 <&>

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= \c -> when c m

tshow :: Show a => a -> Text
tshow = T.pack . show

listDirectory :: FilePath -> IO [FilePath]
listDirectory d = getDirectoryContents d
    <&> filter (`notElem` [".", ".."])
    <&> map (d </>)

removeTree :: FilePath -> IO ()
removeTree path = do
    status <- getSymbolicLinkStatus path
    if isDirectory status
      then listDirectory path
              >>= mapM_ removeTree
              >>  removeDirectory path
      else removeFile path

-- | Print the given message and exit with a non-zero status code.
die :: Text -> IO a
die t = TIO.putStrLn t >> exitFailure


-- | Print the given message and exit with status code zero.
dieHappy :: Text -> IO a
dieHappy t = TIO.putStrLn t >> exitSuccess

warn :: Text -> IO ()
warn t = TIO.hPutStrLn stderr ("WARNING: " <> t)
