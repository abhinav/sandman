{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Data.List           (stripPrefix)
import Data.Maybe          (fromMaybe, isJust, listToMaybe)
import Data.Monoid
import Data.Set            (Set)
import Data.Text           (Text)
import System.Directory    (canonicalizePath, copyFile,
                            createDirectoryIfMissing, doesDirectoryExist,
                            doesFileExist, findExecutable, getHomeDirectory,
                            removeFile)
import System.Exit         (ExitCode (..))
import System.FilePath     (splitDirectories, takeDirectory, takeFileName,
                            (</>))

import qualified Data.Map.Strict                   as Map
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as TIO
import qualified Distribution.InstalledPackageInfo as PInfo
import qualified Distribution.Text                 as Cabal
import qualified Options.Applicative               as O
import qualified System.Process                    as Proc

import Sandman.InstalledPackage
import Sandman.PackageDb
import Sandman.Util

------------------------------------------------------------------------------
-- | Main context for the program.
--
-- Currently this just consists of the root directory where all sandman files
-- will be stored.
newtype Sandman = Sandman { sandmanDirectory :: FilePath }
    deriving (Show, Ord, Eq)


-- | Build the context with default settings.
defaultSandman :: IO Sandman
defaultSandman = do
    home <- getHomeDirectory
    return $! Sandman (home </> ".sandman")


-- | Path to the directory which will hold the sandboxes.
sandboxesDirectory :: Sandman -> FilePath
sandboxesDirectory Sandman{sandmanDirectory} =
    sandmanDirectory </> "sandboxes"


-- | Get all managed sandboxes.
getSandboxes :: Sandman -> IO [Sandbox]
getSandboxes sandman = do
    exists <- doesDirectoryExist sandboxesDir
    if exists
      then map Sandbox <$> listDirectory sandboxesDir
      else return []
  where
    sandboxesDir = sandboxesDirectory sandman


-- | Get the sandbox with the given name.
getSandbox :: Sandman -> Text -> IO (Maybe Sandbox)
getSandbox sandman name = do
    exists <- doesDirectoryExist sandboxDir
    if exists
        then return . Just . Sandbox $ sandboxDir
        else return Nothing
  where
    sandboxDir = sandboxesDirectory sandman </> T.unpack name


------------------------------------------------------------------------------
-- | Represents a cabal sandbox.
newtype Sandbox = Sandbox {
    sandboxRoot :: FilePath
  -- ^ Path to the sandbox root.
  --
  -- Note: This is /not/ the project root. It just happens to be that the
  -- project root and sandbox root is the same for managed sandboxes.
  } deriving (Show, Ord, Eq)


-- | Name of the sandbox.
sandboxName :: Sandbox -> Text
sandboxName = T.pack . takeFileName . sandboxRoot


-- | Create a new managed sandbox with the given name.
createSandbox :: Sandman -> Text -> IO Sandbox
createSandbox sandman name = do
    whenM (doesDirectoryExist sandboxDir) $
        die $ "Sandbox " <> name <> " already exists."

    createDirectoryIfMissing True sandboxDir

    let proc = (Proc.proc "cabal" ["sandbox", "init", "--sandbox=."]) {
            Proc.cwd = Just sandboxDir
          }

    (_, _, _, procHandle) <- Proc.createProcess proc
    exitResult <- Proc.waitForProcess procHandle
    case exitResult of
        ExitSuccess   -> return $! Sandbox sandboxDir
        ExitFailure _ -> die $ "Failed to create sandbox " <> name
  where
    sandboxDir = sandboxesDirectory sandman </> T.unpack name

-- | Install the specified packages into the sandbox.
installPackages :: Sandbox -> [Text] -> IO ()
installPackages sandbox@Sandbox{sandboxRoot} packages = do
    (_, _, _, procHandle) <- Proc.createProcess proc
    exitResult <- Proc.waitForProcess procHandle
    case exitResult of
        ExitSuccess -> return ()
        ExitFailure _ -> die $ "Failed to install packages to " <> name
  where
    name = sandboxName sandbox
    proc = (Proc.proc "cabal" $ ["install"] <> map T.unpack packages) {
        Proc.cwd = Just sandboxRoot
      }

------------------------------------------------------------------------------

-- TODO probably need another data type for Projects.

getPackageGhcPath :: FilePath -> IO (Maybe FilePath)
getPackageGhcPath packageRoot = do
    hasConfig <- doesFileExist cabalConfigPath
    if not hasConfig
      then return Nothing
      else TIO.readFile cabalConfigPath
            <&> map readField . T.lines
            <&> filter (("with-compiler" ==) . fst)
            <&> fmap (T.unpack . snd) . listToMaybe
  where
    readField line = (T.strip k, T.strip $ T.drop 1 v)
      where (k, v) = T.breakOn ":" $ T.strip line
    cabalConfigPath = packageRoot </> "cabal.config"


setPackageGhcPath :: FilePath -> FilePath -> IO ()
setPackageGhcPath packageRoot ghc = do
    hasConfig <- doesFileExist cabalConfigPath
    if not hasConfig
      then TIO.writeFile cabalConfigPath entry
      else TIO.readFile cabalConfigPath
            <&> T.unlines . loop [] . T.lines
            >>= TIO.writeFile cabalConfigPath
  where
    entry = "with-compiler: " <> T.pack ghc
    cabalConfigPath = packageRoot </> "cabal.config"
    loop outLines [] = reverse outLines
    loop outLines (x:xs)
        | "with-compiler" `T.isPrefixOf` T.strip x
                    = loop (entry:outLines) xs
        | otherwise = loop     (x:outLines) xs



-- | Get the PackageDb for the given package.
--
-- The package root is the directory containing the @cabal.sandbox.config@.
determinePackageDb :: FilePath -> IO (Either String PackageDb)
determinePackageDb packageRoot = do
    -- TODO check if sandboxConfig exists
    matches <- TIO.readFile sandboxConfig
        <&> filter ("package-db:" `T.isPrefixOf`) . T.lines
    case listToMaybe matches of
        Nothing -> return . Left $
            "Could not determine package DB for " ++ packageRoot
        Just line ->
            let right = T.drop 1 $ T.dropWhile (/= ':') line
                value = T.dropWhile (\c -> c == ' ' || c == '\t') right
                root  = T.unpack value
            in getPackageDb root
  where
    sandboxConfig = packageRoot </> "cabal.sandbox.config"

-- | Get the base package DB given the path to GHC.
--
-- Uses the default GHC if given @Nothing@.
getBasePackageDb :: Maybe FilePath -> IO (Either String PackageDb)
getBasePackageDb ghcPath' =
    Proc.readProcess ghcPath ["--print-global-package-db"] ""
        <&> T.unpack . T.strip . T.pack
        >>= getPackageDb
  where
    ghcPath = fromMaybe "ghc" ghcPath'

-- | Get the number of packages installed in the given package DB.
installedPackageCount :: PackageDb -> Int
installedPackageCount = length . packageDbInstalledPackages


------------------------------------------------------------------------------
list :: IO ()
list = do
    sandman <- defaultSandman -- FIXME
    sandboxes <- getSandboxes sandman
    when (null sandboxes) $
        putStrLn "No sandboxes created."
    forM_ sandboxes $ \sandbox -> do
        let name = sandboxName sandbox
        packageDb' <- determinePackageDb (sandboxRoot sandbox)
        case packageDb' of
          Left err -> do
              warn (T.pack err)
              TIO.putStrLn $ name <> "(ERROR: could not read package DB)"
          Right packageDb -> do
              let packageCount = installedPackageCount packageDb
              TIO.putStrLn $ T.unwords
                  [name, "(" <> tshow packageCount, "packages)"]


------------------------------------------------------------------------------
new :: Maybe FilePath -> Text -> IO ()
new ghcPath' name = do
    ghcPath <- maybe (return Nothing)
                     (fmap Just . resolveExecutable)
                     ghcPath'
    sandman <- defaultSandman -- FIXME
    Sandbox{sandboxRoot} <- createSandbox sandman name
    maybe (return ())
          (setPackageGhcPath sandboxRoot)
          ghcPath
    TIO.putStrLn $ "Created sandbox " <> name <> "."
  where
    resolveExecutable path = do
        exists <- doesFileExist path
        if exists
            then canonicalizePath path
            else
              findExecutable path >>=
              maybe (die $ "Could not find GHC at " <> T.pack path) return

------------------------------------------------------------------------------
destroy :: Text -> IO ()
destroy name = do
    sandman <- defaultSandman
    Sandbox{sandboxRoot} <- getSandbox sandman name
        >>= maybe (die $ "Sandbox " <> name <> " does not exist.") return
    removeTree sandboxRoot
    TIO.putStrLn $ "Removed sandbox " <> name <> "."


------------------------------------------------------------------------------
install :: Text -> [Text] -> IO ()
install name packages = do
    -- TODO parse package IDs
    sandman <- defaultSandman
    sandbox <- getSandbox sandman name
        >>= maybe (die $ "Sandbox " <> name <> " does not exist.") return
    installPackages sandbox packages


------------------------------------------------------------------------------
listPackages :: Text -> IO ()
listPackages name = do
    sandman <- defaultSandman
    -- TODO get rid of all this duplication
    sandbox <- getSandbox sandman name
        >>= maybe (die $ "Sandbox " <> name <> " does not exist.") return
    packageDb <- determinePackageDb (sandboxRoot sandbox)
             >>= either fail return
    let packageIds = packageDbInstalledPackages packageDb
                 <&> installedPackageId

    when (null packageIds) $
        dieHappy $ name <> " does not contain any packages."

    forM_ packageIds TIO.putStrLn


------------------------------------------------------------------------------
mix :: [Text] -> Bool -> Text -> IO ()
mix packageNames includeExecutables name = do
    currentPackageDb <- determinePackageDb "." >>= either fail return

    sandman <- defaultSandman
    sandbox <- getSandbox sandman name
        >>= maybe (die $ "Sandbox " <> name <> " does not exist.") return
    sandboxPackageDb <- determinePackageDb (sandboxRoot sandbox)
                    >>= either fail return

    let sandboxPackageNames = Set.fromList . map installedPackageName $
            packageDbInstalledPackages sandboxPackageDb

    -- Make sure that all requested packages are installed.
    forM_ packageNames $ \requestedPackage ->
        unless (requestedPackage `Set.member` sandboxPackageNames) $
            die $ requestedPackage <> " is not installed in " <> name

    basePackageIds <- getPackageGhcPath (sandboxRoot sandbox)
        >>= getBasePackageDb >>= either fail return
        <&> map installedPackageId . packageDbInstalledPackages

    let -- Returns True if another package with the same name has already been
        -- installed to the target sandbox
        isAlreadyInstalled =
            (`Set.member` alreadyInstalled) . installedPackageName
          where
            alreadyInstalled = Set.fromList . map installedPackageName $
                packageDbInstalledPackages currentPackageDb


        -- Reverse mapping from Cabal's InstalledPackageId to InstalledPackage
        -- for all packages in the managed sandbox
        installedPackageIdIndex = Map.fromList $ do
            installedPackage <- packageDbInstalledPackages sandboxPackageDb
            let pinfo = installedPackageInfo installedPackage
            return (PInfo.installedPackageId pinfo, installedPackage)

        -- Reverse mapping from package names to InstalledPackages for all
        -- packages in the managed sandbox
        installedPackageNameIndex = Map.fromList $ do
            installedPackage <- packageDbInstalledPackages sandboxPackageDb
            return (installedPackageName installedPackage, installedPackage)

        isBase pkgId' = any (`T.isPrefixOf` pkgId) basePackageIds
          where pkgId = T.pack $ Cabal.display pkgId'

        -- Get the InstalledPackage objects for the direct dependencies of the
        -- given InstaledPackage
        getDirectDependencies pkg = do
          dep <- PInfo.depends (installedPackageInfo pkg)
          if isBase dep then mzero else
            case dep `Map.lookup` installedPackageIdIndex of
              Nothing -> error $ unwords [
                  "Installed package", T.unpack (installedPackageName pkg)
                , "depends on", Cabal.display dep
                , "which is not installed in sandbox", T.unpack name
                ]
              Just depPkg -> return depPkg

        -- Get the InstalledPackage objects for all dependencies of the given
        -- InstaledPackage.
        --
        -- This includes dependencies of dependencies and so on.
        getDependencies _pkg = loop Set.empty [] [_pkg]
          where
            loop _ result [] = result
            loop visited result (pkg:pkgs)
                | pname `Set.member` visited = loop visited result pkgs
                | otherwise =
                    loop (pname `Set.insert` visited)
                         (pkg:result)
                         (pkgs ++ getDirectDependencies pkg)
              where
                pname = installedPackageName pkg

        -- Names of requested packages and their dependencies
        requestedPackages :: Set Text
        requestedPackages = Set.fromList $ do
            pkgName <- packageNames
            case pkgName `Map.lookup` installedPackageNameIndex of
              Nothing -> error $ unwords [
                  "Requested package", T.unpack pkgName
                , "is not installed in sandbox", T.unpack name
                ]
              Just installedPkg -> do
                depPkg <- getDependencies installedPkg
                [pkgName, installedPackageName depPkg]

        -- Whether a package was requested for installation.
        --
        -- Always returns True if --only was skipped. If --only was given,
        -- returns true only for requested packages and their dependencies.
        isRequested = if null packageNames then const True else
            (`Set.member` requestedPackages) . installedPackageName

        -- Returns True if the package should be installed
        shouldInstall pkg = isRequested pkg && not (isAlreadyInstalled pkg)

        -- List of packages that will be installed
        packagesToInstall = filter shouldInstall $
            packageDbInstalledPackages sandboxPackageDb

        newPackageCount = length packagesToInstall

    when (newPackageCount == 0) $
        dieHappy "No packages to mix in."

    putStrLn $ unwords [
        "Mixing", show newPackageCount
      , "new packages into package DB at"
      , packageDbRoot currentPackageDb
      ]

    let currentPackageDbRoot = packageDbRoot currentPackageDb
    forM_ packagesToInstall $ \installedPackage -> do
      let currentPath = installedPackageInfoPath installedPackage
          newPath = currentPackageDbRoot </> takeFileName currentPath
      copyFile currentPath newPath

    executables <- listDirectory (sandboxRoot sandbox </> "bin")
    when (includeExecutables && not (null executables)) $ do
        let newBinDir = takeDirectory currentPackageDbRoot </> "bin"
        createDirectoryIfMissing True newBinDir
        forM_ executables $ \exec -> do
            let newPath = newBinDir </> takeFileName exec
            alreadyExists <- doesFileExist newPath
            unless alreadyExists $ copyFile exec newPath

    ghcPath <- getPackageGhcPath (sandboxRoot sandbox)
    case ghcPath of
        Nothing -> return ()
        Just path -> do
            putStrLn $ "Setting GHC version for project to " ++ path
            setPackageGhcPath "." path

    putStrLn "Rebuilding package cache."
    Proc.callProcess "cabal" ["sandbox", "hc-pkg", "recache"]

------------------------------------------------------------------------------
clean :: IO ()
clean = do
    currentPackageDb <- determinePackageDb "." >>= either fail return
    sandman <- defaultSandman
    putStrLn "Removing all mixed sandboxes."

    let packages = filterPackages sandman $
            packageDbInstalledPackages currentPackageDb

    when (null packages) $
        dieHappy "No packages to remove."

    forM_ packages $ removeFile . installedPackageInfoPath
    putStrLn $ "Removed " <> show (length packages) <> " packages."

    putStrLn "Rebuilding package cache."
    Proc.callProcess "cabal" ["sandbox", "hc-pkg", "recache"]
  where
    -- FIXME this will probably cause all kinds of trouble if one managed
    -- sandbox is mixed into another. That should be disallowed or this should
    -- be smarter.
    filterPackages :: Sandman -> [InstalledPackage] -> [InstalledPackage]
    filterPackages Sandman{sandmanDirectory} = filter isMixedIn
      where
        isSandmanPath p = isJust $
            stripPrefix (splitDirectories sandmanDirectory)
                        (splitDirectories p)

        isMixedIn installedPackage = any isSandmanPath $
            concatMap ($ packageInfo) [
                PInfo.importDirs
              , PInfo.libraryDirs
              , PInfo.haddockInterfaces
              ]
          where
            packageInfo = installedPackageInfo installedPackage


------------------------------------------------------------------------------
argParser :: O.Parser (IO ())
argParser = O.subparser $ mconcat [
      -- TODO come up with a better name for managed sandboxes than "sandman
      -- sandboxes"
      command "list" "List sandman sandboxes or the packages in them" $
        maybe list listPackages <$> listNameArgument
    , command "new" "Create a new sandman sandbox" $
        new <$> newOptions
            <*> nameArgument
    , command "destroy" "Delete a sandman sandbox" $
        destroy <$> nameArgument
    , command "install" "Install a new package" $
        install <$> nameArgument <*> packagesArgument
    , command "mix" "Mix a sandman sandbox into the current project" $
        mix <$> many (T.pack <$> packageNameOption)
            <*> includeExecutablesOption
            <*> nameArgument
    , command "clean" "Remove all mixed sandboxes from the current project" $
        pure clean
    ]
  where
    includeExecutablesOption = O.switch $
        O.long "executables" <> O.short 'x' <>
        O.help "Mix executables from the managed sandbox into the project."
    packageNameOption = O.strOption $
        O.long "only" <> O.short 'o' <> O.metavar "PACKAGE" <>
        O.help (unwords [
            "If added, only the specified packages (and their dependencies)"
          , "will be mixed in. This option may be specified multiple times."
          ])
    newOptions = O.optional . O.strOption $
        O.long "with-ghc" <> O.metavar "GHC" <>
        O.help (unwords [
            "Use a different version of GHC in this sandbox."
          , "When this sandbox is mixed into package sandboxes, their"
          , "cabal.config will be updated to use this version of GHC."
          ])
    listNameArgument = O.optional . textArgument $ O.metavar "name" <>
        O.help (unwords [
            "If given, list packages installed in the specified sandbox,"
          , "otherwise list all sandman sandboxes"
          ])
    packagesArgument = O.some . textArgument $
        O.metavar "PACKAGES" <> O.help "Packages to install"
    nameArgument = textArgument $
        O.metavar "NAME" <> O.help "Name of the sandman sandbox"
    textArgument = fmap T.pack . O.strArgument
    command name desc p =
        O.command name (O.info (O.helper <*> p) (O.progDesc desc))


main :: IO ()
main = join $ O.execParser opts
  where
    opts = O.info (O.helper <*> argParser) O.fullDesc
