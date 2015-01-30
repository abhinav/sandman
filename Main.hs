{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ClassyPrelude
import System.Exit   (ExitCode (..), exitFailure, exitSuccess)

import qualified Distribution.InstalledPackageInfo as PInfo
import qualified Distribution.Text                 as Cabal
import qualified Filesystem                        as FS
import qualified Filesystem.Path.CurrentOS         as FP
import qualified Options.Applicative               as O
import qualified System.Process                    as Proc

-- | '<$>' with the arguments flipped.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 1 <&>

------------------------------------------------------------------------------
newtype Sandman = Sandman { sandmanDirectory :: FilePath }
    deriving (Show, Ord, Eq)


defaultSandman :: IO Sandman
defaultSandman = do
    home <- FS.getHomeDirectory
    return $! Sandman (home </> ".sandman")


sandboxesDirectory :: Sandman -> FilePath
sandboxesDirectory Sandman{sandmanDirectory} =
    sandmanDirectory </> "sandboxes"


getSandboxes :: Sandman -> IO [Sandbox]
getSandboxes sandman = do
    exists <- FS.isDirectory sandboxesDir
    if exists
      then map Sandbox <$> FS.listDirectory sandboxesDir
      else return []
  where
    sandboxesDir = sandboxesDirectory sandman


getSandbox :: Sandman -> Text -> IO (Maybe Sandbox)
getSandbox sandman name = do
    exists <- FS.isDirectory sandboxDir
    if exists
        then return . Just . Sandbox $ sandboxDir
        else return Nothing
  where
    sandboxDir = sandboxesDirectory sandman </> fpFromText name


------------------------------------------------------------------------------
newtype Sandbox = Sandbox { sandboxRoot :: FilePath }
    deriving (Show, Ord, Eq)


sandboxName :: Sandbox -> Text
sandboxName = fpToText . filename . sandboxRoot


createSandbox :: FilePath -> Text -> IO Sandbox
createSandbox dir name = do
    whenM (FS.isDirectory sandboxDir) $
        error $ "Sandbox " <> unpack name <> " already exists."

    FS.createTree sandboxDir

    let proc = (Proc.proc "cabal" ["sandbox", "init", "--sandbox=."]) {
            Proc.cwd = Just (fpToString sandboxDir)
          }

    (_, _, _, procHandle) <- Proc.createProcess proc
    exitResult <- Proc.waitForProcess procHandle
    case exitResult of
        ExitSuccess   -> return $! Sandbox sandboxDir
        ExitFailure _ -> die $ "Failed to create sandbox " <> name
  where
    sandboxDir = dir </> fpFromText name


installPackages :: Sandbox -> [Text] -> IO ()
installPackages sandbox@Sandbox{sandboxRoot} packages = do
    (_, _, _, procHandle) <- Proc.createProcess proc
    exitResult <- Proc.waitForProcess procHandle
    case exitResult of
        ExitSuccess -> return ()
        ExitFailure _ -> die $ "Failed to install packages to " <> name
  where
    name = sandboxName sandbox
    proc = (Proc.proc "cabal" $ ["install"] <> map unpack packages) {
        Proc.cwd = Just (fpToString sandboxRoot)
      }

------------------------------------------------------------------------------
newtype PackageDb = PackageDb { packageDbRoot :: FilePath }
    deriving (Show, Ord, Eq)


getPackageDb :: FilePath -> IO (Maybe PackageDb)
getPackageDb packageRoot = do
    matches <- readFile sandboxConfig
        <&> filter ("package-db:" `isPrefixOf`) . lines

    case listToMaybe matches of
        Nothing -> return Nothing
        Just line ->
            let right = drop 1 $ dropWhile (/= ':') line
                value = dropWhile (\c -> c == ' ' || c == '\t') right
            in return . Just . PackageDb $ fpFromText value
  where
    sandboxConfig =
        packageRoot </> fpFromText "cabal.sandbox.config"


getConfFiles :: PackageDb -> IO [FilePath]
getConfFiles packageDb =
    FS.listDirectory path <&> filter ((".conf" `isSuffixOf`) . fpToText)
  where
    path = packageDbRoot packageDb


getPackageCount :: PackageDb -> IO Int
getPackageCount = getConfFiles >=> return . length


getPackages :: PackageDb -> IO [Package]
getPackages db =
    getConfFiles db >>= mapM parse
  where
    parse :: FilePath -> IO Package
    parse path = do
        contents <- readFile path
        case PInfo.parseInstalledPackageInfo contents of
            PInfo.ParseFailed e -> fail (show e)
            PInfo.ParseOk _ a -> return $! Package a path


------------------------------------------------------------------------------
data Package = Package {
    packageInfo     :: PInfo.InstalledPackageInfo
  , packageInfoPath :: FilePath
  } deriving (Show)


installedPackageId :: Package -> Text
installedPackageId Package{packageInfo} =
    pack $ Cabal.display (PInfo.sourcePackageId packageInfo)

------------------------------------------------------------------------------
die :: Text -> IO a
die t = putStrLn t >> exitFailure

dieHappy :: Text -> IO a
dieHappy t = putStrLn t >> exitSuccess

------------------------------------------------------------------------------
list :: IO ()
list = do
    sandman <- defaultSandman -- FIXME
    sandboxes <- getSandboxes sandman
    when (null sandboxes) $
        putStrLn "No sandboxes created."
    forM_ sandboxes $ \sandbox -> do
        let name = sandboxName sandbox
        packageDb' <- getPackageDb (sandboxRoot sandbox)
        case packageDb' of
          Nothing -> putStrLn $ name <> "(ERROR: could not find package DB)"
          Just packageDb -> do
              packageCount <- getPackageCount packageDb
              putStrLn $ unwords [
                  name, "(" <> tshow packageCount, "packages)"
                ]


------------------------------------------------------------------------------
new :: Text -> IO ()
new name = do
    sandman <- defaultSandman -- FIXME
    _ <- createSandbox (sandboxesDirectory sandman) name
    putStrLn $ "Created sandbox " <> name <> "."


------------------------------------------------------------------------------
destroy :: Text -> IO ()
destroy name = do
    sandman <- defaultSandman
    Sandbox{sandboxRoot} <- getSandbox sandman name
        >>= maybe (die $ "Sandbox " <> name <> " does not exist.") return
    FS.removeTree sandboxRoot
    putStrLn $ "Removed sandbox " <> name <> "."

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
    packageDb <- getPackageDb (sandboxRoot sandbox)
        >>= maybe (die $ "Could not find package DB for " <> name) return
    packageIds <- map (PInfo.sourcePackageId . packageInfo)
        <$> getPackages packageDb

    when (null packageIds) $
        dieHappy $ name <> " does not contain any packages."

    forM_ packageIds $ putStrLn . pack . Cabal.display


------------------------------------------------------------------------------
mix :: Text -> IO ()
mix name = do
    currentPackageDb <- getPackageDb "."
        >>= maybe (die "You're not inside a Cabal sandbox.") return

    sandman <- defaultSandman
    sandbox <- getSandbox sandman name
        >>= maybe (die $ "Sandbox " <> name <> " does not exist.") return
    sandboxPackageDb <- getPackageDb (sandboxRoot sandbox)
        >>= maybe (die $ "Could not find package DB for " <> name) return

    packagesToInstall <- filterPackages
        <$> getPackages currentPackageDb
        <*> getPackages sandboxPackageDb

    let newPackageCount = length packagesToInstall

    when (newPackageCount == 0) $
        dieHappy "No packages to mix in."

    putStrLn $ unwords [
        "Mixing", tshow newPackageCount
      , "new packages into package DB at"
      , fpToText (packageDbRoot currentPackageDb)
      ]

    let currentPackageDbRoot = packageDbRoot currentPackageDb
    forM_ packagesToInstall $ \Package{packageInfoPath} ->
        FS.copyFile
            packageInfoPath
            (currentPackageDbRoot </> filename packageInfoPath)

    putStrLn "Rebuilding package cache."
    Proc.callProcess "cabal" ["sandbox", "hc-pkg", "recache"]
  where
    filterPackages installed = loop []
      where
        installedIndex :: Set Text
        installedIndex = setFromList $ map installedPackageId installed

        loop toInstall [] = toInstall
        loop toInstall (c:candidates)
            | installedPackageId c `member` installedIndex
                = loop toInstall candidates
            | otherwise = loop (c:toInstall) candidates


------------------------------------------------------------------------------
clean :: IO ()
clean = do
    currentPackageDb <- getPackageDb "."
        >>= maybe (die "You're not inside a Cabal sandbox.") return
    sandman <- defaultSandman
    putStrLn "Removing all mixed sandboxes."

    packages <- filterPackages sandman <$> getPackages currentPackageDb

    when (null packages) $
        dieHappy "No packages to remove."

    putStrLn $ "Removing " <> tshow (length packages) <> " packages."
    forM_ packages $ \Package{packageInfoPath} ->
        FS.removeFile packageInfoPath

    putStrLn "Rebuilding package cache."
    Proc.callProcess "cabal" ["sandbox", "hc-pkg", "recache"]
  where
    filterPackages :: Sandman -> [Package] -> [Package]
    filterPackages Sandman{sandmanDirectory} = filter isMixedIn
      where
        isSandmanPath p = isJust $
            stripPrefix (FP.splitDirectories sandmanDirectory)
                        (FP.splitDirectories $ fpFromString p)

        isMixedIn Package{packageInfo} = any isSandmanPath $
            concatMap ($ packageInfo) [
                PInfo.importDirs
              , PInfo.libraryDirs
              , PInfo.haddockInterfaces
              ]


------------------------------------------------------------------------------
argParser :: O.Parser (IO ())
argParser = O.subparser $ concat [
      command "list" "List sandman sandboxes or the packages in them" $
        maybe list listPackages <$> listNameArgument
    , command "new" "Create a new sandman sandbox" $
        new <$> nameArgument
    , command "destroy" "Delete a sandman sandbox" $
        destroy <$> nameArgument
    , command "install" "Install a new package" $
        install <$> nameArgument <*> packagesArgument
    , command "mix" "Mix a sandman sandbox into the current project" $
        mix <$> nameArgument
    , command "clean" "Remove all mixed sandboxes from the current project" $
        pure clean
    ]
  where
    listNameArgument = O.optional . textArgument $ O.metavar "name" ++
        O.help (unwords [
            "If given, list packages installed in the specified sandbox,"
          , "otherwise list all sandman sandboxes"
          ])
    packagesArgument = O.some . textArgument $
        O.metavar "PACKAGES" ++ O.help "Packages to install"
    nameArgument = textArgument $
        O.metavar "NAME" ++ O.help "Name of the sandman sandbox"
    textArgument = map pack . O.strArgument
    command name desc p =
        O.command name (O.info (O.helper <*> p) (O.progDesc desc))


main :: IO ()
main = join $ O.execParser opts
  where
    opts = O.info (O.helper <*> argParser) O.fullDesc
