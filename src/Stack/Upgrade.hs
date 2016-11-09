{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Upgrade
    ( upgrade
    , UpgradeOpts
    , upgradeOpts
    ) where

import           Control.Monad.Catch         (catch, throwM)
import           Control.Exception.Enclosed  (catchAny)
import           Control.Monad               (guard, join, unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource(runResourceT)
import           Data.Aeson                  (Value (Array, Object, String))
import           Data.Conduit                ((=$=))
import           Data.Conduit.Binary         (sinkFile)
import qualified Data.Conduit.Tar            as Tar
import           Data.Conduit.Zlib           (ungzip)
import           Data.Foldable               (fold, forM_)
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing)
import           Data.Monoid.Extra
import qualified Data.Text as T
import           Lens.Micro                  (set)
import           Network.HTTP.Client         (parseUrlThrow)
import           Network.HTTP.Simple         (Request, httpJSON, httpSink,
                                              setRequestHeader, getResponseBody)
import           Options.Applicative
import           Path
import           Path.IO
import qualified Paths_stack as Paths
import           Stack.Build
import           Stack.Config
import           Stack.Fetch
import           Stack.PackageIndex
import           Stack.Setup
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Internal
import           Stack.Types.Resolver
import           Stack.Types.StackT
import qualified System.Directory            as IO
import           System.Exit                 (ExitCode (ExitSuccess))
import qualified System.IO                   as IO
import qualified System.FilePath             as FP
import           System.Process              (rawSystem, readProcess)
import           System.Process.Run

#if !WINDOWS
import           System.Posix.Files (setFileMode)
#endif

upgradeOpts :: Maybe T.Text -- ^ default platform
            -> Parser UpgradeOpts
upgradeOpts mDefPlatform = UpgradeOpts
    <$> (sourceOnly <|> optional binaryOpts)
    <*> (binaryOnly <|> optional sourceOpts)
  where
    binaryOnly = flag' Nothing (long "binary-only" <> help "Do not use a source upgrade path")
    sourceOnly = flag' Nothing (long "source-only" <> help "Do not use a binary upgrade path")

    binaryOpts = BinaryOpts
        <$> fmap T.pack (strOption
              ( long "binary-platform"
             <> help "Platform type for archive to download"
             <> (maybe mempty (value . T.unpack) mDefPlatform)
             <> showDefault))

    sourceOpts = SourceOpts
        <$> ((\fromGit repo -> if fromGit then Just repo else Nothing)
                <$> switch
                    ( long "git"
                    <> help "Clone from Git instead of downloading from Hackage (more dangerous)" )
                <*> strOption
                    ( long "git-repo"
                    <> help "Clone from specified git repository"
                    <> value "https://github.com/commercialhaskell/stack"
                    <> showDefault ))

data BinaryOpts = BinaryOpts
    { _boPlatform :: !T.Text
    }
    deriving Show
data SourceOpts = SourceOpts
    { _soRepo :: !(Maybe String)
    }
    deriving Show

data UpgradeOpts = UpgradeOpts
    { _uoBinary :: !(Maybe BinaryOpts)
    , _uoSource :: !(Maybe SourceOpts)
    }
    deriving Show

upgrade :: (StackM env m, HasConfig env)
        => ConfigMonoid
        -> Maybe AbstractResolver
        -> Maybe String -- ^ git hash at time of building, if known
        -> UpgradeOpts
        -> m ()
upgrade gConfigMonoid mresolver builtHash uo@(UpgradeOpts mbo mso) =
    case (mbo, mso) of
        -- FIXME It would be far nicer to capture this case in the
        -- options parser itself so we get better error messages, but
        -- I can't think of a way to make it happen.
        (Nothing, Nothing) -> error "You must allow either binary or source upgrade paths"
        (Just bo, Nothing) -> binary bo
        (Nothing, Just so) -> source so
        (Just bo, Just so) -> binary bo `catchAny` \e -> do
            $logWarn "Exception occured when trying to perform binary upgrade:"
            $logWarn $ T.pack $ show e
            $logWarn "Falling back to source upgrade"

            source so
  where
    binary bo = binaryUpgrade bo
    source so = sourceUpgrade gConfigMonoid mresolver builtHash so

binaryUpgrade
  :: (StackM env m, HasConfig env)
  => BinaryOpts
  -> m ()
binaryUpgrade (BinaryOpts platform) = do
    $logInfo $ "Querying for archive location for platform: " <> platform
    archiveURL <- liftIO $ findArchive platform

    $logInfo $ "Downloading from: " <> archiveURL

    config <- askConfig
    let destFile = toFilePath (configLocalBin config </> $(mkRelFile "stack"))
#if WINDOWS
                        FP.<.> "exe"
#endif
        tmpFile = destFile FP.<.> "tmp"

    liftIO $ do
      case () of
        ()
          | ".tar.gz" `T.isSuffixOf` archiveURL -> handleTarball archiveURL tmpFile
          | ".zip" `T.isSuffixOf` archiveURL -> error "FIXME: Handle zip files"
          | otherwise -> error $ "Unknown archive format for Stack archive: " ++ T.unpack archiveURL

    $logInfo "Download complete, testing executable"

    liftIO $ do
      absTmpFile <- IO.canonicalizePath tmpFile

#if !WINDOWS
      setFileMode absTmpFile 0o755
#endif

      -- Sanity check!
      ec <- rawSystem absTmpFile ["--version"]

      unless (ec == ExitSuccess)
              $ error $ "Non-success exit code from running newly downloaded executable"

      IO.renameFile tmpFile destFile

    $logInfo $ T.pack $ "New stack executable available at " ++ destFile
  where
    setUserAgent :: Request -> Request
    setUserAgent = setRequestHeader "User-Agent" ["Haskell Stack Upgrade"]

    findArchive :: T.Text -> IO T.Text
    findArchive pattern = do
        val <- getResponseBody <$> httpJSON req
        let ret = do
                Object top <- return val
                Array assets <- HashMap.lookup "assets" top
                getFirst $ fold $ fmap (First . findMatch pattern') assets
        case ret of
            Just url -> return url
            Nothing -> error $ "Could not find Stack archive for platform: " ++ T.unpack pattern
      where
        -- FIXME make the Github repo configurable?
        req = setUserAgent "https://api.github.com/repos/commercialhaskell/stack/releases/latest"

        pattern' = mconcat ["-", pattern, "."]

        findMatch pattern' (Object o) = do
            String name <- HashMap.lookup "name" o
            guard $ not $ ".asc" `T.isSuffixOf` name
            guard $ pattern' `T.isInfixOf` name
            String url <- HashMap.lookup "browser_download_url" o
            Just url
        findMatch _ _ = Nothing

    handleTarball :: T.Text -> IO.FilePath -> IO ()
    handleTarball url tmpFile = do
        req <- fmap setUserAgent $ parseUrlThrow $ T.unpack url
        runResourceT $ httpSink req $ \_ -> ungzip =$= Tar.untar =$= loop
            `catch` \e ->
                case e of
                Tar.NoMoreHeaders -> error $ concat
                    [ "Stack executable "
                    , show exeName
                    , " not found in archive from "
                    , T.unpack url
                    ]
                _ -> throwM e
      where
        -- The takeBaseName drops the .gz, dropExtension drops the .tar
        exeName = FP.dropExtension (FP.takeBaseName (T.unpack url)) FP.</> "stack"
        loop = join $ Tar.withEntry $ \h ->
            if Tar.headerFilePath h == exeName
                then sinkFile tmpFile >> return (return ())
                else return loop

sourceUpgrade
  :: (StackM env m, HasConfig env)
  => ConfigMonoid
  -> Maybe AbstractResolver
  -> Maybe String
  -> SourceOpts
  -> m ()
sourceUpgrade gConfigMonoid mresolver builtHash (SourceOpts gitRepo) =
  withSystemTempDir "stack-upgrade" $ \tmp -> do
    menv <- getMinimalEnvOverride
    mdir <- case gitRepo of
      Just repo -> do
        remote <- liftIO $ readProcess "git" ["ls-remote", repo, "master"] []
        let latestCommit = head . words $ remote
        when (isNothing builtHash) $
            $logWarn $ "Information about the commit this version of stack was "
                    <> "built from is not available due to how it was built. "
                    <> "Will continue by assuming an upgrade is needed "
                    <> "because we have no information to the contrary."
        if builtHash == Just latestCommit
            then do
                $logInfo "Already up-to-date, no upgrade required"
                return Nothing
            else do
                $logInfo "Cloning stack"
                -- NOTE: "--recursive" was added after v1.0.0 (and before the
                -- next release).  This means that we can't use submodules in
                -- the stack repo until we're comfortable with "stack upgrade
                -- --git" not working for earlier versions.
                let args = [ "clone", repo , "stack", "--depth", "1", "--recursive"]
                runCmd (Cmd (Just tmp) "git" menv args) Nothing
                return $ Just $ tmp </> $(mkRelDir "stack")
      Nothing -> do
        updateAllIndices menv
        caches <- getPackageCaches
        let latest = Map.fromListWith max
                   $ map toTuple
                   $ Map.keys

                   -- Mistaken upload to Hackage, just ignore it
                   $ Map.delete (PackageIdentifier
                        $(mkPackageName "stack")
                        $(mkVersion "9.9.9"))

                     caches
        case Map.lookup $(mkPackageName "stack") latest of
            Nothing -> error "No stack found in package indices"
            Just version | version <= fromCabalVersion Paths.version -> do
                $logInfo "Already at latest version, no upgrade required"
                return Nothing
            Just version -> do
                let ident = PackageIdentifier $(mkPackageName "stack") version
                paths <- unpackPackageIdents menv tmp Nothing
                    -- accept latest cabal revision by not supplying a Git SHA
                    $ Map.singleton ident Nothing
                case Map.lookup ident paths of
                    Nothing -> error "Stack.Upgrade.upgrade: invariant violated, unpacked directory not found"
                    Just path -> return $ Just path

    forM_ mdir $ \dir -> do
        lc <- loadConfig
            gConfigMonoid
            mresolver
            (Just $ dir </> $(mkRelFile "stack.yaml"))
        bconfig <- lcLoadBuildConfig lc Nothing
        envConfig1 <- runInnerStackT bconfig $ setupEnv $ Just $
            "Try rerunning with --install-ghc to install the correct GHC into " <>
            T.pack (toFilePath (configLocalPrograms (getConfig bconfig)))
        runInnerStackT (set (envConfigBuildOpts.buildOptsInstallExes) True envConfig1) $
            build (const $ return ()) Nothing defaultBuildOptsCLI
                { boptsCLITargets = ["stack"]
                }
