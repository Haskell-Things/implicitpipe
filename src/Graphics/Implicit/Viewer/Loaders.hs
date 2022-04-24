{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Implicit.Viewer.Loaders where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Data.Foldable (asum)
import Data.List (isSuffixOf)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Typeable (Typeable)

import Graphics.Implicit
import Graphics.Implicit.Primitives (getBox)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.Viewer.Types
import Graphics.Implicit.Export.GL

import qualified Language.Haskell.Interpreter as Hint
import qualified System.FSNotify as FSNotify
import qualified System.FilePath
import qualified System.Directory

-- | Run fsnotify based directory watcher for .hs and .escad files
-- and run appropriate loader based on the file extension.
runUpdater
  :: FilePath
  -> Resolution
  -> TChan Rendered
  -> IO ()
runUpdater modFileRel initialResolution renderChan = void $ do

  watchEvent <- newTChanIO

  modFile <- System.Directory.makeAbsolute modFileRel
  void $ async $ loadByExtension modFile initialResolution renderChan

  let
    eventFilter :: FSNotify.ActionPredicate
    eventFilter (FSNotify.Modified fp _ _)
           | ".hs" `isSuffixOf` fp = True
           | ".escad" `isSuffixOf` fp = True
    eventFilter _ = False

    eventHandler (FSNotify.Modified fp _ _) = atomically $ writeTChan watchEvent fp
    eventHandler _ = return ()

  void $ async $ FSNotify.withManagerConf (FSNotify.defaultConfig { FSNotify.confDebounce = FSNotify.Debounce 1 }) $ \mgr -> do
    void $ FSNotify.watchDir
      mgr
      (System.FilePath.takeDirectory modFile)
      eventFilter
      eventHandler

    forever $ threadDelay 1000000

  void $ async $ forever $ do
    fp <- atomically $ readTChan watchEvent
    putStrLn $ "Reloading, change in " ++ fp
    loadByExtension modFile initialResolution renderChan
    -- Sort of de-bounce (flushes events occured during rendering).
    -- This prevents repeated reloads when multiple files are saved at once
    -- but can cause us to miss real saves, so not ideal.
    let emptyChan c = do
          isE <- isEmptyTChan c
          unless isE $ readTChan c >> emptyChan c
    atomically $ emptyChan watchEvent

-- | Run Hint based or escad loader depending on file extension
loadByExtension
  :: FilePath
  -> Resolution
  -> TChan Rendered
  -> IO ()
loadByExtension f initialResolution renderChan
  | ".hs"    `isSuffixOf` f = loadViaHint f initialResolution renderChan
  | ".scad"  `isSuffixOf` f = loadViaEscad f initialResolution renderChan
  | ".escad" `isSuffixOf` f = loadViaEscad f initialResolution renderChan
loadByExtension f _ _ = putStrLn $ "Don't know how to load " ++ f ++ ", I only know how render .hs, .scad and .escad"

loadViaHint
  :: FilePath
  -> Resolution
  -> TChan Rendered
  -> IO ()
loadViaHint modFile initialResolution renderChan = do
    putStrLn $ "Interpreting " ++ modFile
    mr <- runMaybeT $ asum $ map MaybeT
      [                       evalMay @Double  modFile "res"
      , fmap fromIntegral <$> evalMay @Int     modFile "res"
      , fmap fromIntegral <$> evalMay @Integer modFile "res"
      ]

    let resolution = maybe initialResolution Fixed mr

    mo <- eval @SymbolicObj3 modFile "obj"
    case mo of
      Right o -> renderObjToChan o resolution renderChan
      Left (Hint.WontCompile ghcErrs) -> forM_ ghcErrs $ putStrLn . Hint.errMsg
      Left (Hint.UnknownError str)    -> putStrLn $ "Unknown error: " ++ str
      Left (Hint.NotAllowed str)      -> putStrLn $ "Not allowed: " ++ str
      Left (Hint.GhcException str)    -> putStrLn $ "GHC Exception: " ++ str

eval :: forall t . Typeable t
     => FilePath
     -> String
     -> IO (Either Hint.InterpreterError t)
eval modFile s = Hint.runInterpreter $ do
  Hint.set [Hint.searchPath Hint.:= [System.FilePath.takeDirectory modFile ]]
  Hint.loadModules [modFile]
  Hint.setTopLevelModules [
    System.FilePath.takeBaseName modFile
    ]
  Hint.interpret s (Hint.as :: t)

evalMay :: forall t. Typeable t
        => FilePath
        -> String
        -> IO (Maybe t)
evalMay modFile s = toMaybe $ eval modFile s
  where toMaybe a = a >>= return . \case
          Right x -> Just x
          _ -> Nothing

loadViaEscad
  :: FilePath
  -> Resolution
  -> TChan Rendered
  -> IO ()
loadViaEscad modFile initialResolution renderChan = do
  putStrLn $ "Interpreting " ++ modFile
  System.Directory.setCurrentDirectory (System.FilePath.takeDirectory modFile)

  content <- readFile (System.FilePath.takeFileName modFile)

  (varLookup, objs2, objs3, warnings) <- runOpenscad
    (ScadOpts {
      openScadCompatibility = False
    , importsAllowed = True
    })
    ["$viewer = true;"] -- defines
    content

  case warnings of
    [] -> return ()
    ws -> putStrLn "Warnings:" >> forM_ ws (putStrLn . show)

  let res = case lookupVarIn "$res" varLookup of
        Just (ONum n) -> Fixed n
        _ -> initialResolution

  renderObjToChan
    (unionR 0 ((extrude (unionR 0 objs2) 1):objs3))
    res
    renderChan

renderObjToChan
  :: SymbolicObj3
  -> Resolution
  -> TChan Rendered
  -> IO ()
renderObjToChan o resolution renderChan = do
  case resolution of
    Fixed r -> putStrLn $ "Rendering with resolution of " ++ show r
    _       -> putStrLn "Rendering"

  now <- getCurrentTime
  let
      !mesh = meshToGL
        $ meshFunFromResolution resolution 0 o
      !l = length mesh
      (ba, bb) = getBox o
      rmax (V3 x y z) = maximum [x, y, z]

      -- ratio perserving scaling
      !objScale = realToFrac $ rmax (bb - ba)

  unless (l == 0) $ do
    atomically $ writeTChan renderChan (l, objScale, mesh)
  after <- getCurrentTime
  putStrLn $ "Done in " ++ (show $ diffUTCTime after now)

  when (l == 0) $ putStrLn "Mesh empty"

-- | Run animation
runAnimation
  :: (Double -> SymbolicObj3)
  -> Resolution
  -> TChan Rendered
  -> TVar Double
  -> IO ()
runAnimation f initialResolution renderChan aTime = void $ async $ forever $ do
  isE <- atomically $ isEmptyTChan renderChan
  case isE of
    True -> do
      t <- atomically $ readTVar aTime
      renderObjToChan (f t) initialResolution renderChan
    False -> threadDelay 100000
