module Madf.Blog.Job
    ( enqueue
    , getStatus
    , cancel
    , list
    , Config (..)
    , defaultConfig
    , Env (..)
    , Status (..)
    , StatusWithId (..)
    , State (..)
    , Action
    , ProgressCallback
    , initEnv
    , destroyEnv
    ) where

import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Data.Aeson
import Data.Int
import Data.Map qualified as DM
import Data.Ord (clamp)
import Data.Text
import Data.Time.Clock
import Data.Traversable qualified as T
import UnliftIO.Async qualified as A
import UnliftIO.Concurrent
import UnliftIO.Exception (finally, bracketOnError)
import UnliftIO.STM
import UnliftIO.QSem
import Madf.Blog.Ids

type ProgressCallback m = Int -> m ()

type Action m = ProgressCallback m -> m Value

data TaskProgress = TaskProgress
    { tpProgress :: !(Maybe Int)
    , tpFinished :: !(Maybe UTCTime)
    }

data Job = Job
    { jName     :: !Text
    , jTask     :: !(A.Async Value)
    , jProgress :: !(TVar TaskProgress)
    , jCreated  :: !UTCTime
    }

data State = Queued | Running !Int | Completed !Value | Failed !Text deriving (Show)

instance ToJSON State
    where
        toJSON Queued        = object ["status" .= ("queued" :: Text)]
        toJSON (Running p)   = object ["status" .= ("running" :: Text),   "progress" .= p]
        toJSON (Completed v) = object ["status" .= ("completed" :: Text), "result" .= v]
        toJSON (Failed e)    = object ["status" .= ("failed" :: Text),    "error" .= e]

        toEncoding Queued        = pairs ("status" .= ("queued" :: Text))
        toEncoding (Running p)   = pairs ("status" .= ("running" :: Text)   <> "progress" .= p)
        toEncoding (Completed v) = pairs ("status" .= ("completed" :: Text) <> "result" .= v)
        toEncoding (Failed e)    = pairs ("status" .= ("failed" :: Text)    <> "error" .= e)

data Status = Status
    { stJobName  :: !Text
    , stState    :: !State
    , stCreated  :: !UTCTime
    , stFinished :: !(Maybe UTCTime)
    } deriving (Show)

instance ToJSON Status
    where
        toJSON s = object
            [ "name"     .= stJobName s
            , "state"    .= stState s
            , "created"  .= stCreated s
            , "finished" .= stFinished s
            ]

        toEncoding s = pairs
            (  "name"     .= stJobName s
            <> "state"    .= stState s
            <> "created"  .= stCreated s
            <> "finished" .= stFinished s
            )

data StatusWithId = StatusWithId
    { stwJId     :: !JobId
    , stwJobName :: !Text
    , stwState    :: !State
    , stwCreated  :: !UTCTime
    , stwFinished :: !(Maybe UTCTime)
    } deriving (Show)

instance ToJSON StatusWithId
    where
        toJSON s = object
            [ "id"       .= stwJId s
            , "name"     .= stwJobName s
            , "state"    .= stwState s
            , "created"  .= stwCreated s
            , "finished" .= stwFinished s
            ]

        toEncoding s = pairs
            (  "id"       .= stwJId s
            <> "name"     .= stwJobName s
            <> "state"    .= stwState s
            <> "created"  .= stwCreated s
            <> "finished" .= stwFinished s
            )

data Config = Config
    { cleanupInterval :: !Int -- Seconds
    , maxTTL          :: !NominalDiffTime
    , maxConcurrency  :: !Int
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config 60 300 1

type Registry = TVar (DM.Map JobId Job)

data Env = Env
    { counter     :: !(TVar Int64)
    , registry    :: !Registry
    , semaphore   :: !QSem
    , cleanupTask :: !(A.Async ())
    }

initEnv :: MonadUnliftIO m => Config -> m Env
initEnv conf = do
    ctr <- newTVarIO 0
    sem <- newQSem (maxConcurrency conf)
    reg <- newTVarIO DM.empty
    bracketOnError
        (A.async $ cleanupLoop conf reg)
        A.cancel
        (return . Env ctr reg sem)

destroyEnv :: MonadIO m => Env -> m ()
destroyEnv env = do
    A.cancel (cleanupTask env)
    reg <- atomically $ swapTVar (registry env) DM.empty
    mapM_ (A.cancel . jTask) (DM.elems reg)

enqueue :: MonadUnliftIO m => Env -> Text -> Action m -> m JobId
enqueue env name action = do
    -- Create progress tracker
    tp <- newTVarIO (TaskProgress Nothing Nothing)
    let progressCb v = atomically $ modifyTVar tp (\p -> p{ tpProgress = Just (clamp (0, 100) v) })
    let taskWithProgress = do
            atomically $ writeTVar tp (TaskProgress (Just 0) Nothing)
            finally (action progressCb) $ do
                finishedAt <- liftIO getCurrentTime
                atomically $ modifyTVar tp (\p -> p{ tpFinished = Just finishedAt })
    -- Start the task and put it into the registry. Cancel the task if something goes wrong.
    bracketOnError
        (A.async $ withQSem (semaphore env) taskWithProgress)
        A.cancel
        (\task -> do
            createdAt <- liftIO getCurrentTime
            let job = Job name task tp createdAt
            jid <- nextJId env
            atomically $ modifyTVar (registry env) (DM.insert jid job)
            return jid)

getStatus :: MonadIO m => Env -> JobId -> m (Maybe Status)
getStatus env jid = do
    mtd <- findJobAndProgress env jid
    case mtd of
        Nothing -> return Nothing
        Just (j, tp) -> Just <$> makeStatus j tp

list :: MonadIO m => Env -> m [StatusWithId]
list env = do
    reg <- readTVarIO $ registry env
    T.for (DM.toList reg) $ \(k, j) -> do
        ps <- readTVarIO $ jProgress j
        s <- makeStatus j ps
        return $ StatusWithId k (stJobName s) (stState s) (stCreated s) (stFinished s)

findJobAndProgress :: MonadIO m => Env -> JobId -> m (Maybe (Job, TaskProgress))
findJobAndProgress env jid = atomically $ do
    reg <- readTVar (registry env)
    let mj = DM.lookup jid reg
    case mj of
        Nothing -> return Nothing
        Just j -> do
            tp <- readTVar (jProgress j)
            return $ Just (j, tp)

makeStatus :: MonadIO m => Job -> TaskProgress -> m Status
makeStatus j tp = do
    mer <- A.poll (jTask j)
    let state = case (mer, tpProgress tp) of
            (Nothing, Nothing) -> Queued
            (Nothing, Just p) -> Running p
            (Just er, _) -> either (Failed . pack . show) Completed er
    return $ Status (jName j) state (jCreated j) (tpFinished tp)

cancel :: MonadIO m => Env -> JobId -> m Bool
cancel env jid = do
    mj <- atomically $ do
        reg <- readTVar (registry env)
        let mj = DM.lookup jid reg
        modifyTVar (registry env) (DM.delete jid)
        return mj
    case mj of
        Just j -> do
            A.cancel $ jTask j
            return True
        Nothing -> return False

cleanupLoop :: MonadIO m => Config -> Registry -> m ()
cleanupLoop conf regVar = forever $ do
    threadDelay $ 1000000 * cleanupInterval conf
    now <- liftIO getCurrentTime
    atomically $ do
        reg <- readTVar regVar
        psMap <- T.for reg (readTVar . jProgress)
        let isExpired tp = case tpFinished tp of
                Nothing -> False
                Just t -> diffUTCTime now t > maxTTL conf
        let expired = DM.keysSet $ DM.filter isExpired psMap
        writeTVar regVar $ DM.withoutKeys reg expired

nextJId :: MonadIO m => Env -> m JobId
nextJId env = atomically $ do
    ctr <- readTVar (counter env)
    writeTVar (counter env) (ctr + 1)
    return $ makeId ctr
