
module Ide3.ModuleGraph where

import Data.GraphViz.Types

import Ide3.NewMonad
import Ide3.Types

import Control.Monad.State

import Data.GraphViz.Types.Graph
import qualified Data.Text.Lazy as T
import Data.List (nub)

import Ide3.Module.Query

makeProjectImportGraph :: (SolutionMonad m, MonadIO m) => ProjectInfo -> SolutionResult u m (DotGraph ModuleInfo)
makeProjectImportGraph pji = flip execStateT emptyGraph $ do
    mis <- lift $ getModules pji
    let mkGraphId (ModuleInfo (Symbol s)) = Just $ Str $ T.fromStrict s
    forM_ mis $ \mi -> modify $ addNode mi (mkGraphId mi) []
    forM_ mis $ \mi -> do
        imports <- lift $ importedBy pji mi
        liftIO $ print imports
        forM_ imports $ \(ProjectChild pji' imports') -> if pji /= pji'
            then return ()
            else forM_ imports' $ \(ModuleChild mi' iis) -> case iis of
                [] -> return ()
                _ -> do
                    modify $ addEdge mi' mi []

makeSolutionImportGraph :: (SolutionMonad m, MonadIO m) => SolutionResult u m (DotGraph (ProjectChild ModuleInfo))
makeSolutionImportGraph = flip execStateT emptyGraph $ do
    pjis <- lift getProjects
    mis <- fmap (nub . concat) $ forM pjis $ \pji -> do
        mis <- lift $ getModules pji
        return $ flip map mis $ ProjectChild pji
    liftIO $ putStrLn "adding nodes"
    forM_ mis $ \(ProjectChild pji mi) -> do
        let mkGraphId (ProjectInfo s) = Just $ Str $ T.fromStrict s
        liftIO $ print (ProjectChild pji mi)
        modify $ addNode (ProjectChild pji mi) (mkGraphId pji) []
    liftIO $ putStrLn "adding edges"
    forM_ mis $ \(ProjectChild pji mi) -> do
        imports <- lift $ importedBy pji mi
        --liftIO $ print imports
        forM_ imports $ \(ProjectChild pji' imports') -> do
            mis' <- fmap (nub . concat) $ forM imports' $ \(ModuleChild mi' iis) -> case iis of
                [] -> return []
                _ -> return [ProjectChild pji' mi']
            forM_ mis' $ \mi' -> do
                liftIO $ print (mi', (ProjectChild pji mi))
                modify $ addEdge mi' (ProjectChild pji mi) []
