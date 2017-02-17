
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import CabalFilesystemSolution

import System.Environment

import Control.Monad
import Control.Monad.IO.Class

import Ide3.NewMonad

import Control.Monad.Except

import Ide3.Types
import qualified Ide3.Solution as Solution
import Ide3.NewMonad.Instances.State.Class
import Ide3.NewMonad.Instances.State.Class.Instances.Strict

import Control.Monad.State.Strict

import Ide3.NewMonad.Instances.State

import Data.GraphViz.Commands
import Data.GraphViz.Types
import qualified Data.Text.Lazy as T
import Data.Text (Text)

import Ide3.ModuleGraph

main :: IO ()
main = do
    quitWithoutGraphviz "Graphviz not installed"
    args <- getArgs
    case args of
        [path] -> do
            result <- flip runStateT Solution.empty $ runSolutionStateT $ runStatefulWrapper $ flip runCabalSolution Unopened $ runExceptT $ do
                load path
                graph <- makeSolutionImportGraph
                liftIO $ runGraphvizCanvas' graph Xlib
            case fst $ fst result of
                Left err -> do
                    let _ = err :: SolutionError ()
                    print err
                Right _ -> return ()
        _ -> do
            putStrLn "No path specified"
            putStrLn "USAGE:"
            putStrLn "hs-module-graph CABALFILE"

instance PrintDot (ProjectChild ModuleInfo) where
    unqtDot = unqtDot . T.fromStrict . getSymbol . getModuleName . getChild
    toDot = toDot . T.fromStrict . getSymbol . getModuleName . getChild
    unqtListToDot = unqtListToDot . map (T.fromStrict . getSymbol . getModuleName . getChild) 
    listToDot = listToDot . map (T.fromStrict . getSymbol . getModuleName . getChild)
