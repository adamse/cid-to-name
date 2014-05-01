module Main where

import Control.Monad

import qualified Data.Map as M
import Data.Function
import Data.Maybe
import Data.List

import System.Environment
import System.Exit
import System.IO

type CID = String
type Name = String

data Out = Out { notFound :: [CID], found :: [(CID, Name)] }

main = do
  args <- getArgs
  case args of
    [passwdFile, cidFile] -> program passwdFile cidFile
    _ -> usage >> exitWith (ExitFailure 1)

usage = do
  putStrLn "Usage: cid-to-names passwdfile (cidfile|--)"

program passwdFile cidFile = do
   passwd <- readFile passwdFile
   cids <- case cidFile of 
     "--" -> getContents
     _ -> readFile cidFile

   let (Out nf f) = findNames (passwdToMap passwd) (lines cids)

   mapM_ (putStrLn . uncurry formatOutput) f

   unless (null nf) $ do 
     hPutStr stderr "Could not find the following users: "
     hPutStrLn stderr $ intercalate ", " nf


findNames :: M.Map CID Name -> [CID] -> Out
findNames cidnames cids = out where
  out = foldr (\cid out -> case M.lookup cid cidnames of
    Just name -> addFound out cid name
    Nothing -> addNotFound out cid) (Out [] []) cids

formatOutput :: CID -> Name -> String
formatOutput cid name = concat [name, " & ", cid, "\\\\ \\hline"]

passwdToMap :: String -> M.Map CID Name
passwdToMap passwd = M.fromList $ map cidname . lines $ passwd

cidname :: String -> (CID, Name)
cidname line = (user, name) where 
  contents = splitOn (== ':') line
  user = contents !! 0
  name = contents !! 4

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s = case dropWhile p s of
  [] -> []
  s' -> let (w, s'') = break p s' in w : splitOn p s'' 

addFound :: Out -> CID -> Name -> Out
addFound out@(Out _ f) cid name = out { found = (cid, name):f }

addNotFound :: Out -> CID -> Out
addNotFound out@(Out nf _) cid = out { notFound = cid:nf }
