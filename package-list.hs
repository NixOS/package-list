module Main ( main ) where

import Prelude hiding ( lookup )
import Distribution.Hackage.DB hiding ( null, foldr, map )
import Distribution.Text ( Text(..), display, simpleParse )
import Distribution.Compat.ReadP ( ReadP, (<++), char, munch1, string, look, skipSpaces, pfail )
import Text.PrettyPrint ( text )
import Control.Monad ( unless )
import Data.Char ( isSpace, toLower )
import Data.Ord ( comparing )
import Data.List ( sortBy )
import Data.Maybe ( isJust )
import System.Process ( readProcess )

newtype Path = Path String
  deriving (Show, Eq, Ord)

data NixPkg = NixPkg Path PackageIdentifier
  deriving (Show, Eq, Ord)

type PkgSet = Map PackageName (Version,Path)

instance Text Path where
  disp (Path p) = text p
  parse = Path `fmap` munch1 (not . isSpace)

instance Text NixPkg where
  disp (NixPkg _ pid) = disp pid
  parse = do path <- parse
             _ <- skipSpaces
             (pname,pver) <- hsLibrary <++ hsExecutable <++ other
             return (NixPkg path (PackageIdentifier pname pver))
    where
      hsLibrary    = do { string "haskell-"; pname <- parse; char '-'; string "ghc"; parse :: ReadP r Version; char '-'; pver <- parse; return (pname,pver) }
      hsExecutable = do { PackageIdentifier pname pver <- parse; pEof; return (pname, pver) }
      other        = do { pname <- munch1 (not . isSpace); return (PackageName pname, Version [] []) }
      pEof         = look >>= \s -> unless (null s) pfail

readNixPkgList :: IO [NixPkg]
readNixPkgList = readProcess "sh" ["-c", "nix-env -qaP '*' 2>/dev/tty"] "" >>= mapM p . lines
  where
    p :: String -> IO NixPkg
    p s = maybe (fail ("cannot parse: " ++ show s)) return (simpleParse s)

makeNixPkgSet :: Hackage -> [NixPkg] -> PkgSet
makeNixPkgSet db pkgs = foldr (uncurry (insertWith f)) empty [ (pn,(pv,p)) | NixPkg p (PackageIdentifier pn pv) <- pkgs, isOnHackage pn pv ]
  where
    isOnHackage :: PackageName -> Version -> Bool
    isOnHackage (PackageName n) v = isJust (lookup n db >>= lookup v)

    f :: (Version,Path) -> (Version,Path) -> (Version,Path)
    f x@(v1,p1@(Path path1)) y@(v2,p2@(Path path2))
      | v1 < v2                                                             = y
      | v1 > v2                                                             = x
      | length (takeWhile (/='.') path1) < length (takeWhile (/='.') path2) = x
      | length (takeWhile (/='.') path1) > length (takeWhile (/='.') path2) = y
      | length path1 < length path2                                         = x
      | length path1 > length path2                                         = y
      | p1 > p2                                                             = x
      | p1 < p2                                                             = y
      | otherwise = error ("cannot decide ordering of " ++ show x ++ " versus " ++ show y)

sortCaseless :: [(PackageName, (Version,Path))] -> [(PackageName, (Version,Path))]
sortCaseless = sortBy (\(PackageName x, _) (PackageName y, _) -> comparing (map toLower) x y)

formatPackageLine :: (PackageName,(Version,Path)) -> String
formatPackageLine (name, (version, path)) = show (display name, display version, Just url)
  where
    url = "http://hydra.nixos.org/job/nixpkgs/trunk/" ++ display path ++ ".x86_64-linux"

main :: IO ()
main = do
  hackage <- readHackage
  pkgset <- makeNixPkgSet hackage `fmap` readNixPkgList
  mapM_ (putStrLn . formatPackageLine) (sortCaseless (toList pkgset))
