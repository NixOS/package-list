module Main ( main ) where

import Control.Monad ( unless )
import Data.Char ( isSpace )
import Data.List ( sort, intercalate )
import Data.Map hiding ( null, foldr, map )
import Data.Maybe ( isJust )
import Distribution.Compat.ReadP ( munch1, look, skipSpaces, pfail )
import Distribution.Hackage.DB
import Distribution.Package
import Distribution.Text ( Text(..), display, simpleParse )
import Distribution.Version
import Nix.Paths
import Prelude hiding ( lookup )
import System.Process ( readProcess )
import Text.PrettyPrint ( text )

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
             (pname,pver) <- pkgid
             return (NixPkg path (PackageIdentifier pname pver))
    where
      pkgid  = do { PackageIdentifier pname pver <- parse; pEof; return (pname, pver) }
      pEof   = look >>= \s -> unless (null s) pfail

readNixPkgList :: IO [NixPkg]
readNixPkgList = readProcess nixEnv ["-qaP", "-A", "haskellPackages"] "" >>= mapM p . lines
  where
    p :: String -> IO NixPkg
    p s = maybe (fail ("cannot parse: " ++ show s)) return (simpleParse s)

makeNixPkgSet :: HackageDB -> [NixPkg] -> PkgSet
makeNixPkgSet db pkgs = foldr (uncurry (insertWith f)) empty [ (pn,(pv,p)) | NixPkg p (PackageIdentifier pn pv) <- pkgs, isOnHackage pn pv ]
  where
    isOnHackage :: PackageName -> Version -> Bool
    isOnHackage n v = isJust (lookup n db >>= lookup v)

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

formatPackageLine :: (PackageName,(Version,Path)) -> String
formatPackageLine (name, (version, path)) = intercalate "," (map show [ display name, display version, url ])
  where
    url = "http://hydra.nixos.org/job/nixpkgs/trunk/" ++ display path ++ ".x86_64-linux"

main :: IO ()
main = do
  hackage <- hackageTarball >>= readTarball Nothing
  pkgset <- makeNixPkgSet hackage `fmap` readNixPkgList
  mapM_ (putStrLn . formatPackageLine) (sort (toList pkgset))
