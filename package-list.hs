module Main ( main ) where

import Control.Applicative
import Data.Char
import qualified Data.Map as Map
import Distribution.Hackage.DB ( readHackage, Hackage, PackageIdentifier(..), PackageName(..), Version(..) )
import Distribution.Text
import Text.PrettyPrint ( text )
import Distribution.Compat.ReadP
import System.Process ( readProcess )

newtype Path = Path String
  deriving (Show, Eq, Ord)

instance Text Path where
  disp (Path p) = text p
  parse = Path <$> munch1 (not . isSpace)

data NixPkg = NixPkg Path PackageIdentifier
  deriving (Show, Eq, Ord)

instance Text NixPkg where
  disp (NixPkg _ pid) = disp pid
  parse = do path <- parse
             _ <- skipSpaces
             (pname,pver) <- hsLibrary <++ hsExecutable +++ other
             return (NixPkg path (PackageIdentifier pname pver))
    where
      hsLibrary    = do { string "haskell-"; pname <- parse; char '-'; string "ghc"; parse :: ReadP r Version; char '-'; pver <- parse; return (pname,pver) }
      hsExecutable = do { pname <- parse; char '-'; pver <- parse; return (pname, pver) }
      other        = do { pname <- munch1 (not . isSpace); return (PackageName pname, Version [] []) }

type PkgSet = Map.Map PackageName (Version,Path)

readNixPkgList :: IO [NixPkg]
readNixPkgList = readProcess "sh" ["-c", "nix-env -qaP '*' 2>/dev/tty"] "" >>= mapM p . lines
  where
    p :: String -> IO NixPkg
    p s = maybe (fail ("cannot parse: " ++ show s)) return (simpleParse s)

makeNixPkgSet :: Hackage -> [NixPkg] -> PkgSet
makeNixPkgSet db pkgs = foldr (uncurry (Map.insertWith f)) Map.empty [ (pn,(pv,p)) | NixPkg p (PackageIdentifier pn pv) <- pkgs, isOnHackage pn pv ]
  where
    isOnHackage :: PackageName -> Version -> Bool
    isOnHackage (PackageName n) v = maybe False (const True) (Map.lookup n db)

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
      | otherwise                                                           = error ("cannot decide ordering of " ++ show x ++ " versus " ++ show y)

formatPackageLine :: (PackageName,(Version,Path)) -> String
formatPackageLine (name, (version, path)) = show (display name, display version, Just url)
  where
    url = "http://hydra.nixos.org/job/nixpkgs/trunk/" ++ display path

main :: IO ()
main = do
  hackage <- readHackage
  pkgset <- makeNixPkgSet hackage <$> readNixPkgList
  mapM_ (putStrLn . formatPackageLine) (Map.toAscList pkgset)
