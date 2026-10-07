{- | Builds the fixture site in-process and compares it with the baseline in
@test\/snapshot@. Run it on its own with @cabal test snapshot@; regenerate the
baseline with @cabal test snapshot --test-options=--update@.

Images are compared by format and dimensions (their bytes embed timestamps and
vary with the ImageMagick version); everything else byte for byte.
-}
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (sort)
import Hakyll.Commands qualified as Commands
import Hakyll.Core.Logger qualified as Logger
import Hakyll.Core.Runtime (RunMode (RunModeNormal))
import MathiasSM.Config (siteConfiguration)
import MathiasSM.Site (rules)
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  doesDirectoryExist,
  getCurrentDirectory,
  listDirectory,
  removePathForcibly,
  withCurrentDirectory,
 )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (rawSystem)

main :: IO ()
main = do
  update <- elem "--update" <$> getArgs
  root <- getCurrentDirectory
  bracket
    (getCanonicalTemporaryDirectory >>= \tmp -> createTempDirectory tmp "snapshot")
    removePathForcibly
    $ \work -> check root update work

-- | Builds the fixture site and either refreshes the baseline or diffs against it
check :: FilePath -> Bool -> FilePath -> IO ()
check root update work = do
  let buildDir = work </> "build"
      baseline = root </> "test" </> "snapshot"
      snapshot = work </> "snapshot"
  createDirectoryIfMissing True buildDir
  copyTree (root </> "templates") (buildDir </> "templates")
  copyTree (root </> "assets") (buildDir </> "assets")
  copyTree (root </> "test" </> "fixtures" </> "content") (buildDir </> "content")
  withCurrentDirectory buildDir $ do
    -- An Error-only logger keeps the build silent on success, like the old script
    logger <- Logger.new Logger.Error
    Commands.build RunModeNormal siteConfiguration logger rules >>= \case
      ExitSuccess -> pure ()
      _ -> exitFailure
  buildSnapshot (buildDir </> "_site") snapshot
  if update
    then do
      removePathForcibly baseline
      copyTree snapshot baseline
      putStrLn $ "snapshot: updated " ++ baseline
    else do
      code <- rawSystem "diff" ["-r", baseline, snapshot]
      case code of
        ExitSuccess -> putStrLn "snapshot: output matches test/snapshot"
        _ -> do
          hPutStrLn stderr "snapshot: output differs from test/snapshot"
          hPutStrLn stderr "  (run `cabal test snapshot --test-options=--update` if the change is intended)"
          exitFailure

{- | Mirrors the build output into a comparable snapshot: every file except
images, plus @images.txt@ listing each image's format and dimensions
-}
buildSnapshot :: FilePath -> FilePath -> IO ()
buildSnapshot site snapshot = do
  files <- relativeFiles site
  let images = [file | file <- files, isImage file]
      others = [file | file <- files, not (isImage file)]
  createDirectoryIfMissing True snapshot
  forM_ others $ \file -> do
    let destination = snapshot </> file
    createDirectoryIfMissing True (takeDirectory destination)
    copyFile (site </> file) destination
  listing <- forM (sort images) $ \file -> do
    bytes <- BS.readFile (site </> file)
    pure $ case imageInfo bytes of
      Just (format, width, height) -> "./" ++ file ++ " " ++ format ++ " " ++ show width ++ "x" ++ show height ++ " "
      Nothing -> "./" ++ file ++ " unknown "
  writeFile (snapshot </> "images.txt") (unlines listing)

-- | Image files (compared by signature rather than content)
isImage :: FilePath -> Bool
isImage file = takeExtension file `elem` [".png", ".ico"]

-- | Every file below the root, as paths relative to it
relativeFiles :: FilePath -> IO [FilePath]
relativeFiles root = go ""
 where
  go relative = do
    entries <- listDirectory (root </> relative)
    fmap concat . forM entries $ \entry -> do
      let child = if null relative then entry else relative </> entry
      isDir <- doesDirectoryExist (root </> child)
      if isDir then go child else pure [child]

-- | Copies a directory tree, creating the destination as needed
copyTree :: FilePath -> FilePath -> IO ()
copyTree source destination = do
  createDirectoryIfMissing True destination
  entries <- listDirectory source
  forM_ entries $ \entry -> do
    let from = source </> entry
        to = destination </> entry
    isDir <- doesDirectoryExist from
    if isDir then copyTree from to else copyFile from to

{- | The format and dimensions of a PNG or ICO, read from its header

The bytes of these files change between machines (embedded timestamps, converter
version), so their dimensions are compared instead.
-}
imageInfo :: ByteString -> Maybe (String, Int, Int)
imageInfo bytes
  | BS.length bytes >= 24
  , BS.take 8 bytes == pngSignature =
      Just ("PNG", be32 (BS.drop 16 bytes), be32 (BS.drop 20 bytes))
  | BS.length bytes >= 8
  , BS.take 4 bytes == icoSignature =
      Just ("ICO", iconSize 6, iconSize 7)
  | otherwise = Nothing
 where
  pngSignature = BS.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
  icoSignature = BS.pack [0x00, 0x00, 0x01, 0x00]
  be32 bytes' = foldl (\acc index -> acc * 256 + fromIntegral (BS.index bytes' index)) 0 [0 .. 3]
  -- An ICO entry byte of 0 means 256
  iconSize index =
    let size = fromIntegral (BS.index bytes index)
     in if size == 0 then 256 else size
