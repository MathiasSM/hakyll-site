module MathiasSM.Rules.Favicon (faviconRules) where

import Hakyll (
  Pattern,
  Rules,
  compile,
  customRoute,
  getResourceLBS,
  match,
  route,
  unixFilterLBS,
  version,
  withItemBody,
 )

-- | An icon generated from the logo SVG
data Favicon
  = -- | The SVG itself, served from the site root
    Svg
  | -- | Single-size .ico
    Ico
  | -- | PNG written to @file@, scaled to @size@ with @padding@ pixels on each side
    Png {file :: FilePath, size :: Int, padding :: Int}

favicons :: [Favicon]
favicons =
  [ Svg
  , Ico
  , Png "apple-touch-icon.png" 180 20
  , Png "favicon-192.png" 192 20
  , Png "favicon-512.png" 512 40
  ]

faviconPath :: Favicon -> FilePath
faviconPath Svg = "favicon.svg"
faviconPath Ico = "favicon.ico"
faviconPath Png{file} = file

-- | Generates every favicon from the given logo
faviconRules :: Pattern -> Rules ()
faviconRules ptn = match ptn $ mapM_ processFavicon favicons

processFavicon :: Favicon -> Rules ()
processFavicon favicon = version (faviconVersion favicon) $ do
  route $ customRoute $ const $ faviconPath favicon
  compile $ case faviconCommand favicon of
    Nothing -> getResourceLBS
    Just (cmd, args) -> getResourceLBS >>= withItemBody (unixFilterLBS cmd args)

-- | Distinguishes the versions of the logo item; one per generated file
faviconVersion :: Favicon -> String
faviconVersion = faviconPath

-- | ImageMagick invocation converting the SVG on stdin, if the favicon needs one
faviconCommand :: Favicon -> Maybe (String, [String])
faviconCommand Svg = Nothing
faviconCommand Ico =
  Just
    ( "convert"
    , ["-background", "none", "svg:-", "-define", "icon:auto-resize=32", "+repage", "ico:-"]
    )
faviconCommand Png{size, padding} =
  Just
    ( "convert"
    ,
      [ "-background"
      , "none"
      , "svg:-"
      , "-gravity"
      , "center"
      , "-scale"
      , show inner ++ "x" ++ show inner
      , "-extent"
      , show size ++ "x" ++ show size
      , "+repage"
      , "png:-"
      ]
    )
 where
  inner = size - padding * 2
