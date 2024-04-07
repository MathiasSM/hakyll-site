{-# LANGUAGE OverloadedStrings #-}

module MathiasSM.Rules.Assets.Favicon (faviconRules) where

import Hakyll

data Favicon = Svg | Ico | IOS | Png Int Int

favicons :: [Favicon]
favicons = [Svg, Ico, IOS, Png 192 20, Png 512 40]

faviconPath :: Favicon -> FilePath
faviconPath Ico = "favicon.ico"
faviconPath IOS = "apple-touch-icon.png"
faviconPath Svg = "favicon.svg"
faviconPath (Png size padding) = "favicon-" ++ show size ++ ".png"

faviconRules :: Pattern -> Rules ()
faviconRules ptn = match ptn $ mapM_ processFavicon favicons

processFavicon :: Favicon -> Rules ()
processFavicon favicon@Ico = processIco favicon
processFavicon favicon@Svg = processSvg favicon
processFavicon favicon@IOS = processPng 180 20 favicon
processFavicon favicon@(Png size padding) = processPng size padding favicon

---

processSvg :: Favicon -> Rules ()
processSvg favicon = version "root-svg" $ do
  route $ customRoute $ \_ -> faviconPath favicon
  compile getResourceLBS

processIco :: Favicon -> Rules ()
processIco favicon = version "ico-32" $ do
  route $ customRoute $ \_ -> faviconPath favicon
  let cmd = "convert"
      args =
        [ "-background"
        , "none"
        , "svg:-"
        , "-define"
        , "icon:auto-resize=32"
        , "+repage"
        , "ico:-"
        ]
  compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)

processPng :: Int -> Int -> Favicon -> Rules ()
processPng size padding favicon =
  let versionName = ("png-resized-" ++ show size ++ "-" ++ show padding)
   in version versionName $ do
        route $ customRoute $ \_ -> faviconPath favicon
        let cmd = "convert"
            args =
              [ "-background"
              , "none"
              , "svg:-"
              , "-gravity"
              , "center"
              , "-scale"
              , show (size - padding * 2) ++ "x" ++ show (size - padding * 2)
              , "-extent"
              , show size ++ "x" ++ show size
              , "+repage"
              , "png:-"
              ]
        compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)
