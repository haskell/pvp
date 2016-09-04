{-# LANGUAGE OverloadedStrings #-}

import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ do
        compile templateBodyCompiler

    match "*.svg" $ do
        route   idRoute
        compile copyFileCompiler

    match "pvp-specification.md" $ do
        route (constRoute "index.html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "pvp-faq.md" $ do
        route (constRoute "faq/index.html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
