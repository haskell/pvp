{-# LANGUAGE OverloadedStrings #-}

import           Hakyll
import qualified Text.Pandoc      as Pandoc
import qualified Text.Pandoc.Walk as Pandoc

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
        compile $ pvpPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "pvp-faq.md" $ do
        route (constRoute "faq/index.html")
        compile $ pvpPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


--------------------------------------------------------------------------------
-- | Our own pandoc compiler.
pvpPandocCompiler :: Compiler (Item String)
pvpPandocCompiler = pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    addAnchors


--------------------------------------------------------------------------------
-- | Modifies a headers to add an extra anchor which links to the header.
addAnchors :: Pandoc.Pandoc -> Pandoc.Pandoc
addAnchors =
    Pandoc.walk addAnchor
  where
    addAnchor :: Pandoc.Block -> Pandoc.Block
    addAnchor (Pandoc.Header level attr@(id_, _, _) content) =
        Pandoc.Header level attr $ content ++
            [Pandoc.Link ("", ["anchor"], []) [Pandoc.Str "🔗"] ('#' : id_, "")]
    addAnchor block = block
