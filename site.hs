{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid (mappend)
import Hakyll
import qualified System.Environment as Env
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)
import Text.Pandoc.Options
import Text.Pandoc.Highlighting
import Text.Pandoc.Extensions

main :: IO ()
main = do
  includeDrafts <- fmap (== Just "true") (Env.lookupEnv "INCLUDE_DRAFTS")
  hakyll $ do
    let postsPattern =
          if includeDrafts
            then "posts/*" .||. "posts/drafts/*"
            else "posts/*"

    tags <- buildTags postsPattern (fromCapture "tags/*.html")
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match (fromList ["about.markdown", "contact.markdown", "resume.markdown"]) $ do
      route $ setExtension "html"
      compile $
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    -- Generate PDF version of resume
    match "resume.markdown" $ version "pdf" $ do
      route $ setExtension "pdf"
      compile $ do
        body <- getResourceString
        pdfBytes <- unsafeCompiler $ makePDF' (itemBody body)
        makeItem pdfBytes

    tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx =
              constField "title" ("Posts tagged \"" ++ tag ++ "\"")
                `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                `mappend` defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    match postsPattern $ do
      route $ setExtension "html"
      compile $
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
          >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
          >>= relativizeUrls

    create ["blog.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll postsPattern
        let blogCtx =
              listField "posts" (postCtxWithTags tags) (return posts)
                `mappend` defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/blog.html" blogCtx
          >>= loadAndApplyTemplate "templates/default.html" blogCtx
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll postsPattern
        let indexCtx =
              listField "posts" (postCtxWithTags tags) (return posts)
                `mappend` defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

-- Custom Pandoc compiler with syntax highlighting
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith customReaderOptions customWriterOptions
  where
    customReaderOptions = defaultHakyllReaderOptions
      { readerExtensions = enableExtension Ext_tex_math_dollars $
                          enableExtension Ext_tex_math_single_backslash $
                          enableExtension Ext_tex_math_double_backslash $
                          readerExtensions defaultHakyllReaderOptions
      }
    customWriterOptions = defaultHakyllWriterOptions
      { writerHighlightStyle = Just pygments
      , writerHTMLMathMethod = MathJax ""
      }

makePDF' :: String -> IO BL.ByteString
makePDF' content = do
  let cleanedContent = stripFrontMatter content
  withSystemTempFile "resume.md" $ \mdPath mdHandle -> do
    withSystemTempFile "resume.pdf" $ \pdfPath pdfHandle -> do
      L8.hPutStr mdHandle (L8.pack cleanedContent)
      hClose mdHandle
      hClose pdfHandle
      callProcess
        "pandoc"
        [ mdPath,
          "-o",
          pdfPath,
          "-V",
          "geometry:margin=0.5in",
          "-V",
          "fontsize=11pt",
          "-V",
          "linestretch=0.9"
        ]
      BL.readFile pdfPath

stripFrontMatter :: String -> String
stripFrontMatter content =
  let linesContent = lines content
      afterFrontMatter = dropWhile (/= "---") $ drop 1 $ dropWhile (/= "---") linesContent
      withoutFrontMatter = if null afterFrontMatter then linesContent else tail afterFrontMatter
   in unlines withoutFrontMatter
