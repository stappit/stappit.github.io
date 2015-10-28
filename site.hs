--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))
import           Text.Pandoc.Options
import qualified Data.Set as S
import           System.FilePath

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- build up tags 
    tags <- buildTags postsGlob (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do 
        route idRoute 
        compile $ do 
            posts <- recentFirst =<< loadAll pattern 
            let ctx = constField "title" ("Posts tagged \"" ++ tag ++ "\"") 
    	            `mappend` listField "posts" postCtx (return posts) 
    	            `mappend` defaultContext 
            makeItem "" 
    	        >>= loadAndApplyTemplate "templates/tag.html" ctx 
	        >>= loadAndApplyTemplate "templates/default.html" ctx 
	        >>= relativizeUrls

    -- build up categories 
    categories <- buildCategories postsGlob (fromCapture "categories/*.html")
    tagsRules categories $ \tag pattern -> do 
        route idRoute 
        compile $ do 
            posts <- recentFirst =<< loadAll pattern 
            let ctx = constField "title" ("Posts in category \"" ++ tag ++ "\"") 
    	            `mappend` listField "posts" postCtx (return posts) 
    	            `mappend` defaultContext 
            makeItem "" 
    	        >>= loadAndApplyTemplate "templates/tag.html" ctx 
	        >>= loadAndApplyTemplate "templates/default.html" ctx 
	        >>= relativizeUrls

    match (postsGlob .||. "drafts/*") $ do
        route $ setExtension "html"
	compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= applyFilter postFilters
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    match "posts/**.lhs" $ version "raw" $ do
        route   idRoute
	compile getResourceBody

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (postsGlob .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (postsGlob .&&. hasNoVersion)
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots (postsGlob .&&. hasNoVersion) "content"
            renderAtom myFeedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postsGlob = "posts/**" :: Pattern

postCtx :: Context String
postCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , defaultContext
    ]

postCtxWithTags :: Tags -> Context String 
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

pandocMathCompiler =
    let mathExtensions = [ Ext_tex_math_dollars
                         , Ext_tex_math_double_backslash
			 , Ext_latex_macros
			 ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                writerExtensions = newExtensions
              , writerHTMLMathMethod = MathJax ""
	      , writerHtml5 = True
        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

----------------------------------------------------------------------------------
applyFilter :: (Monad m, Functor f) => (String -> String) -> f String -> m (f String)
applyFilter g fs = return . fmap g $ fs

preFilters :: String -> String
preFilters = noAtxLhs

postFilters :: String -> String
postFilters = mathjaxFix

mathjaxFix = replaceAll "><span class=\"math" (" class=\"mathjaxWide\"" ++)

noAtxLhs = replaceAll "^#" (" "++)
----------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Thoughts from the Café"
    , feedDescription = "This feed summarises some of what I learn in the Café."
    , feedAuthorName  = "Brian"
    , feedAuthorEmail = "ha@hahaha.com"
    , feedRoot        = "http://stappit.github.io"
    }
