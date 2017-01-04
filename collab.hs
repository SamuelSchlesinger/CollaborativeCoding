{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Prelude hiding (lines, writeFile, unlines, drop)
import Data.Text (lines, unlines, isPrefixOf, drop, Text, unpack, pack)
import Data.Text.IO (writeFile)
import System.Environment
import Control.Monad.Trans
import Reddit.Types.Comment
import Reddit.Types.Post
import Reddit

roptions = RedditOptions
  { rateLimitingEnabled = True
  , connectionManager = Nothing
  , loginMethod = Anonymous
  , customUserAgent = Just "CollaborativeCodingScraper" }

main = runRedditWith roptions redditMain 

redditMain :: RedditT IO ()
redditMain = do
  x <- liftIO getArgs
  (title, body) <- case x of
    "post"    : postID    : _ -> post2code (PostID (pack postID))
    "comment" : commentID : _ -> comment2code (CommentID (pack commentID))
  liftIO $ writeFile (unpack title) body 
  
post2code :: PostID -> RedditT IO (Text, Text)
post2code p = do
  x <- getPostInfo p
  SelfPost body _ <- pure $ content x
  code <- pure (codeFilter body)
  return (title x, code)

codeFilter = unlines 
           . map (\x -> drop 4 x) 
           . filter (isPrefixOf "    ") 
           . lines

comment2code :: CommentID -> RedditT IO (Text, Text)
comment2code c = do
  x <- getCommentInfo c
  case inReplyTo x of
    Just c' -> do
      (title, code) <- comment2code c'
      return (title,  unlines (lines code ++ lines (codeFilter (body x))))
    Nothing -> do
      (title, code) <- post2code (parentLink x)
      return (title, unlines (lines code ++ lines (codeFilter (body x))))
