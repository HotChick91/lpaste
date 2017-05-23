{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Report model.

module Hpaste.Model.Report
 (getSomeReports,createReport,countReports)
  where

import           Hpaste.Types
import           Hpaste.Controller.Cache
import           Hpaste.Types.Cache as Key

import           Control.Monad

import           Control.Monad.Env
import           Control.Monad.IO
import           Data.Pagination
import           Data.Maybe
import           Data.Monoid.Operator ((++))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Prelude              hiding ((++))
import           Snap.App

-- | Get some paginated reports.
getSomeReports :: Pagination -> Model c s [Report]
getSomeReports Pagination{..} =
  queryNoParams ["SELECT created,paste,comments"
                ,"FROM report"
                ,"ORDER BY id DESC"
                ,"OFFSET " ++ show (max 0 (pnCurrentPage - 1) * pnPerPage)
                ,"LIMIT " ++ show pnPerPage]

-- | Count reports.
countReports :: Model c s Integer
countReports = do
  rows <- singleNoParams ["SELECT COUNT(*)"
                         ,"FROM report"]
  return $ fromMaybe 0 rows

-- | Create a new report.
createReport :: ReportSubmit -> Model Config s (Maybe ReportId)
createReport rs@ReportSubmit{..} = do
  res <- single ["INSERT INTO report"
                ,"(paste,comments)"
                ,"VALUES"
                ,"(?,?)"
                ,"returning id"]
                (rsPaste,rsComments)
  _ <- exec ["UPDATE paste"
       	    ,"SET public = false"
	    ,"WHERE id = ?"]
	    (Only rsPaste)
  let reset pid = do
        resetCacheModel (Key.Paste pid)
        resetCacheModel (Key.Revision pid)
  reset rsPaste
  return res
