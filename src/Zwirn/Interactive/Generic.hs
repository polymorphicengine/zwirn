{-# LANGUAGE TemplateHaskell #-}
module Zwirn.Interactive.Generic where

import Language.Haskell.TH
import Zwirn.Interactive.Meta (toNum, toPat)
import qualified Prelude as P

-- template haskell stuff

-- mkNumParams2 :: [P.String] -> Q [Dec]
-- mkNumParams2 names = P.return (P.map oneDec names)
--                   where oneDec name = ValD (VarP (mkName name)) (NormalB (AppE (AppE (VarE 'apply) (VarE 'pN)) (AppE (VarE 'P.pure) (LitE (StringL name))))) []

mkStringParams :: [P.String] -> Q [Dec]
mkStringParams names = P.return (P.map oneDec names)
                  where oneDec name = ValD (VarP (mkName name)) (NormalB (AppE (VarE 'toPat) (VarE (mkName ("T." P.++ name))))) []

mkNumParams :: [P.String] -> Q [Dec]
mkNumParams names = P.return (P.map oneDec names)
                  where oneDec name = ValD (VarP (mkName name)) (NormalB (AppE (VarE 'toPat) (AppE (VarE 'toNum) (VarE (mkName ("T." P.++ name)))))) []
