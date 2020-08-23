{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Data.Mutators
       (
         genMutators
       , genMutators'
       ) where

import Language.Haskell.TH
import Data.Char (toUpper)
import Control.Monad (liftM)


upFirst :: String -> String
upFirst [] = []
upFirst (x:xs) = (toUpper x) : xs


type MutatorNameGen = String ->          -- record field name
                      (String, String)   -- (setter, modifier)


defMutNameGen :: MutatorNameGen 
defMutNameGen accessor = (nameSet, nameMod)
  where nameSet = "set" ++ prettyAcc
        nameMod = "mod" ++ prettyAcc
        prettyAcc = upFirst accessor


genMutator :: MutatorNameGen -> Name -> Q [Dec]
genMutator mng accessor = do
    varObj <- newName "obj"
    varVal <- newName "val"
    varFcn <- newName "fcn"
    let (nameSet, nameMod) = mng $ nameBase accessor
 
    return $
      -- setSomething obj value = obj { something = value }
      [ FunD (mkName nameSet)
        [Clause [VarP varObj, VarP varVal]
         (NormalB (RecUpdE (VarE varObj) [(accessor, VarE varVal)]))
         []]

       -- modSomething obj f = { something = f (something obj) }
      , FunD (mkName nameMod)
        [Clause [VarP varObj, VarP varFcn]
         (NormalB (RecUpdE (VarE varObj)
                   [(accessor,
                     AppE (VarE varFcn)
                          (AppE (VarE accessor) (VarE varObj)))]))
         []]
      ]


isRecord :: Con -> Bool
isRecord (RecC _ _) = True
isRecord _= False


genMutators' :: Name -> MutatorNameGen -> Q [Dec]
genMutators' dataName mng = do
    TyConI (DataD _ _ _ _ defs _) <- reify dataName
    let names = map name $ concat $ map fields $ filter isRecord defs
    liftM concat $ mapM (genMutator mng) names
  where fields (RecC _ fs) = fs
        name (n,_,_) = n


genMutators :: Name -> Q [Dec]
genMutators n = genMutators' n defMutNameGen
