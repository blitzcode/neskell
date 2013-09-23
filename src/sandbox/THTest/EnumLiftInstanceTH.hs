
{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleInstances #-}

module EnumLiftInstanceTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Simple TH splice to generate Lift instances for Enum types
makeEnumLiftInstance :: Name -> Q [Dec]
makeEnumLiftInstance name = do
    (TyConI (DataD _ _ _ ctors _)) <- reify name
    sequence $
        [ instanceD (return []) (appT (conT ''Lift) (conT name))
            [ funD 'lift .
                (flip map) ctors $ \(NormalC ctorName _) ->
                    clause [conP ctorName []]
                        ( -- This stringly typed abomination exists because I
                          -- really didn't want to manually derive a Lift
                          -- instance for Name
                          normalB (let cn = show ctorName
                                    in [| do Just nm <- lookupValueName cn
                                             conE nm
                                       |])
                        ) []
            ]
        ]


-- $(reify ''Bool >>= \(TyConI (DataD _ _ _ x _)) -> stringE $ show x)
-- [NormalC GHC.Types.False [],NormalC GHC.Types.True []]

{-
▸▸▸ runQ [d|instance Lift MyEnum where lift MyEnum123 = litE (StringL "123") ; lift MyEnum456 = litE (StringL "456") |]
[InstanceD [] (AppT (ConT Language.Haskell.TH.Syntax.Lift) (ConT :Interactive.MyEnum)) [FunD Language.Haskell.TH.Syntax.lift [Clause [ConP :Interactive.MyEnum123 []] (NormalB (AppE (VarE Language.Haskell.TH.Lib.litE) (AppE (ConE Language.Haskell.TH.Syntax.StringL) (LitE (StringL "123"))))) [],Clause [ConP :Interactive.MyEnum456 []] (NormalB (AppE (VarE Language.Haskell.TH.Lib.litE) (AppE (ConE Language.Haskell.TH.Syntax.StringL) (LitE (StringL "456"))))) []]]]
-}
{-

makeEnumLiftInstance :: Name -> Q [Dec]
makeEnumLiftInstance name = do
    (TyConI (DataD _ _ _ ctors _)) <- reify name
    sequence $
        [
            instanceD (return []) (appT (conT ''Lift) (conT name)) .
                (flip map) ctors $ \(NormalC ctorName _) ->
                    funD 'Language.Haskell.TH.Syntax.lift
                    [
                        clause [conP ctorName []]
                        (
                            normalB
                            (
                                 conE ctorName
                            )
                        ) []
                    ]
        ]

-}

{-

▸▸▸ runQ [d|instance Lift MyEnum where lift MyEnum123 = litE (StringL "ANC") |]

[InstanceD [] (AppT (ConT Language.Haskell.TH.Syntax.Lift) (ConT :Interactive.MyEnum)) [FunD Language.Haskell.TH.Syntax.lift [Clause [ConP :Interactive.MyEnum123 []] (NormalB (AppE (VarE Language.Haskell.TH.Lib.litE) (AppE (ConE Language.Haskell.TH.Syntax.StringL) (LitE (StringL "ANC"))))) []]]]

-}
