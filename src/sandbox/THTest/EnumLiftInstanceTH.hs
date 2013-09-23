
{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleInstances #-}

module EnumLiftInstanceTH (makeEnumLiftInstance) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- TH splice to generate Lift instances for Enum types
makeEnumLiftInstance :: Name -> Q [Dec]
makeEnumLiftInstance name = do
    (TyConI (DataD _ _ _ ctors _)) <- reify name
    sequence [ instanceD (return []) (appT (conT ''Lift) (conT name))
                 [ funD 'lift . (flip map) ctors $ \(NormalC ctorName _) ->
                     clause [conP ctorName []]
                         ( -- This stringly typed abomination exists because I
                           -- really didn't want to manually derive a Lift
                           -- instance for Name
                           normalB $ let cn = show ctorName
                                      in [| do Just nm <- lookupValueName cn
                                               conE nm
                                         |]
                         ) []
                 ]
             ]

