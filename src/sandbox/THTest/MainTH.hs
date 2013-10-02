
{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module MainTH where

import Instruction
import MonadEmulator

import EnumLiftInstanceTH

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
--import Control.Applicative ((<$>))
import Data.Word (Word8)

{-
instance Lift Mnemonic where
    lift ADC = [| ADC |]
    lift AND = [| AND |]
    lift ASL = [| ASL |]
    lift BCC = [| BCC |]
    lift BCS = [| BCS |]
    lift BEQ = [| BEQ |]
    lift BIT = [| BIT |]
    lift BMI = [| BMI |]
    lift BNE = [| BNE |]
    lift BPL = [| BPL |]
    lift BRK = [| BRK |]
    lift BVC = [| BVC |]
    lift BVS = [| BVS |]
    lift CLC = [| CLC |]
    lift CLD = [| CLD |]
    lift CLI = [| CLI |]
    lift CLV = [| CLV |]
    lift CMP = [| CMP |]
    lift CPX = [| CPX |]
    lift CPY = [| CPY |]
    lift DEC = [| DEC |]
    lift DEX = [| DEX |]
    lift DEY = [| DEY |]
    lift EOR = [| EOR |]
    lift INC = [| INC |]
    lift INX = [| INX |]
    lift INY = [| INY |]
    lift JMP = [| JMP |]
    lift JSR = [| JSR |]
    lift LDA = [| LDA |]
    lift LDX = [| LDX |]
    lift LDY = [| LDY |]
    lift LSR = [| LSR |]
    lift NOP = [| NOP |]
    lift ORA = [| ORA |]
    lift PHA = [| PHA |]
    lift PHP = [| PHP |]
    lift PLA = [| PLA |]
    lift PLP = [| PLP |]
    lift ROL = [| ROL |]
    lift ROR = [| ROR |]
    lift RTI = [| RTI |]
    lift RTS = [| RTS |]
    lift SBC = [| SBC |]
    lift SEC = [| SEC |]
    lift SED = [| SED |]
    lift SEI = [| SEI |]
    lift STA = [| STA |]
    lift STX = [| STX |]
    lift STY = [| STY |]
    lift TAX = [| TAX |]
    lift TAY = [| TAY |]
    lift TSX = [| TSX |]
    lift TXA = [| TXA |]
    lift TXS = [| TXS |]
    lift TYA = [| TYA |]
    lift KIL = [| KIL |]
    lift LAX = [| LAX |]
    lift SAX = [| SAX |]
    lift DCP = [| DCP |]
    lift ISC = [| ISC |]
    lift RLA = [| RLA |]
    lift RRA = [| RRA |]
    lift SLO = [| SLO |]
    lift SRE = [| SRE |]
    lift ANC = [| ANC |]
    lift ALR = [| ALR |]
    lift ARR = [| ARR |]
    lift XAA = [| XAA |]
    lift AHX = [| AHX |]
    lift TAS = [| TAS |]
    lift SHX = [| SHX |]
    lift SHY = [| SHY |]
    lift LAS = [| LAS |]
    lift AXS = [| AXS |]
-}

makeEnumLiftInstance ''Mnemonic

makeEnumLiftInstance ''AddressMode

{-
instance Lift Int where
    lift x = return (LitE (IntegerL (fromIntegral x)))

▸▸▸ runQ [| 10 :: Word8 |]
SigE (LitE (IntegerL 10)) (ConT GHC.Word.Word8)
-}

instance Lift Word8 where
    lift x = sigE (litE (integerL (fromIntegral x))) (conT ''Word8)

{-
uopCond :: Q Exp
uopCond = do
    appE [| putStrLn |] (litE $ StringL "123")

opSwitch :: Q Exp
opSwitch = do
    appE [| putStrLn |] (litE $ StringL "123")

sel :: Int -> Int -> ExpQ
sel i n = [| \x -> $(caseE [| x |] [alt]) |]
    where alt :: MatchQ
          alt = match pat (normalB rhs) []
 
          pat :: Pat
          pat = tupP (map varP as)
 
          rhs :: ExpQ
          rhs = varE(as !! (i -1)) -- !! is 0 based
 
          as :: [String]
          as = ["a" ++ show i | i <- [1..n] ]
-}


{-
▸▸▸ runQ [d| execute :: Mnemonic -> AddressMode -> IO (); execute mn am = putStrLn $ show mn ++ show am |]
[SigD execute_4 (AppT (AppT ArrowT (ConT MainTH.Mnemonic)) (AppT (AppT ArrowT (ConT MainTH.AddressMode)) (AppT (ConT GHC.Types.IO) (TupleT 0)))),FunD execute_4 [Clause [VarP mn_5,VarP am_6] (NormalB (InfixE (Just (VarE System.IO.putStrLn)) (VarE GHC.Base.$) (Just (InfixE (Just (AppE (VarE GHC.Show.show) (VarE mn_5))) (VarE GHC.Base.++) (Just (AppE (VarE GHC.Show.show) (VarE am_6))))))) []]]
-}

--where func = [| \mn am -> putStr $ show mn ++ " " ++ show am |]

{-
someDoFunc :: Q Exp
someDoFunc =
    [| do putStr "2"
          x <- getLine
          putStr x
    |]

someDoFunc2 :: Q Exp
someDoFunc2 =
    doE . map noBindS $
        [ [| putStr "1" |]
        , [| putStr "2" |]
        ]
-}

makeExecute :: Q [Dec]
makeExecute =
    [d| {-# SPECIALIZE execute :: Word8 -> RSTEmu s () #-}
        execute :: MonadEmulator m => Word8 -> m ();
        execute = $(f)
    |] where
      f =
        do -- mnName <- newName "mn"
           -- amName <- newName "am"
           w8Name <- newName "w8"
           lamE [{-varP mnName, varP amName-} varP w8Name]
               . caseE (tupE [ {-varE mnName, varE amName-} varE w8Name ])
               . (++ [errMatch])
               . (flip map) [decodeOpCode w8 | w8 <- [0..255]]
               $ \(viewOpCode -> OpCode w8 mn am) ->
                   do --ConE mnName' <- lift mn
                      --ConE amName' <- lift am
                      ConE w8Name' <- lift w8
                      match (tupP [ {-conP mnName' [], conP amName' []-} conP w8Name' [] ])
                            (normalB $ instrImpl w8 mn am) []
      instrImpl w8 mn am = doE . map noBindS $
          [ [| {-putStrLn $ show mn ++ " " ++ show am-} advCycles 1 |]
          ] 
      errMatch = match wildP
                       (normalB $ appE (varE 'error) (litE (StringL "Invalid OpCode")))
                       []

-- $(mapM (\y -> do ConE n <- lift y; return n) [False, True] >>= stringE . show)

--(Just (LamE [VarP x_0] (CaseE (ConE MainTH.LDA) [Match (VarP x_1) (NormalB (LitE (StringL ""))) []])))

--func = [| \mn am -> "" |]
--match (conP n []) (normalB (litE (StringL (show x)))) []
                              
--  $(reify ''Bool >>= \(TyConI (DataD _ _ _ x _)) -> stringE $ show x)

{-
execute :: Mnemonic -> Q Exp
execute mn =
    caseE mn alt
        where alt :: [MatchQ]
              alt = [ match (conP 'MainTH.ANC []) (normalB (litE (StringL "ANC"))) []
                    , match (conP 'MainTH.LDA []) (normalB (litE (StringL "LDA"))) []
                    ]
-}

{-
execute :: Q Exp
execute = do
    mn <- newName "mn"    
    am <- newName "am"    
    lamE [varP mn, varP am]
        (
            caseE (varE mn)
            [
                match ('MainTH.LDA) (normalB (litE (StringL "LDA"))) [],
                match (conP MainTH.ANC []) (normalB (litE (StringL "ANC"))) []
            ]
        )
-}

{-
▸▸▸ runQ [| \a b -> case a of LDA -> "LDA"; ANC -> "ANC" |]
LamE [VarP a_0,VarP b_1] (CaseE (VarE a_0) [Match (ConP MainTH.LDA []) (NormalB (LitE (StringL "LDA"))) [],Match (ConP MainTH.ANC []) (NormalB (LitE (StringL "ANC"))) []])


▸▸▸ let op = 1 in runQ [| case op of 1 -> "1"; 2 -> "2" |]
CaseE (LitE (IntegerL 1))
[
    Match (LitP (IntegerL 1)) (NormalB (LitE (StringL "1"))) [],
    Match (LitP (IntegerL 2)) (NormalB (LitE (StringL "2"))) []
]

▸▸▸ let op = 1 in runQ [| do { putStr "1"; putStr "2" } |]
DoE
[
    NoBindS (AppE (VarE System.IO.putStr) (LitE (StringL "1"))),
    NoBindS (AppE (VarE System.IO.putStr) (LitE (StringL "2")))
]

-}

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

