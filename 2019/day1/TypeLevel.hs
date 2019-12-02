{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevel where
import           GHC.TypeLits

type family IfThenElse (cond :: Bool) (whenTrue :: b) (whenFalse :: b) :: b where
  IfThenElse True trueBranch falseBranch = trueBranch
  IfThenElse False trueBranch falseBranch = falseBranch

type family LessThan (a :: Nat) (b :: Nat) :: Bool where
  LessThan a a = False
  LessThan a 0 = False
  LessThan 0 b = True
  LessThan a b = LessThan (a - 1) (b - 1)

type family ModuleFuel (inputMass :: Nat) :: Nat where
  ModuleFuel 0 = 0
  ModuleFuel n = IfThenElse (LessThan n 6) 0 ((Div n 3) - 2)

type family ModuleFuel' (inputMass :: Nat) :: Nat where
  ModuleFuel' 0 = 0
  ModuleFuel' n = (ModuleFuel n) + ModuleFuel' (ModuleFuel n)

type family SolveFuel (modules :: [Nat]) :: Nat where
  SolveFuel '[] = 0
  SolveFuel (m:ms) = (ModuleFuel' m) + SolveFuel ms

type Input = '[76663
  , 111378
  , 132647
  , 115688
  , 67473
  , 85562
  , 62955
  , 64052
  , 104961
  , 128687
  , 60344
  , 81158
  , 129984
  , 106462
  , 55967
  , 130004
  , 140810
  , 71523
  , 64891
  , 142922
  , 122783
  , 123918
  , 116246
  , 120842
  , 105578
  , 122950
  , 107512
  , 70051
  , 55347
  , 54348
  , 89301
  , 95258
  , 122323
  , 136781
  , 137756
  , 95658
  , 91017
  , 79626
  , 98414
  , 79296
  , 75226
  , 143850
  , 131334
  , 107028
  , 76591
  , 75492
  , 66400
  , 51904
  , 79262
  , 68956
  , 98957
  , 52481
  , 87955
  , 118871
  , 148734
  , 103699
  , 68681
  , 55118
  , 144120
  , 59403
  , 115012
  , 147742
  , 124218
  , 73580
  , 114949
  , 65346
  , 113104
  , 129059
  , 119068
  , 72339
  , 74984
  , 53095
  , 127452
  , 133786
  , 111439
  , 98153
  , 96312
  , 139641
  , 88907
  , 136831
  , 73574
  , 67871
  , 57641
  , 134505
  , 72116
  , 134503
  , 134387
  , 88598
  , 78687
  , 61020
  , 107234
  , 64801
  , 132668
  , 60204
  , 90001
  , 87833
  , 131148
  , 61488
  , 107938
  , 116072]
