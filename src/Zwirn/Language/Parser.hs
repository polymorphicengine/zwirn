{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Parser
    ( parseActionsWithPos
    , parseActions
    , parseBlocks
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.List (intercalate, sortOn)

import qualified Zwirn.Language.Lexer as L
import Zwirn.Language.Syntax
import Zwirn.Language.TypeCheck.Types
import Zwirn.Language.TypeCheck.Infer
import Zwirn.Language.Block
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 t51 t52 t53 t54 t55 t56 t57 t58 t59 t60
	= HappyTerminal (L.RangedToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn7 (Term)
	| HappyAbsSyn10 ([Term])
	| HappyAbsSyn25 (Def)
	| HappyAbsSyn26 ([Def])
	| HappyAbsSyn27 (Action)
	| HappyAbsSyn28 ([Action])
	| HappyAbsSyn29 (Block)
	| HappyAbsSyn30 ([Block])
	| HappyAbsSyn32 (Type)
	| HappyAbsSyn34 (Predicate)
	| HappyAbsSyn35 ([Predicate])
	| HappyAbsSyn36 (Scheme)
	| HappyAbsSyn37 ((Text,Scheme))
	| HappyAbsSyn38 ([(Text,Scheme)])
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44
	| HappyAbsSyn45 t45
	| HappyAbsSyn46 t46
	| HappyAbsSyn47 t47
	| HappyAbsSyn48 t48
	| HappyAbsSyn49 t49
	| HappyAbsSyn50 t50
	| HappyAbsSyn51 t51
	| HappyAbsSyn52 t52
	| HappyAbsSyn53 t53
	| HappyAbsSyn54 t54
	| HappyAbsSyn55 t55
	| HappyAbsSyn56 t56
	| HappyAbsSyn57 t57
	| HappyAbsSyn58 t58
	| HappyAbsSyn59 t59
	| HappyAbsSyn60 t60

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,823) ([0,0,0,53248,1188,1,0,0,0,16384,2,1728,0,0,0,0,48,0,0,0,0,0,4,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,32,0,0,0,0,0,128,0,0,0,0,10548,65,0,0,0,0,42192,260,0,0,0,0,0,0,0,0,0,0,19712,4170,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,6150,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19021,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19712,4170,0,0,0,0,13312,16681,0,0,0,0,53248,1188,1,0,0,0,16384,0,0,0,0,0,0,0,32,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,24704,384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,16384,4755,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,2048,0,0,0,0,0,0,1,0,0,0,0,14644,65,0,0,0,0,0,0,0,0,0,0,37696,1042,0,0,0,0,2048,0,0,0,0,0,8192,0,0,0,0,0,53248,1188,1,0,0,0,16384,4755,4,0,0,0,0,19021,16,0,0,0,0,10548,65,0,0,0,0,42192,260,0,0,0,0,37696,1042,0,0,0,0,0,0,4,0,0,0,1024,0,0,0,0,0,53248,1188,1,0,0,0,16384,2,1728,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,1024,0,0,0,0,0,0,2,4096,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,53248,1188,1,0,0,0,0,0,0,0,0,0,0,19021,16,0,0,0,0,10548,65,0,0,0,0,42192,262,0,0,0,0,37696,1042,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,53248,1188,1,0,0,0,0,0,0,0,0,0,0,19021,16,0,0,0,0,10548,65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,4755,4,0,0,0,0,0,0,0,0,0,0,10548,65,0,0,0,0,42192,260,0,0,0,0,24576,384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19021,16,0,0,0,0,10548,65,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,32,0,0,0,0,0,8,15360,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,2048,0,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,8192,0,0,0,0,27213,24,0,0,0,0,10548,65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,32,61440,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10548,97,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","%start_pActions","%start_pBlocks","%start_pTypeDecls","atom","sequence","sequence2","stack","choice","lambda","polyrhythm","elongate","repeat","fullSequence","alternation","euclid","simple","simpleinfix","infix","simpleApp","simpleSeq","term","def","defs","action","actions","block","blocksrec","blocks","atomType","fullType","predicate","predicates","scheme","typeDecl","typeDecls","many__identifier__","sepBy__action__';'__","sepBy__def__';'__","sepBy__predicate__','__","sepBy__sequence__','__","sepBy__sequence2__'|'__","some__bsep__","some__identifier__","some__line__","some__simpleSeq__","some__typeDecl__","many_rev__identifier__","sepBy_rev__action__';'__","sepBy_rev__def__';'__","sepBy_rev__predicate__','__","sepBy_rev__sequence__','__","sepBy_rev__sequence2__'|'__","some_rev__bsep__","some_rev__identifier__","some_rev__line__","some_rev__simpleSeq__","some_rev__typeDecl__","identifier","operator","string","number","line","bsep","'~'","'!'","'@'","'('","')'","'['","']'","','","'<'","'>'","'|'","'%'","'{'","'}'","'\\\\'","'->'","';'","'<-'","':t'","':show'","'='","':load'","':js'","'::'","typefam","'=>'","textT","numT","controlT","varT","classT","%eof"]
        bit_start = st Prelude.* 98
        bit_end = (st Prelude.+ 1) Prelude.* 98
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..97]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (61) = happyShift action_5
action_0 (63) = happyShift action_43
action_0 (64) = happyShift action_44
action_0 (67) = happyShift action_45
action_0 (70) = happyShift action_46
action_0 (72) = happyShift action_47
action_0 (75) = happyShift action_48
action_0 (81) = happyShift action_49
action_0 (7) = happyGoto action_31
action_0 (12) = happyGoto action_32
action_0 (13) = happyGoto action_33
action_0 (14) = happyGoto action_34
action_0 (15) = happyGoto action_35
action_0 (16) = happyGoto action_36
action_0 (17) = happyGoto action_37
action_0 (18) = happyGoto action_38
action_0 (19) = happyGoto action_39
action_0 (21) = happyGoto action_40
action_0 (22) = happyGoto action_41
action_0 (24) = happyGoto action_42
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (61) = happyShift action_25
action_1 (64) = happyShift action_26
action_1 (85) = happyShift action_27
action_1 (86) = happyShift action_28
action_1 (88) = happyShift action_29
action_1 (89) = happyShift action_30
action_1 (25) = happyGoto action_20
action_1 (27) = happyGoto action_21
action_1 (28) = happyGoto action_22
action_1 (40) = happyGoto action_23
action_1 (51) = happyGoto action_24
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (65) = happyShift action_18
action_2 (66) = happyShift action_19
action_2 (29) = happyGoto action_11
action_2 (30) = happyGoto action_12
action_2 (31) = happyGoto action_13
action_2 (45) = happyGoto action_14
action_2 (47) = happyGoto action_15
action_2 (56) = happyGoto action_16
action_2 (58) = happyGoto action_17
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (61) = happyShift action_10
action_3 (37) = happyGoto action_6
action_3 (38) = happyGoto action_7
action_3 (49) = happyGoto action_8
action_3 (60) = happyGoto action_9
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (61) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_4

action_6 _ = happyReduce_103

action_7 (98) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_71

action_9 (61) = happyShift action_10
action_9 (37) = happyGoto action_87
action_9 _ = happyReduce_82

action_10 (90) = happyShift action_86
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_53

action_12 (66) = happyShift action_19
action_12 (45) = happyGoto action_85
action_12 (56) = happyGoto action_16
action_12 _ = happyReduce_57

action_13 (98) = happyAccept
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (65) = happyShift action_18
action_14 (29) = happyGoto action_11
action_14 (30) = happyGoto action_84
action_14 (47) = happyGoto action_15
action_14 (58) = happyGoto action_17
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_51

action_16 (66) = happyShift action_83
action_16 _ = happyReduce_78

action_17 (65) = happyShift action_82
action_17 _ = happyReduce_80

action_18 _ = happyReduce_99

action_19 _ = happyReduce_95

action_20 _ = happyReduce_45

action_21 _ = happyReduce_85

action_22 (98) = happyAccept
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_50

action_24 (83) = happyShift action_81
action_24 _ = happyReduce_73

action_25 (84) = happyShift action_80
action_25 (39) = happyGoto action_78
action_25 (50) = happyGoto action_79
action_25 _ = happyReduce_83

action_26 (84) = happyShift action_77
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (61) = happyShift action_5
action_27 (63) = happyShift action_43
action_27 (64) = happyShift action_44
action_27 (67) = happyShift action_45
action_27 (70) = happyShift action_46
action_27 (72) = happyShift action_47
action_27 (75) = happyShift action_48
action_27 (81) = happyShift action_49
action_27 (7) = happyGoto action_31
action_27 (12) = happyGoto action_32
action_27 (13) = happyGoto action_33
action_27 (14) = happyGoto action_34
action_27 (15) = happyGoto action_35
action_27 (16) = happyGoto action_36
action_27 (17) = happyGoto action_37
action_27 (18) = happyGoto action_38
action_27 (19) = happyGoto action_39
action_27 (21) = happyGoto action_40
action_27 (22) = happyGoto action_41
action_27 (24) = happyGoto action_76
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (61) = happyShift action_5
action_28 (63) = happyShift action_43
action_28 (64) = happyShift action_44
action_28 (67) = happyShift action_45
action_28 (70) = happyShift action_46
action_28 (72) = happyShift action_47
action_28 (75) = happyShift action_48
action_28 (81) = happyShift action_49
action_28 (7) = happyGoto action_31
action_28 (12) = happyGoto action_32
action_28 (13) = happyGoto action_33
action_28 (14) = happyGoto action_34
action_28 (15) = happyGoto action_35
action_28 (16) = happyGoto action_36
action_28 (17) = happyGoto action_37
action_28 (18) = happyGoto action_38
action_28 (19) = happyGoto action_39
action_28 (21) = happyGoto action_40
action_28 (22) = happyGoto action_41
action_28 (24) = happyGoto action_75
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_48

action_30 (61) = happyShift action_5
action_30 (63) = happyShift action_43
action_30 (64) = happyShift action_44
action_30 (67) = happyShift action_45
action_30 (70) = happyShift action_46
action_30 (72) = happyShift action_47
action_30 (75) = happyShift action_48
action_30 (81) = happyShift action_49
action_30 (7) = happyGoto action_31
action_30 (12) = happyGoto action_32
action_30 (13) = happyGoto action_33
action_30 (14) = happyGoto action_34
action_30 (15) = happyGoto action_35
action_30 (16) = happyGoto action_36
action_30 (17) = happyGoto action_37
action_30 (18) = happyGoto action_38
action_30 (19) = happyGoto action_39
action_30 (21) = happyGoto action_40
action_30 (22) = happyGoto action_41
action_30 (24) = happyGoto action_74
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_24

action_32 _ = happyReduce_27

action_33 _ = happyReduce_28

action_34 _ = happyReduce_29

action_35 _ = happyReduce_30

action_36 _ = happyReduce_26

action_37 _ = happyReduce_25

action_38 _ = happyReduce_31

action_39 (61) = happyReduce_36
action_39 (62) = happyShift action_69
action_39 (63) = happyReduce_36
action_39 (64) = happyReduce_36
action_39 (67) = happyReduce_36
action_39 (68) = happyShift action_70
action_39 (69) = happyShift action_71
action_39 (70) = happyReduce_36
action_39 (71) = happyReduce_36
action_39 (72) = happyReduce_36
action_39 (73) = happyReduce_36
action_39 (74) = happyReduce_36
action_39 (75) = happyReduce_36
action_39 (76) = happyReduce_36
action_39 (77) = happyReduce_36
action_39 (78) = happyShift action_72
action_39 (79) = happyShift action_73
action_39 (80) = happyReduce_36
action_39 (81) = happyReduce_36
action_39 (83) = happyReduce_36
action_39 (98) = happyReduce_36
action_39 _ = happyReduce_36

action_40 (61) = happyReduce_35
action_40 (62) = happyReduce_35
action_40 (63) = happyReduce_35
action_40 (64) = happyReduce_35
action_40 (67) = happyReduce_35
action_40 (68) = happyReduce_35
action_40 (69) = happyReduce_35
action_40 (70) = happyReduce_35
action_40 (71) = happyReduce_35
action_40 (72) = happyReduce_35
action_40 (73) = happyReduce_35
action_40 (74) = happyReduce_35
action_40 (75) = happyReduce_35
action_40 (76) = happyReduce_35
action_40 (77) = happyReduce_35
action_40 (78) = happyReduce_35
action_40 (79) = happyReduce_35
action_40 (80) = happyReduce_35
action_40 (81) = happyReduce_35
action_40 (83) = happyReduce_35
action_40 (98) = happyReduce_35
action_40 _ = happyReduce_35

action_41 (61) = happyReduce_40
action_41 (62) = happyReduce_40
action_41 (63) = happyReduce_40
action_41 (64) = happyReduce_40
action_41 (67) = happyReduce_40
action_41 (68) = happyReduce_40
action_41 (69) = happyReduce_40
action_41 (70) = happyReduce_40
action_41 (71) = happyReduce_40
action_41 (72) = happyReduce_40
action_41 (73) = happyReduce_40
action_41 (74) = happyReduce_40
action_41 (75) = happyReduce_40
action_41 (76) = happyReduce_40
action_41 (77) = happyReduce_40
action_41 (78) = happyReduce_40
action_41 (79) = happyReduce_40
action_41 (80) = happyReduce_40
action_41 (81) = happyReduce_40
action_41 (83) = happyReduce_40
action_41 (98) = happyReduce_40
action_41 _ = happyReduce_40

action_42 (61) = happyShift action_5
action_42 (63) = happyShift action_43
action_42 (64) = happyShift action_44
action_42 (67) = happyShift action_45
action_42 (70) = happyShift action_46
action_42 (72) = happyShift action_47
action_42 (75) = happyShift action_48
action_42 (81) = happyShift action_49
action_42 (98) = happyAccept
action_42 (7) = happyGoto action_31
action_42 (12) = happyGoto action_32
action_42 (13) = happyGoto action_33
action_42 (14) = happyGoto action_34
action_42 (15) = happyGoto action_35
action_42 (16) = happyGoto action_36
action_42 (17) = happyGoto action_37
action_42 (18) = happyGoto action_38
action_42 (19) = happyGoto action_39
action_42 (21) = happyGoto action_40
action_42 (22) = happyGoto action_68
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_6

action_44 _ = happyReduce_5

action_45 _ = happyReduce_7

action_46 (61) = happyShift action_5
action_46 (63) = happyShift action_43
action_46 (64) = happyShift action_44
action_46 (67) = happyShift action_45
action_46 (70) = happyShift action_46
action_46 (72) = happyShift action_47
action_46 (75) = happyShift action_48
action_46 (81) = happyShift action_49
action_46 (7) = happyGoto action_31
action_46 (12) = happyGoto action_32
action_46 (13) = happyGoto action_33
action_46 (14) = happyGoto action_34
action_46 (15) = happyGoto action_35
action_46 (16) = happyGoto action_36
action_46 (17) = happyGoto action_37
action_46 (18) = happyGoto action_38
action_46 (19) = happyGoto action_39
action_46 (21) = happyGoto action_40
action_46 (22) = happyGoto action_41
action_46 (24) = happyGoto action_67
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (61) = happyShift action_5
action_47 (63) = happyShift action_43
action_47 (64) = happyShift action_44
action_47 (67) = happyShift action_45
action_47 (70) = happyShift action_46
action_47 (72) = happyShift action_47
action_47 (75) = happyShift action_48
action_47 (81) = happyShift action_49
action_47 (7) = happyGoto action_31
action_47 (8) = happyGoto action_58
action_47 (9) = happyGoto action_59
action_47 (10) = happyGoto action_60
action_47 (11) = happyGoto action_61
action_47 (12) = happyGoto action_32
action_47 (13) = happyGoto action_33
action_47 (14) = happyGoto action_34
action_47 (15) = happyGoto action_35
action_47 (16) = happyGoto action_36
action_47 (17) = happyGoto action_37
action_47 (18) = happyGoto action_38
action_47 (19) = happyGoto action_53
action_47 (20) = happyGoto action_54
action_47 (23) = happyGoto action_55
action_47 (43) = happyGoto action_62
action_47 (44) = happyGoto action_63
action_47 (48) = happyGoto action_64
action_47 (54) = happyGoto action_65
action_47 (55) = happyGoto action_66
action_47 (59) = happyGoto action_57
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (61) = happyShift action_5
action_48 (63) = happyShift action_43
action_48 (64) = happyShift action_44
action_48 (67) = happyShift action_45
action_48 (70) = happyShift action_46
action_48 (72) = happyShift action_47
action_48 (75) = happyShift action_48
action_48 (81) = happyShift action_49
action_48 (7) = happyGoto action_31
action_48 (12) = happyGoto action_32
action_48 (13) = happyGoto action_33
action_48 (14) = happyGoto action_34
action_48 (15) = happyGoto action_35
action_48 (16) = happyGoto action_36
action_48 (17) = happyGoto action_37
action_48 (18) = happyGoto action_38
action_48 (19) = happyGoto action_53
action_48 (20) = happyGoto action_54
action_48 (23) = happyGoto action_55
action_48 (48) = happyGoto action_56
action_48 (59) = happyGoto action_57
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (61) = happyShift action_52
action_49 (46) = happyGoto action_50
action_49 (57) = happyGoto action_51
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (82) = happyShift action_115
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (61) = happyShift action_114
action_51 _ = happyReduce_79

action_52 _ = happyReduce_97

action_53 (61) = happyReduce_38
action_53 (62) = happyShift action_113
action_53 (63) = happyReduce_38
action_53 (64) = happyReduce_38
action_53 (67) = happyReduce_38
action_53 (68) = happyShift action_70
action_53 (69) = happyShift action_71
action_53 (70) = happyReduce_38
action_53 (72) = happyReduce_38
action_53 (73) = happyReduce_38
action_53 (74) = happyReduce_38
action_53 (75) = happyReduce_38
action_53 (76) = happyReduce_38
action_53 (77) = happyReduce_38
action_53 (78) = happyShift action_72
action_53 (79) = happyShift action_73
action_53 (81) = happyReduce_38
action_53 _ = happyReduce_38

action_54 (61) = happyReduce_37
action_54 (63) = happyReduce_37
action_54 (64) = happyReduce_37
action_54 (67) = happyReduce_37
action_54 (70) = happyReduce_37
action_54 (72) = happyReduce_37
action_54 (73) = happyReduce_37
action_54 (74) = happyReduce_37
action_54 (75) = happyReduce_37
action_54 (76) = happyReduce_37
action_54 (77) = happyReduce_37
action_54 (81) = happyReduce_37
action_54 _ = happyReduce_37

action_55 _ = happyReduce_101

action_56 (76) = happyShift action_112
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (61) = happyShift action_5
action_57 (63) = happyShift action_43
action_57 (64) = happyShift action_44
action_57 (67) = happyShift action_45
action_57 (70) = happyShift action_46
action_57 (72) = happyShift action_47
action_57 (75) = happyShift action_48
action_57 (81) = happyShift action_49
action_57 (7) = happyGoto action_31
action_57 (12) = happyGoto action_32
action_57 (13) = happyGoto action_33
action_57 (14) = happyGoto action_34
action_57 (15) = happyGoto action_35
action_57 (16) = happyGoto action_36
action_57 (17) = happyGoto action_37
action_57 (18) = happyGoto action_38
action_57 (19) = happyGoto action_53
action_57 (20) = happyGoto action_54
action_57 (23) = happyGoto action_111
action_57 _ = happyReduce_81

action_58 _ = happyReduce_91

action_59 _ = happyReduce_93

action_60 (73) = happyShift action_110
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (73) = happyShift action_109
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_10

action_63 _ = happyReduce_11

action_64 (73) = happyShift action_108
action_64 (74) = happyReduce_8
action_64 (77) = happyReduce_9
action_64 _ = happyReduce_8

action_65 (74) = happyShift action_107
action_65 _ = happyReduce_76

action_66 (77) = happyShift action_106
action_66 _ = happyReduce_77

action_67 (61) = happyShift action_5
action_67 (63) = happyShift action_43
action_67 (64) = happyShift action_44
action_67 (67) = happyShift action_45
action_67 (70) = happyShift action_46
action_67 (71) = happyShift action_105
action_67 (72) = happyShift action_47
action_67 (75) = happyShift action_48
action_67 (81) = happyShift action_49
action_67 (7) = happyGoto action_31
action_67 (12) = happyGoto action_32
action_67 (13) = happyGoto action_33
action_67 (14) = happyGoto action_34
action_67 (15) = happyGoto action_35
action_67 (16) = happyGoto action_36
action_67 (17) = happyGoto action_37
action_67 (18) = happyGoto action_38
action_67 (19) = happyGoto action_39
action_67 (21) = happyGoto action_40
action_67 (22) = happyGoto action_68
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (61) = happyReduce_39
action_68 (62) = happyReduce_39
action_68 (63) = happyReduce_39
action_68 (64) = happyReduce_39
action_68 (67) = happyReduce_39
action_68 (68) = happyReduce_39
action_68 (69) = happyReduce_39
action_68 (70) = happyReduce_39
action_68 (71) = happyReduce_39
action_68 (72) = happyReduce_39
action_68 (73) = happyReduce_39
action_68 (74) = happyReduce_39
action_68 (75) = happyReduce_39
action_68 (76) = happyReduce_39
action_68 (77) = happyReduce_39
action_68 (78) = happyReduce_39
action_68 (79) = happyReduce_39
action_68 (80) = happyReduce_39
action_68 (81) = happyReduce_39
action_68 (83) = happyReduce_39
action_68 (98) = happyReduce_39
action_68 _ = happyReduce_39

action_69 (61) = happyShift action_5
action_69 (63) = happyShift action_43
action_69 (64) = happyShift action_44
action_69 (67) = happyShift action_45
action_69 (70) = happyShift action_46
action_69 (72) = happyShift action_47
action_69 (75) = happyShift action_48
action_69 (81) = happyShift action_49
action_69 (7) = happyGoto action_31
action_69 (12) = happyGoto action_32
action_69 (13) = happyGoto action_33
action_69 (14) = happyGoto action_34
action_69 (15) = happyGoto action_35
action_69 (16) = happyGoto action_36
action_69 (17) = happyGoto action_37
action_69 (18) = happyGoto action_38
action_69 (19) = happyGoto action_39
action_69 (21) = happyGoto action_40
action_69 (22) = happyGoto action_41
action_69 (24) = happyGoto action_104
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (61) = happyReduce_17
action_70 (62) = happyReduce_17
action_70 (63) = happyReduce_17
action_70 (64) = happyShift action_103
action_70 (67) = happyReduce_17
action_70 (68) = happyReduce_17
action_70 (69) = happyReduce_17
action_70 (70) = happyReduce_17
action_70 (71) = happyReduce_17
action_70 (72) = happyReduce_17
action_70 (73) = happyReduce_17
action_70 (74) = happyReduce_17
action_70 (75) = happyReduce_17
action_70 (76) = happyReduce_17
action_70 (77) = happyReduce_17
action_70 (78) = happyReduce_17
action_70 (79) = happyReduce_17
action_70 (80) = happyReduce_17
action_70 (81) = happyReduce_17
action_70 (83) = happyReduce_17
action_70 (98) = happyReduce_17
action_70 _ = happyReduce_17

action_71 (61) = happyReduce_15
action_71 (62) = happyReduce_15
action_71 (63) = happyReduce_15
action_71 (64) = happyShift action_102
action_71 (67) = happyReduce_15
action_71 (68) = happyReduce_15
action_71 (69) = happyReduce_15
action_71 (70) = happyReduce_15
action_71 (71) = happyReduce_15
action_71 (72) = happyReduce_15
action_71 (73) = happyReduce_15
action_71 (74) = happyReduce_15
action_71 (75) = happyReduce_15
action_71 (76) = happyReduce_15
action_71 (77) = happyReduce_15
action_71 (78) = happyReduce_15
action_71 (79) = happyReduce_15
action_71 (80) = happyReduce_15
action_71 (81) = happyReduce_15
action_71 (83) = happyReduce_15
action_71 (98) = happyReduce_15
action_71 _ = happyReduce_15

action_72 (61) = happyShift action_5
action_72 (63) = happyShift action_43
action_72 (64) = happyShift action_44
action_72 (67) = happyShift action_45
action_72 (70) = happyShift action_46
action_72 (72) = happyShift action_47
action_72 (75) = happyShift action_48
action_72 (81) = happyShift action_49
action_72 (7) = happyGoto action_31
action_72 (12) = happyGoto action_32
action_72 (13) = happyGoto action_33
action_72 (14) = happyGoto action_34
action_72 (15) = happyGoto action_35
action_72 (16) = happyGoto action_36
action_72 (17) = happyGoto action_37
action_72 (18) = happyGoto action_38
action_72 (19) = happyGoto action_39
action_72 (21) = happyGoto action_40
action_72 (22) = happyGoto action_41
action_72 (24) = happyGoto action_101
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (61) = happyShift action_5
action_73 (63) = happyShift action_43
action_73 (64) = happyShift action_44
action_73 (67) = happyShift action_45
action_73 (70) = happyShift action_46
action_73 (72) = happyShift action_47
action_73 (75) = happyShift action_48
action_73 (81) = happyShift action_49
action_73 (7) = happyGoto action_31
action_73 (12) = happyGoto action_32
action_73 (13) = happyGoto action_33
action_73 (14) = happyGoto action_34
action_73 (15) = happyGoto action_35
action_73 (16) = happyGoto action_36
action_73 (17) = happyGoto action_37
action_73 (18) = happyGoto action_38
action_73 (19) = happyGoto action_39
action_73 (21) = happyGoto action_40
action_73 (22) = happyGoto action_41
action_73 (24) = happyGoto action_100
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (61) = happyShift action_5
action_74 (63) = happyShift action_43
action_74 (64) = happyShift action_44
action_74 (67) = happyShift action_45
action_74 (70) = happyShift action_46
action_74 (72) = happyShift action_47
action_74 (75) = happyShift action_48
action_74 (81) = happyShift action_49
action_74 (7) = happyGoto action_31
action_74 (12) = happyGoto action_32
action_74 (13) = happyGoto action_33
action_74 (14) = happyGoto action_34
action_74 (15) = happyGoto action_35
action_74 (16) = happyGoto action_36
action_74 (17) = happyGoto action_37
action_74 (18) = happyGoto action_38
action_74 (19) = happyGoto action_39
action_74 (21) = happyGoto action_40
action_74 (22) = happyGoto action_68
action_74 _ = happyReduce_49

action_75 (61) = happyShift action_5
action_75 (63) = happyShift action_43
action_75 (64) = happyShift action_44
action_75 (67) = happyShift action_45
action_75 (70) = happyShift action_46
action_75 (72) = happyShift action_47
action_75 (75) = happyShift action_48
action_75 (81) = happyShift action_49
action_75 (7) = happyGoto action_31
action_75 (12) = happyGoto action_32
action_75 (13) = happyGoto action_33
action_75 (14) = happyGoto action_34
action_75 (15) = happyGoto action_35
action_75 (16) = happyGoto action_36
action_75 (17) = happyGoto action_37
action_75 (18) = happyGoto action_38
action_75 (19) = happyGoto action_39
action_75 (21) = happyGoto action_40
action_75 (22) = happyGoto action_68
action_75 _ = happyReduce_47

action_76 (61) = happyShift action_5
action_76 (63) = happyShift action_43
action_76 (64) = happyShift action_44
action_76 (67) = happyShift action_45
action_76 (70) = happyShift action_46
action_76 (72) = happyShift action_47
action_76 (75) = happyShift action_48
action_76 (81) = happyShift action_49
action_76 (7) = happyGoto action_31
action_76 (12) = happyGoto action_32
action_76 (13) = happyGoto action_33
action_76 (14) = happyGoto action_34
action_76 (15) = happyGoto action_35
action_76 (16) = happyGoto action_36
action_76 (17) = happyGoto action_37
action_76 (18) = happyGoto action_38
action_76 (19) = happyGoto action_39
action_76 (21) = happyGoto action_40
action_76 (22) = happyGoto action_68
action_76 _ = happyReduce_46

action_77 (61) = happyShift action_5
action_77 (63) = happyShift action_43
action_77 (64) = happyShift action_44
action_77 (67) = happyShift action_45
action_77 (70) = happyShift action_46
action_77 (72) = happyShift action_47
action_77 (75) = happyShift action_48
action_77 (81) = happyShift action_49
action_77 (7) = happyGoto action_31
action_77 (12) = happyGoto action_32
action_77 (13) = happyGoto action_33
action_77 (14) = happyGoto action_34
action_77 (15) = happyGoto action_35
action_77 (16) = happyGoto action_36
action_77 (17) = happyGoto action_37
action_77 (18) = happyGoto action_38
action_77 (19) = happyGoto action_39
action_77 (21) = happyGoto action_40
action_77 (22) = happyGoto action_41
action_77 (24) = happyGoto action_99
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (87) = happyShift action_98
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (61) = happyShift action_97
action_79 _ = happyReduce_72

action_80 (61) = happyShift action_5
action_80 (63) = happyShift action_43
action_80 (64) = happyShift action_44
action_80 (67) = happyShift action_45
action_80 (70) = happyShift action_46
action_80 (72) = happyShift action_47
action_80 (75) = happyShift action_48
action_80 (81) = happyShift action_49
action_80 (7) = happyGoto action_31
action_80 (12) = happyGoto action_32
action_80 (13) = happyGoto action_33
action_80 (14) = happyGoto action_34
action_80 (15) = happyGoto action_35
action_80 (16) = happyGoto action_36
action_80 (17) = happyGoto action_37
action_80 (18) = happyGoto action_38
action_80 (19) = happyGoto action_39
action_80 (21) = happyGoto action_40
action_80 (22) = happyGoto action_41
action_80 (24) = happyGoto action_96
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (61) = happyShift action_25
action_81 (64) = happyShift action_26
action_81 (85) = happyShift action_27
action_81 (86) = happyShift action_28
action_81 (88) = happyShift action_29
action_81 (89) = happyShift action_30
action_81 (25) = happyGoto action_20
action_81 (27) = happyGoto action_95
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_100

action_83 _ = happyReduce_96

action_84 (66) = happyShift action_19
action_84 (45) = happyGoto action_94
action_84 (56) = happyGoto action_16
action_84 _ = happyReduce_55

action_85 (65) = happyShift action_18
action_85 (29) = happyGoto action_93
action_85 (47) = happyGoto action_15
action_85 (58) = happyGoto action_17
action_85 _ = happyReduce_56

action_86 (70) = happyShift action_91
action_86 (97) = happyShift action_92
action_86 (34) = happyGoto action_88
action_86 (35) = happyGoto action_89
action_86 (36) = happyGoto action_90
action_86 _ = happyReduce_68

action_87 _ = happyReduce_104

action_88 (92) = happyShift action_128
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (91) = happyShift action_127
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_70

action_91 (97) = happyShift action_92
action_91 (34) = happyGoto action_124
action_91 (42) = happyGoto action_125
action_91 (53) = happyGoto action_126
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_65

action_93 _ = happyReduce_52

action_94 (65) = happyShift action_18
action_94 (29) = happyGoto action_93
action_94 (47) = happyGoto action_15
action_94 (58) = happyGoto action_17
action_94 _ = happyReduce_54

action_95 _ = happyReduce_86

action_96 (61) = happyShift action_5
action_96 (63) = happyShift action_43
action_96 (64) = happyShift action_44
action_96 (67) = happyShift action_45
action_96 (70) = happyShift action_46
action_96 (72) = happyShift action_47
action_96 (75) = happyShift action_48
action_96 (81) = happyShift action_49
action_96 (7) = happyGoto action_31
action_96 (12) = happyGoto action_32
action_96 (13) = happyGoto action_33
action_96 (14) = happyGoto action_34
action_96 (15) = happyGoto action_35
action_96 (16) = happyGoto action_36
action_96 (17) = happyGoto action_37
action_96 (18) = happyGoto action_38
action_96 (19) = happyGoto action_39
action_96 (21) = happyGoto action_40
action_96 (22) = happyGoto action_68
action_96 _ = happyReduce_43

action_97 _ = happyReduce_84

action_98 (61) = happyShift action_5
action_98 (63) = happyShift action_43
action_98 (64) = happyShift action_44
action_98 (67) = happyShift action_45
action_98 (70) = happyShift action_46
action_98 (72) = happyShift action_47
action_98 (75) = happyShift action_48
action_98 (81) = happyShift action_49
action_98 (7) = happyGoto action_31
action_98 (12) = happyGoto action_32
action_98 (13) = happyGoto action_33
action_98 (14) = happyGoto action_34
action_98 (15) = happyGoto action_35
action_98 (16) = happyGoto action_36
action_98 (17) = happyGoto action_37
action_98 (18) = happyGoto action_38
action_98 (19) = happyGoto action_39
action_98 (21) = happyGoto action_40
action_98 (22) = happyGoto action_41
action_98 (24) = happyGoto action_123
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (61) = happyShift action_5
action_99 (63) = happyShift action_43
action_99 (64) = happyShift action_44
action_99 (67) = happyShift action_45
action_99 (70) = happyShift action_46
action_99 (72) = happyShift action_47
action_99 (75) = happyShift action_48
action_99 (81) = happyShift action_49
action_99 (7) = happyGoto action_31
action_99 (12) = happyGoto action_32
action_99 (13) = happyGoto action_33
action_99 (14) = happyGoto action_34
action_99 (15) = happyGoto action_35
action_99 (16) = happyGoto action_36
action_99 (17) = happyGoto action_37
action_99 (18) = happyGoto action_38
action_99 (19) = happyGoto action_39
action_99 (21) = happyGoto action_40
action_99 (22) = happyGoto action_68
action_99 _ = happyReduce_44

action_100 (61) = happyShift action_5
action_100 (63) = happyShift action_43
action_100 (64) = happyShift action_44
action_100 (67) = happyShift action_45
action_100 (70) = happyShift action_46
action_100 (72) = happyShift action_47
action_100 (74) = happyShift action_122
action_100 (75) = happyShift action_48
action_100 (81) = happyShift action_49
action_100 (7) = happyGoto action_31
action_100 (12) = happyGoto action_32
action_100 (13) = happyGoto action_33
action_100 (14) = happyGoto action_34
action_100 (15) = happyGoto action_35
action_100 (16) = happyGoto action_36
action_100 (17) = happyGoto action_37
action_100 (18) = happyGoto action_38
action_100 (19) = happyGoto action_39
action_100 (21) = happyGoto action_40
action_100 (22) = happyGoto action_68
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (61) = happyShift action_5
action_101 (62) = happyReduce_13
action_101 (63) = happyShift action_43
action_101 (64) = happyShift action_44
action_101 (67) = happyShift action_45
action_101 (68) = happyReduce_13
action_101 (69) = happyReduce_13
action_101 (70) = happyShift action_46
action_101 (71) = happyReduce_13
action_101 (72) = happyShift action_47
action_101 (73) = happyReduce_13
action_101 (74) = happyReduce_13
action_101 (75) = happyShift action_48
action_101 (76) = happyReduce_13
action_101 (77) = happyReduce_13
action_101 (78) = happyReduce_13
action_101 (79) = happyReduce_13
action_101 (80) = happyReduce_13
action_101 (81) = happyShift action_49
action_101 (83) = happyReduce_13
action_101 (98) = happyReduce_13
action_101 (7) = happyGoto action_31
action_101 (12) = happyGoto action_32
action_101 (13) = happyGoto action_33
action_101 (14) = happyGoto action_34
action_101 (15) = happyGoto action_35
action_101 (16) = happyGoto action_36
action_101 (17) = happyGoto action_37
action_101 (18) = happyGoto action_38
action_101 (19) = happyGoto action_39
action_101 (21) = happyGoto action_40
action_101 (22) = happyGoto action_68
action_101 _ = happyReduce_13

action_102 _ = happyReduce_14

action_103 _ = happyReduce_16

action_104 (61) = happyShift action_5
action_104 (62) = happyReduce_34
action_104 (63) = happyShift action_43
action_104 (64) = happyShift action_44
action_104 (67) = happyShift action_45
action_104 (68) = happyReduce_34
action_104 (69) = happyReduce_34
action_104 (70) = happyShift action_46
action_104 (71) = happyReduce_34
action_104 (72) = happyShift action_47
action_104 (73) = happyReduce_34
action_104 (74) = happyReduce_34
action_104 (75) = happyShift action_48
action_104 (76) = happyReduce_34
action_104 (77) = happyReduce_34
action_104 (78) = happyReduce_34
action_104 (79) = happyReduce_34
action_104 (80) = happyReduce_34
action_104 (81) = happyShift action_49
action_104 (83) = happyReduce_34
action_104 (98) = happyReduce_34
action_104 (7) = happyGoto action_31
action_104 (12) = happyGoto action_32
action_104 (13) = happyGoto action_33
action_104 (14) = happyGoto action_34
action_104 (15) = happyGoto action_35
action_104 (16) = happyGoto action_36
action_104 (17) = happyGoto action_37
action_104 (18) = happyGoto action_38
action_104 (19) = happyGoto action_39
action_104 (21) = happyGoto action_40
action_104 (22) = happyGoto action_68
action_104 _ = happyReduce_34

action_105 _ = happyReduce_32

action_106 (61) = happyShift action_5
action_106 (63) = happyShift action_43
action_106 (64) = happyShift action_44
action_106 (67) = happyShift action_45
action_106 (70) = happyShift action_46
action_106 (72) = happyShift action_47
action_106 (75) = happyShift action_48
action_106 (81) = happyShift action_49
action_106 (7) = happyGoto action_31
action_106 (9) = happyGoto action_120
action_106 (12) = happyGoto action_32
action_106 (13) = happyGoto action_33
action_106 (14) = happyGoto action_34
action_106 (15) = happyGoto action_35
action_106 (16) = happyGoto action_36
action_106 (17) = happyGoto action_37
action_106 (18) = happyGoto action_38
action_106 (19) = happyGoto action_53
action_106 (20) = happyGoto action_54
action_106 (23) = happyGoto action_55
action_106 (48) = happyGoto action_121
action_106 (59) = happyGoto action_57
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (61) = happyShift action_5
action_107 (63) = happyShift action_43
action_107 (64) = happyShift action_44
action_107 (67) = happyShift action_45
action_107 (70) = happyShift action_46
action_107 (72) = happyShift action_47
action_107 (75) = happyShift action_48
action_107 (81) = happyShift action_49
action_107 (7) = happyGoto action_31
action_107 (8) = happyGoto action_118
action_107 (12) = happyGoto action_32
action_107 (13) = happyGoto action_33
action_107 (14) = happyGoto action_34
action_107 (15) = happyGoto action_35
action_107 (16) = happyGoto action_36
action_107 (17) = happyGoto action_37
action_107 (18) = happyGoto action_38
action_107 (19) = happyGoto action_53
action_107 (20) = happyGoto action_54
action_107 (23) = happyGoto action_55
action_107 (48) = happyGoto action_119
action_107 (59) = happyGoto action_57
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_20

action_109 _ = happyReduce_19

action_110 _ = happyReduce_18

action_111 _ = happyReduce_102

action_112 _ = happyReduce_21

action_113 (61) = happyShift action_5
action_113 (63) = happyShift action_43
action_113 (64) = happyShift action_44
action_113 (67) = happyShift action_45
action_113 (70) = happyShift action_46
action_113 (72) = happyShift action_47
action_113 (75) = happyShift action_48
action_113 (81) = happyShift action_49
action_113 (7) = happyGoto action_31
action_113 (12) = happyGoto action_32
action_113 (13) = happyGoto action_33
action_113 (14) = happyGoto action_34
action_113 (15) = happyGoto action_35
action_113 (16) = happyGoto action_36
action_113 (17) = happyGoto action_37
action_113 (18) = happyGoto action_38
action_113 (19) = happyGoto action_117
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_98

action_115 (61) = happyShift action_5
action_115 (63) = happyShift action_43
action_115 (64) = happyShift action_44
action_115 (67) = happyShift action_45
action_115 (70) = happyShift action_46
action_115 (72) = happyShift action_47
action_115 (75) = happyShift action_48
action_115 (81) = happyShift action_49
action_115 (7) = happyGoto action_31
action_115 (12) = happyGoto action_32
action_115 (13) = happyGoto action_33
action_115 (14) = happyGoto action_34
action_115 (15) = happyGoto action_35
action_115 (16) = happyGoto action_36
action_115 (17) = happyGoto action_37
action_115 (18) = happyGoto action_38
action_115 (19) = happyGoto action_39
action_115 (21) = happyGoto action_40
action_115 (22) = happyGoto action_41
action_115 (24) = happyGoto action_116
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (61) = happyShift action_5
action_116 (62) = happyReduce_12
action_116 (63) = happyShift action_43
action_116 (64) = happyShift action_44
action_116 (67) = happyShift action_45
action_116 (68) = happyReduce_12
action_116 (69) = happyReduce_12
action_116 (70) = happyShift action_46
action_116 (71) = happyReduce_12
action_116 (72) = happyShift action_47
action_116 (73) = happyReduce_12
action_116 (74) = happyReduce_12
action_116 (75) = happyShift action_48
action_116 (76) = happyReduce_12
action_116 (77) = happyReduce_12
action_116 (78) = happyReduce_12
action_116 (79) = happyReduce_12
action_116 (80) = happyReduce_12
action_116 (81) = happyShift action_49
action_116 (83) = happyReduce_12
action_116 (98) = happyReduce_12
action_116 (7) = happyGoto action_31
action_116 (12) = happyGoto action_32
action_116 (13) = happyGoto action_33
action_116 (14) = happyGoto action_34
action_116 (15) = happyGoto action_35
action_116 (16) = happyGoto action_36
action_116 (17) = happyGoto action_37
action_116 (18) = happyGoto action_38
action_116 (19) = happyGoto action_39
action_116 (21) = happyGoto action_40
action_116 (22) = happyGoto action_68
action_116 _ = happyReduce_12

action_117 (61) = happyReduce_33
action_117 (63) = happyReduce_33
action_117 (64) = happyReduce_33
action_117 (67) = happyReduce_33
action_117 (68) = happyShift action_70
action_117 (69) = happyShift action_71
action_117 (70) = happyReduce_33
action_117 (72) = happyReduce_33
action_117 (73) = happyReduce_33
action_117 (74) = happyReduce_33
action_117 (75) = happyReduce_33
action_117 (76) = happyReduce_33
action_117 (77) = happyReduce_33
action_117 (78) = happyShift action_72
action_117 (79) = happyShift action_73
action_117 (81) = happyReduce_33
action_117 _ = happyReduce_33

action_118 _ = happyReduce_92

action_119 (73) = happyReduce_8
action_119 (74) = happyReduce_8
action_119 _ = happyReduce_8

action_120 _ = happyReduce_94

action_121 (73) = happyReduce_9
action_121 (77) = happyReduce_9
action_121 _ = happyReduce_9

action_122 (61) = happyShift action_5
action_122 (63) = happyShift action_43
action_122 (64) = happyShift action_44
action_122 (67) = happyShift action_45
action_122 (70) = happyShift action_46
action_122 (72) = happyShift action_47
action_122 (75) = happyShift action_48
action_122 (81) = happyShift action_49
action_122 (7) = happyGoto action_31
action_122 (12) = happyGoto action_32
action_122 (13) = happyGoto action_33
action_122 (14) = happyGoto action_34
action_122 (15) = happyGoto action_35
action_122 (16) = happyGoto action_36
action_122 (17) = happyGoto action_37
action_122 (18) = happyGoto action_38
action_122 (19) = happyGoto action_39
action_122 (21) = happyGoto action_40
action_122 (22) = happyGoto action_41
action_122 (24) = happyGoto action_138
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (61) = happyShift action_5
action_123 (63) = happyShift action_43
action_123 (64) = happyShift action_44
action_123 (67) = happyShift action_45
action_123 (70) = happyShift action_46
action_123 (72) = happyShift action_47
action_123 (75) = happyShift action_48
action_123 (81) = happyShift action_49
action_123 (7) = happyGoto action_31
action_123 (12) = happyGoto action_32
action_123 (13) = happyGoto action_33
action_123 (14) = happyGoto action_34
action_123 (15) = happyGoto action_35
action_123 (16) = happyGoto action_36
action_123 (17) = happyGoto action_37
action_123 (18) = happyGoto action_38
action_123 (19) = happyGoto action_39
action_123 (21) = happyGoto action_40
action_123 (22) = happyGoto action_68
action_123 _ = happyReduce_41

action_124 _ = happyReduce_89

action_125 (71) = happyShift action_137
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (74) = happyShift action_136
action_126 _ = happyReduce_75

action_127 (70) = happyShift action_131
action_127 (93) = happyShift action_132
action_127 (94) = happyShift action_133
action_127 (95) = happyShift action_134
action_127 (96) = happyShift action_135
action_127 (32) = happyGoto action_129
action_127 (33) = happyGoto action_130
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_67

action_129 (82) = happyShift action_144
action_129 _ = happyReduce_62

action_130 _ = happyReduce_69

action_131 (70) = happyShift action_131
action_131 (93) = happyShift action_132
action_131 (94) = happyShift action_133
action_131 (95) = happyShift action_134
action_131 (96) = happyShift action_135
action_131 (32) = happyGoto action_129
action_131 (33) = happyGoto action_143
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_58

action_133 _ = happyReduce_59

action_134 _ = happyReduce_60

action_135 _ = happyReduce_61

action_136 (97) = happyShift action_92
action_136 (34) = happyGoto action_142
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (92) = happyShift action_141
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (61) = happyShift action_5
action_138 (63) = happyShift action_43
action_138 (64) = happyShift action_44
action_138 (67) = happyShift action_45
action_138 (70) = happyShift action_46
action_138 (72) = happyShift action_47
action_138 (74) = happyShift action_139
action_138 (75) = happyShift action_48
action_138 (80) = happyShift action_140
action_138 (81) = happyShift action_49
action_138 (7) = happyGoto action_31
action_138 (12) = happyGoto action_32
action_138 (13) = happyGoto action_33
action_138 (14) = happyGoto action_34
action_138 (15) = happyGoto action_35
action_138 (16) = happyGoto action_36
action_138 (17) = happyGoto action_37
action_138 (18) = happyGoto action_38
action_138 (19) = happyGoto action_39
action_138 (21) = happyGoto action_40
action_138 (22) = happyGoto action_68
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (61) = happyShift action_5
action_139 (63) = happyShift action_43
action_139 (64) = happyShift action_44
action_139 (67) = happyShift action_45
action_139 (70) = happyShift action_46
action_139 (72) = happyShift action_47
action_139 (75) = happyShift action_48
action_139 (81) = happyShift action_49
action_139 (7) = happyGoto action_31
action_139 (12) = happyGoto action_32
action_139 (13) = happyGoto action_33
action_139 (14) = happyGoto action_34
action_139 (15) = happyGoto action_35
action_139 (16) = happyGoto action_36
action_139 (17) = happyGoto action_37
action_139 (18) = happyGoto action_38
action_139 (19) = happyGoto action_39
action_139 (21) = happyGoto action_40
action_139 (22) = happyGoto action_41
action_139 (24) = happyGoto action_147
action_139 _ = happyFail (happyExpListPerState 139)

action_140 _ = happyReduce_22

action_141 _ = happyReduce_66

action_142 _ = happyReduce_90

action_143 (71) = happyShift action_146
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (70) = happyShift action_131
action_144 (93) = happyShift action_132
action_144 (94) = happyShift action_133
action_144 (95) = happyShift action_134
action_144 (96) = happyShift action_135
action_144 (32) = happyGoto action_129
action_144 (33) = happyGoto action_145
action_144 _ = happyFail (happyExpListPerState 144)

action_145 _ = happyReduce_63

action_146 _ = happyReduce_64

action_147 (61) = happyShift action_5
action_147 (63) = happyShift action_43
action_147 (64) = happyShift action_44
action_147 (67) = happyShift action_45
action_147 (70) = happyShift action_46
action_147 (72) = happyShift action_47
action_147 (75) = happyShift action_48
action_147 (80) = happyShift action_148
action_147 (81) = happyShift action_49
action_147 (7) = happyGoto action_31
action_147 (12) = happyGoto action_32
action_147 (13) = happyGoto action_33
action_147 (14) = happyGoto action_34
action_147 (15) = happyGoto action_35
action_147 (16) = happyGoto action_36
action_147 (17) = happyGoto action_37
action_147 (18) = happyGoto action_38
action_147 (19) = happyGoto action_39
action_147 (21) = happyGoto action_40
action_147 (22) = happyGoto action_68
action_147 _ = happyFail (happyExpListPerState 147)

action_148 _ = happyReduce_23

happyReduce_4 = happyMonadReduce 1 7 happyReduction_4
happyReduction_4 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( (mkAtom TVar) happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_5 = happyMonadReduce 1 7 happyReduction_5
happyReduction_5 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( (mkAtom TNum) happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_6 = happyMonadReduce 1 7 happyReduction_6
happyReduction_6 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( (mkAtom TText) happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn7
		 (TRest
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn7
		 (TSeq happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn7
		 (TSeq happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 12 happyReduction_12
happyReduction_12 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TLambda (map unTok happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  13 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TPoly happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  14 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TElong happy_var_1 (Just $ read $ Text.unpack $ unTok happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  14 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TElong happy_var_1 Nothing
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  15 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TRepeat happy_var_1 (Just $ read $ Text.unpack $ unTok happy_var_3)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  15 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TRepeat happy_var_1 Nothing
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  16 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TStack happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyMonadReduce 3 16 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( L.increaseChoice >>= \x -> return $ TChoice x happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_20 = happySpecReduce_3  16 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TSeq happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  17 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TAlt happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 18 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TEuclid  happy_var_1 happy_var_3 happy_var_5 Nothing
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 8 18 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TEuclid  happy_var_1 happy_var_3 happy_var_5 (Just happy_var_7)
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  19 happyReduction_24
happyReduction_24 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  19 happyReduction_25
happyReduction_25 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  19 happyReduction_26
happyReduction_26 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  19 happyReduction_27
happyReduction_27 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  19 happyReduction_29
happyReduction_29 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  19 happyReduction_30
happyReduction_30 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  19 happyReduction_31
happyReduction_31 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  19 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  20 happyReduction_33
happyReduction_33 (HappyAbsSyn7  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TInfix  happy_var_1 (unTok happy_var_2) happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  21 happyReduction_34
happyReduction_34 (HappyAbsSyn7  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TInfix  happy_var_1 (unTok happy_var_2) happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  22 happyReduction_35
happyReduction_35 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  23 happyReduction_37
happyReduction_37 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  24 happyReduction_39
happyReduction_39 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TApp happy_var_1 happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happyReduce 4 25 happyReduction_41
happyReduction_41 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Let (unTok happy_var_1) (map unTok happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_1  26 happyReduction_42
happyReduction_42 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  27 happyReduction_43
happyReduction_43 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (Stream (unTok happy_var_1) happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  27 happyReduction_44
happyReduction_44 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (Stream (unTok happy_var_1) happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  27 happyReduction_45
happyReduction_45 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn27
		 (Def happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  27 happyReduction_46
happyReduction_46 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (Type happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  27 happyReduction_47
happyReduction_47 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (Show happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  27 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (Load $ unTok happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  27 happyReduction_49
happyReduction_49 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (JS happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  28 happyReduction_50
happyReduction_50 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  29 happyReduction_51
happyReduction_51 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn29
		 (toBlock happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  30 happyReduction_52
happyReduction_52 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_3:happy_var_1
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  30 happyReduction_53
happyReduction_53 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn30
		 ([happy_var_1]
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  31 happyReduction_54
happyReduction_54 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  31 happyReduction_55
happyReduction_55 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  31 happyReduction_56
happyReduction_56 _
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  31 happyReduction_57
happyReduction_57 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  32 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn32
		 (TypeCon "Text"
	)

happyReduce_59 = happySpecReduce_1  32 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn32
		 (TypeCon "Number"
	)

happyReduce_60 = happySpecReduce_1  32 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn32
		 (TypeCon "ValueMap"
	)

happyReduce_61 = happySpecReduce_1  32 happyReduction_61
happyReduction_61 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (TypeVar (unTok happy_var_1)
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  33 happyReduction_62
happyReduction_62 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  33 happyReduction_63
happyReduction_63 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (TypeArr happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  33 happyReduction_64
happyReduction_64 _
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (happy_var_2
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  34 happyReduction_65
happyReduction_65 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (mkPred happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happyReduce 4 35 happyReduction_66
happyReduction_66 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn42  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn35
		 (happy_var_2
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_2  35 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn35
		 ([happy_var_1]
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  35 happyReduction_68
happyReduction_68  =  HappyAbsSyn35
		 ([]
	)

happyReduce_69 = happySpecReduce_3  36 happyReduction_69
happyReduction_69 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn36
		 (generalize happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  37 happyReduction_70
happyReduction_70 (HappyAbsSyn36  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn37
		 ((happy_var_1,happy_var_3)
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  38 happyReduction_71
happyReduction_71 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  39 happyReduction_72
happyReduction_72 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn39
		 (reverse happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  40 happyReduction_73
happyReduction_73 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn40
		 (reverse happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  41 happyReduction_74
happyReduction_74 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn41
		 (reverse happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  42 happyReduction_75
happyReduction_75 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn42
		 (reverse happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  43 happyReduction_76
happyReduction_76 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn43
		 (reverse happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  44 happyReduction_77
happyReduction_77 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn44
		 (reverse happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  45 happyReduction_78
happyReduction_78 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn45
		 (reverse happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  46 happyReduction_79
happyReduction_79 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn46
		 (reverse happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  47 happyReduction_80
happyReduction_80 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn47
		 (reverse happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  48 happyReduction_81
happyReduction_81 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn48
		 (reverse happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  49 happyReduction_82
happyReduction_82 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn49
		 (reverse happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_0  50 happyReduction_83
happyReduction_83  =  HappyAbsSyn50
		 ([]
	)

happyReduce_84 = happySpecReduce_2  50 happyReduction_84
happyReduction_84 (HappyTerminal happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_2 : happy_var_1
	)
happyReduction_84 _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  51 happyReduction_85
happyReduction_85 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn51
		 ([happy_var_1]
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  51 happyReduction_86
happyReduction_86 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_3 : happy_var_1
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  52 happyReduction_87
happyReduction_87 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn52
		 ([happy_var_1]
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  52 happyReduction_88
happyReduction_88 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn52
		 (happy_var_3 : happy_var_1
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  53 happyReduction_89
happyReduction_89 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn53
		 ([happy_var_1]
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  53 happyReduction_90
happyReduction_90 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_3 : happy_var_1
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  54 happyReduction_91
happyReduction_91 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn54
		 ([happy_var_1]
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  54 happyReduction_92
happyReduction_92 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_3 : happy_var_1
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  55 happyReduction_93
happyReduction_93 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn55
		 ([happy_var_1]
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  55 happyReduction_94
happyReduction_94 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_3 : happy_var_1
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  56 happyReduction_95
happyReduction_95 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn56
		 ([happy_var_1]
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_2  56 happyReduction_96
happyReduction_96 (HappyTerminal happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_2 : happy_var_1
	)
happyReduction_96 _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  57 happyReduction_97
happyReduction_97 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn57
		 ([happy_var_1]
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_2  57 happyReduction_98
happyReduction_98 (HappyTerminal happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (happy_var_2 : happy_var_1
	)
happyReduction_98 _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  58 happyReduction_99
happyReduction_99 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn58
		 ([happy_var_1]
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_2  58 happyReduction_100
happyReduction_100 (HappyTerminal happy_var_2)
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_2 : happy_var_1
	)
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  59 happyReduction_101
happyReduction_101 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn59
		 ([happy_var_1]
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_2  59 happyReduction_102
happyReduction_102 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn59
		 (happy_var_2 : happy_var_1
	)
happyReduction_102 _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  60 happyReduction_103
happyReduction_103 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn60
		 ([happy_var_1]
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_2  60 happyReduction_104
happyReduction_104 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_2 : happy_var_1
	)
happyReduction_104 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.RangedToken L.EOF _ -> action 98 98 tk (HappyState action) sts stk;
	L.RangedToken (L.Identifier _) _ -> cont 61;
	L.RangedToken (L.Operator _) _ -> cont 62;
	L.RangedToken (L.String _) _ -> cont 63;
	L.RangedToken (L.Number _) _ -> cont 64;
	L.RangedToken (L.LineT _) _ -> cont 65;
	L.RangedToken (L.BlockSep) _ -> cont 66;
	L.RangedToken L.Rest _ -> cont 67;
	L.RangedToken L.Repeat _ -> cont 68;
	L.RangedToken L.Elongate _ -> cont 69;
	L.RangedToken L.LPar _ -> cont 70;
	L.RangedToken L.RPar _ -> cont 71;
	L.RangedToken L.LBrack _ -> cont 72;
	L.RangedToken L.RBrack _ -> cont 73;
	L.RangedToken L.Comma _ -> cont 74;
	L.RangedToken L.LAngle _ -> cont 75;
	L.RangedToken L.RAngle _ -> cont 76;
	L.RangedToken L.Pipe _ -> cont 77;
	L.RangedToken L.Poly _ -> cont 78;
	L.RangedToken L.LBraces _ -> cont 79;
	L.RangedToken L.RBraces _ -> cont 80;
	L.RangedToken L.Lambda _ -> cont 81;
	L.RangedToken L.Arrow _ -> cont 82;
	L.RangedToken L.Colon _ -> cont 83;
	L.RangedToken L.StreamA _ -> cont 84;
	L.RangedToken L.TypeA _ -> cont 85;
	L.RangedToken L.ShowA _ -> cont 86;
	L.RangedToken L.Assign _ -> cont 87;
	L.RangedToken (L.LoadA _ ) _ -> cont 88;
	L.RangedToken L.JSA _ -> cont 89;
	L.RangedToken L.DoubleColon _ -> cont 90;
	L.RangedToken L.PTypeFam _ -> cont 91;
	L.RangedToken L.Context _ -> cont 92;
	L.RangedToken L.TextToken _ -> cont 93;
	L.RangedToken L.NumberToken _ -> cont 94;
	L.RangedToken L.ControlToken _ -> cont 95;
	L.RangedToken (L.VarToken _) _ -> cont 96;
	L.RangedToken (L.TypeClass _ _) _ -> cont 97;
	_ -> happyError' (tk, [])
	})

happyError_ explist 98 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => L.Alex a -> (a -> L.Alex b) -> L.Alex b
happyThen = (>>=)
happyReturn :: () => a -> L.Alex a
happyReturn = (pure)
happyThen1 :: () => L.Alex a -> (a -> L.Alex b) -> L.Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> L.Alex a
happyReturn1 = happyReturn
happyError' :: () => ((L.RangedToken), [Prelude.String]) -> L.Alex a
happyError' tk = parseError tk
parse = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

pActions = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn28 z -> happyReturn z; _other -> notHappyAtAll })

pBlocks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pTypeDecls = happySomeParser where
 happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: (L.RangedToken, [String]) -> L.Alex a
parseError (L.RangedToken t _,poss) = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column
                <> "\n\tunexpected " <> show t
                <> "\n\texpecting " <> (intercalate "," poss)

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

unTok :: L.RangedToken -> Text
unTok (L.RangedToken  (L.Identifier x) _) = x
unTok (L.RangedToken  (L.Number x) _ ) = x
unTok (L.RangedToken  (L.String x) _ )= x
unTok (L.RangedToken  (L.Operator x) _) = x
unTok (L.RangedToken  (L.LoadA x) _) = x
unTok (L.RangedToken  (L.LineT x) _) = x
unTok (L.RangedToken  (L.VarToken x) _) = x
unTok _ = error "can't untok"

mkPred :: L.RangedToken -> Predicate
mkPred (L.RangedToken (L.TypeClass c x) _) = IsIn c (TypeVar x)
mkPred _ = error "can't make predicate"

mkAtom :: (Position -> Text -> Term) -> L.RangedToken -> L.Alex Term
mkAtom constr tok@(L.RangedToken _ range) = do
                          ed <- L.getEditorNum
                          return $ constr (toPosition ed range) (unTok tok)

toPosition :: Int -> L.Range -> Position
toPosition ed (L.Range (L.AlexPn _ line start) (L.AlexPn _ _ end)) = Pos line start end ed

toBlock :: [L.RangedToken] -> Block
toBlock [] = error "Can't happen"
toBlock xs = Block start end content
           where ls = sortOn (\(x,_) -> x) $ map (\r -> (getLn r,unTok r)) xs
                 (start, _) = head ls
                 (end, _) = last ls
                 content = Text.concat $ map snd ls
                 getLn (L.RangedToken _ (L.Range (L.AlexPn _ l _) _)) = l


parseActionsWithPos :: Int -> Int -> Text -> Either String [Action]
parseActionsWithPos ln ed input = L.runAlex input (L.setEditorNum ed >> L.setInitialLineNum ln >> pActions)

parseActions :: Text -> Either String [Action]
parseActions input = L.runAlex input pActions

parseBlocks :: Int -> Text -> Either String [Block]
parseBlocks line input = L.runAlex input (L.lineLexer >> L.setInitialLineNum line >> pBlocks)

parseTypeDecls :: Text -> Either String [(Text,Scheme)]
parseTypeDecls input = L.runAlex input (L.typeLexer >> pTypeDecls)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
