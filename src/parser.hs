{-# OPTIONS_GHC -w #-}
{-# OPTIONS -w #-}
    module Parser 
    ( parseBonsai
    ) where

    import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,437) ([0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,4,0,0,0,256,0,0,0,0,0,0,0,4096,0,0,0,0,2,0,0,0,128,0,0,0,256,0,0,0,512,0,0,0,0,0,0,0,8192,80,0,0,63184,643,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,1,0,0,0,16,0,0,0,256,0,0,0,0,0,0,0,0,0,0,60416,1287,0,0,0,0,0,0,0,0,0,0,0,2048,8,0,0,32474,16464,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,32768,8118,4116,0,0,64948,32928,0,0,60832,1287,0,0,0,0,0,0,0,0,0,0,0,1024,10,0,0,8192,80,0,0,0,36,0,0,0,0,0,0,0,64,0,0,0,512,5,0,0,0,0,0,0,0,0,0,0,0,10756,0,0,0,8192,0,0,0,0,0,0,0,0,512,0,0,0,1280,0,0,0,8192,0,0,0,4096,0,0,0,512,0,0,0,16384,256,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,40960,2029,1029,0,0,16237,8232,0,0,64360,321,1,0,0,64,0,0,55808,20606,64,0,0,0,128,0,0,0,2048,0,0,0,0,0,0,0,10240,0,0,0,0,0,0,26624,16891,257,0,0,4096,0,0,0,32768,4,0,0,63184,643,2,0,0,0,0,0,46080,41213,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20480,0,0,0,20512,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1282,0,0,0,0,4,0,0,0,36,0,16384,4059,2058,0,0,0,0,0,0,0,36,0,0,45056,5151,1,0,2048,0,16,0,0,0,0,0,0,8192,0,0,0,0,8208,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,512,0,0,0,0,0,0,24576,10303,2,0,0,0,0,0,0,4056,138,0,0,32448,1104,0,0,0,0,0,0,45056,5151,1,0,0,0,0,0,0,0,144,0,0,16237,8232,0,0,0,0,0,0,0,0,0,0,0,0,8,0,53248,33782,514,0,0,0,64,0,0,0,16384,0,0,0,10240,0,0,0,0,0,0,0,32768,8,0,0,0,0,0,0,32474,16464,0,0,63184,643,2,0,0,8192,2,0,0,0,16,0,0,0,0,0,0,16224,552,0,0,0,0,0,0,0,8,0,0,55808,20606,64,0,0,0,64,0,0,0,512,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","Type_decs","Type_dec","Cons_list","Cons","Comp_type","Comp_rep","Type_spec","Var_decs","Var_dec","Match","Match_body","Let_in","Case","Case_body","Vars","Vars_body","Typed_var","Literal","Pattern","Struc_pat","Pat_body","Expr","Two_infix","Three_infix","Unary_infix","Left_expr","Func_expr","Lit_expr","Lambda","Tuple_body","List_body","var","let","in","match","case","type","true","false","'=>'","int","float","char","string","type_id","var_id","'|'","'='","'{'","'}'","'::'","'['","']'","'('","')'","'->'","','","'?'","one_op","two_op","three_op","unary_op","%eof"]
        bit_start = st * 67
        bit_end = (st + 1) * 67
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..66]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (41) = happyShift action_6
action_2 (6) = happyGoto action_4
action_2 (12) = happyGoto action_5
action_2 _ = happyReduce_16

action_3 (67) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (36) = happyShift action_9
action_5 (13) = happyGoto action_8
action_5 _ = happyReduce_1

action_6 (49) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (52) = happyShift action_12
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_17

action_9 (50) = happyShift action_11
action_9 (21) = happyGoto action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (52) = happyShift action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (55) = happyShift action_15
action_11 (11) = happyGoto action_14
action_11 _ = happyReduce_30

action_12 (53) = happyShift action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (51) = happyShift action_50
action_13 (7) = happyGoto action_48
action_13 (8) = happyGoto action_49
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_31

action_15 (49) = happyShift action_45
action_15 (56) = happyShift action_46
action_15 (58) = happyShift action_47
action_15 (9) = happyGoto action_44
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (37) = happyShift action_30
action_16 (39) = happyShift action_31
action_16 (40) = happyShift action_32
action_16 (42) = happyShift action_33
action_16 (43) = happyShift action_34
action_16 (45) = happyShift action_35
action_16 (46) = happyShift action_36
action_16 (47) = happyShift action_37
action_16 (48) = happyShift action_38
action_16 (49) = happyShift action_39
action_16 (50) = happyShift action_40
action_16 (56) = happyShift action_41
action_16 (58) = happyShift action_42
action_16 (66) = happyShift action_43
action_16 (14) = happyGoto action_17
action_16 (16) = happyGoto action_18
action_16 (17) = happyGoto action_19
action_16 (21) = happyGoto action_20
action_16 (22) = happyGoto action_21
action_16 (26) = happyGoto action_22
action_16 (27) = happyGoto action_23
action_16 (28) = happyGoto action_24
action_16 (29) = happyGoto action_25
action_16 (30) = happyGoto action_26
action_16 (31) = happyGoto action_27
action_16 (32) = happyGoto action_28
action_16 (33) = happyGoto action_29
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_57

action_18 _ = happyReduce_58

action_19 _ = happyReduce_59

action_20 (44) = happyShift action_70
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_64

action_22 (63) = happyShift action_69
action_22 _ = happyReduce_18

action_23 (64) = happyShift action_68
action_23 _ = happyReduce_50

action_24 (65) = happyShift action_67
action_24 _ = happyReduce_52

action_25 _ = happyReduce_54

action_26 _ = happyReduce_56

action_27 (42) = happyShift action_33
action_27 (43) = happyShift action_34
action_27 (45) = happyShift action_35
action_27 (46) = happyShift action_36
action_27 (47) = happyShift action_37
action_27 (48) = happyShift action_38
action_27 (49) = happyShift action_39
action_27 (50) = happyShift action_40
action_27 (56) = happyShift action_41
action_27 (58) = happyShift action_42
action_27 (21) = happyGoto action_20
action_27 (22) = happyGoto action_21
action_27 (32) = happyGoto action_66
action_27 (33) = happyGoto action_29
action_27 _ = happyReduce_60

action_28 _ = happyReduce_62

action_29 _ = happyReduce_63

action_30 (50) = happyShift action_11
action_30 (58) = happyShift action_65
action_30 (19) = happyGoto action_63
action_30 (21) = happyGoto action_64
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (37) = happyShift action_30
action_31 (39) = happyShift action_31
action_31 (40) = happyShift action_32
action_31 (42) = happyShift action_33
action_31 (43) = happyShift action_34
action_31 (45) = happyShift action_35
action_31 (46) = happyShift action_36
action_31 (47) = happyShift action_37
action_31 (48) = happyShift action_38
action_31 (49) = happyShift action_39
action_31 (50) = happyShift action_40
action_31 (56) = happyShift action_41
action_31 (58) = happyShift action_42
action_31 (66) = happyShift action_43
action_31 (14) = happyGoto action_17
action_31 (16) = happyGoto action_18
action_31 (17) = happyGoto action_19
action_31 (21) = happyGoto action_20
action_31 (22) = happyGoto action_21
action_31 (26) = happyGoto action_62
action_31 (27) = happyGoto action_23
action_31 (28) = happyGoto action_24
action_31 (29) = happyGoto action_25
action_31 (30) = happyGoto action_26
action_31 (31) = happyGoto action_27
action_31 (32) = happyGoto action_28
action_31 (33) = happyGoto action_29
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (53) = happyShift action_61
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_36

action_34 _ = happyReduce_37

action_35 _ = happyReduce_34

action_36 _ = happyReduce_35

action_37 _ = happyReduce_33

action_38 _ = happyReduce_32

action_39 _ = happyReduce_65

action_40 (44) = happyReduce_30
action_40 (55) = happyShift action_15
action_40 (11) = happyGoto action_14
action_40 _ = happyReduce_66

action_41 (37) = happyShift action_30
action_41 (39) = happyShift action_31
action_41 (40) = happyShift action_32
action_41 (42) = happyShift action_33
action_41 (43) = happyShift action_34
action_41 (45) = happyShift action_35
action_41 (46) = happyShift action_36
action_41 (47) = happyShift action_37
action_41 (48) = happyShift action_38
action_41 (49) = happyShift action_39
action_41 (50) = happyShift action_40
action_41 (56) = happyShift action_41
action_41 (58) = happyShift action_42
action_41 (66) = happyShift action_43
action_41 (14) = happyGoto action_17
action_41 (16) = happyGoto action_18
action_41 (17) = happyGoto action_19
action_41 (21) = happyGoto action_20
action_41 (22) = happyGoto action_21
action_41 (26) = happyGoto action_57
action_41 (27) = happyGoto action_23
action_41 (28) = happyGoto action_24
action_41 (29) = happyGoto action_25
action_41 (30) = happyGoto action_26
action_41 (31) = happyGoto action_27
action_41 (32) = happyGoto action_28
action_41 (33) = happyGoto action_29
action_41 (34) = happyGoto action_59
action_41 (35) = happyGoto action_60
action_41 _ = happyReduce_72

action_42 (37) = happyShift action_30
action_42 (39) = happyShift action_31
action_42 (40) = happyShift action_32
action_42 (42) = happyShift action_33
action_42 (43) = happyShift action_34
action_42 (45) = happyShift action_35
action_42 (46) = happyShift action_36
action_42 (47) = happyShift action_37
action_42 (48) = happyShift action_38
action_42 (49) = happyShift action_39
action_42 (50) = happyShift action_40
action_42 (56) = happyShift action_41
action_42 (58) = happyShift action_42
action_42 (66) = happyShift action_43
action_42 (14) = happyGoto action_17
action_42 (16) = happyGoto action_18
action_42 (17) = happyGoto action_19
action_42 (21) = happyGoto action_20
action_42 (22) = happyGoto action_21
action_42 (26) = happyGoto action_57
action_42 (27) = happyGoto action_23
action_42 (28) = happyGoto action_24
action_42 (29) = happyGoto action_25
action_42 (30) = happyGoto action_26
action_42 (31) = happyGoto action_27
action_42 (32) = happyGoto action_28
action_42 (33) = happyGoto action_29
action_42 (34) = happyGoto action_58
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (37) = happyShift action_30
action_43 (39) = happyShift action_31
action_43 (40) = happyShift action_32
action_43 (42) = happyShift action_33
action_43 (43) = happyShift action_34
action_43 (45) = happyShift action_35
action_43 (46) = happyShift action_36
action_43 (47) = happyShift action_37
action_43 (48) = happyShift action_38
action_43 (49) = happyShift action_39
action_43 (50) = happyShift action_40
action_43 (56) = happyShift action_41
action_43 (58) = happyShift action_42
action_43 (14) = happyGoto action_17
action_43 (16) = happyGoto action_18
action_43 (17) = happyGoto action_19
action_43 (21) = happyGoto action_20
action_43 (22) = happyGoto action_21
action_43 (30) = happyGoto action_56
action_43 (31) = happyGoto action_27
action_43 (32) = happyGoto action_28
action_43 (33) = happyGoto action_29
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_15

action_45 _ = happyReduce_9

action_46 (49) = happyShift action_45
action_46 (56) = happyShift action_46
action_46 (58) = happyShift action_47
action_46 (9) = happyGoto action_55
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (49) = happyShift action_45
action_47 (56) = happyShift action_46
action_47 (58) = happyShift action_47
action_47 (9) = happyGoto action_54
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (51) = happyShift action_50
action_48 (54) = happyShift action_53
action_48 (8) = happyGoto action_52
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_6

action_50 (49) = happyShift action_51
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (49) = happyShift action_45
action_51 (56) = happyShift action_46
action_51 (58) = happyShift action_47
action_51 (9) = happyGoto action_88
action_51 _ = happyReduce_8

action_52 _ = happyReduce_5

action_53 _ = happyReduce_4

action_54 (49) = happyShift action_45
action_54 (56) = happyShift action_46
action_54 (58) = happyShift action_47
action_54 (60) = happyShift action_87
action_54 (9) = happyGoto action_85
action_54 (10) = happyGoto action_86
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (57) = happyShift action_84
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_55

action_57 (63) = happyShift action_69
action_57 _ = happyReduce_71

action_58 (59) = happyShift action_83
action_58 (61) = happyShift action_82
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (61) = happyShift action_82
action_59 _ = happyReduce_73

action_60 (57) = happyShift action_81
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (51) = happyShift action_80
action_61 (18) = happyGoto action_79
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (53) = happyShift action_78
action_62 (63) = happyShift action_69
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (52) = happyShift action_77
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_26

action_65 (50) = happyShift action_11
action_65 (20) = happyGoto action_75
action_65 (21) = happyGoto action_76
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_61

action_67 (37) = happyShift action_30
action_67 (39) = happyShift action_31
action_67 (40) = happyShift action_32
action_67 (42) = happyShift action_33
action_67 (43) = happyShift action_34
action_67 (45) = happyShift action_35
action_67 (46) = happyShift action_36
action_67 (47) = happyShift action_37
action_67 (48) = happyShift action_38
action_67 (49) = happyShift action_39
action_67 (50) = happyShift action_40
action_67 (56) = happyShift action_41
action_67 (58) = happyShift action_42
action_67 (66) = happyShift action_43
action_67 (14) = happyGoto action_17
action_67 (16) = happyGoto action_18
action_67 (17) = happyGoto action_19
action_67 (21) = happyGoto action_20
action_67 (22) = happyGoto action_21
action_67 (29) = happyGoto action_74
action_67 (30) = happyGoto action_26
action_67 (31) = happyGoto action_27
action_67 (32) = happyGoto action_28
action_67 (33) = happyGoto action_29
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (37) = happyShift action_30
action_68 (39) = happyShift action_31
action_68 (40) = happyShift action_32
action_68 (42) = happyShift action_33
action_68 (43) = happyShift action_34
action_68 (45) = happyShift action_35
action_68 (46) = happyShift action_36
action_68 (47) = happyShift action_37
action_68 (48) = happyShift action_38
action_68 (49) = happyShift action_39
action_68 (50) = happyShift action_40
action_68 (56) = happyShift action_41
action_68 (58) = happyShift action_42
action_68 (66) = happyShift action_43
action_68 (14) = happyGoto action_17
action_68 (16) = happyGoto action_18
action_68 (17) = happyGoto action_19
action_68 (21) = happyGoto action_20
action_68 (22) = happyGoto action_21
action_68 (28) = happyGoto action_73
action_68 (29) = happyGoto action_25
action_68 (30) = happyGoto action_26
action_68 (31) = happyGoto action_27
action_68 (32) = happyGoto action_28
action_68 (33) = happyGoto action_29
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (37) = happyShift action_30
action_69 (39) = happyShift action_31
action_69 (40) = happyShift action_32
action_69 (42) = happyShift action_33
action_69 (43) = happyShift action_34
action_69 (45) = happyShift action_35
action_69 (46) = happyShift action_36
action_69 (47) = happyShift action_37
action_69 (48) = happyShift action_38
action_69 (49) = happyShift action_39
action_69 (50) = happyShift action_40
action_69 (56) = happyShift action_41
action_69 (58) = happyShift action_42
action_69 (66) = happyShift action_43
action_69 (14) = happyGoto action_17
action_69 (16) = happyGoto action_18
action_69 (17) = happyGoto action_19
action_69 (21) = happyGoto action_20
action_69 (22) = happyGoto action_21
action_69 (27) = happyGoto action_72
action_69 (28) = happyGoto action_24
action_69 (29) = happyGoto action_25
action_69 (30) = happyGoto action_26
action_69 (31) = happyGoto action_27
action_69 (32) = happyGoto action_28
action_69 (33) = happyGoto action_29
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (53) = happyShift action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (37) = happyShift action_30
action_71 (39) = happyShift action_31
action_71 (40) = happyShift action_32
action_71 (42) = happyShift action_33
action_71 (43) = happyShift action_34
action_71 (45) = happyShift action_35
action_71 (46) = happyShift action_36
action_71 (47) = happyShift action_37
action_71 (48) = happyShift action_38
action_71 (49) = happyShift action_39
action_71 (50) = happyShift action_40
action_71 (56) = happyShift action_41
action_71 (58) = happyShift action_42
action_71 (66) = happyShift action_43
action_71 (14) = happyGoto action_17
action_71 (16) = happyGoto action_18
action_71 (17) = happyGoto action_19
action_71 (21) = happyGoto action_20
action_71 (22) = happyGoto action_21
action_71 (26) = happyGoto action_101
action_71 (27) = happyGoto action_23
action_71 (28) = happyGoto action_24
action_71 (29) = happyGoto action_25
action_71 (30) = happyGoto action_26
action_71 (31) = happyGoto action_27
action_71 (32) = happyGoto action_28
action_71 (33) = happyGoto action_29
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (64) = happyShift action_68
action_72 _ = happyReduce_49

action_73 (65) = happyShift action_67
action_73 _ = happyReduce_51

action_74 _ = happyReduce_53

action_75 (59) = happyShift action_99
action_75 (61) = happyShift action_100
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_29

action_77 (37) = happyShift action_30
action_77 (39) = happyShift action_31
action_77 (40) = happyShift action_32
action_77 (42) = happyShift action_33
action_77 (43) = happyShift action_34
action_77 (45) = happyShift action_35
action_77 (46) = happyShift action_36
action_77 (47) = happyShift action_37
action_77 (48) = happyShift action_38
action_77 (49) = happyShift action_39
action_77 (50) = happyShift action_40
action_77 (56) = happyShift action_41
action_77 (58) = happyShift action_42
action_77 (66) = happyShift action_43
action_77 (14) = happyGoto action_17
action_77 (16) = happyGoto action_18
action_77 (17) = happyGoto action_19
action_77 (21) = happyGoto action_20
action_77 (22) = happyGoto action_21
action_77 (26) = happyGoto action_98
action_77 (27) = happyGoto action_23
action_77 (28) = happyGoto action_24
action_77 (29) = happyGoto action_25
action_77 (30) = happyGoto action_26
action_77 (31) = happyGoto action_27
action_77 (32) = happyGoto action_28
action_77 (33) = happyGoto action_29
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (51) = happyShift action_97
action_78 (15) = happyGoto action_96
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (51) = happyShift action_94
action_79 (54) = happyShift action_95
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (37) = happyShift action_30
action_80 (39) = happyShift action_31
action_80 (40) = happyShift action_32
action_80 (42) = happyShift action_33
action_80 (43) = happyShift action_34
action_80 (45) = happyShift action_35
action_80 (46) = happyShift action_36
action_80 (47) = happyShift action_37
action_80 (48) = happyShift action_38
action_80 (49) = happyShift action_39
action_80 (50) = happyShift action_40
action_80 (56) = happyShift action_41
action_80 (58) = happyShift action_42
action_80 (66) = happyShift action_43
action_80 (14) = happyGoto action_17
action_80 (16) = happyGoto action_18
action_80 (17) = happyGoto action_19
action_80 (21) = happyGoto action_20
action_80 (22) = happyGoto action_21
action_80 (26) = happyGoto action_93
action_80 (27) = happyGoto action_23
action_80 (28) = happyGoto action_24
action_80 (29) = happyGoto action_25
action_80 (30) = happyGoto action_26
action_80 (31) = happyGoto action_27
action_80 (32) = happyGoto action_28
action_80 (33) = happyGoto action_29
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_68

action_82 (37) = happyShift action_30
action_82 (39) = happyShift action_31
action_82 (40) = happyShift action_32
action_82 (42) = happyShift action_33
action_82 (43) = happyShift action_34
action_82 (45) = happyShift action_35
action_82 (46) = happyShift action_36
action_82 (47) = happyShift action_37
action_82 (48) = happyShift action_38
action_82 (49) = happyShift action_39
action_82 (50) = happyShift action_40
action_82 (56) = happyShift action_41
action_82 (58) = happyShift action_42
action_82 (66) = happyShift action_43
action_82 (14) = happyGoto action_17
action_82 (16) = happyGoto action_18
action_82 (17) = happyGoto action_19
action_82 (21) = happyGoto action_20
action_82 (22) = happyGoto action_21
action_82 (26) = happyGoto action_92
action_82 (27) = happyGoto action_23
action_82 (28) = happyGoto action_24
action_82 (29) = happyGoto action_25
action_82 (30) = happyGoto action_26
action_82 (31) = happyGoto action_27
action_82 (32) = happyGoto action_28
action_82 (33) = happyGoto action_29
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_67

action_84 _ = happyReduce_10

action_85 _ = happyReduce_14

action_86 (59) = happyShift action_90
action_86 (61) = happyShift action_91
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (49) = happyShift action_45
action_87 (56) = happyShift action_46
action_87 (58) = happyShift action_47
action_87 (9) = happyGoto action_89
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_7

action_89 (59) = happyShift action_118
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_11

action_91 (49) = happyShift action_45
action_91 (56) = happyShift action_46
action_91 (58) = happyShift action_47
action_91 (9) = happyGoto action_117
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (63) = happyShift action_69
action_92 _ = happyReduce_70

action_93 (60) = happyShift action_116
action_93 (63) = happyShift action_69
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (37) = happyShift action_30
action_94 (39) = happyShift action_31
action_94 (40) = happyShift action_32
action_94 (42) = happyShift action_33
action_94 (43) = happyShift action_34
action_94 (45) = happyShift action_35
action_94 (46) = happyShift action_36
action_94 (47) = happyShift action_37
action_94 (48) = happyShift action_38
action_94 (49) = happyShift action_39
action_94 (50) = happyShift action_40
action_94 (56) = happyShift action_41
action_94 (58) = happyShift action_42
action_94 (66) = happyShift action_43
action_94 (14) = happyGoto action_17
action_94 (16) = happyGoto action_18
action_94 (17) = happyGoto action_19
action_94 (21) = happyGoto action_20
action_94 (22) = happyGoto action_21
action_94 (26) = happyGoto action_115
action_94 (27) = happyGoto action_23
action_94 (28) = happyGoto action_24
action_94 (29) = happyGoto action_25
action_94 (30) = happyGoto action_26
action_94 (31) = happyGoto action_27
action_94 (32) = happyGoto action_28
action_94 (33) = happyGoto action_29
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_23

action_96 (51) = happyShift action_113
action_96 (54) = happyShift action_114
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (42) = happyShift action_33
action_97 (43) = happyShift action_34
action_97 (45) = happyShift action_35
action_97 (46) = happyShift action_36
action_97 (47) = happyShift action_37
action_97 (48) = happyShift action_38
action_97 (49) = happyShift action_108
action_97 (50) = happyShift action_109
action_97 (56) = happyShift action_110
action_97 (58) = happyShift action_111
action_97 (62) = happyShift action_112
action_97 (22) = happyGoto action_105
action_97 (23) = happyGoto action_106
action_97 (24) = happyGoto action_107
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (38) = happyShift action_104
action_98 (63) = happyShift action_69
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_27

action_100 (50) = happyShift action_11
action_100 (21) = happyGoto action_103
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (54) = happyShift action_102
action_101 (63) = happyShift action_69
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_69

action_103 _ = happyReduce_28

action_104 (58) = happyShift action_128
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_39

action_106 (60) = happyShift action_127
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_38

action_108 (42) = happyShift action_33
action_108 (43) = happyShift action_34
action_108 (45) = happyShift action_35
action_108 (46) = happyShift action_36
action_108 (47) = happyShift action_37
action_108 (48) = happyShift action_38
action_108 (49) = happyShift action_108
action_108 (50) = happyShift action_109
action_108 (56) = happyShift action_110
action_108 (58) = happyShift action_111
action_108 (62) = happyShift action_112
action_108 (22) = happyGoto action_105
action_108 (23) = happyGoto action_126
action_108 (24) = happyGoto action_107
action_108 _ = happyReduce_43

action_109 _ = happyReduce_40

action_110 (42) = happyShift action_33
action_110 (43) = happyShift action_34
action_110 (45) = happyShift action_35
action_110 (46) = happyShift action_36
action_110 (47) = happyShift action_37
action_110 (48) = happyShift action_38
action_110 (49) = happyShift action_108
action_110 (50) = happyShift action_109
action_110 (56) = happyShift action_110
action_110 (58) = happyShift action_111
action_110 (62) = happyShift action_112
action_110 (22) = happyGoto action_105
action_110 (23) = happyGoto action_124
action_110 (24) = happyGoto action_107
action_110 (25) = happyGoto action_125
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (42) = happyShift action_33
action_111 (43) = happyShift action_34
action_111 (45) = happyShift action_35
action_111 (46) = happyShift action_36
action_111 (47) = happyShift action_37
action_111 (48) = happyShift action_38
action_111 (49) = happyShift action_108
action_111 (50) = happyShift action_109
action_111 (56) = happyShift action_110
action_111 (58) = happyShift action_111
action_111 (62) = happyShift action_112
action_111 (22) = happyGoto action_105
action_111 (23) = happyGoto action_122
action_111 (24) = happyGoto action_107
action_111 (25) = happyGoto action_123
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_41

action_113 (42) = happyShift action_33
action_113 (43) = happyShift action_34
action_113 (45) = happyShift action_35
action_113 (46) = happyShift action_36
action_113 (47) = happyShift action_37
action_113 (48) = happyShift action_38
action_113 (49) = happyShift action_108
action_113 (50) = happyShift action_109
action_113 (56) = happyShift action_110
action_113 (58) = happyShift action_111
action_113 (62) = happyShift action_112
action_113 (22) = happyGoto action_105
action_113 (23) = happyGoto action_121
action_113 (24) = happyGoto action_107
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_19

action_115 (60) = happyShift action_120
action_115 (63) = happyShift action_69
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (37) = happyShift action_30
action_116 (39) = happyShift action_31
action_116 (40) = happyShift action_32
action_116 (42) = happyShift action_33
action_116 (43) = happyShift action_34
action_116 (45) = happyShift action_35
action_116 (46) = happyShift action_36
action_116 (47) = happyShift action_37
action_116 (48) = happyShift action_38
action_116 (49) = happyShift action_39
action_116 (50) = happyShift action_40
action_116 (56) = happyShift action_41
action_116 (58) = happyShift action_42
action_116 (66) = happyShift action_43
action_116 (14) = happyGoto action_17
action_116 (16) = happyGoto action_18
action_116 (17) = happyGoto action_19
action_116 (21) = happyGoto action_20
action_116 (22) = happyGoto action_21
action_116 (26) = happyGoto action_119
action_116 (27) = happyGoto action_23
action_116 (28) = happyGoto action_24
action_116 (29) = happyGoto action_25
action_116 (30) = happyGoto action_26
action_116 (31) = happyGoto action_27
action_116 (32) = happyGoto action_28
action_116 (33) = happyGoto action_29
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_13

action_118 _ = happyReduce_12

action_119 (63) = happyShift action_69
action_119 _ = happyReduce_25

action_120 (37) = happyShift action_30
action_120 (39) = happyShift action_31
action_120 (40) = happyShift action_32
action_120 (42) = happyShift action_33
action_120 (43) = happyShift action_34
action_120 (45) = happyShift action_35
action_120 (46) = happyShift action_36
action_120 (47) = happyShift action_37
action_120 (48) = happyShift action_38
action_120 (49) = happyShift action_39
action_120 (50) = happyShift action_40
action_120 (56) = happyShift action_41
action_120 (58) = happyShift action_42
action_120 (66) = happyShift action_43
action_120 (14) = happyGoto action_17
action_120 (16) = happyGoto action_18
action_120 (17) = happyGoto action_19
action_120 (21) = happyGoto action_20
action_120 (22) = happyGoto action_21
action_120 (26) = happyGoto action_136
action_120 (27) = happyGoto action_23
action_120 (28) = happyGoto action_24
action_120 (29) = happyGoto action_25
action_120 (30) = happyGoto action_26
action_120 (31) = happyGoto action_27
action_120 (32) = happyGoto action_28
action_120 (33) = happyGoto action_29
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (60) = happyShift action_135
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (65) = happyShift action_134
action_122 _ = happyReduce_48

action_123 (59) = happyShift action_133
action_123 (61) = happyShift action_132
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_48

action_125 (57) = happyShift action_131
action_125 (61) = happyShift action_132
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_42

action_127 (37) = happyShift action_30
action_127 (39) = happyShift action_31
action_127 (40) = happyShift action_32
action_127 (42) = happyShift action_33
action_127 (43) = happyShift action_34
action_127 (45) = happyShift action_35
action_127 (46) = happyShift action_36
action_127 (47) = happyShift action_37
action_127 (48) = happyShift action_38
action_127 (49) = happyShift action_39
action_127 (50) = happyShift action_40
action_127 (56) = happyShift action_41
action_127 (58) = happyShift action_42
action_127 (66) = happyShift action_43
action_127 (14) = happyGoto action_17
action_127 (16) = happyGoto action_18
action_127 (17) = happyGoto action_19
action_127 (21) = happyGoto action_20
action_127 (22) = happyGoto action_21
action_127 (26) = happyGoto action_130
action_127 (27) = happyGoto action_23
action_127 (28) = happyGoto action_24
action_127 (29) = happyGoto action_25
action_127 (30) = happyGoto action_26
action_127 (31) = happyGoto action_27
action_127 (32) = happyGoto action_28
action_127 (33) = happyGoto action_29
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (37) = happyShift action_30
action_128 (39) = happyShift action_31
action_128 (40) = happyShift action_32
action_128 (42) = happyShift action_33
action_128 (43) = happyShift action_34
action_128 (45) = happyShift action_35
action_128 (46) = happyShift action_36
action_128 (47) = happyShift action_37
action_128 (48) = happyShift action_38
action_128 (49) = happyShift action_39
action_128 (50) = happyShift action_40
action_128 (56) = happyShift action_41
action_128 (58) = happyShift action_42
action_128 (66) = happyShift action_43
action_128 (14) = happyGoto action_17
action_128 (16) = happyGoto action_18
action_128 (17) = happyGoto action_19
action_128 (21) = happyGoto action_20
action_128 (22) = happyGoto action_21
action_128 (26) = happyGoto action_129
action_128 (27) = happyGoto action_23
action_128 (28) = happyGoto action_24
action_128 (29) = happyGoto action_25
action_128 (30) = happyGoto action_26
action_128 (31) = happyGoto action_27
action_128 (32) = happyGoto action_28
action_128 (33) = happyGoto action_29
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (59) = happyShift action_140
action_129 (63) = happyShift action_69
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (63) = happyShift action_69
action_130 _ = happyReduce_21

action_131 _ = happyReduce_45

action_132 (42) = happyShift action_33
action_132 (43) = happyShift action_34
action_132 (45) = happyShift action_35
action_132 (46) = happyShift action_36
action_132 (47) = happyShift action_37
action_132 (48) = happyShift action_38
action_132 (49) = happyShift action_108
action_132 (50) = happyShift action_109
action_132 (56) = happyShift action_110
action_132 (58) = happyShift action_111
action_132 (62) = happyShift action_112
action_132 (22) = happyGoto action_105
action_132 (23) = happyGoto action_139
action_132 (24) = happyGoto action_107
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_44

action_134 (50) = happyShift action_138
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (37) = happyShift action_30
action_135 (39) = happyShift action_31
action_135 (40) = happyShift action_32
action_135 (42) = happyShift action_33
action_135 (43) = happyShift action_34
action_135 (45) = happyShift action_35
action_135 (46) = happyShift action_36
action_135 (47) = happyShift action_37
action_135 (48) = happyShift action_38
action_135 (49) = happyShift action_39
action_135 (50) = happyShift action_40
action_135 (56) = happyShift action_41
action_135 (58) = happyShift action_42
action_135 (66) = happyShift action_43
action_135 (14) = happyGoto action_17
action_135 (16) = happyGoto action_18
action_135 (17) = happyGoto action_19
action_135 (21) = happyGoto action_20
action_135 (22) = happyGoto action_21
action_135 (26) = happyGoto action_137
action_135 (27) = happyGoto action_23
action_135 (28) = happyGoto action_24
action_135 (29) = happyGoto action_25
action_135 (30) = happyGoto action_26
action_135 (31) = happyGoto action_27
action_135 (32) = happyGoto action_28
action_135 (33) = happyGoto action_29
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (63) = happyShift action_69
action_136 _ = happyReduce_24

action_137 (63) = happyShift action_69
action_137 _ = happyReduce_20

action_138 (59) = happyShift action_141
action_138 _ = happyFail (happyExpListPerState 138)

action_139 _ = happyReduce_47

action_140 _ = happyReduce_22

action_141 _ = happyReduce_46

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	_
	 =  HappyAbsSyn4
		 (
	)

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 (
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 _
	_
	 =  HappyAbsSyn5
		 (
	)

happyReduce_4 = happyReduce 6 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	_
	 =  HappyAbsSyn7
		 (
	)

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (
	)

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 _
	_
	_
	 =  HappyAbsSyn8
		 (
	)

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 _
	_
	 =  HappyAbsSyn8
		 (
	)

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 (
	)

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 _
	_
	_
	 =  HappyAbsSyn9
		 (
	)

happyReduce_11 = happyReduce 4 9 happyReduction_11
happyReduction_11 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 9 happyReduction_12
happyReduction_12 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 _
	_
	_
	 =  HappyAbsSyn10
		 (
	)

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn10
		 (
	)

happyReduce_15 = happySpecReduce_2  11 happyReduction_15
happyReduction_15 _
	_
	 =  HappyAbsSyn11
		 (
	)

happyReduce_16 = happySpecReduce_0  12 happyReduction_16
happyReduction_16  =  HappyAbsSyn12
		 (
	)

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 _
	_
	 =  HappyAbsSyn12
		 (
	)

happyReduce_18 = happyReduce 4 13 happyReduction_18
happyReduction_18 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 5 14 happyReduction_19
happyReduction_19 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 5 15 happyReduction_20
happyReduction_20 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 15 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 8 16 happyReduction_22
happyReduction_22 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 17 happyReduction_23
happyReduction_23 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 18 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 18 happyReduction_25
happyReduction_25 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  19 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn19
		 (
	)

happyReduce_27 = happySpecReduce_3  19 happyReduction_27
happyReduction_27 _
	_
	_
	 =  HappyAbsSyn19
		 (
	)

happyReduce_28 = happySpecReduce_3  20 happyReduction_28
happyReduction_28 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn20
		 (
	)

happyReduce_30 = happySpecReduce_1  21 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn21
		 (
	)

happyReduce_31 = happySpecReduce_2  21 happyReduction_31
happyReduction_31 _
	_
	 =  HappyAbsSyn21
		 (
	)

happyReduce_32 = happySpecReduce_1  22 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn22
		 (
	)

happyReduce_33 = happySpecReduce_1  22 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn22
		 (
	)

happyReduce_34 = happySpecReduce_1  22 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn22
		 (
	)

happyReduce_35 = happySpecReduce_1  22 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn22
		 (
	)

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn22
		 (
	)

happyReduce_37 = happySpecReduce_1  22 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn22
		 (
	)

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn23
		 (
	)

happyReduce_39 = happySpecReduce_1  23 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn23
		 (
	)

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn23
		 (
	)

happyReduce_41 = happySpecReduce_1  23 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn23
		 (
	)

happyReduce_42 = happySpecReduce_2  23 happyReduction_42
happyReduction_42 _
	_
	 =  HappyAbsSyn23
		 (
	)

happyReduce_43 = happySpecReduce_1  23 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn23
		 (
	)

happyReduce_44 = happySpecReduce_3  24 happyReduction_44
happyReduction_44 _
	_
	_
	 =  HappyAbsSyn24
		 (
	)

happyReduce_45 = happySpecReduce_3  24 happyReduction_45
happyReduction_45 _
	_
	_
	 =  HappyAbsSyn24
		 (
	)

happyReduce_46 = happyReduce 5 24 happyReduction_46
happyReduction_46 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_3  25 happyReduction_47
happyReduction_47 _
	_
	_
	 =  HappyAbsSyn25
		 (
	)

happyReduce_48 = happySpecReduce_1  25 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn25
		 (
	)

happyReduce_49 = happySpecReduce_3  26 happyReduction_49
happyReduction_49 _
	_
	_
	 =  HappyAbsSyn26
		 (
	)

happyReduce_50 = happySpecReduce_1  26 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn26
		 (
	)

happyReduce_51 = happySpecReduce_3  27 happyReduction_51
happyReduction_51 _
	_
	_
	 =  HappyAbsSyn27
		 (
	)

happyReduce_52 = happySpecReduce_1  27 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn27
		 (
	)

happyReduce_53 = happySpecReduce_3  28 happyReduction_53
happyReduction_53 _
	_
	_
	 =  HappyAbsSyn28
		 (
	)

happyReduce_54 = happySpecReduce_1  28 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn28
		 (
	)

happyReduce_55 = happySpecReduce_2  29 happyReduction_55
happyReduction_55 _
	_
	 =  HappyAbsSyn29
		 (
	)

happyReduce_56 = happySpecReduce_1  29 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn29
		 (
	)

happyReduce_57 = happySpecReduce_1  30 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn30
		 (
	)

happyReduce_58 = happySpecReduce_1  30 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn30
		 (
	)

happyReduce_59 = happySpecReduce_1  30 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn30
		 (
	)

happyReduce_60 = happySpecReduce_1  30 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn30
		 (
	)

happyReduce_61 = happySpecReduce_2  31 happyReduction_61
happyReduction_61 _
	_
	 =  HappyAbsSyn31
		 (
	)

happyReduce_62 = happySpecReduce_1  31 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn31
		 (
	)

happyReduce_63 = happySpecReduce_1  32 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn32
		 (
	)

happyReduce_64 = happySpecReduce_1  32 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn32
		 (
	)

happyReduce_65 = happySpecReduce_1  32 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn32
		 (
	)

happyReduce_66 = happySpecReduce_1  32 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn32
		 (
	)

happyReduce_67 = happySpecReduce_3  32 happyReduction_67
happyReduction_67 _
	_
	_
	 =  HappyAbsSyn32
		 (
	)

happyReduce_68 = happySpecReduce_3  32 happyReduction_68
happyReduction_68 _
	_
	_
	 =  HappyAbsSyn32
		 (
	)

happyReduce_69 = happyReduce 5 33 happyReduction_69
happyReduction_69 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (
	) `HappyStk` happyRest

happyReduce_70 = happySpecReduce_3  34 happyReduction_70
happyReduction_70 _
	_
	_
	 =  HappyAbsSyn34
		 (
	)

happyReduce_71 = happySpecReduce_1  34 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn34
		 (
	)

happyReduce_72 = happySpecReduce_0  35 happyReduction_72
happyReduction_72  =  HappyAbsSyn35
		 (
	)

happyReduce_73 = happySpecReduce_1  35 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn35
		 (
	)

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 67 67 tk (HappyState action) sts stk;
	Token _ VarToken -> cont 36;
	Token _ LetToken -> cont 37;
	Token _ InToken -> cont 38;
	Token _ MatchToken -> cont 39;
	Token _ CaseToken -> cont 40;
	Token _ TypeToken -> cont 41;
	Token _ TrueToken -> cont 42;
	Token _ FalseToken -> cont 43;
	Token _ FollowsToken -> cont 44;
	Token _ (IntToken happy_dollar_dollar) -> cont 45;
	Token _ (FloatToken happy_dollar_dollar) -> cont 46;
	Token _ (CharToken happy_dollar_dollar) -> cont 47;
	Token _ (StringToken happy_dollar_dollar) -> cont 48;
	Token _ (TypeIdToken happy_dollar_dollar) -> cont 49;
	Token _ (VarIdToken happy_dollar_dollar) -> cont 50;
	Token _ GuardToken -> cont 51;
	Token _ DeclareToken -> cont 52;
	Token _ CurlyOpenToken -> cont 53;
	Token _ CurlyCloseToken -> cont 54;
	Token _ AnnotateToken -> cont 55;
	Token _ SquareOpenToken -> cont 56;
	Token _ SquareCloseToken -> cont 57;
	Token _ ParenOpenToken -> cont 58;
	Token _ ParenCloseToken -> cont 59;
	Token _ EntailsToken -> cont 60;
	Token _ CommaToken -> cont 61;
	Token _ WildcardToken -> cont 62;
	Token _ (LevelOneOpToken happy_dollar_dollar) -> cont 63;
	Token _ (LevelTwoOpToken happy_dollar_dollar) -> cont 64;
	Token _ (LevelThreeOpToken happy_dollar_dollar) -> cont 65;
	Token _ (UnaryOpToken happy_dollar_dollar) -> cont 66;
	_ -> happyError' (tk, [])
	})

happyError_ explist 67 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (>>=)
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> Alex a
happyError' tk = (\(tokens, _) -> happyError tokens) tk
parse = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("Parse error at token '" ++ terminalString t ++ "'")

parseBonsai :: FilePath -> String -> Either String Prog
parseBonsai = runAlex' parse
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "C:/Users/Thomas/AppData/Local/Programs/stack/x86_64-windows/ghc-8.6.4/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "C:/Users/Thomas/AppData/Local/Temp/ghc6312_0/ghc_2.h" #-}




















































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = case happyDrop (k - ((1) :: Int)) sts of
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





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
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
