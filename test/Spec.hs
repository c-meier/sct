
import Test.Tasty
import Test.Tasty.HUnit

import Sct
import SctConfig

main = defaultMain tests

tests = testGroup "Tests" [tagLinesTests, configTests, sctTests]

checkTagLines cmdPrefix taggedLines = (tagLines cmdPrefix . map snd) taggedLines @?= taggedLines

tagLinesTests = testGroup "TagLines spec"
    [ testCase "Correctly tag CommandZone (StudentZone)" $
        checkTagLines "//!" [(CommandZone StudentZone, "//!-")]

    , testCase "Correctly tag CommandZone (StudentZone)" $
        checkTagLines "//!" [(CommandZone StudentZone, "//![-")]

    , testCase "Correctly tag CommandZone (CorrectionZone)" $
        checkTagLines "//!" [(CommandZone CorrectionZone, "//![")]

    , testCase "Correctly trim leading spaces" $
        trim "       //![" @?= "//!["

    , testCase "Correctly tag with leading spaces" $
        checkTagLines "//!" [(CommandZone CorrectionZone, "       //![")]

    , testCase "Correctly tag CommandZone (AllZone)" $
        checkTagLines "//!" [(CommandZone AllZone, "//!-]")]

    , testCase "Correctly tag CommandZone (AllZone)" $
        checkTagLines "//!" [(CommandZone AllZone, "//!]")]

    , testCase "Correctly tag AllZone (as default zone)" $
        checkTagLines "//!" [(AllZone, "hello")]

    , testCase "Correctly tag StudentZone" $
        checkTagLines "//!" [(CommandZone StudentZone, "//!-"), (StudentZone, "hello student")]

    , testCase "Correctly tag CorrectionZone" $
        checkTagLines "//!" [(CommandZone CorrectionZone, "//!["), (CorrectionZone, "hello teacher")]

    , testCase "Correctly tag AllZone (after another zone)" $
        checkTagLines "//!" [(CommandZone StudentZone, "   //![-"), (StudentZone, "   hello student"), (CommandZone AllZone, "   //!]"), (AllZone, "hello all")]
    ]

javaConfig = Config "//" "//!"

sctTests = testGroup "Sct spec"
    [ testCase "Correctly transform entry when only student" $
        sct (javaConfig True Student) javaEntry @?= [ "hello all {"
                , "    hello student"
                , "    goodbye all"
                , "}"
                ]

    , testCase "Correctly transform entry when only correction" $
        sct (javaConfig True Correction) javaEntry @?= [ "hello all {"
                , "    hello teacher"
                , "    goodbye all"
                , "}"
                ]

    , testCase "Correctly transform entry when student" $
        sct (javaConfig False Student) javaEntry @?= [ "hello all {"
                , "    //!["
                , "//    hello teacher"
                , "    //!-"
                , "    hello student"
                , "    //!]"
                , "    goodbye all"
                , "}"
                ]

    , testCase "Correctly transform entry when correction" $
        sct (javaConfig False Correction) javaEntry @?= [ "hello all {"
                , "    //!["
                , "    hello teacher"
                , "    //!-"
                , "//    hello student"
                , "    //!]"
                , "    goodbye all"
                , "}"
                ]
    ]
    where
        javaEntry = [ "hello all {"
                , "    //!["
                , "    hello teacher"
                , "    //!-"
                , "//    hello student"
                , "    //!]"
                , "    goodbye all"
                , "}"
                ]

configTests = testGroup "Config spec" 
    [ testCase "Correctly determine comment symbol of .sctignore file" $
        divineCmdPrefix ".sctignore" @?= ("#", "##!")

    , testCase "Correctly determine comment symbol of .sctignore file" $
        divineCmdPrefix "hello.java" @?= ("//", "//!")
    ]