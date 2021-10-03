{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Sct
import SctConfig

main = defaultMain tests

tests = testGroup "Tests" [tagLinesTests, configTests, sctTests]

javaLangSpec = LangSpec "//" "//!" ""
ignoreLangSpec = LangSpec "#" "##!" ""
xmlLangSpec = LangSpec "<!--" "<!--!" "-->"

checkTagLines lang taggedLines = (tagLines lang . map snd) taggedLines @?= taggedLines

tagLinesTests = testGroup "TagLines spec"
    [ testCase "Correctly tag CommandZone (StudentZone)" $
        checkTagLines javaLangSpec [(CommandZone StudentZone, "//!-")]

    , testCase "Correctly tag CommandZone (StudentZone)" $
        checkTagLines javaLangSpec [(CommandZone StudentZone, "//![-")]

    , testCase "Correctly tag CommandZone (CorrectionZone)" $
        checkTagLines javaLangSpec [(CommandZone CorrectionZone, "//![")]

    , testCase "Correctly tag with leading spaces" $
        checkTagLines javaLangSpec [(CommandZone CorrectionZone, "       //![")]

    , testCase "Correctly tag CommandZone (AllZone)" $
        checkTagLines javaLangSpec [(CommandZone AllZone, "//!-]")]

    , testCase "Correctly tag CommandZone (AllZone)" $
        checkTagLines javaLangSpec [(CommandZone AllZone, "//!]")]

    , testCase "Correctly tag AllZone (as default zone)" $
        checkTagLines javaLangSpec [(AllZone, "hello")]

    , testCase "Correctly tag StudentZone" $
        checkTagLines javaLangSpec [(CommandZone StudentZone, "//!-"), (StudentZone, "hello student")]

    , testCase "Correctly tag CorrectionZone" $
        checkTagLines javaLangSpec [(CommandZone CorrectionZone, "//!["), (CorrectionZone, "hello teacher")]

    , testCase "Correctly tag AllZone (after another zone)" $
        checkTagLines javaLangSpec [(CommandZone StudentZone, "   //![-"), (StudentZone, "   hello student"), (CommandZone AllZone, "   //!]"), (AllZone, "hello all")]
    ]

javaConfig = Config javaLangSpec

sctTests = testGroup "Sct spec" [javaFileTests, xmlFileTests]

javaFileTests = testGroup "for java files"
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

xmlConfig = Config xmlLangSpec

xmlFileTests = testGroup "for xml files"
    [ testCase "Correctly transform entry when only student" $
        sct (xmlConfig True Student) xmlEntry @?= [ "<dependencies>"
                , "    <dependency>for student</dependency>"
                , "</dependencies>"
                ]

    , testCase "Correctly transform entry when only correction" $
        sct (xmlConfig True Correction) xmlEntry @?= [ "<dependencies>"
                , "    <dependency>for teacher</dependency>"
                , "</dependencies>"
                ]

    , testCase "Correctly transform entry when student" $
        sct (xmlConfig False Student) xmlEntry @?= [ "<dependencies>"
                , "    <!--![-->"
                , "<!--    <dependency>for teacher</dependency>-->"
                , "    <!--!--->"
                , "    <dependency>for student</dependency>"
                , "    <!--!]-->"
                , "</dependencies>"
                ]

    , testCase "Correctly transform entry when correction" $
        sct (xmlConfig False Correction) xmlEntry @?= [ "<dependencies>"
                , "    <!--![-->"
                , "    <dependency>for teacher</dependency>"
                , "    <!--!--->"
                , "<!--    <dependency>for student</dependency>-->"
                , "    <!--!]-->"
                , "</dependencies>"
                ]
    ]
    where
        xmlEntry = [ "<dependencies>"
                , "    <!--![-->"
                , "    <dependency>for teacher</dependency>"
                , "    <!--!--->"
                , "<!--    <dependency>for student</dependency>-->"
                , "    <!--!]-->"
                , "</dependencies>"
                ]

configTests = testGroup "Language specification" 
    [ testCase "Correctly determine lang spec of .sctignore file" $
        divineLangSpec ".sctignore" @?= ignoreLangSpec

    , testCase "Correctly determine lang spec of .java file" $
        divineLangSpec "hello.java" @?= javaLangSpec

    , testCase "Correctly determine lang spec of .xml file" $
        divineLangSpec "hello.xml" @?= xmlLangSpec
    ]