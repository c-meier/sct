{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

import Data.Maybe

import Sct
import SctConfig
import Sct (Zone(ContextZone))

main = defaultMain tests

tests = testGroup "Tests" [tagLinesTests, configTests, sctTests]

javaLangSpec = LangSpec "//" "//!" ""
ignoreLangSpec = LangSpec "#" "##!" ""
xmlLangSpec = LangSpec "<!--" "<!--!" "-->"

checkTagLines :: LangSpec -> [(Zone, T.Text)] -> Assertion
checkTagLines lang taggedLines = (tagLines lang . map snd) taggedLines @?= taggedLines

tagLinesTests = testGroup "TagLines spec"
    [ testCase "Correctly tag CommandZone (StudentZone)" $
        checkTagLines javaLangSpec [(CommandZone (StudentZone []), "//!-")]

    , testCase "Correctly tag CommandZone (StudentZone)" $
        checkTagLines javaLangSpec [(CommandZone (StudentZone []), "//![-")]

    , testCase "Correctly tag CommandZone (CorrectionZone)" $
        checkTagLines javaLangSpec [(CommandZone (CorrectionZone []), "//![")]

    , testCase "Correctly tag with leading spaces" $
        checkTagLines javaLangSpec [(CommandZone (CorrectionZone []), "       //![")]

    , testCase "Correctly tag CommandZone (AllZone)" $
        checkTagLines javaLangSpec [(CommandZone (AllZone []), "//!-]")]

    , testCase "Correctly tag CommandZone (AllZone)" $
        checkTagLines javaLangSpec [(CommandZone (AllZone []), "//!]")]

    , testCase "Correctly tag AllZone (as default zone)" $
        checkTagLines javaLangSpec [(AllZone [], "hello")]

    , testCase "Correctly tag StudentZone" $
        checkTagLines javaLangSpec 
            [ (CommandZone (StudentZone []), "//!-")
            , (StudentZone [], "hello student")
            ]

    , testCase "Correctly tag CorrectionZone" $
        checkTagLines javaLangSpec 
            [ (CommandZone (CorrectionZone []), "//![")
            , (CorrectionZone [], "hello teacher")
            ]

    , testCase "Correctly tag AllZone (after another zone)" $
        checkTagLines javaLangSpec 
            [ (CommandZone (StudentZone []), "   //![-")
            , (StudentZone [], "   hello student")
            , (CommandZone (AllZone []), "   //!]")
            , (AllZone [], "hello all")
            ]

    , testCase "Correctly tag StudentZone with context outside" $
        checkTagLines javaLangSpec 
            [ (ContextZone (AllZone [[T.pack "part1"]]), "//!@ part1")
            , (CommandZone (StudentZone [[T.pack "part1"]]), "//!-")
            , (StudentZone [[T.pack "part1"]], "hello student")
            ]

    , testCase "Correctly tag StudentZone with context inside" $
        checkTagLines javaLangSpec 
            [ (CommandZone (StudentZone []), "//!-")
            , (ContextZone (StudentZone [[T.pack "part1"]]), "//!@ part1")
            , (StudentZone [[T.pack "part1"]], "hello student")
            ]
    ]


javaConfig :: Bool -> Tag -> Config
javaConfig only tag = Config javaLangSpec only tag Nothing

sctTests = testGroup "Sct spec" [javaFileTests, javaFileWithContextsTests, xmlFileTests]

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

javaConfigWithContext :: Bool -> Tag -> Maybe T.Text -> Config
javaConfigWithContext = Config javaLangSpec

javaFileWithContextsTests = testGroup "for java files with contexts"
    [ testCase "Correctly transform entry when only student with no context" $
        sct (javaConfigWithContext True Student Nothing) javaEntry @?= [ "hello all {"
                , "    hello student"
                , "    goodbye all"
                , "}"
                ]

    , testCase "Correctly transform entry when only correction with no context" $
        sct (javaConfigWithContext True Correction Nothing) javaEntry @?= [ "hello all {"
                , "    hello teacher"
                , "    goodbye all"
                , "}"
                ]

    , testCase "Correctly transform entry when only student with part2 context" $
        sct (javaConfigWithContext True Student  (Just "part2")) javaEntry @?= [ "hello all {"
                , "    hello student of part2 or part3"
                , "    hello part2 or part3"
                , "    hello student"
                , "    hello student of part2"
                , "    goodbye all"
                , "}"
                ]

    , testCase "Correctly transform entry when only correction with part2 context" $
        sct (javaConfigWithContext True Correction (Just "part2")) javaEntry @?= [ "hello all {"
                , "    hello teacher of part2 or part3"
                , "    hello part2 or part3"
                , "    hello teacher"
                , "    hello teacher of part2"
                , "    goodbye all"
                , "}"
                ]
    ]
    where
        javaEntry = [ "hello all {"
                , "    //!@ part2 part3"
                , "    //!["
                , "    hello teacher of part2 or part3"
                , "    //!-"
                , "//    hello student of part2 or part3"
                , "    //!]"
                , "    hello part2 or part3"
                , "    //!@"
                , "    //!["
                , "    hello teacher"
                , "    //!@ part2"
                , "    hello teacher of part2"
                , "    //!@"
                , "    //!-"
                , "//    hello student"
                , "    //!@ part2"
                , "//    hello student of part2"
                , "    //!@"
                , "    //!]"
                , "    goodbye all"
                , "}"
                ]

xmlConfig only tag = Config xmlLangSpec only tag Nothing

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