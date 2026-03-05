{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Data.Maybe

import Sct
import SctConfig
import SctConfigFile (decodeFileConfig)
import ArgsParsing (Flag(..))
import Sct (Zone(ContextZone))

main = defaultMain tests

tests = testGroup "Tests" [tagLinesTests, configTests, sctTests, formatterSpaceTests, fileConfigTests]

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

    , testCase "Correctly tag command with discarded text (CorrectionZone)" $
        checkTagLines javaLangSpec [(CommandZone (CorrectionZone []), "//![ some discarded text")]

    , testCase "Correctly tag command with discarded text (StudentZone [-)" $
        checkTagLines javaLangSpec [(CommandZone (StudentZone []), "//![- some discarded text")]

    , testCase "Correctly tag command with discarded text (StudentZone -)" $
        checkTagLines javaLangSpec [(CommandZone (StudentZone []), "//!- some discarded text")]

    , testCase "Correctly tag command with discarded text (AllZone ])" $
        checkTagLines javaLangSpec [(CommandZone (AllZone []), "//!] some discarded text")]

    , testCase "Correctly tag command with discarded text (AllZone -])" $
        checkTagLines javaLangSpec [(CommandZone (AllZone []), "//!-] some discarded text")]

    , testCase "Correctly tag command with discarded text (python style)" $
        checkTagLines ignoreLangSpec [(CommandZone (CorrectionZone []), "##![ teacher code below")]

    , testCase "Correctly tag set directive as CommandZone" $
        checkTagLines javaLangSpec [(CommandZone (AllZone []), "//!set formatter-space")]

    , testCase "Correctly tag set directive preserves current zone" $
        checkTagLines javaLangSpec
            [ (CommandZone (StudentZone []), "//!-")
            , (CommandZone (StudentZone []), "//!set formatter-space")
            , (StudentZone [], "//hello student")
            ]

    , testCase "Correctly tag set directive with python syntax" $
        checkTagLines ignoreLangSpec [(CommandZone (AllZone []), "##!set formatter-space")]
    ]


javaConfig :: Bool -> Tag -> Config
javaConfig only tag = Config javaLangSpec only tag Nothing False

sctTests = testGroup "Sct spec" [javaFileTests, javaFileWithContextsTests, xmlFileTests, pythonFileTests]

javaConfigFmt :: Bool -> Tag -> Config
javaConfigFmt only tag = Config javaLangSpec only tag Nothing True

pythonConfigFmt :: Bool -> Tag -> Config
pythonConfigFmt only tag = Config pythonLangSpec only tag Nothing True

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
                , "    //hello teacher"
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
    , testCase "Correctly transform entry with indented comments when student" $
        sct (javaConfig False Student) javaEntryIndented @?= [ "hello all {"
                , "    //!["
                , "    //hello teacher"
                , "    //!-"
                , "    hello student"
                , "    //!]"
                , "    goodbye all"
                , "}"
                ]

    , testCase "Correctly transform entry with indented comments when only student" $
        sct (javaConfig True Student) javaEntryIndented @?= [ "hello all {"
                , "    hello student"
                , "    goodbye all"
                , "}"
                ]

    , testCase "Correctly transform entry with indented comments when only correction" $
        sct (javaConfig True Correction) javaEntryIndented @?= [ "hello all {"
                , "    hello teacher"
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
        javaEntryIndented = [ "hello all {"
                , "    //!["
                , "    hello teacher"
                , "    //!-"
                , "    //hello student"
                , "    //!]"
                , "    goodbye all"
                , "}"
                ]

javaConfigWithContext :: Bool -> Tag -> Maybe T.Text -> Config
javaConfigWithContext only tag ctx = Config javaLangSpec only tag ctx False

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

xmlConfig only tag = Config xmlLangSpec only tag Nothing False

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
                , "    <!--<dependency>for teacher</dependency>-->"
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

pythonLangSpec = LangSpec "#" "##!" ""

pythonConfig :: Bool -> Tag -> Config
pythonConfig only tag = Config pythonLangSpec only tag Nothing False

pythonFileTests = testGroup "for python files"
    [ testCase "Correctly transform python entry with discarded text when only student" $
        sct (pythonConfig True Student) pythonEntry @?= [ "def main():"
                , "    student_code()"
                , "    common_code()"
                ]

    , testCase "Correctly transform python entry with discarded text when only correction" $
        sct (pythonConfig True Correction) pythonEntry @?= [ "def main():"
                , "    teacher_code()"
                , "    common_code()"
                ]

    , testCase "Correctly transform python entry with discarded text when student" $
        sct (pythonConfig False Student) pythonEntry @?= [ "def main():"
                , "    ##![ teacher code below"
                , "    #teacher_code()"
                , "    ##!- student code below"
                , "    student_code()"
                , "    ##!] end"
                , "    common_code()"
                ]
    ]
    where
        pythonEntry = [ "def main():"
                , "    ##![ teacher code below"
                , "    teacher_code()"
                , "    ##!- student code below"
                , "    #student_code()"
                , "    ##!] end"
                , "    common_code()"
                ]

formatterSpaceTests = testGroup "Formatter space"
    [ testGroup "via CLI flag"
        [ testCase "Strip one space after comment prefix when uncommenting (only student)" $
            sct (javaConfigFmt True Student) javaFmtEntry @?=
                [ "hello all {"
                , "    hello student"
                , "    goodbye all"
                , "}"
                ]

        , testCase "Preserve space when formatter-space is off (only student)" $
            sct (javaConfig True Student) javaFmtEntry @?=
                [ "hello all {"
                , "     hello student"
                , "    goodbye all"
                , "}"
                ]

        , testCase "Strip only one space, preserve extras (only student)" $
            sct (javaConfigFmt True Student) javaFmtEntryDoubleSpace @?=
                [ "hello all {"
                , "     hello student"
                , "    goodbye all"
                , "}"
                ]

        , testCase "No space to strip (only student)" $
            sct (javaConfigFmt True Student) javaFmtEntryNoSpace @?=
                [ "hello all {"
                , "    hello student"
                , "    goodbye all"
                , "}"
                ]

        , testCase "Python with formatter-space (only student)" $
            sct (pythonConfigFmt True Student) pythonFmtEntry @?=
                [ "def main():"
                , "    greeting = f\"Hello student, {name}!\""
                , "    common_code()"
                ]
        ]

    , testGroup "via file directive"
        [ testCase "Directive enables formatter-space (only student)" $
            sct (javaConfig True Student) javaDirectiveEntry @?=
                [ "hello all {"
                , "    hello student"
                , "    goodbye all"
                , "}"
                ]

        , testCase "Directive filtered with only (student)" $
            sct (javaConfig False Student) javaDirectiveEntry @?=
                [ "//!set formatter-space"
                , "hello all {"
                , "    //!["
                , "    //hello teacher"
                , "    //!-"
                , "    hello student"
                , "    //!]"
                , "    goodbye all"
                , "}"
                ]

        , testCase "Python directive enables formatter-space (only student)" $
            sct (pythonConfig True Student) pythonDirectiveEntry @?=
                [ "def main():"
                , "    greeting = f\"Hello student, {name}!\""
                , "    common_code()"
                ]
        ]
    ]
    where
        javaFmtEntry =
            [ "hello all {"
            , "    //!["
            , "    hello teacher"
            , "    //!-"
            , "    // hello student"
            , "    //!]"
            , "    goodbye all"
            , "}"
            ]
        javaFmtEntryDoubleSpace =
            [ "hello all {"
            , "    //!["
            , "    hello teacher"
            , "    //!-"
            , "    //  hello student"
            , "    //!]"
            , "    goodbye all"
            , "}"
            ]
        javaFmtEntryNoSpace =
            [ "hello all {"
            , "    //!["
            , "    hello teacher"
            , "    //!-"
            , "    //hello student"
            , "    //!]"
            , "    goodbye all"
            , "}"
            ]
        pythonFmtEntry =
            [ "def main():"
            , "    ##!["
            , "    greeting = f\"Hello, {name}!\""
            , "    ##!-"
            , "    # greeting = f\"Hello student, {name}!\""
            , "    ##!]"
            , "    common_code()"
            ]
        javaDirectiveEntry =
            [ "//!set formatter-space"
            , "hello all {"
            , "    //!["
            , "    hello teacher"
            , "    //!-"
            , "    // hello student"
            , "    //!]"
            , "    goodbye all"
            , "}"
            ]
        pythonDirectiveEntry =
            [ "##!set formatter-space"
            , "def main():"
            , "    ##!["
            , "    greeting = f\"Hello, {name}!\""
            , "    ##!-"
            , "    # greeting = f\"Hello student, {name}!\""
            , "    ##!]"
            , "    common_code()"
            ]

configTests = testGroup "Language specification"
    [ testCase "Correctly determine lang spec of .sctignore file" $
        divineLangSpec ".sctignore" @?= ignoreLangSpec

    , testCase "Correctly determine lang spec of .java file" $
        divineLangSpec "hello.java" @?= javaLangSpec

    , testCase "Correctly determine lang spec of .xml file" $
        divineLangSpec "hello.xml" @?= xmlLangSpec
    ]

fileConfigTests = testGroup "FileConfig"
    [ testGroup "mergeFileConfigs"
        [ testCase "Merging two empty configs gives empty" $
            mergeFileConfigs emptyFileConfig emptyFileConfig @?= emptyFileConfig

        , testCase "Right scalar overrides left" $
            let a = emptyFileConfig { fcFormatterSpace = Just True }
                b = emptyFileConfig { fcFormatterSpace = Just False }
            in fcFormatterSpace (mergeFileConfigs a b) @?= Just False

        , testCase "Left scalar preserved when right is Nothing" $
            let a = emptyFileConfig { fcOnly = Just True }
            in fcOnly (mergeFileConfigs a emptyFileConfig) @?= Just True

        , testCase "Right tag overrides left tag" $
            let a = emptyFileConfig { fcTag = Just Correction }
                b = emptyFileConfig { fcTag = Just Student }
            in fcTag (mergeFileConfigs a b) @?= Just Student

        , testCase "Right context overrides left context" $
            let a = emptyFileConfig { fcContext = Just "part1" }
                b = emptyFileConfig { fcContext = Just "part2" }
            in fcContext (mergeFileConfigs a b) @?= Just "part2"

        , testCase "Right languages override left for same extension" $
            let pySpec1 = LangSpec "#" "##!" ""
                pySpec2 = LangSpec "#" "#!" ""
                a = emptyFileConfig { fcLanguages = Map.singleton ".py" pySpec1 }
                b = emptyFileConfig { fcLanguages = Map.singleton ".py" pySpec2 }
            in Map.lookup ".py" (fcLanguages (mergeFileConfigs a b)) @?= Just pySpec2

        , testCase "Languages from both sides are merged" $
            let pySpec = LangSpec "#" "##!" ""
                jsSpec = LangSpec "//" "//!" ""
                a = emptyFileConfig { fcLanguages = Map.singleton ".py" pySpec }
                b = emptyFileConfig { fcLanguages = Map.singleton ".js" jsSpec }
                merged = mergeFileConfigs a b
            in do
                Map.lookup ".py" (fcLanguages merged) @?= Just pySpec
                Map.lookup ".js" (fcLanguages merged) @?= Just jsSpec
        ]

    , testGroup "TOML decoding"
        [ testCase "Decode empty TOML" $
            decodeFileConfig "" @?= Right emptyFileConfig

        , testCase "Decode formatter-space" $
            let toml = "formatter-space = true\n"
            in case decodeFileConfig toml of
                Right fc -> fcFormatterSpace fc @?= Just True
                Left err -> assertFailure err

        , testCase "Decode tag as student" $
            let toml = "tag = \"student\"\n"
            in case decodeFileConfig toml of
                Right fc -> fcTag fc @?= Just Student
                Left err -> assertFailure err

        , testCase "Decode tag as correction" $
            let toml = "tag = \"correction\"\n"
            in case decodeFileConfig toml of
                Right fc -> fcTag fc @?= Just Correction
                Left err -> assertFailure err

        , testCase "Decode context" $
            let toml = "context = \"part1\"\n"
            in case decodeFileConfig toml of
                Right fc -> fcContext fc @?= Just "part1"
                Left err -> assertFailure err

        , testCase "Decode language spec" $
            let toml = T.unlines
                    [ "[languages.\".jsx\"]"
                    , "comment-prefix = \"/\""
                    , "cmd-prefix = \"/!\""
                    , "comment-suffix = \"\""
                    ]
            in case decodeFileConfig toml of
                Right fc -> Map.lookup ".jsx" (fcLanguages fc)
                    @?= Just (LangSpec "/" "/!" "")
                Left err -> assertFailure err

        , testCase "Decode full config" $
            let toml = T.unlines
                    [ "formatter-space = true"
                    , "only = true"
                    , "tag = \"student\""
                    , "context = \"part2\""
                    , ""
                    , "[languages.\".rs\"]"
                    , "comment-prefix = \"//\""
                    , "cmd-prefix = \"//!\""
                    , "comment-suffix = \"\""
                    ]
            in case decodeFileConfig toml of
                Right fc -> do
                    fcFormatterSpace fc @?= Just True
                    fcOnly fc @?= Just True
                    fcTag fc @?= Just Student
                    fcContext fc @?= Just "part2"
                    Map.lookup ".rs" (fcLanguages fc)
                        @?= Just (LangSpec "//" "//!" "")
                Left err -> assertFailure err
        ]

    , testGroup "generateConfig with FileConfig"
        [ testCase "Empty FileConfig matches old behavior" $
            let config = generateConfig emptyFileConfig [] "test.java"
            in do
                langSpec config @?= javaLangSpec
                only config @?= False
                wantedTag config @?= Correction
                context config @?= Nothing
                formatterSpace config @?= False

        , testCase "FileConfig provides defaults" $
            let fc = emptyFileConfig
                    { fcFormatterSpace = Just True
                    , fcOnly = Just True
                    , fcTag = Just Student
                    , fcContext = Just "part1"
                    }
                config = generateConfig fc [] "test.java"
            in do
                formatterSpace config @?= True
                only config @?= True
                wantedTag config @?= Student
                context config @?= Just "part1"

        , testCase "CLI flags override FileConfig" $
            let fc = emptyFileConfig
                    { fcTag = Just Student
                    , fcContext = Just "part1"
                    }
                config = generateConfig fc [WantCorrection, Context "part2"] "test.java"
            in do
                wantedTag config @?= Correction
                context config @?= Just "part2"

        , testCase "FileConfig lang spec overrides hardcoded" $
            let customSpec = LangSpec "--" "--!" ""
                fc = emptyFileConfig { fcLanguages = Map.singleton ".java" customSpec }
                config = generateConfig fc [] "test.java"
            in langSpec config @?= customSpec

        , testCase "Hardcoded lang spec used when FileConfig has no override" $
            let fc = emptyFileConfig { fcLanguages = Map.singleton ".rs" (LangSpec "//" "//!" "") }
                config = generateConfig fc [] "test.java"
            in langSpec config @?= javaLangSpec

        , testCase "FileConfig lang spec for custom extension used in sct" $
            let customSpec = LangSpec ";" ";;!" ""
                fc = emptyFileConfig { fcLanguages = Map.singleton ".asm" customSpec }
                config = generateConfig fc [WantStudent, Only] "test.asm"
                input = [ "mov eax, 1"
                        , ";;!["
                        , "add eax, 2"
                        , ";;!-"
                        , ";sub eax, 3"
                        , ";;!]"
                        , "ret"
                        ]
            in sct config input @?= [ "mov eax, 1"
                                     , "sub eax, 3"
                                     , "ret"
                                     ]
        ]
    ]