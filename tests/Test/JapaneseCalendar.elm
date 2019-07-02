module Test.JapaneseCalendar exposing (suite)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Result.Extra
import JapaneseCalendar as JC

suite : Test
suite =
    describe "The JapaneseCalendar module"
        [ describe "ymd"
            [ fuzz (intRange -9999 1867) "should return Err for too past ymd" <|
                \year ->
                    JC.ymd year 1 1
                        |> Result.Extra.isErr
                        |> Expect.true "Result is Err"
            , test "should return Err for not exist ymd" <|
                \_ ->
                    let
                        tests =
                            [ JC.ymd 1890 10 50
                            , JC.ymd -1 -1 -1
                            , JC.ymd 2019 5 0
                            , JC.ymd 2019 5 32
                            , JC.ymd 2019 6 31
                            , JC.ymd 2019 2 29
                            ]
                    in
                    tests
                        |> List.map Result.Extra.isErr
                        |> List.foldl (&&) True
                        |> Expect.true "Result is Err"
            , test "should consider leap years" <|
                \_ ->
                    let
                        tests =
                            [ { year = 1888, expect = True }
                            , { year = 1900, expect = False }
                            , { year = 2000, expect = True }
                            , { year = 2004, expect = True }
                            , { year = 2005, expect = False }
                            , { year = 2010, expect = False }
                            , { year = 2012, expect = True }
                            , { year = 2016, expect = True }
                            , { year = 2400, expect = True }
                            ]
                    in
                    tests
                        |> List.map (\test -> JC.ymd test.year 2 29)
                        |> List.map Result.Extra.isOk
                        |> Expect.equal (List.map .expect tests)

            ]
        , describe "fromYMD"
            [ test "should return correct JapaneseCalendar" <|
                \_ ->
                    let
                        tests =
                            [ { input = JC.ymd 1868  1 25, expect = "明治元年" }
                            , { input = JC.ymd 1912  7 29, expect = "明治45年" }
                            , { input = JC.ymd 1912  7 30, expect = "大正元年" }
                            , { input = JC.ymd 1926 12 24, expect = "大正15年" }
                            , { input = JC.ymd 1926 12 25, expect = "昭和元年" }
                            , { input = JC.ymd 1989  1  7, expect = "昭和64年" }
                            , { input = JC.ymd 1989  1  8, expect = "平成元年" }
                            , { input = JC.ymd 2019  4 30, expect = "平成31年" }
                            , { input = JC.ymd 2019  5  1, expect = "令和元年" }
                            , { input = JC.ymd 2019  6 29, expect = "令和元年" }
                            ]
                    in
                    tests
                        |> List.map .input
                        |> List.filterMap Result.toMaybe
                        |> List.map JC.fromYMD
                        |> List.map JC.toString
                        |> Expect.equal (List.map .expect tests)

            ]
        , describe "fromEraWithYear"
            [ test "should return correct JapaneseCalendar" <|
                \_ ->
                    let
                        tests =
                            [ { input = ("明治",  1), expect = 1868 }
                            , { input = ("明治", 10), expect = 1877 }
                            , { input = ("明治", 45), expect = 1912 }
                            , { input = ("大正",  1), expect = 1912 }
                            , { input = ("大正", 10), expect = 1921 }
                            , { input = ("大正", 15), expect = 1926 }
                            , { input = ("昭和",  1), expect = 1926 }
                            , { input = ("昭和", 10), expect = 1935 }
                            , { input = ("昭和", 64), expect = 1989 }
                            , { input = ("平成",  1), expect = 1989 }
                            , { input = ("平成", 10), expect = 1998 }
                            , { input = ("平成", 31), expect = 2019 }
                            , { input = ("令和",  1), expect = 2019 }
                            , { input = ("令和", 10), expect = 2028 }
                            ]
                    in
                    tests
                        |> List.map .input
                        |> List.map (\(eraName, year) -> JC.fromEraWithYear eraName year)
                        |> List.filterMap Result.toMaybe
                        |> List.map .gregorianYear
                        |> Expect.equal (List.map .expect tests)

            , test "should ignore boundaries" <|
                \_ ->
                    JC.fromEraWithYear "平成" 32
                        |> Result.map .gregorianYear
                        |> Expect.equal (Ok 2020)
            ]
        , describe "toString"
            [ test "should return `${eraName}${japaneseYearString}年`" <|
                \_ ->
                    let
                        tests =
                            [ { input = JC.ymd 2019  5  1, expect = "令和元年" }
                            , { input = JC.ymd 2019  4 30, expect = "平成31年" }
                            ]
                    in
                    tests
                        |> List.map .input
                        |> List.filterMap Result.toMaybe
                        |> List.map JC.fromYMD
                        |> List.map JC.toString
                        |> Expect.equal (List.map .expect tests)
            ]
        ]
