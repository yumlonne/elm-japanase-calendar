module Test.JapaneseCalendar exposing (suite)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import JapaneseCalendar as JC

ymd : Int -> Int -> Int -> JC.YMD
ymd y m d =
    { year = y
    , month = m
    , day = d
    }

suite : Test
suite =
    describe "The JapaneseCalendar module"
        [ describe "fromYMD"
            [ test "should return correct JapaneseCalendar" <|
                \_ ->
                    let
                        tests =
                            [ { input = ymd 1868  1 25, expect = "明治元年" }
                            , { input = ymd 1912  7 29, expect = "明治45年" }
                            , { input = ymd 1912  7 30, expect = "大正元年" }
                            , { input = ymd 1926 12 24, expect = "大正15年" }
                            , { input = ymd 1926 12 25, expect = "昭和元年" }
                            , { input = ymd 1989  1  7, expect = "昭和64年" }
                            , { input = ymd 1989  1  8, expect = "平成元年" }
                            , { input = ymd 2019  4 30, expect = "平成31年" }
                            , { input = ymd 2019  5  1, expect = "令和元年" }
                            , { input = ymd 2019  6 29, expect = "令和元年" }
                            ]
                    in
                    tests
                        |> List.map .input
                        |> List.map JC.fromYMD
                        |> List.map JC.toString
                        |> Expect.equal (List.map .expect tests)

            , fuzz (intRange -9999 1867) "should return unknownEra for too past ymd" <|
                \year ->
                    JC.fromYMD (ymd year 1 1)
                        |> .era
                        |> Expect.equal JC.unknownEra
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
                        |> List.map .gregorianYear
                        |> Expect.equal (List.map .expect tests)

            , test "should ignore boundaries" <|
                \_ ->
                    JC.fromEraWithYear "平成" 32
                        |> .gregorianYear
                        |> Expect.equal 2020
            ]
        ]
