module JapaneseCalendar exposing
    ( JapaneseCalendar, YMDRecord, Era
    , ymd
    , lastJapaneseYear
    , fromYMD, fromEraWithYear
    , calendar
    , toString
    )

{-| JapaneseCalendar for Elm


# Types

@docs JapaneseCalendar, YMDRecord, Era


# Helpers

@docs ymd
@docs lastJapaneseYear


# Converts

@docs fromYMD, fromEraWithYear


# Data set

@docs calendar


# toString

@docs toString

-}

import Array
import List.Extra
import Maybe.Extra


{-| JapaneseCalendar object
-}
type alias JapaneseCalendar =
    { era : Era
    , gregorianYear : Int
    , japaneseYear : Int
    , japaneseYearString : String
    }


{-| Simple Date object

don't validate date

-}
type alias YMDRecord =
    { year : Int
    , month : Int
    , day : Int
    }


{-| Japanese Era
-}
type alias Era =
    { name : String
    , startedOn : YMDRecord
    , endedOn : Maybe YMDRecord
    }


{-| Simple alias for YMDRecord.

    ymd 2000 10 20 == { year = 2000, month = 10, day = 20 }

-}
ymd : Int -> Int -> Int -> YMDRecord
ymd y m d =
    { year = y
    , month = m
    , day = d
    }


{-| Convert YMDRecord to JapaneseCalendar.

Returns errors or JapaneseCalendar.

    fromYMD (ymd 2019 4 20) == Ok { era = { name = "平成", ... }, ... }
    fromYMD (ymd 1700 1 10) == Err ["Too past year! `1700` < 1868"]

-}
fromYMD : YMDRecord -> Result (List String) JapaneseCalendar
fromYMD ymdRecord =
    case ymdErrors ymdRecord of
        [] ->
            let
                resultEra =
                    List.Extra.find (\e -> ymdLessThanEqual e.startedOn ymdRecord) calendar
                        |> Result.fromMaybe [ "can't find Era" ]
            in
            Result.map
                (\era ->
                    let
                        japaneseYear =
                            ymdRecord.year - era.startedOn.year + 1

                        japaneseYearString =
                            toJapaneseYearString japaneseYear
                    in
                    { era = era
                    , gregorianYear = ymdRecord.year
                    , japaneseYear = japaneseYear
                    , japaneseYearString = japaneseYearString
                    }
                )
                resultEra

        errors ->
            Err errors


{-| Convert Era and JapaneseYear to JapaneseCalendar

Returns Errors or JapaneseCalendar.

    fromEraWithYear "令和" 1 == Ok { era = { name = "令和", ... }, ... }
    fromEraWithYear "ほげ" 1 == Err ["unknown era `ほげ`"]

-}
fromEraWithYear : String -> Int -> Result (List String) JapaneseCalendar
fromEraWithYear eraName japaneseYear =
    let
        resultEra =
            List.Extra.find (\e -> e.name == eraName) calendar
                |> Result.fromMaybe [ "unknown era `" ++ eraName ++ "`" ]
    in
    Result.map
        (\era ->
            let
                gregorianYear =
                    era.startedOn.year + japaneseYear - 1

                japaneseYearString =
                    toJapaneseYearString japaneseYear
            in
            { era = era
            , gregorianYear = gregorianYear
            , japaneseYear = japaneseYear
            , japaneseYearString = japaneseYearString
            }
        )
        resultEra


{-| Get last Japanese year.

Needs currentYear for calculate current-era's last Japanese year.

    calendar
        |> List.map (\era -> { era = era.name, lastYear = lastJapaneseYear 2019 era })
        == [ { era = "令和", lastYear = 1 }
           , { era = "平成", lastYear = 31 }
           , { era = "昭和", lastYear = 64 }
           , { era = "大正", lastYear = 15 }
           , { era = "明治", lastYear = 45 }
           ]

-}
lastJapaneseYear : Int -> Era -> Int
lastJapaneseYear currentYear era =
    case era.endedOn of
        Just endedOn ->
            endedOn.year - era.startedOn.year + 1

        Nothing ->
            currentYear - era.startedOn.year + 1


{-| The list of recent eras

    calendar =
        [ { name = "令和", startedOn = ymd 2019 5 1, endedOn = Nothing }
        , { name = "平成", startedOn = ymd 1989 1 8, endedOn = Just <| ymd 2019 4 30 }
        , { name = "昭和", startedOn = ymd 1926 12 25, endedOn = Just <| ymd 1989 1 7 }
        , { name = "大正", startedOn = ymd 1912 7 30, endedOn = Just <| ymd 1926 12 24 }
        , { name = "明治", startedOn = ymd 1868 1 1, endedOn = Just <| ymd 1912 7 29 }

        -- more?
        ]

-}
calendar : List Era
calendar =
    [ { name = "令和", startedOn = ymd 2019 5 1, endedOn = Nothing }
    , { name = "平成", startedOn = ymd 1989 1 8, endedOn = Just <| ymd 2019 4 30 }
    , { name = "昭和", startedOn = ymd 1926 12 25, endedOn = Just <| ymd 1989 1 7 }
    , { name = "大正", startedOn = ymd 1912 7 30, endedOn = Just <| ymd 1926 12 24 }
    , { name = "明治", startedOn = ymd 1868 1 1, endedOn = Just <| ymd 1912 7 29 }

    -- more?
    ]


{-|

    fromEraWithYear "令和" 1
        |> Result.map toString
        == Ok "令和元年"

-}
toString : JapaneseCalendar -> String
toString jc =
    jc.era.name ++ jc.japaneseYearString ++ "年"


ymdErrors : YMDRecord -> List String
ymdErrors ymdRecord =
    let
        year =
            ymdRecord.year

        month =
            ymdRecord.month

        day =
            ymdRecord.day

        yearChecker y =
            if y >= 1868 then
                Nothing

            else
                Just <| "Too past year! `" ++ String.fromInt y ++ "` < 1868"

        monthChecker m =
            if 1 <= m && m <= 12 then
                Nothing

            else
                Just <| "Month `" ++ String.fromInt m ++ "` is out of range 1 and 12"

        isLeapYear y =
            modBy 4 y
                == 0
                && (modBy 100 y /= 0 || modBy 400 y == 0)

        dayChecker y m d =
            let
                lastDays =
                    Array.fromList [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

                maybeLastDay =
                    if isLeapYear y && m == 2 then
                        Just 29

                    else
                        Array.get (m - 1) lastDays
            in
            maybeLastDay
                |> Maybe.andThen
                    (\lastDay ->
                        if 1 <= d && d <= lastDay then
                            Nothing

                        else
                            Just <| "Day `" ++ String.fromInt d ++ "` is out of range 1 and " ++ String.fromInt lastDay
                    )

        errors y m d =
            Maybe.Extra.values [ yearChecker y, monthChecker m, dayChecker y m d ]
    in
    errors year month day


ymdLessThanEqual : YMDRecord -> YMDRecord -> Bool
ymdLessThanEqual d1 d2 =
    if d1.year /= d2.year then
        d1.year < d2.year

    else if d1.month /= d2.month then
        d1.month < d2.month

    else
        d1.day <= d2.day


toJapaneseYearString : Int -> String
toJapaneseYearString year =
    if year == 1 then
        "元"

    else
        String.fromInt year
