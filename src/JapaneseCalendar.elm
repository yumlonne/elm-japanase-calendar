module JapaneseCalendar exposing (JapaneseCalendar, ymd, ymdErrors, calendar, fromEraWithYear, fromYMD, toString)

import Array
import List.Extra
import Maybe.Extra


type alias JapaneseCalendar =
    { era : Era
    , gregorianYear : Int
    , japaneseYear : Int
    , japaneseYearString : String
    }


type alias YMDRecord =
    { year : Int
    , month : Int
    , day : Int
    }


ymd : Int -> Int -> Int -> YMDRecord
ymd y m d =
    { year = y
    , month = m
    , day = d
    }


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


fromEraWithYear : String -> Int -> Result (List String) JapaneseCalendar
fromEraWithYear eraName japaneseYear =
    let
        resultEra =
            List.Extra.find (\e -> e.name == eraName) calendar
                |> Result.fromMaybe ["unknown era `" ++ eraName ++ "`"]
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


type alias Era =
    { name : String
    , startedOn : YMDRecord
    }


calendar : List Era
calendar =
    [ { name = "令和", startedOn = { year = 2019, month = 5, day = 1 } }
    , { name = "平成", startedOn = { year = 1989, month = 1, day = 8 } }
    , { name = "昭和", startedOn = { year = 1926, month = 12, day = 25 } }
    , { name = "大正", startedOn = { year = 1912, month = 7, day = 30 } }
    , { name = "明治", startedOn = { year = 1868, month = 1, day = 1 } }

    -- more?
    ]
