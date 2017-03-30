
module ASPico.Utils where

import ClassyPrelude hiding ( (<.>) )

import Control.Lens ( Prism', prism' )
import Data.Time.Calendar.WeekDate ( toWeekDate )
import Data.Time.Format ( parseTimeM )
import Data.Time.LocalTime ( LocalTime(..), ZonedTime(..) )
import Text.EmailAddress ( EmailAddress, toByteString )


-- | Convert 'EmailAddress' to 'Text'.
emailToText :: EmailAddress -> Text
emailToText = decodeUtf8 . toByteString

-- | Double fmap.
--
-- >>> let foo = Just $ Just 3
-- >>> (+1) <$$> foo
-- Just (Just 4)
(<$$>)
    :: forall f g a b . (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
infixl 4 <$$>

-- | Convert a ZonedTime into a day of week.  1 is monday, 2 is tuesday, ...,
-- and 7 is sunday.
--
-- Here is the @parseTime@ function to help us in the tests.
--
-- >>> import Data.Time.Format
-- >>> let parse = parseTimeM True defaultTimeLocale "%FT%H:%M:%S%z"
-- >>> let parseTime = fromMaybe (error "could not parse time") . parse
--
-- >>> zonedTimeToDayOfWeek $ parseTime "2016-05-30T05:00:00+09:00"
-- 1
-- >>> zonedTimeToDayOfWeek $ parseTime "2016-05-31T05:00:00+09:00"
-- 2
-- >>> zonedTimeToDayOfWeek $ parseTime "2016-06-01T05:00:00+09:00"
-- 3
-- >>> zonedTimeToDayOfWeek $ parseTime "2016-06-02T05:00:00+09:00"
-- 4
-- >>> zonedTimeToDayOfWeek $ parseTime "2016-06-03T05:00:00+09:00"
-- 5
-- >>> zonedTimeToDayOfWeek $ parseTime "2016-06-04T05:00:00+09:00"
-- 6
-- >>> zonedTimeToDayOfWeek $ parseTime "2016-06-05T05:00:00+09:00"
-- 7
-- >>> zonedTimeToDayOfWeek $ parseTime "2016-06-06T05:00:00+09:00"
-- 1
zonedTimeToDayOfWeek :: ZonedTime -> Int
zonedTimeToDayOfWeek zonedTime =
    case toWeekDate . localDay $ zonedTimeToLocalTime zonedTime of
        (_, _, dayOfWeek) -> dayOfWeek

-- | Convert a dayOfWeek as an 'Int' to the kanji day of week.
--
-- >>> putStrLn . pack $ dayOfWeekToKanji 1
-- 月
-- >>> putStrLn . pack $ dayOfWeekToKanji 2
-- 火
-- >>> putStrLn . pack $ dayOfWeekToKanji 3
-- 水
-- >>> putStrLn . pack $ dayOfWeekToKanji 4
-- 木
-- >>> putStrLn . pack $ dayOfWeekToKanji 5
-- 金
-- >>> putStrLn . pack $ dayOfWeekToKanji 6
-- 土
-- >>> putStrLn . pack $ dayOfWeekToKanji 7
-- 日
--
-- Make sure that any other date doesn't return anything:
--
-- >>> dayOfWeekToKanji 0
-- ""
-- >>> dayOfWeekToKanji 8
-- ""
dayOfWeekToKanji :: Int -> String
dayOfWeekToKanji 1 = "月"
dayOfWeekToKanji 2 = "火"
dayOfWeekToKanji 3 = "水"
dayOfWeekToKanji 4 = "木"
dayOfWeekToKanji 5 = "金"
dayOfWeekToKanji 6 = "土"
dayOfWeekToKanji 7 = "日"
dayOfWeekToKanji _ = ""

-- | Convert a ZonedTime to a day of week in kanji.
--
-- Here is a @test@ function to actually help us test 'zonedTimeToDayOfWeekKanji'.
--
-- >>> import Data.Time.Format
-- >>> let parse = parseTimeM True defaultTimeLocale "%FT%H:%M:%S%z"
-- >>> let parseTime = fromMaybe (error "could not parse time") . parse
-- >>> let test = putStrLn . pack . zonedTimeToDayOfWeekKanji . parseTime
--
-- >>> test "2016-05-30T05:00:00+09:00"
-- 月
-- >>> test "2016-05-31T05:00:00+09:00"
-- 火
-- >>> test "2016-06-01T05:00:00+09:00"
-- 水
-- >>> test "2016-06-02T05:00:00+09:00"
-- 木
-- >>> test "2016-06-03T05:00:00+09:00"
-- 金
-- >>> test "2016-06-04T05:00:00+09:00"
-- 土
-- >>> test "2016-06-05T05:00:00+09:00"
-- 日
zonedTimeToDayOfWeekKanji :: ZonedTime -> String
zonedTimeToDayOfWeekKanji = dayOfWeekToKanji . zonedTimeToDayOfWeek

-- | ISO Time format.
isoTimeFormat :: String
isoTimeFormat = "%Y-%m-%dT%H:%M:%S%QZ"

-- | Parse a time in ISO format.
--
-- >>> parseISOTime "2016-02-10T20:30:40.1234Z"
-- Just 2016-02-10 20:30:40.1234 UTC
-- >>> parseISOTime "foobar"
-- Nothing
parseISOTime :: String -> Maybe UTCTime
parseISOTime = parseTimeM True defaultTimeLocale isoTimeFormat

formatISOTime :: UTCTime -> String
formatISOTime = formatTime defaultTimeLocale isoTimeFormat

isoTimePrism :: Prism' String UTCTime
isoTimePrism = prism' formatISOTime parseISOTime
