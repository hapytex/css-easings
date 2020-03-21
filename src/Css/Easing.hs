{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module Css.Easing (
    -- * Easing patterns
      Easing(Steps, CubicBezier)
    , steps, steps'
    , cubicBezier, cubicBezier'
    -- * Jump terms
    , JumpTerm(JumpStart, JumpEnd, JumpNone, JumpBoth)
    , pattern Start, pattern End
    -- * Standard easing aliasses
    , pattern StepStart, pattern StepEnd
    , pattern Ease, pattern Linear, pattern EaseIn, pattern EaseOut, pattern EaseInOut
    -- * PostCSS easing aliasses
    , pattern EaseInSine, pattern EaseOutSine, pattern EaseInOutSine
    , pattern EaseInQuad, pattern EaseOutQuad, pattern EaseInOutQuad
    , pattern EaseInCubic, pattern EaseOutCubic, pattern EaseInOutCubic
    , pattern EaseInQuart, pattern EaseOutQuart, pattern EaseInOutQuart
    , pattern EaseInQuint, pattern EaseOutQuint, pattern EaseInOutQuint
    , pattern EaseInExpo, pattern EaseOutExpo, pattern EaseInOutExpo
    , pattern EaseInCirc, pattern EaseOutCirc, pattern EaseInOutCirc
    , pattern EaseInBack, pattern EaseOutBack, pattern EaseInOutBack
  ) where

import Data.Aeson(Value(String), ToJSON(toJSON))
import Data.Default(Default(def))
import Data.Scientific(Scientific, scientific)
import Data.Text(Text, intercalate, pack)

import Text.Blaze(ToMarkup(toMarkup), text)
import Text.Julius(ToJavascript(toJavascript))

import Test.QuickCheck(Gen, choose, oneof)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- references:
--   https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function
--   https://easings.net/en

-- | A type that describes the different types of css-easings (also known as
-- "transition timing functions"). There are basically two modes: 'Steps' and
-- 'CubicBezier's.
data Easing
    = Steps Int JumpTerm
    -- ^ Displays the transition along n stops along the transition, displaying each stop for
    -- equal lengths of time. For example, if n is 5,  there are 5 steps. Whether the transition
    -- holds temporarily at 0%, 20%, 40%, 60% and 80%, on the 20%, 40%, 60%, 80% and 100%, or
    -- makes 5 stops between the 0% and 100% along the transition, or makes 5 stops including
    -- the 0% and 100% marks (on the 0%, 25%, 50%, 75%, and 100%) depends on which of the
    -- 'JumpTerm' is used.
    | CubicBezier Scientific Scientific Scientific Scientific
    -- ^ An author defined cubic-Bezier curve, where the p1 and p3 values must
    -- be in the range of 0 to 1.
    deriving (Eq, Ord, Show)

_easingToCss :: Easing -> Text
_easingToCss (Steps n j) = "steps(" <> pack (show n) <> ", " <> _jumpTermToCss j <> ")"
_easingToCss (CubicBezier p1 p2 p3 p4) = "cubic-bezier(" <> intercalate ", " (map (pack . show) [p1, p2, p3, p4]) <> ")"

-- | A type that is used to describe how the jumps are done in a 'Steps'
-- construction.
data JumpTerm
    = JumpStart -- ^ In css this is denoted as @jump-start@. This denotes a left-continuous function, so that the first jump happens when the transition begins.
    | JumpEnd -- ^ In css this is denoted as @jump-end@. Denotes a right-continuous function, so that the last jump happens when the animation ends.
    | JumpNone -- ^ In css this is denoted as @jump-none@. There is no jump on either end. Instead, holding at both the 0% mark and the 100% mark, each for 1/n of the duration.
    | JumpBoth -- ^ In css this is denoted as @jump-both@. Includes pauses at both the 0% and 100% marks, effectively adding a step during the transition time.
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

_jumpTermToCss :: JumpTerm -> Text
_jumpTermToCss JumpStart = "jump-start"
_jumpTermToCss JumpEnd = "jump-end"
_jumpTermToCss JumpNone = "jump-none"
_jumpTermToCss JumpBoth = "jump-both"

_validPoint :: Scientific -> Bool
_validPoint x = 0.0 <= x && x <= 1.0

-- | Constructs a 'CubicBezier' given that the first and third value are between @0.0@
-- and @1.0@. If that is the case, it returns a 'Just' that wraps the 'Easing'.
-- Otherwise 'Nothing' is returned.
cubicBezier :: Scientific -> Scientific -> Scientific -> Scientific -> Maybe Easing
cubicBezier p1 p2 p3
    | _validPoint p1 && _validPoint p2 = Just . CubicBezier p1 p2 p3
    | otherwise = const Nothing

-- | Constructs a 'CubicBezier' given the first and third value are between @0.0@
-- and @1.0@. If this is the case, it returns that 'Easing', otherwise it will
-- raise an error.
cubicBezier' :: Scientific -> Scientific -> Scientific -> Scientific -> Easing
cubicBezier' p1 p2 p3
    | _validPoint p1 && _validPoint p3 = CubicBezier p1 p2 p3
    | otherwise = error "The first and third value needs to be between 0 and 1."

-- | Constructs a 'Steps' given the first item is strictly greater than zero. If
-- that is the case, it returns the 'Easing' wrapped in a 'Just', otherwise a
-- 'Nothing' is returned.
steps :: Int -> JumpTerm -> Maybe Easing
steps n | n > 0 = Just . Steps n
        | otherwise = const Nothing

-- | Construct a 'Steps' given the first item is strictly greater than ero. If
-- that is the case, it returns the 'Easing' object, otherwise it will raise an
-- error.
steps' :: Int -> JumpTerm -> Easing
steps' n | n > 0 = Steps n
         | otherwise = error "The number of steps should be larger than 0."

-- | A pattern that defines the css alias @start@ that is equal to @jump-start@.
pattern Start :: JumpTerm
pattern Start = JumpStart

-- | A pattern that defines the css alias @end@ that is equal to @jump-end@.
pattern End :: JumpTerm
pattern End = JumpEnd

-- | A pattern that defines the css alias @step-start@ that is equal to @steps(1, jump-start)@.
pattern StepStart :: Easing
pattern StepStart = Steps 1 JumpStart

-- | A pattern that defines the css alias @step-end@ that is equal to @steps(1, jump-end)@.
pattern StepEnd :: Easing
pattern StepEnd = Steps 1 JumpEnd

-- | A pattern that defines the css alias @ease@ that is equal to @cubic-bezier(0.25, 0.1, 0.25, 1)@.
pattern Ease :: Easing
pattern Ease = CubicBezier 0.25 0.1 0.25 1

-- | A pattern that defines the css alias @linear@ that is equal to @cubic-bezier(0, 0, 1, 1)@.
pattern Linear :: Easing
pattern Linear = CubicBezier 0 0 1 1

-- | A pattern that defines the css alias @ease-in@ that is equal to @cubic-bezier(0.42, 0, 1, 1)@.
pattern EaseIn :: Easing
pattern EaseIn = CubicBezier 0.42 0 1 1

-- | A pattern that defines the css alias @ease-out@ that is equal to @cubic-bezier(0, 0, 0.58, 1)@.
pattern EaseOut :: Easing
pattern EaseOut = CubicBezier 0 0 0.58 1

-- | A pattern that defines the css alias @ease-in-out@ that is equal to @cubic-bezier(0.42, 0, 0.58, 1)@.
pattern EaseInOut :: Easing
pattern EaseInOut = CubicBezier 0.42 0 0.58 1

instance Default Easing where
    def = Ease

instance Default JumpTerm where
    def = JumpNone

-- PostCSS
-- | A pattern that defines the PostCSS easing pattern @easeInSine@.
pattern EaseInSine :: Easing
pattern EaseInSine = CubicBezier 0.12 0 0.39 0

-- | A pattern that defines the PostCSS easing pattern @easeOutSine@.
pattern EaseOutSine :: Easing
pattern EaseOutSine = CubicBezier 0.61 1 0.88 1

-- | A pattern that defines the PostCSS easing pattern @easeInOutSine@.
pattern EaseInOutSine :: Easing
pattern EaseInOutSine = CubicBezier 0.37 0 0.63 1

-- | A pattern that defines the PostCSS easing pattern @easeInQuad@.
pattern EaseInQuad :: Easing
pattern EaseInQuad = CubicBezier 0.11 0 0.5 0

-- | A pattern that defines the PostCSS easing pattern @easeOutQuad@.
pattern EaseOutQuad :: Easing
pattern EaseOutQuad = CubicBezier 0.5 1 0.89 1

-- | A pattern that defines the PostCSS easing pattern @easeInOutQuad@.
pattern EaseInOutQuad :: Easing
pattern EaseInOutQuad = CubicBezier 0.45 0 0.55 1

-- | A pattern that defines the PostCSS easing pattern @easeInCubic@.
pattern EaseInCubic :: Easing
pattern EaseInCubic = CubicBezier 0.32 0 0.67 0

-- | A pattern that defines the PostCSS easing pattern @easeOutCubic@.
pattern EaseOutCubic :: Easing
pattern EaseOutCubic = CubicBezier 0.33 1 0.68 1

-- | A pattern that defines the PostCSS easing pattern @easeInOutCubic@.
pattern EaseInOutCubic :: Easing
pattern EaseInOutCubic = CubicBezier 0.65 0 0.35 1

-- | A pattern that defines the PostCSS easing pattern @easeInQuart@.
pattern EaseInQuart :: Easing
pattern EaseInQuart = CubicBezier 0.5 0 0.75 0

-- | A pattern that defines the PostCSS easing pattern @easeOutQuart@.
pattern EaseOutQuart :: Easing
pattern EaseOutQuart = CubicBezier 0.25 1 0.5 1

-- | A pattern that defines the PostCSS easing pattern @easeInOutQuart@.
pattern EaseInOutQuart :: Easing
pattern EaseInOutQuart = CubicBezier 0.76 0 0.24 1

-- | A pattern that defines the PostCSS easing pattern @easeInQuint@.
pattern EaseInQuint :: Easing
pattern EaseInQuint = CubicBezier 0.64 0 0.78 0

-- | A pattern that defines the PostCSS easing pattern @easeOutQuint@.
pattern EaseOutQuint :: Easing
pattern EaseOutQuint = CubicBezier 0.22 1 0.36 1

-- | A pattern that defines the PostCSS easing pattern @easeInOutQuint@.
pattern EaseInOutQuint :: Easing
pattern EaseInOutQuint = CubicBezier 0.83 0 0.17 1

-- | A pattern that defines the PostCSS easing pattern @easeInExpo@.
pattern EaseInExpo :: Easing
pattern EaseInExpo = CubicBezier 0.7 0 0.84 0

-- | A pattern that defines the PostCSS easing pattern @easeOutExpo@.
pattern EaseOutExpo :: Easing
pattern EaseOutExpo = CubicBezier 0.16 1 0.3 1

-- | A pattern that defines the PostCSS easing pattern @easeInOutExpo@.
pattern EaseInOutExpo :: Easing
pattern EaseInOutExpo = CubicBezier 0.87 0 0.13 1

-- | A pattern that defines the PostCSS easing pattern @easeInCirc@.
pattern EaseInCirc :: Easing
pattern EaseInCirc = CubicBezier 0.55 0 1 0.45

-- | A pattern that defines the PostCSS easing pattern @easeOutCirc@.
pattern EaseOutCirc :: Easing
pattern EaseOutCirc = CubicBezier 0 0.55 0.45 1

-- | A pattern that defines the PostCSS easing pattern @easeInOutCirc@.
pattern EaseInOutCirc :: Easing
pattern EaseInOutCirc = CubicBezier 0.85 0 0.15 1

-- | A pattern that defines the PostCSS easing pattern @easeInBack@.
pattern EaseInBack :: Easing
pattern EaseInBack = CubicBezier 0.36 0 0.66 (-0.56)

-- | A pattern that defines the PostCSS easing pattern @easeOutBack@.
pattern EaseOutBack :: Easing
pattern EaseOutBack = CubicBezier 0.34 1.56 0.64 1

-- | A pattern that defines the PostCSS easing pattern @easeInOutBack@.
pattern EaseInOutBack :: Easing
pattern EaseInOutBack = CubicBezier 0.68 (-0.6) 0.32 1.6

_genS :: Gen Scientific
_genS = scientific <$> arbitrary <*> arbitrary

_genBoundedS :: Gen Scientific
_genBoundedS = do
    e <- fmap abs arbitrary
    (`scientific` (-e)) <$> choose (0, 10^e)


-- Arbitrary instances
instance Arbitrary Easing where
    arbitrary = oneof [Steps <$> choose (0, maxBound) <*> arbitrary, CubicBezier <$> _genBoundedS <*> _genS <*> _genBoundedS <*> _genS]

instance Arbitrary JumpTerm where
    arbitrary = arbitraryBoundedEnum

-- ToMarkup instances
instance ToMarkup Easing where
    toMarkup = text . _easingToCss

instance ToMarkup JumpTerm where
    toMarkup = text . _jumpTermToCss

-- ToJavascript instances
instance ToJavascript Easing where
    toJavascript = toJavascript . _easingToCss

instance ToJavascript JumpTerm where
    toJavascript = toJavascript . _jumpTermToCss

-- ToJSON instances
instance ToJSON Easing where
    toJSON = String . _easingToCss

instance ToJSON JumpTerm where
    toJSON = String . _jumpTermToCss
