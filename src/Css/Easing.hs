{-# LANGUAGE PatternSynonyms #-}

module Css.Easing where

import Data.Scientific
import Data.Default(Default(def))

data Easing
    = Steps Int JumpTerm
    | CubicBezier Scientific Scientific Scientific Scientific
    deriving (Eq, Ord, Show)

data JumpTerm
    = JumpStart
    | JumpEnd
    | JumpNone
    | JumpBoth
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

pattern Start :: JumpTerm
pattern Start = JumpStart

pattern End :: JumpTerm
pattern End = JumpEnd

pattern StepStart :: Easing
pattern StepStart = Steps 1 JumpStart

pattern StepEnd :: Easing
pattern StepEnd = Steps 1 JumpEnd

pattern Ease :: Easing
pattern Ease = CubicBezier 0.25 0.1 0.25 1

pattern Linear :: Easing
pattern Linear = CubicBezier 0.0 0.0 1 1

pattern EaseIn :: Easing
pattern EaseIn = CubicBezier 0.42 0 1 1

pattern EaseOut :: Easing
pattern EaseOut = CubicBezier 0 0 0.58 1

pattern EaseInOut :: Easing
pattern EaseInOut = CubicBezier 0.42 0 0.58 1

instance Default Easing where
    def = Ease

instance Default JumpTerm where
    def = JumpNone

-- PostCSS
pattern EaseInSine :: Easing
pattern EaseInSine = CubicBezier 0.12 0 0.39 0

pattern EaseOutSine :: Easing
pattern EaseOutSine = CubicBezier 0.61 1 0.88 1

pattern EaseInOutSine :: Easing
pattern EaseInOutSine = CubicBezier 0.37 0 0.63 1

pattern EaseInQuad :: Easing
pattern EaseInQuad = CubicBezier 0.11 0 0.5 0

pattern EaseOutQuad :: Easing
pattern EaseOutQuad = CubicBezier 0.5 1 0.89 1

pattern EaseInOutQuad :: Easing
pattern EaseInOutQuad = CubicBezier 0.45 0 0.55 1

pattern EaseInCubic :: Easing
pattern EaseInCubic = CubicBezier 0.32 0 0.67 0

pattern EaseOutCubic :: Easing
pattern EaseOutCubic = CubicBezier 0.33 1 0.68 1

pattern EaseInOutCubic :: Easing
pattern EaseInOutCubic = CubicBezier 0.65 0 0.35 1

pattern EaseInQuart :: Easing
pattern EaseInQuart = CubicBezier 0.5 0 0.75 0

pattern EaseOutQuart :: Easing
pattern EaseOutQuart = CubicBezier 0.25 1 0.5 1

pattern EaseInOutQuart :: Easing
pattern EaseInOutQuart = CubicBezier 0.76 0 0.24 1

pattern EaseInQuint :: Easing
pattern EaseInQuint = CubicBezier 0.64 0 0.78 0

pattern EaseOutQuint :: Easing
pattern EaseOutQuint = CubicBezier 0.22 1 0.36 1

pattern EaseInOutQuint :: Easing
pattern EaseInOutQuint = CubicBezier 0.83 0 0.17 1

pattern EaseInExpo :: Easing
pattern EaseInExpo = CubicBezier 0.7 0 0.84 0

pattern EaseOutExpo :: Easing
pattern EaseOutExpo = CubicBezier 0.16 1 0.3 1

pattern EaseInOutExpo :: Easing
pattern EaseInOutExpo = CubicBezier 0.87 0 0.13 1

pattern EaseInCirc :: Easing
pattern EaseInCirc = CubicBezier 0.55 0 1 0.45

pattern EaseOutCirc :: Easing
pattern EaseOutCirc = CubicBezier 0 0.55 0.45 1

pattern EaseInOutCirc :: Easing
pattern EaseInOutCirc = CubicBezier 0.85 0 0.15 1

pattern EaseInBack :: Easing
pattern EaseInBack = CubicBezier 0.36 0 0.66 (-0.56)

pattern EaseOutBack :: Easing
pattern EaseOutBack = CubicBezier 0.34 1.56 0.64 1

pattern EaseInOutBack :: Easing
pattern EaseInOutBack = CubicBezier 0.68 (-0.6) 0.32 1.6
