{-# LANGUAGE PatternSynonyms #-}

module Css.Easing where

import Data.Scientific

data Easing
    = Steps Int JumpTerm
    | CubicBezier Scientific Scientific Scientific Scientific

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
