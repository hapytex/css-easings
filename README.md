# css-easings

[![Build Status of the package by Travis](https://travis-ci.com/hapytex/css-easings.svg?branch=master)](https://travis-ci.com/hapytex/css-easings)

A package that can be used to render CSS easing functions. One can use the
`Easing` type to specify how the "speed" of a certain animation should be
handled in time.

This package can be used in other items, such as objects that are then converted
to JSON to make it more safe, since it narrows the amount of possible css
strings to valid CSS easing functions.

The package documentation can be found on the [GitHub pages](https://hapytex.github.io/css-easings/).

## Easing structure

There are two types of CSS easings:

 1. A <code>steps(<i>n</i>, <i>jump-term</i>)</code> with *n* the number of
    steps, and *jump-term* the type of jump; and
 2. A <code>quadratic-bezier(<i>x<sub>1</sub></i>, <i>y<sub>1</sub></i>, <i>x<sub>2</sub></i>, <i>y<sub>2</sub></i>)</code>
    that works with a quaratic Bezier curve. Here both *x<sub>1</sub>* and *x<sub>2</sub>* need to between 0 and 1.

The type that we use for easings in this package is `Easing`.

There are four types of jumps: `jump-start`, `jump-end`, `jump-none`, and
`jump-both`. `start` and `end` are aliasses for `jump-start` and `jump-end`
respectively. The type we use in this package for these is `JumpTerm`.

Besides that, CSS has some aliasses for common easing types like `steps-start`,
`steps-end`, `ease`, `linear`, `ease-in`, `ease-out`, and `ease-in-out`.

PostCSS defines extra aliases like:

 1. `easeInSine`, `easeOutSine`, and `easeInOutSine`;
 2. `easeInQuad`, `easeOutQuad`, and `easeInOutQuad`;
 3. `easeInCubic`, `easeOutCubic`, and `easeInOutCubic`;
 4. `easeInQuant`, `easeOutQuant`, and `easeInOutQuant`;
 5. `easeInQuint`, `easeOutQuint`, and `easeInOutQuint`;
 6. `easeInExpo`, `easeOutExpo`, and `easeInOutExpo`;
 7. `easeInCirc`, `easeOutCirc`, and `easeInOutCirc`; and
 8. `easeInBack`, `easeOutBack`, and `easeInOutBack`.

These are also included as patterns in this project. When these are rendered,
for example as a JSON string, it will use the CSS equivalent.

## `ToMarkup`, `ToJSON`, and `ToJavascript` instances

Both the `JumpTerm` and `Easing` type are members of the `ToMarkup`, `ToJSON`
and `ToJavascript` type classes to convert the easings to their text equivalent.

For the `ToMarkup`, the CSS counterparts are just written to the markup stream,
and thus *not* wrapped in a string literal. Since the possible easings do not
contain any characters that need to be escaped, there is no problem with
escaping.

For both `ToJavascript` and `ToJSON` instances, the values are wrapped in a JSON
string, since in many Javascript libraries, one uses strings to update easings.

## `Arbitrary` css easings

One can generate arbitrary `Easing`s and `JumpTerm`s. It is however not advisable to
use this for anything other than for validation purposes (like with `QuickCheck`).

## `css-easings` is not *safe* Haskell

There are not extensions that are used that make the library *itself*
unsafe, but it makes use of `aeson`, `blaze-markup`, etc. and the packages are
not safe. Hence this package is not *safe Haskell*.

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/css-easings).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

---

This package is dedicated to *Nordin Allaert* (2001-2020). His life went in an `ease-in` manner, but was taken too soon.
