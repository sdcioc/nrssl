package ro.upb.nrs.sl


/*
https://www.mathsisfun.com/numbers/rounding-methods.html

 */

trait RoundingType {
  
}

case object RoundUp extends RoundingType
case object RoundDown extends RoundingType
case object RoundEven extends RoundingType
case object RoundZero extends RoundingType
case object RoundAwayZero extends RoundingType
case object NoRounding extends RoundingType