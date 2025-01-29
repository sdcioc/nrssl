package ro.upb.nrs.sl

import scala.annotation.tailrec

/*
We consider FixedFloatingPoint a FloatingPoint
(-1)^sign * 2^exponent * ( mantissa / 2^fraction_size )
mantissa / 2^fraction_size in [1, 2) for normal floats and [0, 1) for subnormal
fraction_size exponent_size and rounding are fixed
mantissa has the hiddent bit is 1 for normal floats and 0 for subnormals
Constructor takes care of subnormal numbers
*/
abstract class FixedFloatingPoint_B extends FixedPrecisionNumberRepresentationSystem with AccumulatorTrait {
  /*
  value the Float Point
  size of size of the exponent
  size of the number
  rounding
  */
  val value : FloatingPoint_B
  val exponent_size : Int
  val fraction_size : Int
  val rounding : RoundingType
  /*
  Arithmetic operations:
  addition
  substraction
  multiplication
  division
  exact division
  modulo
  power
  negate
  inverse
  */
  /*
  addition
  */
  override def +(that: NumberRepresentationSystem): FixedFloatingPoint_B = that match {
    case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
    case that : FixedFloatingPoint_B => FixedFloatingPoint(this.value + that.value, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint +")
  }
  /*
  subtraction
  */
  override def -(that: NumberRepresentationSystem): FixedFloatingPoint_B = that match {
    case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
    case that : FixedFloatingPoint_B => FixedFloatingPoint(this.value - that.value, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint -")
  }
  /*
  multiplication
  */
  override def *(that: NumberRepresentationSystem): FixedFloatingPoint_B = that match {
    case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
    case that : FixedFloatingPoint_B => FixedFloatingPoint(this.value * that.value, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint *")
  }
  override def fusedMultiply(that : AccumulatorTrait, size : Int, fractionSize : Int) : FixedPoint_B = that match {
    case that : FixedFloatingPoint_B => this.value.fusedMultiply(that.value).toFixedPoint(size, fractionSize, NoRounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint fusedMultiply")
  }
  /*
  division
  */
  override def /(that: NumberRepresentationSystem): FixedFloatingPoint_B = that match {
    case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
    case that : FixedFloatingPoint_B => FixedFloatingPoint(this.value / that.value, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint /")
  }
  override def /\(that: NumberRepresentationSystem): FixedFloatingPoint_B = this / that
  override def %(that: NumberRepresentationSystem): FixedFloatingPoint_B = throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint %")
  /*
  X^Y = X^([Y]+{Y}) where [Y] is integer part of Y and {Y} is fractional part of Y
  X^(-Y) = 1 / X^Y
  */
  override def pow(that: NumberRepresentationSystem): FixedFloatingPoint_B = that match {
    case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
    case that : FixedFloatingPoint_B => {
      /*
      if that is negative
      X^(-Y) = 1 / X^Y
      */
      if(that.value.sign)
        this.inverse.pow(-that)
      else {
        //[Y] because Y is positive [Y] is positive
        val integer_value = that.toBigInt
        //{Y} in [0, 1)
        val fractional_value = that - FixedFloatingPoint(integer_value.toDouble, this.exponent_size , this.fraction_size, this.rounding)
        
        /*
        X^Y = X^([Y]+{Y}) = X^[Y] * X^{Y} = X^[Y] * X^( 2^exponent * ( mantissa / 2^fraction_size ) )
        mantissa / 2^fraction_size is in [1, 2) so  we can use tha format mantissa / 2^fraction_size = 1 +  fraction / 2^fraction_size
        X^Y = X^[Y] * X^( 2^exponent * ( 1 +  fraction / 2^fraction_size ) ) =
        = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        */
        //X^[Y]
        val x_integerY = mathFunctions.pow_integer(
                        FixedFloatingPoint.one(this.exponent_size , this.fraction_size, this.rounding)) (this.abs,
                        integer_value
                      )
        //X^2^exponent //exponent is negative so X^(1/2^exponent.abs)
        val x_exponentY = mathFunctions.pow_fractional(
                          FixedFloatingPoint.one(this.exponent_size , this.fraction_size, this.rounding)) (this.abs ,
                          BigInt(1), fractional_value.value.exponent.abs.toInt
                        )
        //(X^(2^exponent))^( fraction / 2^fraction_size )
        val x_exponentY_fraction = mathFunctions.pow_fractional(
                        FixedFloatingPoint.one(this.exponent_size , this.fraction_size, this.rounding)) (
                        x_exponentY,
                        fractional_value.value.mantissa.value, fractional_value.value.fraction_size
                      )
        //if fractional value is zero
        // result = X^[Y]
        // else result = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        // if the current number is negative we only do pow for integer numbers
        val result = if(fractional_value == FixedFloatingPoint.zero(false, this.exponent_size , this.fraction_size, this.rounding))
                      if(this.value.sign == false)
                        x_integerY
                      else 
                        if((integer_value % 2) ==0)
                          x_integerY
                        else
                          -x_integerY
                     else 
                      if(this.value.sign == false)
                        x_integerY * x_exponentY * x_exponentY_fraction
                      else
                        FixedFloatingPoint_NR(this.exponent_size , this.fraction_size, this.rounding)
        result match {
          case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size , this.fraction_size, this.rounding)
          case that : FixedFloatingPoint_B => that
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint pow math")
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint pow")
  }
  override def unary_- : FixedFloatingPoint_B = FixedFloatingPoint(-this.value, this.exponent_size, this.fraction_size, this.rounding)
  override def inverse : FixedFloatingPoint_B = FixedFloatingPoint.one(this.exponent_size, this.fraction_size, this.rounding) / this
  /*
  logical operation (ordering)
  less
  equal
  not equal
  greater
  greater or equal
  less or equal
  */
  override def <(that: NumberRepresentationSystem): Boolean = that match {
    case FixedFloatingPoint_NR(_, _,_) => false
    case that : FixedFloatingPoint_B => this.value < that.value
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case FixedFloatingPoint_NR(_, _,_) => false
    case that : FixedFloatingPoint_B => this.value == that.value
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint ==")
  }
  /*
  complex mathematical operations
  minimum
  maximum
  absolute value
  signum
  nth_root with rest
  nth_root - nqrt
  sqrt
  exponential
  natural logarithm
  logarithm
  */
  override def min(that: NumberRepresentationSystem): FixedFloatingPoint_B = that match {
    case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
    case that : FixedFloatingPoint_B => if (this<that) this else that
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint min")
  }
  override def max(that: NumberRepresentationSystem): FixedFloatingPoint_B = that match {
    case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
    case that : FixedFloatingPoint_B => if (this>that) this else that
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint max")
  }
  override def abs: FixedFloatingPoint_B = FixedFloatingPoint(this.value.abs,  this.exponent_size, this.fraction_size, this.rounding)
  override def signum: FixedFloatingPoint_B = FixedFloatingPoint(this.value.signum, this.exponent_size, this.fraction_size, this.rounding)
  override def nth_root (n : Int) : (FixedFloatingPoint_B, FixedFloatingPoint_B) = {
    val root = this.nqrt(n)
    (root, this-root.pow(FixedFloatingPoint(n, this.exponent_size, this.fraction_size, this.rounding)))
  }
  /*
  NQRT
  */
  override def nqrt (n : Int) : FixedFloatingPoint_B = FixedFloatingPoint(this.value.nqrt(n), this.exponent_size, this.fraction_size, this.rounding)
  override def sqrt: FixedFloatingPoint_B = this.nqrt(2)
  override def exp : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.exp)
  override def ln : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : FixedFloatingPoint_B = base match {
    case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
    case that : FixedFloatingPoint_B => this.ln / that.ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint_B log")
  }
  /*
  trigonometric functions
  sin
  cos
  tan
  cot
  sec
  csc
  */
  override def sin : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.sin)
  override def cos : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.cos)
  override def tan : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.tan)
  override def cot : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.cot)
  override def sec : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.sec)
  override def csc : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.coth)
  override def sech : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.sech)
  override def csch : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : FixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccsch)
/*
  conversion functions
  toRationalNumber
  toBigDecimal
  toBigInt
  toInt
  toLong
  toFloat
  toDouble
  */
  /*
  (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size )
  */
  override def toRationalNumber : RationalNumber_B = this.value.toRationalNumber
  override def toBigDecimal : BigDecimal = this.toRationalNumber.toBigDecimal
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = this.value.toInternalString
  override def toString: String = this.value.toString
  override def toBinaryString: String = {
    if(this.value.mantissa == NaturalNumber(0)) { //zero case +0 and -0
        (if(this.value.sign) "1" else "0") +
        auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(this.exponent_size + this.fraction_size)(false) )
    } else if(this.value.toString == "+INF") {
        (if(this.value.sign) "1" else "0") + ("1" * exponent_size) + ("1" * fraction_size)
    } else if(this.value.toString == "-INF") {
        (if(this.value.sign) "1" else "0") + ("1" * exponent_size) + ("1" * fraction_size)
    } else if(this.value.toString == "NR") {
        "1" + ("1" * exponent_size) + ("1" * fraction_size)
    } else {
        val exponentWithoutBias = this.value.exponent - FixedFloatingPoint.minimum_exponent(exponent_size)
        val binaryExponent : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_size, exponentWithoutBias.value.binaryEncode))
        val mantissaWithoutHiddenBit = this.value.mantissa.binaryEncode take fraction_size
        val binarymantissa : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(fraction_size, mantissaWithoutHiddenBit))
        val binaryValue : String = (if (this.value.sign) "1" else "0") + binaryExponent + binarymantissa
        binaryValue
    }
  }
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : FixedFloatingPoint_B = {
    val maximumExponent : Int = FixedFloatingPoint.maximum_exponent(this.exponent_size).toInt
    val precision : Int = if(maximumExponent >= 108) 30 else if (maximumExponent >= 62) 20 else if(maximumExponent >= 30) 12 else if(maximumExponent >= 16) 8 else if(maximumExponent >= 7) 5 else 2
    val result = func(
      FixedFloatingPoint.positiveZero(this.exponent_size , this.fraction_size, this.rounding),
      FixedFloatingPoint.one(this.exponent_size , this.fraction_size, this.rounding)) (this,
      FixedFloatingPoint(precision.toDouble, this.exponent_size , this.fraction_size, this.rounding))
    result match {
      case FixedFloatingPoint_NR(_, _,_) => FixedFloatingPoint_NR(this.exponent_size, this.fraction_size, this.rounding)
      case that : FixedFloatingPoint_B => that
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint helper_taylor_function")
    }
  }
}

class FixedFloatingPoint_NR(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends FixedFloatingPoint_B {
  override val value : FloatingPoint_B = FloatingPoint_NR(fraction_size_c);
  override val exponent_size : Int = exponent_size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
  override def toBinaryString: String = "1" + ("1" * exponent_size) + "1" + ("1" * (fraction_size - 1))
}

object FixedFloatingPoint_NR {
  def apply(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = new FixedFloatingPoint_NR(exponent_size_c, fraction_size_c, rounding_c)
  def unapply(input : FixedFloatingPoint_B) = if(input.toString == "NR") Some(input.exponent_size, input.fraction_size, input.rounding) else None
}

class FixedFloatingPoint(value_c : FloatingPoint_B, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends FixedFloatingPoint_B {
  override val value : FloatingPoint_B = value_c;
  override val exponent_size : Int = exponent_size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c
}


object FixedFloatingPoint {
  /*
  default sizes and rounding
  */
  var default_exponent_size : Int = 8
  var default_fraction_size : Int = 23
  var default_rounding : RoundingType = RoundEven
  /*
  set new sizes and rounding
  */
  def set_size_rounding(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) = {
    this.default_exponent_size = exponent_size_c
    this.default_rounding = rounding_c
    this.default_fraction_size = fraction_size_c
  }
  implicit def fromIntToFixedFloatingPoint(a : Int):FixedFloatingPoint_B = FixedFloatingPoint(a)
  implicit def fromLongToFixedFloatingPoint(a : Long):FixedFloatingPoint_B = FixedFloatingPoint(a)
  implicit def fromFloatToFixedFloatingPoint(a : Float):FixedFloatingPoint_B = FixedFloatingPoint(a)
  implicit def fromDoubleToFixedFloatingPoint(a : Double):FixedFloatingPoint_B = FixedFloatingPoint(a)
  

  def minimum_exponent(exponent_size_c : Int): IntegerNumber_B = {
    IntegerNumber(false, NaturalNumber(0)) - IntegerNumber(false, ( (NaturalNumber(1) << (exponent_size_c - 1)) - NaturalNumber(1) ) )
  }
  def maximum_exponent(exponent_size_c : Int): IntegerNumber_B = {
    IntegerNumber(false, (NaturalNumber(1) << exponent_size_c)-NaturalNumber(1)) - IntegerNumber(false, ( (NaturalNumber(1) << (exponent_size_c - 1)) - NaturalNumber(1) ) )
  }

  /*
  FixedFloatingPoint has normally only even rounding
  Rounding has effect on mantissa
  down we use mantissa + 1 
  ro.upb.nrs.sl.RoundUp -> if positive and (g|r|s) than mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundDown -> if negative and (g|r|s) than mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundZero -> mantissa
  ro.upb.nrs.sl.RoundAwayZero -> if (g|r|s) mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundEven -> if (g and (l|r|s) ) mantissa + 1 else mantissa
  */
  @tailrec
  def apply(value_c : FloatingPoint_B, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = {
      value_c match {
        case FloatingPoint_NR(_) => FixedFloatingPoint_NR(exponent_size_c, fraction_size_c, rounding_c)
        case FloatingPoint_INF(x, _) => new FixedFloatingPoint(FloatingPoint_INF(x, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
        case FloatingPoint(sign_c, exponent_c, mantissa_c, rest_bits_c, fraction_size_c) => {
            if(mantissa_c == NaturalNumber(0)) {
                /*
                if mantissa is 0
                if there are no rest bits than is a hard 0
                else it does rounding
                */
                val l = false
                rest_bits_c match {
                    case Nil => {
                        new FixedFloatingPoint(
                            FloatingPoint.zero(
                            sign_c,
                            fraction_size_c
                            ),
                            exponent_size_c, fraction_size_c, rounding_c)
                    }
                    case x::xs => {
                        val g = x
                        val r = xs match {case Nil => false case _ => xs.head}
                        val s = xs match {case Nil => false case y::ys => ys match {case Nil => false case _ => ys.foldLeft(false)((a,b)=>(a|b))}}
                        val subnormal_exponent = minimum_exponent(exponent_size_c) + IntegerNumber(1)
                        rounding_c match {
                            case RoundUp => if(!sign_c & (g|r|s))
                                            new FixedFloatingPoint(
                                                FloatingPoint(sign_c,
                                                subnormal_exponent,
                                                NaturalNumber(1),
                                                Nil, fraction_size_c),
                                                exponent_size_c, fraction_size_c, rounding_c)
                                            else 
                                            new FixedFloatingPoint(
                                                FloatingPoint.zero(
                                                sign_c,
                                                fraction_size_c
                                                ),
                                                exponent_size_c, fraction_size_c, rounding_c)
                            case RoundDown => if(sign_c & (g|r|s)) new FixedFloatingPoint(FloatingPoint(sign_c, subnormal_exponent, NaturalNumber(1), Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                                            else new FixedFloatingPoint(FloatingPoint.zero(sign_c, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                            case RoundZero => new FixedFloatingPoint(FloatingPoint.zero(sign_c, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                            case RoundAwayZero => if(g|r|s) 
                                            new FixedFloatingPoint(FloatingPoint(sign_c, subnormal_exponent, NaturalNumber(1), Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                                            else 
                                            new FixedFloatingPoint(FloatingPoint.zero(sign_c, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                            case RoundEven => if(g&(r|s|l)) 
                                            new FixedFloatingPoint(FloatingPoint(sign_c, subnormal_exponent, NaturalNumber(1), Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                                            else 
                                            new FixedFloatingPoint(FloatingPoint.zero(sign_c, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                            case _ => new FixedFloatingPoint(FloatingPoint.zero(sign_c, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                        }
                    }
                }
            } else {
                /*
                if the exponent is in range and we have a normal float number
                */
                if( 
                    ( exponent_c >= minimum_exponent(exponent_size_c) ) && 
                    ( exponent_c <= maximum_exponent(exponent_size_c) ) ) {
                    
                    /*
                    mantissa is in the right range because we use FloatingPoint that keeps mantissa in [1,2)
                    if there are no rest bits the exponent is for sure in right range and sizes
                    because it is bigger than minimum value and smaller than maximum value for exponent
                    mantissa is in right size because is in the right range
                    If we have rest bits than we calculate l g r s
                    and apply the rounding to mantissa
                    */
                    val l = mantissa_c.binaryEncode.head
                    rest_bits_c match {
                        case Nil => {
                              new FixedFloatingPoint(
                                FloatingPoint(
                                sign_c,
                                exponent_c,
                                mantissa_c,
                                Nil,
                                fraction_size_c
                                ),
                                exponent_size_c, fraction_size_c, rounding_c)
                        }
                        case x::xs => {
                            val g = x
                            val r = xs match {case Nil => false case _ => xs.head}
                            val s = xs match {case Nil => false case y::ys => ys match {case Nil => false case _ => ys.foldLeft(false)((a,b)=>(a|b))}}

                            rounding_c match {
                                case RoundUp => if(!sign_c & (g|r|s))
                                                    this.apply(
                                                        FloatingPoint(sign_c,
                                                        exponent_c,
                                                        mantissa_c + NaturalNumber(1),
                                                        Nil, fraction_size_c),
                                                        exponent_size_c, fraction_size_c, rounding_c)
                                                else
                                                    new FixedFloatingPoint(FloatingPoint(sign_c, exponent_c, mantissa_c, Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                                case RoundDown =>   if(sign_c & (g|r|s))
                                                        this.apply(
                                                            FloatingPoint(sign_c,
                                                            exponent_c,
                                                            mantissa_c + NaturalNumber(1),
                                                            Nil, fraction_size_c),
                                                            exponent_size_c, fraction_size_c, rounding_c)
                                                    else
                                                        new FixedFloatingPoint(FloatingPoint(sign_c, exponent_c, mantissa_c, Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                                case RoundZero =>  new FixedFloatingPoint(FloatingPoint(sign_c, exponent_c, mantissa_c, Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                                case RoundAwayZero =>   if(g|r|s) 
                                                            this.apply(
                                                                FloatingPoint(sign_c,
                                                                exponent_c,
                                                                mantissa_c + NaturalNumber(1),
                                                                Nil, fraction_size_c),
                                                                exponent_size_c, fraction_size_c, rounding_c)
                                                        else
                                                            new FixedFloatingPoint(FloatingPoint(sign_c, exponent_c, mantissa_c, Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                                case RoundEven =>   if(g&(r|s|l)) 
                                                        this.apply(
                                                            FloatingPoint(sign_c,
                                                            exponent_c,
                                                            mantissa_c + NaturalNumber(1),
                                                            Nil, fraction_size_c),
                                                            exponent_size_c, fraction_size_c, rounding_c)
                                                    else
                                                        new FixedFloatingPoint(FloatingPoint(sign_c, exponent_c, mantissa_c, Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                                case _ =>  new FixedFloatingPoint(FloatingPoint(sign_c, exponent_c, mantissa_c, Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
                            }
                        }
                    }
                } else if(exponent_c > maximum_exponent(exponent_size_c)) {
                    //if exponent is bigger than maximum value than it is a INF
                    this.infinite(sign_c, exponent_size_c, fraction_size_c, rounding_c)
                } else {
                    /*if the exponent is smaller than minimum exponent than is zero
                    */
                    this.zero(sign_c, exponent_size_c, fraction_size_c, rounding_c)
                }
            }
        }
        case _=> throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint apply")
      }
  }

  def apply(a : Double, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = {
    val bits : Long = java.lang.Double.doubleToRawLongBits(a)
    val negative : Boolean = (bits & 0x8000000000000000L) != 0
    val exponent_bits : Long = ((bits & 0x7ff0000000000000L) >> 52)
    val exponent : Long = exponent_bits - 1023
    val new_exponent = if(exponent != -1023) exponent else exponent + 1
    val mantissa : Long = if(exponent != -1023) 
                            (bits & 0x000fffffffffffffL) | 0x0010000000000000L
                          else
                            (bits & 0x000fffffffffffffL)
    if (exponent == 1024) { //INF or NAN
        if(mantissa == 0x0010000000000000L) {
            this.infinite(negative, exponent_size_c, fraction_size_c, rounding_c)
        } else {
            FixedFloatingPoint_NR(exponent_size_c, fraction_size_c, rounding_c)
        }
    } else {
        val fraction_diff : Int = 52 - fraction_size_c
        if(fraction_diff>0) {
            val rest_value = mantissa & ((BigInt(1).toLong << fraction_diff)-1)
            val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(fraction_diff, NaturalNumber(rest_value).binaryEncode).reverse
            val new_mantissa = mantissa >> fraction_diff
            this.apply(FloatingPoint(negative, IntegerNumber(new_exponent), NaturalNumber(new_mantissa), rest_bits_computed, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
        } else {
            this.apply(FloatingPoint(negative, IntegerNumber(new_exponent), NaturalNumber(mantissa) << (-fraction_diff), Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)
        }
    }
  }
  
  def apply(a : Int, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Int) : FixedFloatingPoint_B = this.apply(a.toDouble, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Long, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Long) : FixedFloatingPoint_B = this.apply(a.toDouble, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Double) : FixedFloatingPoint_B = this.apply(a, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Float, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Float) : FixedFloatingPoint_B = this.apply(a.toDouble)
  def apply(binaryString : String, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = {
    //special cases
    val positiveZeroBinaryString = auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c + 1)(false) )
    val negativeZeroBinaryString = "1" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c)(false) )
    val positiveInfiniteBinaryString = "0" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c)(true) )
    val negativeInfiniteBinaryString = "1" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c)(true) )
    if(binaryString == positiveZeroBinaryString) {
        this.positiveZero(exponent_size_c, fraction_size_c, rounding_c)
    } else if(binaryString == negativeZeroBinaryString) {
        this.negativeZero(exponent_size_c, fraction_size_c, rounding_c)
    } else if(binaryString == positiveInfiniteBinaryString) {
        this.infinite(false, exponent_size_c, fraction_size_c, rounding_c)
    } else if(binaryString == negativeInfiniteBinaryString) {
        this.infinite(true, exponent_size_c, fraction_size_c, rounding_c)
    } else {
      val exponentString :  String = "0" + ((binaryString drop 1) take exponent_size_c)
      val mantissaString : String = "0" + (binaryString drop ( 1 + exponent_size_c ))
      val binaryExponent : IntegerNumber_B = IntegerNumber(false, NaturalNumber(BigInt(exponentString, 2)))
      val binaryMantissaWihoutHiddenBit : NaturalNumber_B = NaturalNumber(BigInt(mantissaString, 2))
      val binaryMantissa : NaturalNumber_B = binaryMantissaWihoutHiddenBit + (NaturalNumber(1) << fraction_size_c)
      val real_exponent = binaryExponent + minimum_exponent(exponent_size_c)
      /*
      println("exponentString " + exponentString)
      println("mantissaString " + mantissaString)
      println("binaryExponent " + binaryExponent)
      println("binaryMantissaWihoutHiddenBit " + binaryMantissaWihoutHiddenBit)
      println("binaryMantissa " + binaryMantissa)
      println("real_exponent " + real_exponent)
      */
      new FixedFloatingPoint(FloatingPoint(binaryString(0) == '1', real_exponent, binaryMantissa, Nil, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)       
    }
  }

  def one(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = new FixedFloatingPoint(FloatingPoint.one(fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)

  def zero(sign_c : Boolean, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = new FixedFloatingPoint(FloatingPoint.zero(sign_c, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)

  def infinite(sign_c : Boolean, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = new FixedFloatingPoint(FloatingPoint.infinite(sign_c, fraction_size_c), exponent_size_c, fraction_size_c, rounding_c)

  def positiveZero(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = this.zero(false, exponent_size_c, fraction_size_c, rounding_c)

  def negativeZero(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedFloatingPoint_B = this.zero(true, exponent_size_c, fraction_size_c, rounding_c)

  def unapply(input : FixedFloatingPoint_B) = Some(input.value, input.exponent_size, input.fraction_size, input.rounding)
}

//testing binary string with Integer.toBinaryString(java.lang.Float.floatToRawIntBits(flaotvalue))