package ro.upb.nrs.sl




import scala.annotation.tailrec

/*
We consider Morris
a TaperedFloatingPoint
(-1)^sign * 2^exponent * ( mantissa / 2^fraction_size )
mantissa / 2^fraction_size in [1, 2)
mantissa has the hiddent bit that is always 1 except zero value
size, g_size and rounding are fixed
*/
abstract class Morris_B extends FixedPrecisionNumberRepresentationSystem with AccumulatorTrait {
  /*
  value the Tapered Float Point
  size of size of the exponent
  size of the number
  rounding
  */
  val value : TaperedFloatingPoint_B
  val g_size : Int
  val size : Int
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
  override def +(that: NumberRepresentationSystem): Morris_B = that match {
    case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
    case Morris(that_value, that_g_size, that_size, that_rounding) => Morris(this.value + that_value, this.g_size, this.size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris +")
  }
  /*
  subtraction
  */
  override def -(that: NumberRepresentationSystem): Morris_B = that match {
    case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
    case Morris(that_value, that_g_size, that_size, that_rounding) => Morris(this.value - that_value, this.g_size, this.size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris -")
  }
  /*
  multiplication
  */
  override def *(that: NumberRepresentationSystem): Morris_B = that match {
    case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
    case Morris(that_value, that_g_size, that_size, that_rounding) => Morris(this.value * that_value, this.g_size, this.size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris *")
  }
  override def fusedMultiply(that : AccumulatorTrait, size : Int, fractionSize : Int) : FixedPoint_B = that match {
    case that : Morris_B => (this.value * that.value).toFixedPoint(size, fractionSize, NoRounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedFloatingPoint fusedMultiply")
  }
  /*
  division
  */
  override def /(that: NumberRepresentationSystem): Morris_B = that match {
    case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
    case Morris(that_value, that_g_size, that_size, that_rounding) => Morris(this.value / that_value, this.g_size, this.size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris /")
  }
  override def /\(that: NumberRepresentationSystem): Morris_B = this / that
  override def %(that: NumberRepresentationSystem): Morris_B = throw new NotImplementedError("ro.upb.nrs.sl.Morris %")
  /*
  X^Y = X^([Y]+{Y}) where [Y] is integer part of Y and {Y} is fractional part of Y
  X^(-Y) = 1 / X^Y
  */
  override def pow(that: NumberRepresentationSystem): Morris_B = that match {
    case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
    case Morris(that_value, that_g_size, that_size, that_rounding) => {
      /*
      if that is negative
      X^(-Y) = 1 / X^Y
      */
      if(that_value.sign)
        this.inverse.pow(-that)
      else {
        //[Y] because Y is positive [Y] is positive
        val integer_value = that.toBigInt
        //{Y} in [0, 1)
        val fractional_value = Morris( 
          TaperedFloatingPoint(that_value.sign, that_value.exponent, that_value.mantissa, that_value.fraction_size, Nil, this.size),
          this.g_size, this.size, this.rounding) - Morris(integer_value.toDouble, this.g_size , this.size, this.rounding)
        
        /*
        X^Y = X^([Y]+{Y}) = X^[Y] * X^{Y} = X^[Y] * X^( 2^exponent * ( mantissa / 2^fraction_size ) )
        mantissa / 2^fraction_size is in [1, 2) so  we can use tha format mantissa / 2^fraction_size = 1 +  fraction / 2^fraction_size
        X^Y = X^[Y] * X^( 2^exponent * ( 1 +  fraction / 2^fraction_size ) ) =
        = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        */
        //X^[Y]
        val x_integerY = mathFunctions.pow_integer(
                        Morris(1.0d, this.g_size , this.size, this.rounding)) (this.abs,
                        integer_value
                      )
        //X^2^exponent //exponent is negative so X^(1/2^exponent.abs)
        val x_exponentY = mathFunctions.pow_fractional(
                          Morris(1.0d, this.g_size , this.size, this.rounding)) (this.abs ,
                          BigInt(1), fractional_value.value.exponent.abs.toInt
                        )
        //(X^(2^exponent))^( fraction / 2^fraction_size )
        val x_exponentY_fraction = mathFunctions.pow_fractional(
                        Morris(1.0d, this.g_size , this.size, this.rounding)) (
                        x_exponentY,
                        fractional_value.value.mantissa.value, fractional_value.value.fraction_size
                      )
        //if fractional value is zero
        // result = X^[Y]
        // else result = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        // if the current number is negative we only do pow for integer numbers
        val result = if(fractional_value == Morris(0.0d, this.g_size , this.size, this.rounding))
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
                        Morris_NR(this.g_size , this.size, this.rounding)
        result match {
          case Morris_NR(_, _,_) => Morris_NR(this.g_size , this.size, this.rounding)
          case Morris(that_value, that_g_size , that_size, that_round) => Morris( 
                      TaperedFloatingPoint(that_value.sign, that_value.exponent, that_value.mantissa, that_value.fraction_size, Nil, this.size),
                      this.g_size, this.size, this.rounding)
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris pow math")
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris pow")
  }
  override def unary_- : Morris_B = Morris(-this.value, this.g_size, this.size, this.rounding)
  override def inverse : Morris_B = Morris(1.0d, this.g_size, this.size, this.rounding) / this
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
    case Morris_NR(_, _,_) => false
    /*
    case Morris(that_value, that_g_size, that_size, that_rounding) => {
      if(this.value.sign != that_value.sign) this.value.sign
      else if(this.value.exponent<that_value.exponent) !this.value.sign
      else if(this.value.exponent>that_value.exponent) this.value.sign
      else if(this.value.mantissa<that_value.mantissa) !this.value.sign
      else if(this.value.mantissa>that_value.mantissa) this.value.sign
      else false
    }
    */
    case Morris(that_value, that_g_size, that_size, that_rounding) => this.value < that_value
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case Morris_NR(_, _,_) => false
    //case Morris(that_value, that_g_size, that_size, that_rounding) => (this.value.sign == that_value.sign) && (this.value.exponent == that_value.exponent) && (this.value.mantissa == that_value.mantissa)
    case Morris(that_value, that_g_size, that_size, that_rounding) => this.value == that_value
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris ==")
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
  override def min(that: NumberRepresentationSystem): Morris_B = that match {
    case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
    case Morris(that_value, that_g_size, that_size, that_rounding) => if (this<that) this else Morris(that_value, this.g_size, this.size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris min")
  }
  override def max(that: NumberRepresentationSystem): Morris_B = that match {
    case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
    case Morris(that_value, that_g_size, that_size, that_rounding) => if (this>that) this else Morris(that_value, this.g_size, this.size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris max")
  }
  override def abs: Morris_B = Morris(this.value.abs,  this.g_size, this.size, this.rounding)
  override def signum: Morris_B = Morris(this.value.signum, this.g_size, this.size, this.rounding)
  override def nth_root (n : Int) : (Morris_B, Morris_B) = {
    val root = this.nqrt(n)
    (root, this-root.pow(Morris(n, this.g_size, this.size, this.rounding)))
  }
  /*
  NQRT
  */
  override def nqrt (n : Int) : Morris_B = Morris(this.value.nqrt(n), this.g_size, this.size, this.rounding)
  override def sqrt: Morris_B = this.nqrt(2)
  override def exp : Morris_B = helper_taylor_function(mathFunctions.exp)
  override def ln : Morris_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : Morris_B = base match {
    case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
    case Morris(that_value, that_g_size , that_size, that_round) => this.ln / Morris(that_value, that_g_size , that_size, that_round).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris_B log")
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
  override def sin : Morris_B = helper_taylor_function(mathFunctions.sin)
  override def cos : Morris_B = helper_taylor_function(mathFunctions.cos)
  override def tan : Morris_B = helper_taylor_function(mathFunctions.tan)
  override def cot : Morris_B = helper_taylor_function(mathFunctions.cot)
  override def sec : Morris_B = helper_taylor_function(mathFunctions.sec)
  override def csc : Morris_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : Morris_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : Morris_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : Morris_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : Morris_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : Morris_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : Morris_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : Morris_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : Morris_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : Morris_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : Morris_B = helper_taylor_function(mathFunctions.coth)
  override def sech : Morris_B = helper_taylor_function(mathFunctions.sech)
  override def csch : Morris_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : Morris_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : Morris_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : Morris_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : Morris_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : Morris_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : Morris_B = helper_taylor_function(mathFunctions.arccsch)
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
  override def toBinaryString: String = {
    if(this.value.mantissa == NaturalNumber(0)) { //zero case
      auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(this.size)(false) )
    } else {
      val G = this.value.exponent.value.binaryEncode.length - 1
      val binaryG : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(this.g_size, NaturalNumber(G).binaryEncode))
      val binaryExponent : String = (if(this.value.exponent.sign) "1" else "0") + auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(G + 1, this.value.exponent.value.binaryEncode))
      val mantissaWithoutHiddenBit = this.value.mantissa.binaryEncode take this.value.fraction_size
      val binarymantissa : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(this.value.fraction_size, mantissaWithoutHiddenBit))
      val binaryAbsolute : String = (binaryG + binaryExponent + binarymantissa) take (this.size - 1)
      val finalBinary : String = (if(this.value.sign) "1" else "0") + binaryAbsolute
      finalBinary
    }
  }
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : Morris_B = {
    val maximumExponent : Int = Morris.maximum_exponent(this.g_size, size).toInt
    val precision : Int = if(maximumExponent >= 108) 30 else if (maximumExponent >= 62) 20 else if(maximumExponent >= 30) 12 else if(maximumExponent >= 16) 8 else if(maximumExponent >= 7) 5 else 2
    val result = func(
      Morris(0.0d, this.g_size, size, rounding),
      Morris(1.0d, this.g_size, size, rounding)) (this,
      Morris(precision.toDouble, this.g_size, size, rounding))
    result match {
      case Morris_NR(_, _,_) => Morris_NR(g_size, size, rounding)
      case Morris(that_value, that_g_size , that_size, that_round) => Morris(that_value, that_g_size , that_size, that_round)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.Morris helper_taylor_function")
    }
  }
}

class Morris_NR(g_size_c : Int, size_c : Int, rounding_c : RoundingType) extends Morris_B {
  override val value : TaperedFloatingPoint_B = TaperedFloatingPoint_NR(size_c);
  override val g_size : Int = g_size_c
  override val size : Int = size_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
  override def toBinaryString: String = "1" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(this.size-1)(false) )
}

object Morris_NR {
  def apply(g_size_c : Int, size_c : Int, rounding_c : RoundingType) : Morris_B = new Morris_NR(g_size_c, size_c, rounding_c)
  def unapply(input : Morris_B) = if(input.value.mantissa.equals(NaturalNumber_NR)) Some(input.g_size, input.size, input.rounding) else None
}

class Morris(value_c : TaperedFloatingPoint_B, g_size_c : Int, size_c : Int, rounding_c : RoundingType) extends Morris_B {
  override val value : TaperedFloatingPoint_B = value_c;
  override val g_size : Int = g_size_c
  override val size : Int = size_c
  override val rounding : RoundingType = rounding_c
}


object Morris {
  /*
  default sizes and rounding
  */
  var default_g_size : Int = 4
  var default_size : Int = 32
  var default_rounding : RoundingType = RoundEven
  /*
  set new sizes and rounding
  */
  def set_size_rounding(g_size_c : Int, size_c : Int, rounding_c : RoundingType) = {
    this.default_g_size = g_size_c
    this.default_size = size_c
    this.default_rounding = rounding_c
  }
  implicit def fromIntToMorris(a : Int):Morris_B = Morris(a)
  implicit def fromLongToMorris(a : Long):Morris_B = Morris(a)
  implicit def fromFloatToMorris(a : Float):Morris_B = Morris(a)
  implicit def fromDoubleToMorris(a : Double):Morris_B = Morris(a)
  

  def minimum_exponent(g_size_c : Int, size_c : Int): IntegerNumber_B = {
    -this.maximum_exponent(g_size_c, size_c)
  }
  def maximum_exponent(g_size_c : Int, size_c : Int): IntegerNumber_B = {
    val max_exponent_size = 1 << g_size_c // G=g_value+1 , max g_value = 2^g_size - 1
    val remaining_bits = size_c - 1 - g_size_c - 1 // fraction sign, g_size and exponent sign
    if(max_exponent_size <= remaining_bits) {
      IntegerNumber(false, (NaturalNumber(1) << max_exponent_size) - NaturalNumber(1))
    } else {
      IntegerNumber(false, ((NaturalNumber(1) << remaining_bits) - NaturalNumber(1)) << (max_exponent_size - remaining_bits) )
    }
  }

  /*
  Morris has normally only even rounding
  Rounding has effect on mantissa or exponent depending on case
  down we use mantissa + 1 but but on the case can be exponent 1,2,...,2^exponent_size
  ro.upb.nrs.sl.RoundUp -> if positive and (g|r|s) than mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundDown -> if negative and (g|r|s) than mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundZero -> mantissa
  ro.upb.nrs.sl.RoundAwayZero -> if (g|r|s) mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundEven -> if (g and (l|r|s) ) mantissa + 1 else mantissa
  */
  @tailrec
  def apply(value_c : TaperedFloatingPoint_B, g_size_c : Int, size_c : Int, rounding_c : RoundingType) : Morris_B = {
      value_c match {
        case TaperedFloatingPoint_NR(_) => Morris_NR(g_size_c, size_c, rounding_c)
        case TaperedFloatingPoint(sign_c, exponent_c, mantissa_c, fraction_size_c, rest_bits_c, _) => {
          if(mantissa_c == NaturalNumber(0)) {
                                        /*
                                        if mantissa is 0
                                        if there are no rest bits than is a hard 0
                                        else it does rounding
                                        */
                                        val l = false
                                        rest_bits_c match {
                                            case Nil => {
                                                new Morris(
                                                  TaperedFloatingPoint(
                                                    false,
                                                    this.minimum_exponent(g_size_c, size_c),
                                                    NaturalNumber(0),
                                                    0,
                                                    Nil,
                                                    size_c
                                                  ),
                                                  g_size_c, size_c, rounding_c)
                                            }
                                            case x::xs => {
                                              val g = x
                                              val r = xs match {case Nil => false case _ => xs.head}
                                              val s = xs match {case Nil => false case y::ys => ys match {case Nil => false case _ => ys.foldLeft(false)((a,b)=>(a|b))}}
                                              //TODO: maybe hard 0
                                              rounding_c match {
                                                  case RoundUp => if(!sign_c & (g|r|s))
                                                                    this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c + NaturalNumber(1),fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                  else 
                                                                    this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c, fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                  case RoundDown => if(sign_c & (g|r|s)) 
                                                                    this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c + NaturalNumber(1),fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                  else 
                                                                    this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c, fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                  case RoundZero => this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c, fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                  case RoundAwayZero => if(g|r|s) 
                                                                    this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c + NaturalNumber(1),fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                  else 
                                                                    this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c, fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                  case RoundEven => if(g&(r|s|l)) 
                                                                    this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c + NaturalNumber(1),fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                  else 
                                                                    this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c, fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                                  case _ => this.apply(TaperedFloatingPoint(sign_c, minimum_exponent(g_size_c, size_c), mantissa_c, fraction_size_c, Nil, size_c), g_size_c, size_c, rounding_c)
                                              }
                                            }
                                        }
                                    } else
                                         {
                                            /*if mantissa is in [1,2)
                                            if the exponent is smaller than minimum value
                                            Morris will underflow to zero
                                            */
                                            if(exponent_c < minimum_exponent(g_size_c, size_c))
                                                new Morris(
                                                  TaperedFloatingPoint(
                                                    false,
                                                    this.minimum_exponent(g_size_c, size_c),
                                                    NaturalNumber(0),
                                                    0,
                                                    Nil,
                                                    size_c
                                                  ),
                                                  g_size_c, size_c, rounding_c)
                                            /*
                                            if the exponent is bigger than maximum value
                                            Morris will return NR
                                            */
                                            else if (exponent_c > maximum_exponent(g_size_c, size_c))
                                                new Morris_NR(g_size_c, size_c, rounding_c)
                                            else {
                                                /*
                                                if exponent and mantissa are in right ranges than we
                                                calculate exponent_size and real size for exponent and fraction
                                                */
                                                // g_size set
                                                // G = exponent.bitlength - 1
                                                // e = exponent value
                                                // f = fraction value
                                                // fs = fraction size
                                                // res = real exponent size
                                                // rfs = real fraction size
                                                // G = exponent.bitlength - 1
                                                val G = exponent_c.value.binaryEncode.length - 1
                                                // res = (size - 1 - g_size - 1, max_exponent_size)
                                                // max_exponent_size=G+1
                                                // size - 1(sign) - g_size - 1 (exponent_sign)
                                                val real_exponent_size : Int = if( (size_c - 1 - g_size_c - 1) > 0 )
                                                                                  if((size_c - 1 - g_size_c - 1) >= G + 1)
                                                                                    G + 1
                                                                                  else size_c - 1 - g_size_c - 1
                                                                                else 0
                                                /*
                                                rfs =  = max(0, size - 1 - g_size - 1 - G - 1) // -(G+1) = -max_exponent_size
                                                */
                                                val real_fraction_size : Int = if( (size_c - 1 - g_size_c - 1 - G - 1) > 0 )
                                                                                (size_c - 1 - g_size_c - 1 - G - 1)
                                                                               else 0
                                                /*
                                                ediff = es - res
                                                */
                                                val exponent_diff : Int = G + 1 - real_exponent_size
                                                // e = exponent 
                                                val exponent : IntegerNumber_B = exponent_c
                                                /*
                                                If the difference between (real) representable exponent size and exponent size is
                                                bigger or equal with 2 it means that g and r are taken from  the first 2 exponent bits that
                                                will not be reprezentable
                                                */
                                                if(exponent_diff>=1) {
                                                      // take g from exponent bits 
                                                      /*
                                                      If the difference between (real) representable exponent size and exponent size is
                                                      1 it means that g is taken from the exponent bit that
                                                      will not be representable
                                                      */
                                                      val g : Boolean = (exponent.value >> (exponent_diff-1)).value.testBit(0)
                                                      //r from exponent bits
                                                      val r : Boolean = if(exponent_diff > 1) {
                                                        (exponent.value >> (exponent_diff-2)).value.testBit(0)
                                                      } else {
                                                        // r first mantissa bit  if we have fraction size or first rest bit
                                                        // if we do not have mantissa bits
                                                        if(fraction_size_c >= 1)
                                                          ( ( mantissa_c.value & ( 1 << (fraction_size_c - 1) ) ) != 0 )
                                                        else if(rest_bits_c != Nil)
                                                          rest_bits_c.head
                                                        else false
                                                      }
                                                      // calcualte s from the other exponent bits, mantissa bits and any other rest bits
                                                      val s : Boolean = if(exponent_diff > 1) {
                                                        ((exponent.value.value & ((1<<(exponent_diff-2))-1)) != 0) || (mantissa_c != NaturalNumber(1<<fraction_size_c)) || rest_bits_c.foldLeft(false)((a,b)=>(a|b))
                                                      } else {
                                                        // s are all the remaining mantissa bits execpt first one and all the rest bits
                                                        // or if we do not have mantissa bits is the rest bits except the first one
                                                        if(fraction_size_c >= 1) 
                                                          ( ( mantissa_c.value & ( ( 1 << (fraction_size_c - 1) ) - 1 ) ) != 0 ) || rest_bits_c.foldLeft(false)( (a, b) => (a | b) )
                                                        else if(rest_bits_c != Nil)
                                                          rest_bits_c.tail.foldLeft(false)( (a, b) => (a | b) )
                                                        else
                                                          false
                                                      }
                                                    // because the entire exponent can not be representable will calculate the most fit representable exponent
                                                    val new_exponent = IntegerNumber(exponent.sign, ( (exponent.value >> exponent_diff) << exponent_diff ) )
                                                    /*
                                                     if real_exponent_size is 0 than it is a wrong g_size value
                                                    */
                                                    val l : Boolean = if(real_exponent_size == 0) 
                                                                        throw new NotImplementedError("ro.upb.nrs.sl.Morris apply g_size high value g_size < size-3")
                                                                      else
                                                                        (exponent.value >> exponent_diff).value.testBit(0)
                                                    // if we do not have any rounding bits
                                                    if(!g && !r && !s)
                                                      new Morris(
                                                        TaperedFloatingPoint(
                                                          sign_c,
                                                          new_exponent,
                                                          NaturalNumber(1),
                                                          0,
                                                          Nil,
                                                          size_c
                                                        ),
                                                        g_size_c, size_c, rounding_c)
                                                    else 
                                                        rounding_c match { //real_fraction_size rfs for sure zero
                                                            case RoundUp => if(!sign_c & (g|r|s))
                                                                              this.apply(TaperedFloatingPoint(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                            else
                                                                              this.apply(TaperedFloatingPoint(sign_c, new_exponent, NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                            case RoundDown => if(sign_c & (g|r|s)) 
                                                                              this.apply(TaperedFloatingPoint(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                            else
                                                                              this.apply(TaperedFloatingPoint(sign_c, new_exponent, NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                            case RoundZero => this.apply(TaperedFloatingPoint(sign_c, new_exponent, NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                            case RoundAwayZero => if(g|r|s) 
                                                                              this.apply(TaperedFloatingPoint(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                            else
                                                                              this.apply(TaperedFloatingPoint(sign_c, new_exponent, NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                            case RoundEven => if(g&(r|s|l)) 
                                                                              this.apply(TaperedFloatingPoint(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                            else
                                                                              this.apply(TaperedFloatingPoint(sign_c, new_exponent, NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                            case _ => this.apply(TaperedFloatingPoint(sign_c, new_exponent, NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                        }
                                                } else {
                                                    /*
                                                    all the exponent bits are representable so the exponent has the exponent value
                                                    we have to calculate ho many bits of fraction are representable
                                                    fraction_diff = fs - rfs (fraction bits that are not representable)
                                                    */
                                                    val fraction_diff = fraction_size_c - real_fraction_size
                                                    if(fraction_diff < 0) {
                                                      val rest_bits_mantissa =  auxiliaryFunctions.BinaryEncodeFixedWidth(-fraction_diff, rest_bits_c.take(-fraction_diff)).reverse
                                                      val add_mantissa_binaryString = auxiliaryFunctions.BinaryEncodetoBinaryString(rest_bits_mantissa)
                                                      val add_mantissa_value = NaturalNumber(BigInt(add_mantissa_binaryString, 2))
                                                      this.apply(
                                                        TaperedFloatingPoint(
                                                          sign_c,
                                                          exponent_c,
                                                          ( mantissa_c << (-fraction_diff) ) + add_mantissa_value, // take the necesary bits for mantissa from rest bits
                                                          real_fraction_size,
                                                          rest_bits_c drop (-fraction_diff),
                                                          size_c
                                                        ), g_size_c, size_c, rounding_c)
                                                    } else { 
                                                      /*
                                                      calculate g r s bits
                                                      g is the first not reprsentable mantissa bit or if all bits are repsentable the first rest bit
                                                      r is next bit of not reprsentable mantissa or from rest bit
                                                      */
                                                      val g : Boolean = if(fraction_diff>=1)
                                                                          ((mantissa_c.value & (1<<(fraction_diff-1))) != 0)
                                                                        else if(rest_bits_c != Nil)
                                                                          rest_bits_c.head
                                                                        else false
                                                      val r : Boolean = if(fraction_diff>=2)
                                                                          ((mantissa_c.value & (1<<(fraction_diff-2))) != 0)
                                                                        else if(fraction_diff==1) {
                                                                          if(rest_bits_c != Nil)
                                                                            rest_bits_c.head
                                                                          else false
                                                                        } else 
                                                                          if(rest_bits_c != Nil)
                                                                            if(rest_bits_c.tail != Nil)
                                                                              rest_bits_c.tail.head
                                                                            else false
                                                                          else false
                                                      val s : Boolean = if(fraction_diff>=2) 
                                                                          ( ( mantissa_c.value & ( (1 << ( fraction_diff - 1 ) ) - 1 ) ) != 0) || rest_bits_c.foldLeft(false)( (a, b)=>(a | b) )
                                                                        else if(fraction_diff==1) {
                                                                          if(rest_bits_c != Nil)
                                                                            rest_bits_c.tail.foldLeft(false)( (a, b)=>(a | b) )
                                                                          else
                                                                            false
                                                                        } else
                                                                          if(rest_bits_c != Nil)
                                                                            if(rest_bits_c.tail != Nil)
                                                                              rest_bits_c.tail.tail.foldLeft(false)( (a, b) => (a | b) )
                                                                            else false
                                                                          else false
                                                      // reprsentable mantissa
                                                      val mantissa : NaturalNumber_B = mantissa_c >> fraction_diff
                                                      // calculate l
                                                      // if we do not have mantissa we try exponent and fi wwe do not have
                                                      // exponent we try regime
                                                      val l : Boolean = if(real_fraction_size == 0) 
                                                                          if(real_exponent_size == 0)
                                                                            throw new NotImplementedError("ro.upb.nrs.sl.Morris apply g_size high value g_size < size-3")
                                                                          else exponent.value.value.testBit(0)
                                                                        else mantissa.value.testBit(0)
                                                      if(!g && !r && !s)
                                                        new Morris(
                                                          TaperedFloatingPoint(
                                                            sign_c,
                                                            exponent_c,
                                                            mantissa,
                                                            real_fraction_size,
                                                            Nil,
                                                            size_c
                                                          ),
                                                          g_size_c, size_c, rounding_c)
                                                      else 
                                                          rounding_c match {
                                                              case RoundUp => if(!sign_c & (g|r|s))
                                                                              this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa + NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                            else
                                                                              this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa, real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                              case RoundDown => if(sign_c & (g|r|s)) 
                                                                              this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa + NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                            else
                                                                              this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa, real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                              case RoundZero => this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa, real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                              case RoundAwayZero => if(g|r|s) 
                                                                              this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa + NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                            else
                                                                              this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa, real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                              case RoundEven => if(g&(r|s|l)) 
                                                                              this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa + NaturalNumber(1), real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                                            else
                                                                              this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa, real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                              case _ => this.apply(TaperedFloatingPoint(sign_c, exponent_c, mantissa, real_fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
                                                          }

                                                    }
                                                }
                                            }
                                        }
        }
        case _=> throw new NotImplementedError("ro.upb.nrs.sl.Morris apply")
      }
  }

  def apply(a : Double, g_size_c : Int, size_c : Int, rounding_c : RoundingType) : Morris_B = {
    val bits : Long = java.lang.Double.doubleToRawLongBits(a)
    val negative : Boolean = (bits & 0x8000000000000000L) != 0
    val exponent_bits : Long = ((bits & 0x7ff0000000000000L) >> 52)
    val exponent : Long = exponent_bits - 1023
    val mantissa : Long = if(exponent != -1023) 
                            (bits & 0x000fffffffffffffL) | 0x0010000000000000L
                          else
                            (bits & 0x000fffffffffffffL)
    this.apply(TaperedFloatingPoint(negative, IntegerNumber(exponent), NaturalNumber(mantissa), 52, Nil, size_c), g_size_c, size_c, rounding_c)
  }
  
  def apply(a : Int, g_size_c : Int, size_c : Int, rounding_c : RoundingType) : Morris_B = this.apply(a.toDouble, g_size_c, size_c, rounding_c)
  def apply(a : Int) : Morris_B = this.apply(a.toDouble, this.default_g_size, this.default_size, this.default_rounding)
  def apply(a : Long, g_size_c : Int, size_c : Int, rounding_c : RoundingType) : Morris_B = this.apply(a.toDouble, g_size_c, size_c, rounding_c)
  def apply(a : Long) : Morris_B = this.apply(a.toDouble, this.default_g_size, this.default_size, this.default_rounding)
  def apply(a : Double) : Morris_B = this.apply(a, this.default_g_size, this.default_size, this.default_rounding)
  def apply(a : Float, g_size_c : Int, size_c : Int, rounding_c : RoundingType) : Morris_B = this.apply(a.toDouble, g_size_c, size_c, rounding_c)
  def apply(a : Float) : Morris_B = this.apply(a.toDouble)
  def apply(binaryString : String, g_size_c : Int, size_c : Int, rounding_c : RoundingType) : Morris_B = {
    //special cases
    val zeroBinaryString = auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(size_c)(false) )
    val nrBinaryString = "1" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(size_c-1)(false) )
    if(binaryString == zeroBinaryString) {
        new Morris(
            TaperedFloatingPoint(
            false,
            this.minimum_exponent(g_size_c, size_c),
            NaturalNumber(0),
            0,
            Nil,
            size_c
            ),
            g_size_c, size_c, rounding_c)
    } else if(binaryString == nrBinaryString) {
        new Morris_NR(g_size_c, size_c, rounding_c)
    } else {
      val sign : Boolean = binaryString(0) == '1'
      val toDecode : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size_c, NaturalNumber((BigInt(binaryString, 2))).binaryEncode)) drop 1 //sign bit
      val g_string : String = toDecode take g_size_c
      val g_value : Int = BigInt(g_string, 2).toInt
      val exponent_sign = BigInt((toDecode drop g_size_c) take 1, 2)
      val exponentString : String = (toDecode drop (g_size_c + 1)) take (g_value + 1)
      val exponent : BigInt = (if(exponent_sign == 1) BigInt(-1) else BigInt(1)) * (if (exponentString.length > 0) BigInt(exponentString, 2) << (g_value + 1 - exponentString.length) else BigInt(0))
      val fractionString : String =  toDecode drop (g_size_c + 1 + g_value + 1)
      val fraction_size : Int = fractionString.length
      val fraction : BigInt =  (if(fraction_size > 0) BigInt(fractionString, 2) else BigInt(0)) + ( BigInt(1) << fraction_size )
      /*
      println("sign:" + sign + " g_string:" + g_string +
              " g_value:" + g_value + " exponent_sign:" + exponent_sign +
              " exponentString:" + exponentString + " exponent:" + exponent +
              " fractionString:" + fractionString + " fraction_size:" + fraction_size +
              " fraction:" + fraction)
      */
      this.apply(TaperedFloatingPoint(sign, IntegerNumber(exponent.toLong), NaturalNumber(fraction.toLong), fraction_size, Nil, size_c), g_size_c, size_c, rounding_c)
    }
  }

  def unapply(input : Morris_B) = Some(input.value, input.g_size, input.size, input.rounding)
}
