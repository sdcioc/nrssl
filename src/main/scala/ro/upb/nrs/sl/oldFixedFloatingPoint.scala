package ro.upb.nrs.sl




import scala.annotation.tailrec

/*
We consider FloatingPoint
(-1)^sign * 2^exponent * ( mantissa / 2^fraction_size )
mantissa / 2^fraction_size in [1, 2)
fraction_size exponent_size and rounding are fixed
mantissa has the hiddent bit that is always 1
*/
abstract class oldFixedFloatingPoint_B extends NumberRepresentationSystem {
  /*
  sign
  exponent in Z
  mantissa in N (with hidden bit)
  size of the exponent
  size of the mantissa (without the hidden bit)
  rounding
  */
  val sign : Boolean
  val exponent : IntegerNumber_B
  val mantissa : NaturalNumber_B
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
  We can consider for this operation that both operands have the same sign and
  the first one has a bigger absolute value which means that is exponent is bigger.
  For the other cases: if they have different signs we can do substraction with the same sign
  this + that = this - (-that) // (negate second operand sign).
  If second operand has a bigger absolute value we can swap them. this + that = that + this
  (-1)^sign * 2^exponent1 * ( mantissa1 / 2^fraction_size ) + (-1)^sign * 2^exponent2 * ( mantissa2 / 2^fraction_size ) =
  = (-1)^sign * 2^exponent1 * ( ( mantissa1 + 2^( exponent2 - exponent1 ) * mantissa2 ) / 2^fraction_size ) =
  = (-1)^sign * 2^exponent1 * ( ( mantissa1 + mantissa2 / 2^( exponent1 - exponent2 ) ) / 2^fraction_size ) =
  = (-1)^sign * 2^exponent1 * ( ( mantissa1 + ( mantissa2 >> ( exponent1 - exponent2 ) ) ) / 2^fraction_size )
  */
  override def +(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = that match {
    case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case that : oldFixedFloatingPoint_B => {
      if(that.sign != this.sign) this - (-that)
      else if(that.abs <= this.abs) {
        val exponent_diff = (this.exponent - that.exponent).toInt
        val rest_value = that.mantissa.value & ((BigInt(1) << exponent_diff)-1)
        val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(rest_value).binaryEncode).reverse
        val new_mantissa = this.mantissa + (that.mantissa >> exponent_diff)
        oldFixedFloatingPoint(this.sign, this.exponent,
          new_mantissa,
          rest_bits_computed,
          this.exponent_size, this.fraction_size, this.rounding
        )
      }
      else that + this
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint +")
  }
  /*
  we can consider the same case as addition with the same sign and first element with bigger absolute value.
  If the sign differ this - that = this + (-that)
  if the second absolute value is bigger this - that = -(that - this)
  (-1)^sign * 2^exponent1 * ( mantissa1 / 2^fraction_size ) - (-1)^sign * 2^exponent2 * ( mantissa2 / 2^fraction_size ) =
  = (-1)^sign * 2^exponent1 * ( ( mantissa1 - 2^( exponent2 - exponent1 ) * mantissa2 ) / 2^fraction_size ) =
  = (-1)^sign * 2^exponent1 * ( ( mantissa1 - mantissa2 / 2^( exponent1 - exponent2 ) ) / 2^fraction_size ) =
  = (-1)^sign * 2^exponent1 * ( ( mantissa1 - ( mantissa2 >> ( exponent1 - exponent2 ) ) ) / 2^fraction_size )
  */
  override def -(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = that match {
    case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case that : oldFixedFloatingPoint_B => {
      if(that.sign != this.sign) this + (-that)
      else if(that.abs <= this.abs) {
        val exponent_diff = (this.exponent - that.exponent).toInt
        val extra_value = BigInt(1) << exponent_diff
        val rest_value = that.mantissa.value & ((BigInt(1) << exponent_diff)-1)
        val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(extra_value - rest_value).binaryEncode).reverse
        val new_mantissa = this.mantissa - (that.mantissa >> exponent_diff)
        if(rest_value == 0) {
          oldFixedFloatingPoint(this.sign, this.exponent,
          new_mantissa,
          Nil,
          this.exponent_size, this.fraction_size, this.rounding)
        } else {
          oldFixedFloatingPoint(this.sign, this.exponent,
          new_mantissa - NaturalNumber(1),
          rest_bits_computed,
          this.exponent_size, this.fraction_size, this.rounding)
        }
      } else {
        -(that - this)
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint -")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantissa1 / 2^fraction_size ) * (-1)^sign2 * 2^exponent2 * ( mantissa2 / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( mantissa1 * mantissa2 ) / 2^(2 * fraction_size) ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( ( mantissa1 * mantissa2 ) / 2^fraction_size ) / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( ( mantissa1 * mantissa2 ) >> fraction_size ) / 2^fraction_size ) =
  */
  override def *(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = that match {
    case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case oldFixedFloatingPoint(x, y, z, _ , _, _) => 
      oldFixedFloatingPoint(this.sign^x, this.exponent+y, (this.mantissa * z)>>this.fraction_size, (this.mantissa * z).binaryEncode.take(this.fraction_size).reverse, this.exponent_size, this.fraction_size, this.rounding   )
    
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint *")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantissa1 / 2^fraction_size ) / (-1)^sign2 * 2^exponent2 * ( mantissa2 / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( mantissa1 / mantissa2 ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( ( mantissa1 * 2^fraction_size ) / ( mantissa2 * 2^fraction_size ) ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( ( ( mantissa1 << fraction_size ) /  mantissa2 ) / 2^fraction_size ) =
  */
  override def /(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = that match {
    case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case oldFixedFloatingPoint(x, y, z, _ , _, _) => if(z!=NaturalNumber(0)) {
        // we add two extra bits to find g and r
        val eq = (this.mantissa<<(this.fraction_size + 2)) / z
        // for finding s value
        val er = (this.mantissa<<(this.fraction_size + 2)) % z
        val g  = eq.value.testBit(1)
        val r  = eq.value.testBit(0)
        val s = !(er==NaturalNumber(0))
        // eliminate the extra bits
        val new_q = eq >> 2
        val l = new_q.value.testBit(0)
        oldFixedFloatingPoint(this.sign^x, this.exponent-y, new_q, g::r::s::Nil, this.exponent_size, this.fraction_size, this.rounding)
    } else oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint /")
  }
  override def /\(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = this / that
  override def %(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint %")
  /*
  X^Y = X^([Y]+{Y}) where [Y] is integer part of Y and {Y} is fractional part of Y
  X^(-Y) = 1 / X^Y
  */
  override def pow(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = that match {
    case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case oldFixedFloatingPoint(x, y, z, u , v, w) => {
      /*
      if that is negative
      X^(-Y) = 1 / X^Y
      */
      if(x)
        this.inverse.pow(-that)
      else {
        //[Y] because Y is positive [Y] is positive
        val integer_value = that.toBigInt
        //{Y} in [0, 1)
        val fractional_value = oldFixedFloatingPoint(x, y, z, Nil, u , v, w) - oldFixedFloatingPoint(integer_value.toDouble, u , v, w)
        /*
        X^Y = X^([Y]+{Y}) = X^[Y] * X^{Y} = X^[Y] * X^( 2^exponent * ( mantissa / 2^fraction_size ) )
        mantissa / 2^fraction_size is in [1, 2) so  we can use tha format mantissa / 2^fraction_size = 1 +  fraction / 2^fraction_size
        X^Y = X^[Y] * X^( 2^exponent * ( 1 +  fraction / 2^fraction_size ) ) =
        = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        */
        //X^[Y]
        val x_integerY = mathFunctions.pow_integer(
                        oldFixedFloatingPoint(1.0d, exponent_size, fraction_size, rounding)) (this.abs,
                        integer_value
                      )
        //X^2^exponent //exponent is negative so X^(1/2^exponent.abs)
        val x_exponentY = mathFunctions.pow_fractional(
                          oldFixedFloatingPoint(1.0d, exponent_size, fraction_size, rounding)) (this.abs ,
                          BigInt(1), fractional_value.exponent.abs.toInt
                        )
        //(X^(2^exponent))^( fraction / 2^fraction_size )
        val x_exponentY_fraction = mathFunctions.pow_fractional(
                        oldFixedFloatingPoint(1.0d, exponent_size, fraction_size, rounding)) (
                        x_exponentY,
                        fractional_value.mantissa.value, fractional_value.fraction_size
                      )
        //if fractional value is zero
        // result = X^[Y]
        // else result = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        // if the current number is negative we only do pow for integer numbers
        val result = if(fractional_value == oldFixedFloatingPoint(0.0d, exponent_size, fraction_size, rounding))
                      if(this.sign == false)
                        x_integerY
                      else 
                        if((integer_value % 2) ==0)
                          x_integerY
                        else
                          -x_integerY
                     else 
                      if(this.sign == false)
                        x_integerY * x_exponentY * x_exponentY_fraction
                      else
                        oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
        result match {
          case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
          case oldFixedFloatingPoint(x, y, z, u , v, w) => oldFixedFloatingPoint(x, y, z, Nil, u , v, w)
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint_B pow math")
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint_B pow")
  }
  override def unary_- : oldFixedFloatingPoint_B = oldFixedFloatingPoint(!this.sign, this.exponent, this.mantissa, Nil, this.exponent_size, this.fraction_size, this.rounding)
  override def inverse : oldFixedFloatingPoint_B = oldFixedFloatingPoint(1.0d, this.exponent_size, this.fraction_size, this.rounding) / this
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
    case oldFixedFloatingPoint_NR(_, _,_) => false
    case oldFixedFloatingPoint(x, y, z, _ , _, _) => {
      if(this.sign != x) this.sign
      else if(this.exponent<y) !this.sign
      else if(this.exponent>y) this.sign
      else if(this.mantissa<z) !this.sign
      else if(this.mantissa>z) this.sign
      else false
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case oldFixedFloatingPoint_NR(_, _,_) => false
    case oldFixedFloatingPoint(x, y, z, _ , _, _) => {
      if( (this.exponent == y) && (this.mantissa == z) ) {
        if (this.sign == x) true
        else (this.mantissa == NaturalNumber(0)) // -0 == +0
      } else false
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint ==")
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
  override def min(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = that match {
    case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case oldFixedFloatingPoint(x, y, z, _ , _, _) => if (this<that) this else oldFixedFloatingPoint(x, y,z, Nil, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint min")
  }
  override def max(that: NumberRepresentationSystem): oldFixedFloatingPoint_B = that match {
    case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case oldFixedFloatingPoint(x, y, z, _ , _, _) => if (this>that) this else oldFixedFloatingPoint(x, y,z, Nil, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint max")
  }
  override def abs: oldFixedFloatingPoint_B = oldFixedFloatingPoint(false, this.exponent, this.mantissa, Nil, this.exponent_size, this.fraction_size, this.rounding)
  override def signum: oldFixedFloatingPoint_B = {
    if(this.mantissa == NaturalNumber(0)) this
    else oldFixedFloatingPoint(this.sign, IntegerNumber(0), NaturalNumber(1)<<this.fraction_size, Nil, this.exponent_size, this.fraction_size, this.rounding)
  }
  override def nth_root (n : Int) : (oldFixedFloatingPoint_B, oldFixedFloatingPoint_B) = {
    val root = this.nqrt(n)
    (root, this-root.pow(oldFixedFloatingPoint(n, this.exponent_size, this.fraction_size, this.rounding)))
  }
  /*
  NQRT( (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) ) =
  = NQRT( (-1)^sign ) * NQRT( 2^( ( n * [exponent / n] ) + ( exponent % n ) ) * ( ( mantissa * 2^( (n - 1) * fraction_size ) ) / 2^( ( n * fraction_size ) ) ) )
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( 2^( exponent % n ) * ( mantissa * 2^( (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantissa * 2^( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantissa * << ( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  */
  override def nqrt (n : Int) : oldFixedFloatingPoint_B = {
    val n_ip = IntegerNumber(n)
    //TODO: Does it work on negativ exponent % yes because is in ro.upb.nrs.sl.IntegerNumber
    // 2*n for the extra 2 bits for g and r
    val (eq, er) = (this.mantissa<<(2*n+(n-1)*fraction_size+(this.exponent%n_ip).toInt)).nth_root(n)
    val g  = eq.value.testBit(1)
    val r  = eq.value.testBit(0)
    val s = !(er==NaturalNumber(0))
    // eliminate extra bits
    val new_q = eq >> 2
    val l = new_q.value.testBit(0)
    if(this.sign & n%2==0) {
      if(this.mantissa == NaturalNumber(0)) this //SQRT(-0) = -0
      else oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    }
    else oldFixedFloatingPoint(this.sign, this.exponent/n_ip, new_q, g::r::s::Nil, this.exponent_size, this.fraction_size, this.rounding)
  }
  override def sqrt: oldFixedFloatingPoint_B = this.nqrt(2)
  override def exp : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.exp)
  override def ln : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : oldFixedFloatingPoint_B = base match {
    case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
    case oldFixedFloatingPoint(x, y, z, u , v, w) => this.ln / oldFixedFloatingPoint(x, y, z, Nil, u , v, w).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint log")
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
  override def sin : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.sin)
  override def cos : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.cos)
  override def tan : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.tan)
  override def cot : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.cot)
  override def sec : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.sec)
  override def csc : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.coth)
  override def sech : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.sech)
  override def csch : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : oldFixedFloatingPoint_B = helper_taylor_function(mathFunctions.arccsch)
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
  override def toRationalNumber : RationalNumber_B = (if(sign) RationalNumber(-1) else RationalNumber(1)) * (RationalNumber(2).pow(RationalNumber(exponent)-RationalNumber(fraction_size)) * mantissa.toRationalNumber)
  override def toBigDecimal : BigDecimal = this.toRationalNumber.toBigDecimal
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = "sign=" + sign.toString +"exp=" + exponent.toInternalString + " mantissa=" + mantissa.toInternalString
  def toBinaryString: String = {
    val exponentWithoutBias = this.exponent - oldFixedFloatingPoint.minimum_exponent(exponent_size)
    val binaryExponent : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_size, exponentWithoutBias.value.binaryEncode))
    val mantissaWithoutHiddenBit = this.mantissa.binaryEncode take fraction_size
    val binarymantissa : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(fraction_size, mantissaWithoutHiddenBit))
    val binaryValue : String = (if (this.sign) "1" else "0") + binaryExponent + binarymantissa
    binaryValue
  }
  
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : oldFixedFloatingPoint_B = {
    val precision : Int = if(exponent_size >= 8) 30 else if (exponent_size >= 5) 12 else if(exponent_size >= 4) 7 else 5
    val result = func(
      oldFixedFloatingPoint(0.0d, exponent_size, fraction_size, rounding),
      oldFixedFloatingPoint(1.0d, exponent_size, fraction_size, rounding)) (this,
      oldFixedFloatingPoint(precision.toDouble, exponent_size, fraction_size, rounding))
    result match {
      case oldFixedFloatingPoint_NR(_, _,_) => oldFixedFloatingPoint_NR(exponent_size, fraction_size, rounding)
      case oldFixedFloatingPoint(x, y, z, u , v, w) => oldFixedFloatingPoint(x, y, z, Nil, u , v, w)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint helper_taylor_function")
    }
  }
}

class oldFixedFloatingPoint_NR(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends oldFixedFloatingPoint_B {
  override val sign : Boolean = false;
  override val exponent : IntegerNumber_B = IntegerNumber_NR
  override val mantissa : NaturalNumber_B = NaturalNumber_NR
  override val exponent_size : Int = exponent_size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
}

object oldFixedFloatingPoint_NR {
  def apply(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldFixedFloatingPoint_B = new oldFixedFloatingPoint_NR(exponent_size_c, fraction_size_c, rounding_c)
  def unapply(input : oldFixedFloatingPoint_B) = if(input.mantissa.equals(NaturalNumber_NR)) Some(input.exponent_size, input.fraction_size, input.rounding) else None
}

class oldFixedFloatingPoint(sign_c : Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends oldFixedFloatingPoint_B {
  override val sign : Boolean = sign_c;
  override val exponent : IntegerNumber_B = exponent_c
  override val mantissa : NaturalNumber_B = mantissa_c
  override val exponent_size : Int = exponent_size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c
}


object oldFixedFloatingPoint {
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

  def maximum_exponent(exponent_size_c : Int): IntegerNumber_B = {
    IntegerNumber(false, (NaturalNumber(1) << exponent_size_c)-NaturalNumber(1)) - IntegerNumber(false, ( (NaturalNumber(1) << (exponent_size_c - 1)) - NaturalNumber(1) ) )
  }
  def minimum_exponent(exponent_size_c : Int): IntegerNumber_B = {
    IntegerNumber(true, (NaturalNumber(1) << (exponent_size_c - 1))-NaturalNumber(1))
  }

  /*
  Rounding has effect only on mantissa
  ro.upb.nrs.sl.RoundUp -> if positive and (g|r|s) than mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundDown -> if negative and (g|r|s) than mantissa - 1 else mantissa
  ro.upb.nrs.sl.RoundZero -> mantissa
  ro.upb.nrs.sl.RoundAwayZero -> if (g|r|s) { if(negative) than mantissa - 1 else mantissa + 1 } else mantissa
  ro.upb.nrs.sl.RoundEven -> if (g and (l|r|s) ) { if(negative) than mantissa - 1 else mantissa + 1 } else mantissa
  */
  def roundingLGRS(sign_c: Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B,
  exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType,
  l : Boolean, g  : Boolean, r : Boolean, s : Boolean): oldFixedFloatingPoint_B = rounding_c match {
      case RoundUp => if(!sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case RoundDown => if(sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case RoundZero => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case RoundAwayZero => if(g|r|s) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case RoundEven => if(g&(r|s|l)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case _ => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c) 
  }

  @tailrec
  def apply(sign_c: Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, rest_bits: List[Boolean], exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldFixedFloatingPoint_B = {
      (exponent_c, mantissa_c) match {
        case (IntegerNumber_NR(_), _)  => oldFixedFloatingPoint_NR(exponent_size_c, fraction_size_c, rounding_c)
        case (_, NaturalNumber_NR(_))  => oldFixedFloatingPoint_NR(exponent_size_c, fraction_size_c, rounding_c)
        case (_, NaturalNumber(a)) => if(NaturalNumber(a)==NaturalNumber(0)) {
                                    //if mantissa is 0
                                    val l = false
                                    /*
                                    if it does not have extra bits than the vaue is absolute zero
                                    (because of the way float is done in standard we have minus and pozitive zero)
                                    if we have rest bits than we find the values for g r and s.
                                    l is 0 obviously
                                    than we do rounding
                                    */
                                    rest_bits match {
                                        case Nil => {
                                          // TODO: can repair to have only positive zero (sign_c -> false)
                                          new oldFixedFloatingPoint(
                                            sign_c,
                                            minimum_exponent(exponent_size_c),
                                            NaturalNumber(0), //zero mantissa
                                            exponent_size_c, fraction_size_c, rounding_c
                                          )
                                        }
                                        case x::xs => {
                                          val g = x
                                          val r = xs match {case Nil => false case _ => xs.head}
                                          val s = xs match {case Nil => false case y::ys => ys match {case Nil => false case _ => ys.foldLeft(false)((a,b)=>(a|b))}}

                                          rounding_c match {
                                              case RoundUp => if(!sign_c & (g | r | s))
                                                                this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                              else
                                                                this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                              case RoundDown => if(sign_c & (g | r | s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                              case RoundZero => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                              case RoundAwayZero => if(g | r | s) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                              case RoundEven => if(g & (r | s | l)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                              case _ => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c) 
                                          }
                                        }
                                    }
                                } else {
                                  /*
                                  if mantissa / 2^fraction_size >= 2
                                  we divide mantissa by 2 (shifting right with 1) and increase the exponent by one
                                  (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) =
                                  = (-1)^sign * 2^exponent * ( ( 2 * ( mantissa / 2 ) ) / 2^fraction_size )
                                  = (-1)^sign * 2^(exponent + 1) * ( ( mantissa / 2 ) / 2^fraction_size )
                                  */
                                  if(mantissa_c.binaryEncode.length > (fraction_size_c+1)) 
                                    this.apply(
                                      sign_c,
                                      exponent_c + IntegerNumber(1),
                                      mantissa_c >> 1,
                                      mantissa_c.binaryEncode.head :: rest_bits, //add the eliminated bit to rest bits
                                      exponent_size_c, fraction_size_c, rounding_c
                                    )
                                  /*
                                  if mantissa / 2^fraction_size < 1
                                  we multiply mantissa by 2 (shifting left with 1) and decrease the exponent by one
                                  Because we have rest bit to protect the value we have to add the first bit from the rest bits
                                  (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) =
                                  = (-1)^sign * 2^exponent * ( ( ( 2 * mantissa ) / 2 ) / 2^fraction_size )
                                  = (-1)^sign * 2^(exponent - 1) * ( ( 2 * mantissa ) / 2^fraction_size )
                                  */
                                  else if (mantissa_c.binaryEncode.length < (fraction_size_c+1)) 
                                    if(rest_bits != Nil)
                                        this.apply(
                                          sign_c,
                                          exponent_c - IntegerNumber(1), 
                                          (mantissa_c << 1) + (if(rest_bits.head) NaturalNumber(1) else NaturalNumber(0)), //add the first bit of rest bits
                                          rest_bits.tail,
                                          exponent_size_c, fraction_size_c, rounding_c
                                        )
                                    else this.apply(
                                          sign_c,
                                          exponent_c - IntegerNumber(1), 
                                          (mantissa_c << 1), //if no rest bits
                                          Nil,
                                          exponent_size_c, fraction_size_c, rounding_c
                                        )
                                  else {
                                    /*
                                    if mantissa is in the right range
                                    if there are no rest bits we verify that the exponent match the required size
                                    because mantissa is in right size because is in the right range
                                    if the exponent is in the right range (exponent_size) we create the number otherwise NR
                                    If we have rest bits than we calculate l g r s
                                    */
                                    val l = mantissa_c.binaryEncode.head
                                    rest_bits match {
                                      case Nil => {
                                          if(
                                            (exponent_c >= minimum_exponent(exponent_size_c)) &&
                                            (exponent_c <= maximum_exponent(exponent_size_c))
                                          )
                                            new oldFixedFloatingPoint(
                                              sign_c,
                                              exponent_c,
                                              mantissa_c,
                                              exponent_size_c, fraction_size_c, rounding_c
                                            )
                                          else
                                            oldFixedFloatingPoint_NR(exponent_size_c, fraction_size_c, rounding_c)
                                      }
                                      case x::xs => {
                                        val g = x
                                        val r = xs match {case Nil => false case _ => xs.head}
                                        val s = xs match {case Nil => false case y::ys => ys match {case Nil => false case _ => ys.foldLeft(false)((a,b)=>(a|b))}}

                                        rounding_c match {
                                            case RoundUp => if(!sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                            case RoundDown => if(sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                            case RoundZero => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                            case RoundAwayZero => if(g|r|s) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                            case RoundEven => if(g&(r|s|l)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                            case _ => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c) 
                                        }
                                      }
                                    }
                                  }
                                }
        case _=> throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint apply")
      }
  } 
  def apply(a : Double, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldFixedFloatingPoint_B = {
    val bits : Long = java.lang.Double.doubleToRawLongBits(a)
    val negative : Boolean = (bits & 0x8000000000000000L) != 0
    val exponent_bits : Long = ((bits & 0x7ff0000000000000L) >> 52)
    val exponent : Long = exponent_bits - 1023
    val mantissa : Long = if(exponent != -1023) 
                            (bits & 0x000fffffffffffffL) | 0x0010000000000000L
                          else
                            (bits & 0x000fffffffffffffL)
    val fraction_diff : Int = 52 - fraction_size_c
    if(fraction_diff>0)
      // we add the mantissa bits that were eliminated by fraction_diff to rest bits in order of importance
      this.apply(negative, IntegerNumber(exponent), NaturalNumber(mantissa) >> fraction_diff, NaturalNumber(mantissa).binaryEncode.take(fraction_diff).reverse, exponent_size_c, fraction_size_c, rounding_c)
    else
      this.apply(negative, IntegerNumber(exponent), NaturalNumber(mantissa) << (-fraction_diff), Nil, exponent_size_c, fraction_size_c, rounding_c)
  }
  
  def apply(a : Int, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldFixedFloatingPoint_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Int) : oldFixedFloatingPoint_B = this.apply(a.toDouble, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Long, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldFixedFloatingPoint_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Long) : oldFixedFloatingPoint_B = this.apply(a.toDouble, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Double) : oldFixedFloatingPoint_B = this.apply(a, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Float, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldFixedFloatingPoint_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Float) : oldFixedFloatingPoint_B = this.apply(a.toDouble)
  def apply(binaryString : String, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldFixedFloatingPoint_B = {
    //special cases
    val positiveZeroBinaryString = auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c + 1)(false) )
    val negativeZeroBinaryString = "1" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c)(false) )
    val nrBinaryString = "0" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c)(false) )
    if(binaryString == positiveZeroBinaryString) {
      new oldFixedFloatingPoint(
        false,
        minimum_exponent(exponent_size_c),
        NaturalNumber(0), //zero mantissa
        exponent_size_c, fraction_size_c, rounding_c
      )
    } else if(binaryString == negativeZeroBinaryString) {
        new oldFixedFloatingPoint(
        true,
        minimum_exponent(exponent_size_c),
        NaturalNumber(0), //zero mantissa
        exponent_size_c, fraction_size_c, rounding_c
      )
    } /*else if(binaryString == nrBinaryString) {
        new oldFixedFloatingPoint_NR(exponent_size_c, fraction_size_c, rounding_c)
    }*/ else {
      val exponentString :  String = "0" + ((binaryString drop 1) take exponent_size_c)
      val mantissaString : String = "0" + (binaryString drop ( 1 + exponent_size_c ))
      val binaryExponent : IntegerNumber_B = IntegerNumber(false, NaturalNumber(BigInt(exponentString, 2)))
      val binaryMantissaWihoutHiddenBit : NaturalNumber_B = NaturalNumber(BigInt(mantissaString, 2))
      val binaryMantissa : NaturalNumber_B = binaryMantissaWihoutHiddenBit + ( if( (binaryExponent == IntegerNumber(0)) && (binaryMantissaWihoutHiddenBit == NaturalNumber(0))   ) NaturalNumber(0) else NaturalNumber(1) << fraction_size_c )
      this.apply(binaryString(0) == '1', binaryExponent + minimum_exponent(exponent_size_c), binaryMantissa, Nil, exponent_size_c, fraction_size_c, rounding_c )
    }
  }
  def unapply(input : oldFixedFloatingPoint_B) = Some(input.sign, input.exponent, input.mantissa, input.exponent_size, input.fraction_size, input.rounding)
}
