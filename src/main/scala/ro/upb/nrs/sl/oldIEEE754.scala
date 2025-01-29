package ro.upb.nrs.sl



import scala.annotation.tailrec

/*
We consider FloatingPoint
(-1)^sign * 2^exponent * ( mantissa / 2^fraction_size )
mantissa / 2^fraction_size in [1, 2) for normal floats and [0, 1) for subnormal
fraction_size exponent_size and rounding are fixed
mantissa has the hiddent bit is 1 for normal floats and 0 for subnormals
Constructor takes care of subnormal numbers
*/
abstract class oldIEEE754_B extends NumberRepresentationSystem {
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
  override def +(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => oldIEEE754_INF(x, exponent_size, fraction_size, rounding)
    case that : oldIEEE754_B => {
      if(that.sign != this.sign) this - (-that)
      else if(that.abs <= this.abs) {
        val exponent_diff = (this.exponent - that.exponent).toInt
        val rest_value = that.mantissa.value & ((BigInt(1) << exponent_diff)-1)
        val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(rest_value).binaryEncode).reverse
        val new_mantissa = this.mantissa + (that.mantissa >> exponent_diff)
        oldIEEE754(this.sign, this.exponent,
          new_mantissa,
          rest_bits_computed,
          this.exponent_size, this.fraction_size, this.rounding
        )
      }
      else that + this
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 +")
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
  override def -(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => oldIEEE754_INF(!x, exponent_size, fraction_size, rounding)
    case that : oldIEEE754_B => {
      if(that.sign != this.sign) this + (-that)
      else if(that.abs <= this.abs) {
        val exponent_diff = (this.exponent - that.exponent).toInt
        val extra_value = BigInt(1) << exponent_diff
        val rest_value = that.mantissa.value & ((BigInt(1) << exponent_diff)-1)
        val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(extra_value - rest_value).binaryEncode).reverse
        val new_mantissa = this.mantissa - (that.mantissa >> exponent_diff)
        if(rest_value == 0) {
          oldIEEE754(this.sign, this.exponent,
          new_mantissa,
          Nil,
          this.exponent_size, this.fraction_size, this.rounding)
        } else {
          oldIEEE754(this.sign, this.exponent,
          new_mantissa - NaturalNumber(1),
          rest_bits_computed,
          this.exponent_size, this.fraction_size, this.rounding)
        }
      } else {
        -(that - this)
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 -")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantissa1 / 2^fraction_size ) * (-1)^sign2 * 2^exponent2 * ( mantissa2 / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( mantissa1 * mantissa2 ) / 2^(2 * fraction_size) ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( ( mantissa1 * mantissa2 ) / 2^fraction_size ) / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( ( mantissa1 * mantissa2 ) >> fraction_size ) / 2^fraction_size ) =
  */
  override def *(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => oldIEEE754_INF(x^this.sign, exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, _ , _, _) => 
      oldIEEE754(this.sign^x, this.exponent+y, (this.mantissa * z)>>this.fraction_size, (this.mantissa * z).binaryEncode.take(this.fraction_size).reverse, this.exponent_size, this.fraction_size, this.rounding   )
    
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 *")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantissa1 / 2^fraction_size ) / (-1)^sign2 * 2^exponent2 * ( mantissa2 / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( mantissa1 / mantissa2 ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( ( mantissa1 * 2^fraction_size ) / ( mantissa2 * 2^fraction_size ) ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( ( ( mantissa1 << fraction_size ) /  mantissa2 ) / 2^fraction_size ) =
  */
  override def /(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => oldIEEE754(this.sign^x, oldIEEE754.minimum_exponent(this.exponent_size), NaturalNumber(0), Nil, this.exponent_size, this.fraction_size, this.rounding)
    case oldIEEE754(x, y, z, _ , _, _) => if(z!=NaturalNumber(0)) {
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
        oldIEEE754(this.sign^x, this.exponent-y, new_q, g::r::s::Nil, this.exponent_size, this.fraction_size, this.rounding)
    } else oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 /")
  }
  override def /\(that: NumberRepresentationSystem): oldIEEE754_B = this / that
  override def %(that: NumberRepresentationSystem): oldIEEE754_B = throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 %")
  /*
  X^Y = X^([Y]+{Y}) where [Y] is integer part of Y and {Y} is fractional part of Y
  X^(-Y) = 1 / X^Y
  */
  override def pow(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => if(x) this.inverse.pow(-that)
                                        else {
                                          if(this.exponent >= IntegerNumber(0))  oldIEEE754_INF(false, exponent_size, fraction_size, rounding)
                                          else if ( (this.exponent == IntegerNumber(0)) && (this.mantissa==(NaturalNumber(1)<<fraction_size)) ) oldIEEE754(false, IntegerNumber(0), this.mantissa, Nil, this.exponent_size, this.fraction_size, this.rounding)
                                          else oldIEEE754(false, oldIEEE754.minimum_exponent(this.exponent_size), NaturalNumber(0), Nil, this.exponent_size, this.fraction_size, this.rounding)
                                        }
    case oldIEEE754(x, y, z, u , v, w) => {
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
        val fractional_value = oldIEEE754(x, y, z, Nil, u , v, w) - oldIEEE754(integer_value.toDouble, u , v, w)
        /*
        X^Y = X^([Y]+{Y}) = X^[Y] * X^{Y} = X^[Y] * X^( 2^exponent * ( mantissa / 2^fraction_size ) )
        mantissa / 2^fraction_size is in [1, 2) so  we can use tha format mantissa / 2^fraction_size = 1 +  fraction / 2^fraction_size
        X^Y = X^[Y] * X^( 2^exponent * ( 1 +  fraction / 2^fraction_size ) ) =
        = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        for subnormal numbers mantissa / 2^fraction_size = fraction / 2^fraction_size
        = X^[Y] * (X^(2^exponent))^( fraction / 2^fraction_size )
        */
        //X^[Y]
        val x_integerY = mathFunctions.pow_integer(
                        oldIEEE754(1.0d, exponent_size, fraction_size, rounding)) (this.abs,
                        integer_value
                      )
        //X^2^exponent //exponent is negative so X^(1/2^exponent.abs)
        val x_exponentY = mathFunctions.pow_fractional(
                          oldIEEE754(1.0d, exponent_size, fraction_size, rounding)) (this.abs ,
                          BigInt(1), fractional_value.exponent.abs.toInt
                        )
        //(X^(2^exponent))^( fraction / 2^fraction_size )
        val x_exponentY_fraction = mathFunctions.pow_fractional(
                        oldIEEE754(1.0d, exponent_size, fraction_size, rounding)) (
                        x_exponentY,
                        fractional_value.mantissa.value, fractional_value.fraction_size
                      )
        //if fractional value is zero
        // result = X^[Y]
        // else if normal float fractional result = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        // else is subnormal float fractional  result = X^[Y] * (X^(2^exponent))^( fraction / 2^fraction_size )
        // if the current number is negative we only do pow for integer numbers
        val result = if(fractional_value == oldIEEE754(0.0d, exponent_size, fraction_size, rounding))
                      if(this.sign == false)
                        x_integerY
                      else 
                        if((integer_value % 2) ==0)
                          x_integerY
                        else
                          -x_integerY
                     else 
                      if(this.sign == false)
                        //if fraction value is subnormal we don not have 1 + so no x_exponentY
                        if (fractional_value.mantissa.binaryEncode.length < (fractional_value.fraction_size + 1)) 
                          x_integerY * x_exponentY_fraction
                        else
                          x_integerY * x_exponentY * x_exponentY_fraction
                      else
                        oldIEEE754_NR(exponent_size, fraction_size, rounding)
        result match {
          case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
          case oldIEEE754(x, y, z, u , v, w) => oldIEEE754(x, y, z, Nil, u , v, w)
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 pow math")
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 pow")
  }
  override def unary_- : oldIEEE754_B = oldIEEE754(!this.sign, this.exponent, this.mantissa, Nil, this.exponent_size, this.fraction_size, this.rounding)
  override def inverse : oldIEEE754_B = oldIEEE754(1.0d, this.exponent_size, this.fraction_size, this.rounding) / this
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
    case oldIEEE754_NR(_, _,_) => false
    case oldIEEE754_INF(x, _, _,_) => if(x) false else true
    case oldIEEE754(x, y, z, _ , _, _) => {
      if(this.sign != x) this.sign
      else if(this.exponent<y) !this.sign
      else if(this.exponent>y) this.sign
      else if(this.mantissa<z) !this.sign
      else if(this.mantissa>z) this.sign
      else false
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case oldIEEE754_NR(_, _,_) => false
    //special case for mantissa 0 when you can have +0 or -0
    case oldIEEE754(x, y, z, _ , _, _) => (this.mantissa == z) && ( ((this.sign == x) && (this.exponent == y)) || (this.mantissa == NaturalNumber(0)) )
    /* (this.sign == x) && (this.exponent == y) && (this.mantissa == z) ||
                                           ( (this.mantissa == z) && (this.mantissa == NaturalNumber(0)) ) // -0 == +0
                                           */
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 ==")
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
  override def min(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, _ , _, _) => if (this<that) this else oldIEEE754(x, y,z, Nil, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 min")
  }
  override def max(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, _ , _, _) => if (this>that) this else oldIEEE754(x, y,z, Nil, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 max")
  }
  override def abs: oldIEEE754_B = oldIEEE754(false, this.exponent, this.mantissa, Nil, this.exponent_size, this.fraction_size, this.rounding)
  override def signum: oldIEEE754_B = {
    if(this.mantissa == NaturalNumber(0)) this
    else oldIEEE754(this.sign, IntegerNumber(0), NaturalNumber(1)<<this.fraction_size, Nil, this.exponent_size, this.fraction_size, this.rounding)
  }
  override def nth_root (n : Int) : (oldIEEE754_B, oldIEEE754_B) = {
    val root = this.nqrt(n)
    (root, this-root.pow(oldIEEE754(n, this.exponent_size, this.fraction_size, this.rounding)))
  }
  /*
  NQRT( (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) ) =
  = NQRT( (-1)^sign ) * NQRT( 2^( ( n * [exponent / n] ) + ( exponent % n ) ) * ( ( mantissa * 2^( (n - 1) * fraction_size ) ) / 2^( ( n * fraction_size ) ) ) )
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( 2^( exponent % n ) * ( mantissa * 2^( (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantissa * 2^( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantissa * << ( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  */
  override def nqrt (n : Int) : oldIEEE754_B = {
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
      else oldIEEE754_NR(exponent_size, fraction_size, rounding)
    }
    else oldIEEE754(this.sign, this.exponent/n_ip, new_q, g::r::s::Nil, this.exponent_size, this.fraction_size, this.rounding)
  }
  override def sqrt: oldIEEE754_B = this.nqrt(2)
  override def exp : oldIEEE754_B = helper_taylor_function(mathFunctions.exp)
  override def ln : oldIEEE754_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : oldIEEE754_B = base match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, u , v, w) => this.ln / oldIEEE754(x, y, z, Nil, u , v, w).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 log")
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
  override def sin : oldIEEE754_B = helper_taylor_function(mathFunctions.sin)
  override def cos : oldIEEE754_B = helper_taylor_function(mathFunctions.cos)
  override def tan : oldIEEE754_B = helper_taylor_function(mathFunctions.tan)
  override def cot : oldIEEE754_B = helper_taylor_function(mathFunctions.cot)
  override def sec : oldIEEE754_B = helper_taylor_function(mathFunctions.sec)
  override def csc : oldIEEE754_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : oldIEEE754_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : oldIEEE754_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : oldIEEE754_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : oldIEEE754_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : oldIEEE754_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : oldIEEE754_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : oldIEEE754_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : oldIEEE754_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : oldIEEE754_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : oldIEEE754_B = helper_taylor_function(mathFunctions.coth)
  override def sech : oldIEEE754_B = helper_taylor_function(mathFunctions.sech)
  override def csch : oldIEEE754_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : oldIEEE754_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : oldIEEE754_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : oldIEEE754_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : oldIEEE754_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : oldIEEE754_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : oldIEEE754_B = helper_taylor_function(mathFunctions.arccsch)
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
    val exponentWithoutBias = this.exponent - oldIEEE754.minimum_exponent(exponent_size) - (if(this.mantissa.value.bitLength < (fraction_size + 1)) IntegerNumber(1) else IntegerNumber(0))
    val binaryExponent : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_size, exponentWithoutBias.value.binaryEncode))
    val mantissaWithoutHiddenBit = this.mantissa.binaryEncode take fraction_size
    val binarymantissa : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(fraction_size, mantissaWithoutHiddenBit))
    val binaryValue : String = (if (this.sign) "1" else "0") + binaryExponent + binarymantissa
    binaryValue
  }
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : oldIEEE754_B = {
    val precision : Int = if(exponent_size >= 8) 30 else if (exponent_size >= 5) 12 else if(exponent_size >= 4) 7 else 5
    val result = func(
      oldIEEE754(0.0d, exponent_size, fraction_size, rounding),
      oldIEEE754(1.0d, exponent_size, fraction_size, rounding)) (this,
      oldIEEE754(precision.toDouble, exponent_size, fraction_size, rounding))
    result match {
      case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
      case oldIEEE754(x, y, z, u , v, w) => oldIEEE754(x, y, z, Nil, u , v, w)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 helper_taylor_function")
    }
  }
}

class oldIEEE754_NR(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends oldIEEE754_B {
  override val sign : Boolean = false;
  override val exponent : IntegerNumber_B = IntegerNumber_NR
  override val mantissa : NaturalNumber_B = NaturalNumber_NR
  override val exponent_size : Int = exponent_size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
}

object oldIEEE754_NR {
  def apply(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = new oldIEEE754_NR(exponent_size_c, fraction_size_c, rounding_c)
  def unapply(input : oldIEEE754_B) = if(input.mantissa.equals(NaturalNumber_NR)) Some(input.exponent_size, input.fraction_size, input.rounding) else None
}

class oldIEEE754_INF(sign_c : Boolean, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends oldIEEE754_B {
  override val sign : Boolean = sign_c;
  override val exponent : IntegerNumber_B = oldIEEE754.maximum_exponent(exponent_size_c)
  override val mantissa : NaturalNumber_B = NaturalNumber(1) << fraction_size_c
  override val exponent_size : Int = exponent_size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c

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
  +INF + (-INF) = NR
  */
  override def +(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => if(x != this.sign) oldIEEE754_NR(exponent_size, fraction_size, rounding) else this
    case oldIEEE754(_, _, _, _ , _, _) => this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754_INF +")
  }
  /*
  +INF - +INF = NR
  -INF - -INF = NR
  */
  override def -(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => if(x == this.sign) oldIEEE754_NR(exponent_size, fraction_size, rounding) else this
    case oldIEEE754(_, _, _, _ , _, _) => this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754_INF -")
  }
  /*
  INF*INF
  */
  override def *(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => oldIEEE754_INF(x^this.sign, exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, _ , _, _) => oldIEEE754_INF(x^this.sign, exponent_size, fraction_size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754_INF *")
  }
  /*
  INF / INF = NR
  INF / x = INF
  */
  override def /(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, _ , _, _) => oldIEEE754_INF(x^this.sign, exponent_size, fraction_size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754_INF /")
  }
  /*
  sINF^-INF -> -0 or +0
  sINF^INF=INF
  */
  override def pow(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => if(x)
                                          if(this.sign)
                                            oldIEEE754.negativeZero(exponent_size, fraction_size, rounding)
                                          else
                                            oldIEEE754.positiveZero(exponent_size, fraction_size, rounding)
                                        else oldIEEE754.infinite(false, exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, u , v, w) => {
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
        val fractional_value = oldIEEE754(x, y, z, Nil, u , v, w) - oldIEEE754(integer_value.toDouble, u , v, w)
        //Y == 0 INF^0 = 1
        if( that == oldIEEE754.zero(exponent_size, fraction_size, rounding) )
            oldIEEE754.one(exponent_size, fraction_size, rounding)
        else {
          //INF^Y Y>0 = INF
          if(this.sign==false) this
          //-INF^Y Y>0 -> +INF or -INF
          else {
            if(fractional_value == oldIEEE754.zero(exponent_size, fraction_size, rounding)) {
              if( (integer_value % 2 == 0) ) oldIEEE754.infinite(false, exponent_size, fraction_size, rounding)
              else this
            } else oldIEEE754_NR(exponent_size, fraction_size, rounding)
          }
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 pow")
  }
  override def unary_- : oldIEEE754_B = oldIEEE754.infinite(!this.sign, exponent_size, fraction_size, rounding)
  /*
  1/-INF=-0
  1/+INF=+0
  */
  override def inverse : oldIEEE754_B = if(this.sign) oldIEEE754.negativeZero(exponent_size, fraction_size, rounding)
                                          else oldIEEE754.positiveZero(exponent_size, fraction_size, rounding)
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
    case oldIEEE754_NR(_, _,_) => false
    case oldIEEE754_INF(x, _, _,_) => if(x) false else !this.sign
    case oldIEEE754(x, y, z, _ , _, _) => !this.sign
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754_INF <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case oldIEEE754_NR(_, _,_) => false
    case oldIEEE754_INF(x, _, _,_) => x == this.sign
    case oldIEEE754(x, y, z, _ , _, _) => false
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754_INF ==")
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
  override def min(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => if(this.sign==x)
                                          this
                                        else
                                          if(this.sign==true)
                                            this
                                          else
                                            oldIEEE754.infinite(true, exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, _ , _, _) => if (this<that) this else oldIEEE754(x, y,z, Nil, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 min")
  }
  override def max(that: NumberRepresentationSystem): oldIEEE754_B = that match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => if(this.sign==x)
                                          this
                                        else
                                          if(this.sign==true)
                                            oldIEEE754.infinite(false, exponent_size, fraction_size, rounding)
                                          else
                                            this
    case oldIEEE754(x, y, z, _ , _, _) => if (this>that) this else oldIEEE754(x, y,z, Nil, this.exponent_size, this.fraction_size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 max")
  }
  override def abs: oldIEEE754_B = oldIEEE754.infinite(false, exponent_size, fraction_size, rounding)
  override def signum: oldIEEE754_B = if(this.sign)
                                          oldIEEE754.negativeOne(this.exponent_size, this.fraction_size, this.rounding)
                                        else
                                          oldIEEE754.positiveOne(this.exponent_size, this.fraction_size, this.rounding)
  override def nth_root (n : Int) : (oldIEEE754_B, oldIEEE754_B) = (this, this)
  override def nqrt (n : Int) : oldIEEE754_B = if(this.sign & n%2==0)
                                                    oldIEEE754_NR(exponent_size, fraction_size, rounding)
                                                 else
                                                    this
  override def sqrt: oldIEEE754_B = this.nqrt(2)
  override def exp : oldIEEE754_B = if(this.sign) oldIEEE754.zero(exponent_size, fraction_size, rounding)
                                      else oldIEEE754.infinite(false, exponent_size, fraction_size, rounding)
  /*
  ln(+INF)=+INF
  ln(-INF)=-INF
  */
  override def ln : oldIEEE754_B = if(this.sign == false) 
                                      oldIEEE754.infinite(false, exponent_size, fraction_size, rounding)
                                     else
                                      oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def log(base: NumberRepresentationSystem) : oldIEEE754_B = base match {
    case oldIEEE754_NR(_, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754_INF(x, _, _,_) => oldIEEE754_NR(exponent_size, fraction_size, rounding)
    case oldIEEE754(x, y, z, u , v, w) => this.ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754_INF log")
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
  override def sin : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def cos : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def tan : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def cot : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def sec : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def csc : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def arccos : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def arctan : oldIEEE754_B = helper_taylor_function(mathFunctions.arctan) //TODO: pi/2
  override def arccot : oldIEEE754_B = if(this.sign) oldIEEE754.negativeZero(exponent_size, fraction_size, rounding)
                                         else oldIEEE754.positiveZero(exponent_size, fraction_size, rounding)
  override def arcsec : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def arccsc : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : oldIEEE754_B = this
  override def cosh : oldIEEE754_B = oldIEEE754.infinite(false, exponent_size, fraction_size, rounding)
  override def tanh : oldIEEE754_B = if(this.sign) oldIEEE754.negativeOne(exponent_size, fraction_size, rounding)
                                       else oldIEEE754.positiveOne(exponent_size, fraction_size, rounding)
  override def coth : oldIEEE754_B = if(this.sign) oldIEEE754.negativeOne(exponent_size, fraction_size, rounding)
                                       else oldIEEE754.positiveOne(exponent_size, fraction_size, rounding)
  override def sech : oldIEEE754_B = oldIEEE754.zero(exponent_size, fraction_size, rounding)
  override def csch : oldIEEE754_B = if(this.sign) oldIEEE754.negativeZero(exponent_size, fraction_size, rounding)
                                       else oldIEEE754.positiveZero(exponent_size, fraction_size, rounding)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : oldIEEE754_B = this
  override def arccosh : oldIEEE754_B = if(this.sign) oldIEEE754_NR(exponent_size, fraction_size, rounding) else this
  override def arctanh : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def arccoth : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def arcsech : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
  override def arccsch : oldIEEE754_B = oldIEEE754_NR(exponent_size, fraction_size, rounding)
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
  override def toRationalNumber : RationalNumber_B = RationalNumber_NR
  override def toBigDecimal : BigDecimal = this.toRationalNumber.toBigDecimal
  /*
  Show functions
  toString
  toInternalString
  */
  override def toString : String = (if(sign) "-" else "+") + "INF"
  override def toBinaryString: String = (if(sign) "1" else "0") + ("1" * exponent_size) + ("0" * fraction_size)
}

object oldIEEE754_INF {
  def apply(sign_c : Boolean, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = new oldIEEE754_INF(sign_c, exponent_size_c, fraction_size_c, rounding_c)
  def unapply(input : oldIEEE754_B) = if(input.mantissa.equals(NaturalNumber_NR)) Some(input.sign, input.exponent_size, input.fraction_size, input.rounding) else None
}

class oldIEEE754(sign_c : Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends oldIEEE754_B {
  override val sign : Boolean = sign_c;
  override val exponent : IntegerNumber_B = exponent_c
  override val mantissa : NaturalNumber_B = mantissa_c
  override val exponent_size : Int = exponent_size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c
}


object oldIEEE754 {
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
  def minimum_exponent(exponent_size_c : Int): IntegerNumber_B = {
    IntegerNumber(false, NaturalNumber(0)) - IntegerNumber(false, ( (NaturalNumber(1) << (exponent_size_c - 1)) - NaturalNumber(1) ) )
  }
  def maximum_exponent(exponent_size_c : Int): IntegerNumber_B = {
    IntegerNumber(false, (NaturalNumber(1) << exponent_size_c)-NaturalNumber(1)) - IntegerNumber(false, ( (NaturalNumber(1) << (exponent_size_c - 1)) - NaturalNumber(1) ) )
  }

  /*
  Rounding has effect only on mantissa
  ro.upb.nrs.sl.RoundUp -> if positive and (g|r|s) than mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundDown -> if negative and (g|r|s) than mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundZero -> mantissa
  ro.upb.nrs.sl.RoundAwayZero -> if (g|r|s) mantissa + 1 else mantissa
  ro.upb.nrs.sl.RoundEven -> if (g and (l|r|s) ) mantissa + 1 else mantissa
  */
  def roundingLGRS(sign_c: Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B,
  exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType,
  l : Boolean, g  : Boolean, r : Boolean, s : Boolean): oldIEEE754_B = rounding_c match {
      case RoundUp => if(!sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case RoundDown => if(sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case RoundZero => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case RoundAwayZero => if(g|r|s) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case RoundEven => if(g&(r|s|l)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
      case _ => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c) 
  }

  @tailrec
  def apply(sign_c: Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, rest_bits: List[Boolean], exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = {
      (exponent_c, mantissa_c) match {
        case (IntegerNumber_NR(_), _)  => oldIEEE754_NR(exponent_size_c, fraction_size_c, rounding_c)
        case (_, NaturalNumber_NR(_))  => oldIEEE754_NR(exponent_size_c, fraction_size_c, rounding_c)
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
                                              // we can have prositive zero and negative zero
                                              new oldIEEE754(
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
                                                case RoundUp => if(!sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                case RoundDown => if(sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                case RoundZero => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                case RoundAwayZero => if(g|r|s) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                case RoundEven => if(g&(r|s|l)) this.apply(sign_c, exponent_c, mantissa_c + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c) else this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                case _ => this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size_c, fraction_size_c, rounding_c) 
                                            }
                                            }
                                        }
                                    } else {
                                        /*
                                        if the exponent is in range and we have a normal float number
                                        */
                                        if( 
                                          ( exponent_c > minimum_exponent(exponent_size_c) ) && 
                                          ( exponent_c < maximum_exponent(exponent_size_c) ) ) {
                                          
                                          /*
                                          if mantissa / 2^fraction_size >= 2
                                          we divide mantissa by 2 (shifting right with 1) and increase the exponent by one
                                          (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) =
                                          = (-1)^sign * 2^exponent * ( ( 2 * ( mantissa / 2 ) ) / 2^fraction_size )
                                          = (-1)^sign * 2^(exponent + 1) * ( ( mantissa / 2 ) / 2^fraction_size )
                                          */
                                          if(mantissa_c.binaryEncode.length > (fraction_size_c + 1)) 
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
                                          else if (mantissa_c.binaryEncode.length < (fraction_size_c + 1)) 
                                              if(rest_bits != Nil)
                                                  this.apply(
                                                    sign_c,
                                                    exponent_c - IntegerNumber(1), 
                                                    (mantissa_c << 1) + (if(rest_bits.head) NaturalNumber(1) else NaturalNumber(0)), //add the first bit of rest bits
                                                    rest_bits.tail,
                                                    exponent_size_c, fraction_size_c, rounding_c
                                                  )
                                              else
                                                this.apply(
                                                  sign_c,
                                                  exponent_c - IntegerNumber(1), 
                                                  (mantissa_c << 1),
                                                  Nil,
                                                  exponent_size_c, fraction_size_c, rounding_c
                                                )
                                          else {
                                            /*
                                            if mantissa is in the right range
                                            if there are no rest bits the exponent is for sure in right range and sizes
                                            because it is bigger than minimum value and smaller than maximum value for exponent
                                            mantissa is in right size because is in the right range
                                            If we have rest bits than we calculate l g r s
                                            and apply the rounding to mantissa
                                            */
                                            val l = mantissa_c.binaryEncode.head
                                            rest_bits match {
                                                case Nil => {
                                                    new oldIEEE754(
                                                      sign_c,
                                                      exponent_c,
                                                      mantissa_c,
                                                      exponent_size_c, fraction_size_c, rounding_c
                                                    )
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
                                        } else if(exponent_c >= maximum_exponent(exponent_size_c)) {
                                            //if exponent is bigger than maximum value than it is a INF
                                            oldIEEE754_INF(sign_c, exponent_size_c, fraction_size_c, rounding_c)
                                        } else {
                                            /*if the exponent is smaller than minimum exponent than is
                                            a subnormal number. if don't have the minimum exponent we transform
                                            it to minimum exponent and the absolute differnce is taken from mantissa
                                            the rest bits will be affected.
                                            (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) =
                                            = (-1)^sign * 2^(minimum_exponent + exponent - minimum_exponent) * ( mantissa / 2^fraction_size )
                                            = (-1)^sign * 2^minimum_exponent * ( 2^(exponent - minimum_exponent) * mantissa / 2^fraction_size )
                                            = (-1)^sign * 2^minimum_exponent * ( ( mantissa / 2^(minimum_exponent - exponent) ) / 2^fraction_size )
                                            = (-1)^sign * 2^minimum_exponent * ( ( mantissa >> (minimum_exponent - exponent) ) / 2^fraction_size )
                                            */
                                            if(exponent_c <  minimum_exponent(exponent_size_c)) {
                                              val exponent_diff : Int = (minimum_exponent(exponent_size_c) - exponent_c).toInt
                                              /*
                                              if the exponent is bigger than fraction_size_c + 1 than we have to add zeroes as rest bits
                                              MSB
                                              */
                                              val new_rest_bits : List[Boolean] = (
                                                                  if(exponent_diff > (fraction_size_c + 1) ) 
                                                                    List.fill(exponent_diff - (fraction_size_c + 1))(false) ++ mantissa_c.binaryEncode.take(fraction_size_c + 1).reverse
                                                                  else 
                                                                    mantissa_c.binaryEncode.take(exponent_diff).reverse
                                                                  ) ++
                                                                  rest_bits
                                              this.apply(
                                                sign_c,
                                                minimum_exponent(exponent_size_c),
                                                mantissa_c >> exponent_diff,
                                                new_rest_bits,
                                                exponent_size_c, fraction_size_c, rounding_c
                                              )
                                            } else {
                                              //if exponent_c is minimum_exponent(exponent_size_c)
                                              /*
                                              if mantissa / 2^fraction_size >= 2
                                              we divide mantissa by 2 (shifting right with 1) and increase the exponent by one
                                              (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) =
                                              = (-1)^sign * 2^exponent * ( ( 2 * ( mantissa / 2 ) ) / 2^fraction_size )
                                              = (-1)^sign * 2^(exponent + 1) * ( ( mantissa / 2 ) / 2^fraction_size )
                                              HERE is not in the good case and we have to transfer it upper to normal floats
                                              */
                                              if(mantissa_c.binaryEncode.length > (fraction_size_c + 1)) 
                                                this.apply(
                                                  sign_c,
                                                  exponent_c + IntegerNumber(1),
                                                  mantissa_c >> 1,
                                                  mantissa_c.binaryEncode.head :: rest_bits, //add the eliminated bit to rest bits
                                                  exponent_size_c, fraction_size_c, rounding_c
                                                )
                                              else if(mantissa_c.binaryEncode.length == (fraction_size_c + 1)) {
                                                /*
                                                MEGA SPECIAL CASE when 2^minimum_exponent * mantissa and mantis in [1,2)
                                                we have to decrease mantissa to [0, 1) adn after the exponent will be increased
                                                for the cases where there are no rounding we have to return the new subnormal float
                                                the exponent will be minimum_exponent+1 and new mantissa will be old mantissa shifted right with one
                                                */
                                                val new_mantissa = mantissa_c >> 1
                                                val l = new_mantissa.binaryEncode.head
                                                val g = mantissa_c.binaryEncode.head
                                                val r = rest_bits match {case Nil => false case _ => rest_bits.head}
                                                val s = rest_bits match {case Nil => false case y::ys => ys match {case Nil => false case _ => ys.foldLeft(false)((a,b)=>(a|b))}}

                                                rounding_c match {
                                                    case RoundUp => if(!sign_c & (g|r|s)) 
                                                                      this.apply(sign_c, exponent_c + IntegerNumber(1), new_mantissa + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                                    else
                                                                       new oldIEEE754(
                                                                        sign_c,
                                                                        exponent_c + IntegerNumber(1),
                                                                        new_mantissa,
                                                                        exponent_size_c, fraction_size_c, rounding_c
                                                                      )
                                                    case RoundDown => if(sign_c & (g|r|s))
                                                                        this.apply(sign_c, exponent_c + IntegerNumber(1), new_mantissa + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                                      else
                                                                        new oldIEEE754(
                                                                          sign_c,
                                                                          exponent_c + IntegerNumber(1),
                                                                          new_mantissa,
                                                                          exponent_size_c, fraction_size_c, rounding_c
                                                                        )
                                                    case RoundZero => new oldIEEE754(
                                                                            sign_c,
                                                                            exponent_c + IntegerNumber(1),
                                                                            new_mantissa,
                                                                            exponent_size_c, fraction_size_c, rounding_c
                                                                          )
                                                    case RoundAwayZero => if(g|r|s)
                                                                            this.apply(sign_c, exponent_c + IntegerNumber(1), new_mantissa + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                                          else
                                                                            new oldIEEE754(
                                                                              sign_c,
                                                                              exponent_c + IntegerNumber(1),
                                                                              new_mantissa,
                                                                              exponent_size_c, fraction_size_c, rounding_c
                                                                            )
                                                    case RoundEven => if(g&(r|s|l))
                                                                        this.apply(sign_c, exponent_c + IntegerNumber(1), new_mantissa + NaturalNumber(1), Nil, exponent_size_c, fraction_size_c, rounding_c)
                                                                      else
                                                                        new oldIEEE754(
                                                                              sign_c,
                                                                              exponent_c + IntegerNumber(1),
                                                                              new_mantissa,
                                                                              exponent_size_c, fraction_size_c, rounding_c
                                                                            )
                                                    case _ => new oldIEEE754(
                                                                    sign_c,
                                                                    exponent_c + IntegerNumber(1),
                                                                    new_mantissa,
                                                                    exponent_size_c, fraction_size_c, rounding_c
                                                                  )
                                                }
                                              } else {
                                                /*
                                                in this case of subnormals mantissa can be as small as it can be
                                                we don't have a lower limit mantissa is in [0,1)
                                                If we have rest bits it needs rouding
                                                we have to increase the exponent by 1 tor espect the subnormal format
                                                so exponent will be minimum_exponent + 1 and the new mantissa will
                                                be divided by two 
                                                */
                                                
                                                val new_mantissa = mantissa_c >> 1
                                                val l = new_mantissa.binaryEncode.head
                                                val g = mantissa_c.binaryEncode.head
                                                val r = rest_bits match {case Nil => false case _ => rest_bits.head}
                                                val s = rest_bits match {case Nil => false case y::ys => ys match {case Nil => false case _ => ys.foldLeft(false)((a,b)=>(a|b))}}
                                                rounding_c match {
                                                    case RoundUp => if(!sign_c & (g|r|s)) 
                                                                      new oldIEEE754(
                                                                        sign_c,
                                                                        exponent_c + IntegerNumber(1),
                                                                        new_mantissa + NaturalNumber(1),
                                                                        exponent_size_c, fraction_size_c, rounding_c
                                                                      )
                                                                    else
                                                                       new oldIEEE754(
                                                                        sign_c,
                                                                        exponent_c + IntegerNumber(1),
                                                                        new_mantissa,
                                                                        exponent_size_c, fraction_size_c, rounding_c
                                                                      )
                                                    case RoundDown => if(sign_c & (g|r|s))
                                                                        new oldIEEE754(
                                                                          sign_c,
                                                                          exponent_c + IntegerNumber(1),
                                                                          new_mantissa + NaturalNumber(1),
                                                                          exponent_size_c, fraction_size_c, rounding_c
                                                                        )
                                                                      else
                                                                        new oldIEEE754(
                                                                          sign_c,
                                                                          exponent_c + IntegerNumber(1),
                                                                          new_mantissa,
                                                                          exponent_size_c, fraction_size_c, rounding_c
                                                                        )
                                                    case RoundZero => new oldIEEE754(
                                                                            sign_c,
                                                                            exponent_c + IntegerNumber(1),
                                                                            new_mantissa,
                                                                            exponent_size_c, fraction_size_c, rounding_c
                                                                          )
                                                    case RoundAwayZero => if(g|r|s)
                                                                            new oldIEEE754(
                                                                              sign_c,
                                                                              exponent_c + IntegerNumber(1),
                                                                              new_mantissa + NaturalNumber(1),
                                                                              exponent_size_c, fraction_size_c, rounding_c
                                                                            )
                                                                          else
                                                                            new oldIEEE754(
                                                                              sign_c,
                                                                              exponent_c + IntegerNumber(1),
                                                                              new_mantissa,
                                                                              exponent_size_c, fraction_size_c, rounding_c
                                                                            )
                                                    case RoundEven => if(g&(r|s|l))
                                                                        new oldIEEE754(
                                                                          sign_c,
                                                                          exponent_c + IntegerNumber(1),
                                                                          new_mantissa + NaturalNumber(1),
                                                                          exponent_size_c, fraction_size_c, rounding_c
                                                                        )
                                                                      else
                                                                        new oldIEEE754(
                                                                              sign_c,
                                                                              exponent_c + IntegerNumber(1),
                                                                              new_mantissa,
                                                                              exponent_size_c, fraction_size_c, rounding_c
                                                                            )
                                                    case _ => new oldIEEE754(
                                                                    sign_c,
                                                                    exponent_c + IntegerNumber(1),
                                                                    new_mantissa,
                                                                    exponent_size_c, fraction_size_c, rounding_c
                                                                  )
                                                }
                                              }
                                            }
                                        }
                                     }
        case _=> throw new NotImplementedError("ro.upb.nrs.sl.oldIEEE754 apply")
      }
  }

  
  def apply(sign_c: Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = {
      val exponent_size : Int = 4 * math.log(size_c.toDouble).toInt - 13 //TODO: for size less than 64
      val fraction_size : Int = size_c-exponent_size
      this.apply(sign_c, exponent_c, mantissa_c, Nil, exponent_size, fraction_size, rounding_c)
  }
  
  def apply(a : Double, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = {
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
      this.apply(negative, IntegerNumber(exponent), NaturalNumber(mantissa) >> fraction_diff, NaturalNumber(mantissa).binaryEncode.take(fraction_diff).reverse, exponent_size_c, fraction_size_c, rounding_c)
    else
      this.apply(negative, IntegerNumber(exponent), NaturalNumber(mantissa) << (-fraction_diff), Nil, exponent_size_c, fraction_size_c, rounding_c)
  }
  def apply(a : Int, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Int) : oldIEEE754_B = this.apply(a.toDouble, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Long, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Long) : oldIEEE754_B = this.apply(a.toDouble, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Double) : oldIEEE754_B = this.apply(a, this.default_exponent_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Float, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.apply(a.toDouble, exponent_size_c, fraction_size_c, rounding_c)
  def apply(a : Float) : oldIEEE754_B = this.apply(a.toDouble)

  def apply(binaryString : String, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = {
    //special cases
    val positiveZeroBinaryString = auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c + 1)(false) )
    val negativeZeroBinaryString = "1" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c)(false) )
    val nrBinaryString = "1" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(fraction_size_c + exponent_size_c)(false) )
    if(binaryString == positiveZeroBinaryString) {
      new oldIEEE754(
        false,
        minimum_exponent(exponent_size_c),
        NaturalNumber(0), //zero mantissa
        exponent_size_c, fraction_size_c, rounding_c
      )
    } else if(binaryString == negativeZeroBinaryString) {
        new oldIEEE754(
        true,
        minimum_exponent(exponent_size_c),
        NaturalNumber(0), //zero mantissa
        exponent_size_c, fraction_size_c, rounding_c
      )
    } /*else if(binaryString == nrBinaryString) {
        new oldIEEE754_NR(exponent_size_c, fraction_size_c, rounding_c)
    }*/ else {
      val exponentString :  String = "0" + ((binaryString drop 1) take exponent_size_c)
      val mantissaString : String = "0" + (binaryString drop ( 1 + exponent_size_c ))
      val binaryExponent : IntegerNumber_B = IntegerNumber(false, NaturalNumber(BigInt(exponentString, 2)))
      val binaryMantissaWihoutHiddenBit : NaturalNumber_B = NaturalNumber(BigInt(mantissaString, 2))
      val binaryMantissa : NaturalNumber_B = binaryMantissaWihoutHiddenBit + ( if(binaryExponent == IntegerNumber(0)) NaturalNumber(0) else NaturalNumber(1) << fraction_size_c )
      if( (binaryExponent == IntegerNumber(0)) && (binaryMantissa != NaturalNumber(0)) ) //subnormal
        this.apply(binaryString(0) == '1', binaryExponent + minimum_exponent(exponent_size_c) + IntegerNumber(1), binaryMantissa, Nil, exponent_size_c, fraction_size_c, rounding_c )
      else
        this.apply(binaryString(0) == '1', binaryExponent + minimum_exponent(exponent_size_c), binaryMantissa, Nil, exponent_size_c, fraction_size_c, rounding_c )
    }
  }

  def unapply(input : oldIEEE754_B) = Some(input.sign, input.exponent, input.mantissa, input.exponent_size, input.fraction_size, input.rounding)

  def positiveOne(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.apply(false, IntegerNumber(0), NaturalNumber(1) << fraction_size_c, Nil, exponent_size_c, fraction_size_c, rounding_c)

  def negativeOne(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.apply(false, IntegerNumber(0), NaturalNumber(1) << fraction_size_c, Nil, exponent_size_c, fraction_size_c, rounding_c)

  def one(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.positiveOne(exponent_size_c, fraction_size_c, rounding_c)

  def positiveZero(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.apply(false, minimum_exponent(exponent_size_c), NaturalNumber(0), Nil, exponent_size_c, fraction_size_c, rounding_c)

  def negativeZero(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.apply(true, minimum_exponent(exponent_size_c), NaturalNumber(0), Nil, exponent_size_c, fraction_size_c, rounding_c)

  def zero(exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = this.positiveZero(exponent_size_c, fraction_size_c, rounding_c)

  def infinite(sign_c : Boolean, exponent_size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : oldIEEE754_B = oldIEEE754_INF(sign_c, exponent_size_c, fraction_size_c, rounding_c)
}
