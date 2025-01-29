package ro.upb.nrs.sl



import scala.annotation.tailrec

/*
We consider FloatingPoint
(-1)^sign * 2^exponent * ( mantissa / 2^fraction_size )
mantissa / 2^fraction_size in [1, 2) for normal floats and [0, 1) for subnormal
fraction_size is fixed
mantissa has the hiddent bit with the value 1
*/
abstract class FloatingPoint_B extends NumberRepresentationSystem {
  /*
  sign
  exponent in Z
  mantissa in N (with hidden bit)
  size of the mantissa (without the hidden bit)
  */
  val sign : Boolean
  val exponent : IntegerNumber_B
  val mantissa : NaturalNumber_B
  val rest_bits : List[Boolean]
  val fraction_size : Int
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
  override def +(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => FloatingPoint_INF(x, fraction_size)
    case that : FloatingPoint_B => {
      if(this.mantissa == NaturalNumber(0)) {
        if(that.mantissa == NaturalNumber(0)) {
          if(that.sign) {
            this
          } else {
            that
          }
        } else {
          that
        }
      } else if(that.mantissa == NaturalNumber(0)) this
      else {
        if(that.sign != this.sign) this - (-that)
        else if(that.abs <= this.abs) {
          val exponent_diff = (this.exponent - that.exponent).toInt
          val rest_value = that.mantissa.value & ((BigInt(1) << exponent_diff)-1)
          val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(rest_value).binaryEncode).reverse
          val new_mantissa = this.mantissa + (that.mantissa >> exponent_diff)
          FloatingPoint(this.sign, this.exponent,
            new_mantissa,
            rest_bits_computed,
            this.fraction_size
          )
        }
        else that + this
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint +")
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
  override def -(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => FloatingPoint_INF(!x, fraction_size)
    case that : FloatingPoint_B => {
      if(this.mantissa == NaturalNumber(0)) {
        if(that.mantissa == NaturalNumber(0)) {
          if(this.sign) {
            if(!that.sign) {
              this
            } else {
              -this
            }
          } else {
            this
          }
        } else {
          -that
        }
      } else if(that.mantissa == NaturalNumber(0)) this
      else if(this == that) FloatingPoint.positiveZero(this.fraction_size)
      else {
        if(that.sign != this.sign) this + (-that)
        else if(that.abs <= this.abs) {
          val exponent_diff = (this.exponent - that.exponent).toInt
          val extra_value = BigInt(1) << exponent_diff
          val rest_value = that.mantissa.value & ((BigInt(1) << exponent_diff)-1)
          val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(extra_value - rest_value).binaryEncode).reverse
          val new_mantissa = this.mantissa - (that.mantissa >> exponent_diff)
          if(rest_value == 0) {
            FloatingPoint(this.sign, this.exponent,
            new_mantissa,
            Nil,
            this.fraction_size)
          } else {
            FloatingPoint(this.sign, this.exponent,
            new_mantissa - NaturalNumber(1),
            rest_bits_computed,
            this.fraction_size)
          }
        } else {
          -(that - this)
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint -")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantissa1 / 2^fraction_size ) * (-1)^sign2 * 2^exponent2 * ( mantissa2 / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( mantissa1 * mantissa2 ) / 2^(2 * fraction_size) ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( ( mantissa1 * mantissa2 ) / 2^fraction_size ) / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) * ( ( ( mantissa1 * mantissa2 ) >> fraction_size ) / 2^fraction_size ) =
  */
  override def *(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => if(this.mantissa == NaturalNumber(0)) FloatingPoint_NR(fraction_size) else FloatingPoint_INF(x^this.sign, fraction_size)
    case that: FloatingPoint_B => {
        val prod_mantissa = this.mantissa * that.mantissa
        val rest_value = prod_mantissa.value & ((BigInt(1) << this.fraction_size)-1)
        val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(this.fraction_size, NaturalNumber(rest_value).binaryEncode).reverse
        val new_mantissa = prod_mantissa >> this.fraction_size
        FloatingPoint(this.sign^that.sign, this.exponent + that.exponent,
            new_mantissa,
            rest_bits_computed,
            this.fraction_size
        )
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint *")
  }
  def fusedMultiply(that : FloatingPoint_B) : FloatingPoint_B = {
        FloatingPoint(
            this.sign^that.sign,
            this.exponent + that.exponent,
            this.mantissa * that.mantissa,
            Nil,
            this.fraction_size + that.fraction_size
        )
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantissa1 / 2^fraction_size ) / (-1)^sign2 * 2^exponent2 * ( mantissa2 / 2^fraction_size ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( mantissa1 / mantissa2 ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( ( mantissa1 * 2^fraction_size ) / ( mantissa2 * 2^fraction_size ) ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) * ( ( ( mantissa1 << fraction_size ) /  mantissa2 ) / 2^fraction_size ) =
  */
  override def /(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => FloatingPoint.zero(this.sign^x, this.fraction_size)
    case that : FloatingPoint_B => if(that.mantissa != NaturalNumber(0)) {
        // we add two extra bits to find g and r
        val eq = (this.mantissa<<(this.fraction_size + 2)) / that.mantissa
        // for finding s value
        val er = (this.mantissa<<(this.fraction_size + 2)) % that.mantissa
        val g  = eq.value.testBit(1)
        val r  = eq.value.testBit(0)
        val s = !(er==NaturalNumber(0))
        // eliminate the extra bits
        val new_q = eq >> 2
        val l = new_q.value.testBit(0)
        FloatingPoint(this.sign^that.sign, this.exponent - that.exponent,
            new_q,
            g::r::s::Nil,
            this.fraction_size
        )
    } else {
      if(this.mantissa == NaturalNumber(0)) FloatingPoint_NR(fraction_size) else FloatingPoint_INF(this.sign^that.sign, fraction_size)
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint /")
  }
  override def /\(that: NumberRepresentationSystem): FloatingPoint_B = this / that
  override def %(that: NumberRepresentationSystem): FloatingPoint_B = throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint %")
  /*
  X^Y = X^([Y]+{Y}) where [Y] is integer part of Y and {Y} is fractional part of Y
  X^(-Y) = 1 / X^Y
  */
  override def pow(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => if(x) this.inverse.pow(-that)
                                        else {
                                          if(this.exponent >= IntegerNumber(0))  FloatingPoint_INF(false, fraction_size) //X^INF x>>1
                                          else if ( (this.exponent == IntegerNumber(0)) && (this.mantissa==(NaturalNumber(1)<<fraction_size)) ) FloatingPoint.one(this.fraction_size)
                                          else FloatingPoint.zero(this.sign, this.fraction_size)
                                        }
    case that : FloatingPoint_B => {
      /*
      if that is negative
      X^(-Y) = 1 / X^Y
      */
      if(that.sign)
        this.inverse.pow(-that)
      else {
        //[Y] because Y is positive [Y] is positive
        val integer_value = that.toBigInt
        //{Y} in [0, 1)
        val fractional_value = that - FloatingPoint(integer_value.toDouble, that.fraction_size)
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
                        FloatingPoint.one(fraction_size)) (this.abs,
                        integer_value
                      )
        //X^2^exponent //exponent is negative so X^(1/2^exponent.abs)
        val x_exponentY = mathFunctions.pow_fractional(
                          FloatingPoint.one(fraction_size)) (this.abs ,
                          BigInt(1), fractional_value.exponent.abs.toInt
                        )
        //(X^(2^exponent))^( fraction / 2^fraction_size )
        val x_exponentY_fraction = mathFunctions.pow_fractional(
                        FloatingPoint.one(fraction_size)) (
                        x_exponentY,
                        fractional_value.mantissa.value, fractional_value.fraction_size
                      )
        //if fractional value is zero
        // result = X^[Y]
        // else if normal float fractional result = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        // else is subnormal float fractional  result = X^[Y] * (X^(2^exponent))^( fraction / 2^fraction_size )
        // if the current number is negative we only do pow for integer numbers
        val result = if( (fractional_value == FloatingPoint.positiveZero(fraction_size))
                        || (fractional_value == FloatingPoint.negativeZero(fraction_size)))
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
                        FloatingPoint_NR(fraction_size)
        result match {
          case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
          case that : FloatingPoint_B => that
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint pow math")
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint pow")
  }
  override def unary_- : FloatingPoint_B = FloatingPoint(!this.sign, this.exponent, this.mantissa, this.rest_bits, this.fraction_size)
  override def inverse : FloatingPoint_B = FloatingPoint.one(this.fraction_size) / this
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
    case FloatingPoint_NR(_) => false
    case FloatingPoint_INF(x, _) => if(x) false else true
    case that : FloatingPoint_B => {
      if(that.mantissa == NaturalNumber(0)) {
        if(this.mantissa == NaturalNumber(0)) false
        else this.sign
      } else if(this.mantissa == NaturalNumber(0)) {
        !that.sign
      } else {
        if(this.sign != that.sign) this.sign
        else if(this.exponent < that.exponent) !this.sign
        else if(this.exponent > that.exponent) this.sign
        else if(this.mantissa < that.mantissa) !this.sign
        else if(this.mantissa > that.mantissa) this.sign
        else false
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case FloatingPoint_NR(_) => false
    case FloatingPoint_INF(x, _) => false
    //special case for mantissa 0 when you can have +0 or -0
    case that: FloatingPoint_B => (this.mantissa == that.mantissa) && ( ((this.sign == that.sign) && (this.exponent == that.exponent)) || (this.mantissa == NaturalNumber(0)) )
    /* (this.sign == x) && (this.exponent == y) && (this.mantissa == z) ||
                                           ( (this.mantissa == z) && (this.mantissa == NaturalNumber(0)) ) // -0 == +0
                                           */
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint ==")
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
  override def min(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case that : FloatingPoint_B => if (this<that) this else that
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint min")
  }
  override def max(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case that : FloatingPoint_B => if (this>that) this else that
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint max")
  }
  override def abs: FloatingPoint_B = FloatingPoint(false, this.exponent, this.mantissa, Nil, this.fraction_size)
  override def signum: FloatingPoint_B = {
    if(this.mantissa == NaturalNumber(0)) this
    else {
        if(this.sign) FloatingPoint.negativeOne(this.fraction_size)
        else FloatingPoint.positiveOne(this.fraction_size)
    }
  }
  override def nth_root (n : Int) : (FloatingPoint_B, FloatingPoint_B) = {
    val root = this.nqrt(n)
    (root, this-root.pow(FloatingPoint(n, this.fraction_size)))
  }
  /*
  NQRT( (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) ) =
  = NQRT( (-1)^sign ) * NQRT( 2^( ( n * [exponent / n] ) + ( exponent % n ) ) * ( ( mantissa * 2^( (n - 1) * fraction_size ) ) / 2^( ( n * fraction_size ) ) ) )
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( 2^( exponent % n ) * ( mantissa * 2^( (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantissa * 2^( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantissa * << ( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  */
  override def nqrt (n : Int) : FloatingPoint_B = {
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
      else FloatingPoint_NR(fraction_size)
    }
    else FloatingPoint(this.sign, this.exponent/n_ip,
    new_q, g::r::s::Nil,
    this.fraction_size)
  }
  override def sqrt: FloatingPoint_B = this.nqrt(2)
  override def exp : FloatingPoint_B = helper_taylor_function(mathFunctions.exp)
  override def ln : FloatingPoint_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : FloatingPoint_B = base match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => FloatingPoint_NR(fraction_size)
    case that : FloatingPoint_B => this.ln / that.ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint log")
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
  override def sin : FloatingPoint_B = helper_taylor_function(mathFunctions.sin)
  override def cos : FloatingPoint_B = helper_taylor_function(mathFunctions.cos)
  override def tan : FloatingPoint_B = helper_taylor_function(mathFunctions.tan)
  override def cot : FloatingPoint_B = helper_taylor_function(mathFunctions.cot)
  override def sec : FloatingPoint_B = helper_taylor_function(mathFunctions.sec)
  override def csc : FloatingPoint_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : FloatingPoint_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : FloatingPoint_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : FloatingPoint_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : FloatingPoint_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : FloatingPoint_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : FloatingPoint_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : FloatingPoint_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : FloatingPoint_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : FloatingPoint_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : FloatingPoint_B = helper_taylor_function(mathFunctions.coth)
  override def sech : FloatingPoint_B = helper_taylor_function(mathFunctions.sech)
  override def csch : FloatingPoint_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : FloatingPoint_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : FloatingPoint_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : FloatingPoint_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : FloatingPoint_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : FloatingPoint_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : FloatingPoint_B = helper_taylor_function(mathFunctions.arccsch)
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
  def toFixedPoint(size : Int, fractionSize : Int, rounding : RoundingType) : FixedPoint_B = {
    val normalisedMantissa = if(this.fraction_size < fractionSize) 
                              this.mantissa << (fractionSize - this.fraction_size)
                             else
                              this.mantissa >> (this.fraction_size - fractionSize)
    val finalMantissa = if(this.exponent.sign)
                          normalisedMantissa >> this.exponent.value.toInt
                        else
                          normalisedMantissa << this.exponent.value.toInt
    FixedPoint(IntegerNumber(this.sign, finalMantissa), size, fractionSize, rounding)
  }
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = "sign=" + sign.toString +"exp=" + exponent.toInternalString + " mantissa=" + mantissa.toInternalString + " fraction_size=" + this.fraction_size
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : FloatingPoint_B = {
    val precision : Int = 30
    val result = func(
      FloatingPoint.positiveZero(fraction_size),
      FloatingPoint.one(fraction_size)) (this,
      FloatingPoint(precision.toDouble, fraction_size))
    result match {
      case that : FloatingPoint_B => that
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint helper_taylor_function")
    }
  }
}

class FloatingPoint_NR(fraction_size_c : Int) extends FloatingPoint_B {
  override val sign : Boolean = false;
  override val exponent : IntegerNumber_B = IntegerNumber_NR
  override val mantissa : NaturalNumber_B = NaturalNumber_NR
  override val rest_bits : List[Boolean] = Nil
  override val fraction_size : Int = fraction_size_c
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
  override def +(that: NumberRepresentationSystem): FloatingPoint_B = this
  override def -(that: NumberRepresentationSystem): FloatingPoint_B = this
  override def *(that: NumberRepresentationSystem): FloatingPoint_B = this
  override def /(that: NumberRepresentationSystem): FloatingPoint_B = this
  override def pow(that: NumberRepresentationSystem): FloatingPoint_B = this
  override def unary_- : FloatingPoint_B = this
  override def inverse : FloatingPoint_B = this
  /*
  logical operation (ordering)
  less
  equal
  not equal
  greater
  greater or equal
  less or equal
  */
  override def <(that: NumberRepresentationSystem): Boolean = false
  override def ==(that: NumberRepresentationSystem): Boolean = false
  override def >=(that: NumberRepresentationSystem): Boolean = false
  override def !=(that: NumberRepresentationSystem): Boolean = false
  override def <=(that: NumberRepresentationSystem): Boolean = false
  override def >(that: NumberRepresentationSystem): Boolean = false
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
  override def min(that: NumberRepresentationSystem): FloatingPoint_B = this
  override def max(that: NumberRepresentationSystem): FloatingPoint_B = this
  override def abs: FloatingPoint_B = this
  override def signum: FloatingPoint_B = this
  override def nth_root (n : Int) : (FloatingPoint_B, FloatingPoint_B) = (this, this)
  override def nqrt (n : Int) : FloatingPoint_B = this
  override def sqrt: FloatingPoint_B = this
  override def exp : FloatingPoint_B = this
  /*
  ln(+INF)=+INF
  ln(-INF)=-INF
  */
  override def ln : FloatingPoint_B = this
  override def log(base: NumberRepresentationSystem) : FloatingPoint_B = this
  /*
  trigonometric functions
  sin
  cos
  tan
  cot
  sec
  csc
  */
  override def sin : FloatingPoint_B = this
  override def cos : FloatingPoint_B = this
  override def tan : FloatingPoint_B = this
  override def cot : FloatingPoint_B = this
  override def sec : FloatingPoint_B = this
  override def csc : FloatingPoint_B = this
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : FloatingPoint_B = this
  override def arccos : FloatingPoint_B = this
  override def arctan : FloatingPoint_B = this
  override def arccot : FloatingPoint_B = this
  override def arcsec : FloatingPoint_B = this
  override def arccsc : FloatingPoint_B = this
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : FloatingPoint_B = this
  override def cosh : FloatingPoint_B = this
  override def tanh : FloatingPoint_B = this
  override def coth : FloatingPoint_B = this
  override def sech : FloatingPoint_B = this
  override def csch : FloatingPoint_B = this
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : FloatingPoint_B = this
  override def arccosh : FloatingPoint_B = this
  override def arctanh : FloatingPoint_B = this
  override def arccoth : FloatingPoint_B = this
  override def arcsech : FloatingPoint_B = this
  override def arccsch : FloatingPoint_B = this
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
  override def toString : String = "NR"
  
}

object FloatingPoint_NR {
  def apply(fraction_size_c : Int) : FloatingPoint_B = new FloatingPoint_NR(fraction_size_c)
  def unapply(input : FloatingPoint_B) = if(input.toString == "NR") Some(input.fraction_size) else None
}

class FloatingPoint_INF(sign_c : Boolean, fraction_size_c : Int) extends FloatingPoint_B {
  override val sign : Boolean = sign_c;
  override val exponent : IntegerNumber_B = IntegerNumber_NR
  override val mantissa : NaturalNumber_B = NaturalNumber_NR
  override val rest_bits : List[Boolean] = Nil
  override val fraction_size : Int = fraction_size_c

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
  override def +(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => if(x != this.sign) FloatingPoint_NR(fraction_size) else this
    case that : FloatingPoint_B => this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint_INF +")
  }
  /*
  +INF - +INF = NR
  -INF - -INF = NR
  */
  override def -(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => if(x == this.sign) FloatingPoint_NR(fraction_size) else this
    case that : FloatingPoint_B => this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint_INF -")
  }
  /*
  INF*INF
  */
  override def *(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => FloatingPoint_INF(x^this.sign, fraction_size)
    case that : FloatingPoint_B => if(that.mantissa == NaturalNumber(0)) FloatingPoint_NR(fraction_size) else FloatingPoint_INF(that.sign^this.sign, fraction_size)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint_INF *")
  }
  /*
  INF / INF = NR
  INF / x = INF
  */
  override def /(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => FloatingPoint_NR(fraction_size)
    case that : FloatingPoint_B => FloatingPoint_INF(that.sign^this.sign, fraction_size)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint_INF /")
  }
  /*
  sINF^-INF -> -0 or +0
  sINF^INF=INF
  */
  override def pow(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => if(x)
                                          if(this.sign)
                                            FloatingPoint.negativeZero(fraction_size)
                                          else
                                            FloatingPoint.positiveZero(fraction_size)
                                        else FloatingPoint.infinite(false, fraction_size)
    case that : FloatingPoint_B => {
      /*
      if that is negative
      X^(-Y) = 1 / X^Y
      */
      if(that.sign)
        this.inverse.pow(-that)
      else {
        //[Y] because Y is positive [Y] is positive
        val integer_value = that.toBigInt
        //{Y} in [0, 1)
        val fractional_value = that - FloatingPoint(integer_value.toDouble, that.fraction_size)
        //Y == 0 INF^0 = 1
        if( (that == FloatingPoint.positiveZero(fraction_size)) ||
            (that == FloatingPoint.negativeZero(fraction_size)) )
            FloatingPoint.one(fraction_size)
        else {
          //INF^Y Y>0 = INF
          if(this.sign==false) this
          //-INF^Y Y>0 -> +INF or -INF
          else {
            if( (fractional_value == FloatingPoint.positiveZero(fraction_size)) ||
               (fractional_value == FloatingPoint.negativeZero(fraction_size)) ) {
              if( (integer_value % 2 == 0) ) FloatingPoint.infinite(false, fraction_size)
              else this
            } else FloatingPoint_NR(fraction_size)
          }
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint pow")
  }
  override def unary_- : FloatingPoint_B = FloatingPoint.infinite(!this.sign, fraction_size)
  /*
  1/-INF=-0
  1/+INF=+0
  */
  override def inverse : FloatingPoint_B = if(this.sign) FloatingPoint.negativeZero(fraction_size)
                                          else FloatingPoint.positiveZero(fraction_size)
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
    case FloatingPoint_NR(_) => false
    case FloatingPoint_INF(x, _) => if(x) false else !this.sign
    case that : FloatingPoint_B => !this.sign
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint_INF <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case FloatingPoint_NR(_) => false
    case FloatingPoint_INF(x, _) => x == this.sign
    case that : FloatingPoint_B => false
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint_INF ==")
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
  override def min(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => if(this.sign==x)
                                          this
                                        else
                                          if(this.sign==true)
                                            this
                                          else
                                            FloatingPoint.infinite(true, fraction_size)
    case that : FloatingPoint_B => if (this.sign) this else that
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint min")
  }
  override def max(that: NumberRepresentationSystem): FloatingPoint_B = that match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => if(this.sign==x)
                                          this
                                        else
                                          if(this.sign==true)
                                            FloatingPoint.infinite(false, fraction_size)
                                          else
                                            this
    case that : FloatingPoint_B => if (this.sign) that else this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint max")
  }
  override def abs: FloatingPoint_B = FloatingPoint.infinite(false, fraction_size)
  override def signum: FloatingPoint_B = if(this.sign)
                                          FloatingPoint.negativeOne(this.fraction_size)
                                        else
                                          FloatingPoint.positiveOne(this.fraction_size)
  override def nth_root (n : Int) : (FloatingPoint_B, FloatingPoint_B) = (this, this)
  override def nqrt (n : Int) : FloatingPoint_B = if(this.sign & n%2==0)
                                                    FloatingPoint_NR(fraction_size)
                                                 else
                                                    this
  override def sqrt: FloatingPoint_B = this.nqrt(2)
  override def exp : FloatingPoint_B = if(this.sign) FloatingPoint.positiveZero(fraction_size)
                                      else FloatingPoint.infinite(false, fraction_size)
  /*
  ln(+INF)=+INF
  ln(-INF)=-INF
  */
  override def ln : FloatingPoint_B = if(this.sign == false) 
                                      FloatingPoint.infinite(false, fraction_size)
                                     else
                                      FloatingPoint_NR(fraction_size)
  override def log(base: NumberRepresentationSystem) : FloatingPoint_B = base match {
    case FloatingPoint_NR(_) => FloatingPoint_NR(fraction_size)
    case FloatingPoint_INF(x, _) => FloatingPoint_NR(fraction_size)
    case that : FloatingPoint_B => this.ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint_INF log")
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
  override def sin : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def cos : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def tan : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def cot : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def sec : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def csc : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def arccos : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  //TODO: arctan
  override def arctan : FloatingPoint_B = helper_taylor_function(mathFunctions.arctan) //TODO: pi/2
  override def arccot : FloatingPoint_B = if(this.sign) FloatingPoint.negativeZero(fraction_size)
                                         else FloatingPoint.positiveZero(fraction_size)
  override def arcsec : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def arccsc : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : FloatingPoint_B = this
  override def cosh : FloatingPoint_B = FloatingPoint.infinite(false, fraction_size)
  override def tanh : FloatingPoint_B = if(this.sign) FloatingPoint.negativeOne(fraction_size)
                                       else FloatingPoint.positiveOne(fraction_size)
  override def coth : FloatingPoint_B = if(this.sign) FloatingPoint.negativeOne(fraction_size)
                                       else FloatingPoint.positiveOne(fraction_size)
  override def sech : FloatingPoint_B = FloatingPoint.positiveZero(fraction_size)
  override def csch : FloatingPoint_B = if(this.sign) FloatingPoint.negativeZero(fraction_size)
                                       else FloatingPoint.positiveZero(fraction_size)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : FloatingPoint_B = this
  override def arccosh : FloatingPoint_B = if(this.sign) FloatingPoint_NR(fraction_size) else this
  override def arctanh : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def arccoth : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def arcsech : FloatingPoint_B = FloatingPoint_NR(fraction_size)
  override def arccsch : FloatingPoint_B = FloatingPoint_NR(fraction_size)
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
}

object FloatingPoint_INF {
  def apply(sign_c : Boolean, fraction_size_c : Int) : FloatingPoint_B = new FloatingPoint_INF(sign_c, fraction_size_c)
  def unapply(input : FloatingPoint_B) = if(input.mantissa.equals(NaturalNumber_NR)) Some(input.sign, input.fraction_size) else None
}

class FloatingPoint(sign_c : Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, rest_bits_c : List[Boolean], fraction_size_c : Int) extends FloatingPoint_B {
    override val sign : Boolean = sign_c;
    override val exponent : IntegerNumber_B = exponent_c
    override val mantissa : NaturalNumber_B = mantissa_c
    override val rest_bits : List[Boolean] = rest_bits_c
    override val fraction_size : Int = fraction_size_c
}


object FloatingPoint {
  @tailrec
  def apply(sign_c: Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, rest_bits_c: List[Boolean], fraction_size_c : Int) : FloatingPoint_B = {
      (exponent_c, mantissa_c) match {
        case (IntegerNumber_NR(_), _)  => FloatingPoint_NR(fraction_size_c)
        case (_, NaturalNumber_NR(_))  => FloatingPoint_NR(fraction_size_c)
        case (_, NaturalNumber(a)) => if(NaturalNumber(a)==NaturalNumber(0)) {
                                    if(rest_bits_c != Nil)
                                          this.apply(
                                            sign_c,
                                            exponent_c - IntegerNumber(1), 
                                            (mantissa_c << 1) + (if(rest_bits_c.head) NaturalNumber(1) else NaturalNumber(0)), //add the first bit of rest bits
                                            rest_bits_c.tail,
                                            fraction_size_c
                                          )
                                    else
                                        /*
                                        if mantissa is 0
                                        */
                                        new FloatingPoint(
                                                  sign_c, // TaperedFloatingPoint has only positive zero
                                                  IntegerNumber(0),
                                                  NaturalNumber(0),
                                                  Nil,
                                                  fraction_size_c
                                                )
                                  } else {
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
                                            mantissa_c.binaryEncode.head :: rest_bits_c, //add the eliminated bit to rest bits
                                            fraction_size_c
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
                                            if(rest_bits_c != Nil)
                                                this.apply(
                                                  sign_c,
                                                  exponent_c - IntegerNumber(1), 
                                                  (mantissa_c << 1) + (if(rest_bits_c.head) NaturalNumber(1) else NaturalNumber(0)), //add the first bit of rest bits
                                                  rest_bits_c.tail,
                                                  fraction_size_c
                                                )
                                            else
                                              this.apply(
                                                sign_c,
                                                exponent_c - IntegerNumber(1), 
                                                (mantissa_c << 1),
                                                Nil,
                                                fraction_size_c
                                              )
                                        else {
                                            /*
                                            if mantissa is in the right range
                                            */
                                            new FloatingPoint(
                                              sign_c,
                                              exponent_c,
                                              mantissa_c,
                                              rest_bits_c,
                                              fraction_size_c
                                            )
                                            
                                        }
                                    }
        case _=> throw new NotImplementedError("ro.upb.nrs.sl.FloatingPoint apply")
      }
  }


  def apply(a : Double, fraction_size_c : Int) : FloatingPoint_B = {
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
      this.apply(negative, IntegerNumber(exponent), NaturalNumber(mantissa) >> fraction_diff, NaturalNumber(mantissa).binaryEncode.take(fraction_diff).reverse, fraction_size_c)
    else
      this.apply(negative, IntegerNumber(exponent), NaturalNumber(mantissa) << (-fraction_diff), Nil, fraction_size_c)
  }
  def apply(a : Int, fraction_size_c : Int) : FloatingPoint_B = this.apply(a.toDouble, fraction_size_c)
  def apply(a : Long, fraction_size_c : Int) : FloatingPoint_B = this.apply(a.toDouble, fraction_size_c)
  def apply(a : Float, fraction_size_c : Int) : FloatingPoint_B = this.apply(a.toDouble, fraction_size_c)

  def unapply(input : FloatingPoint_B) = Some(input.sign, input.exponent, input.mantissa, input.rest_bits, input.fraction_size)

  def positiveOne(fraction_size_c : Int) : FloatingPoint_B = this.apply(false, IntegerNumber(0), NaturalNumber(1) << fraction_size_c, Nil, fraction_size_c)

  def negativeOne(fraction_size_c : Int) : FloatingPoint_B = this.apply(true, IntegerNumber(0), NaturalNumber(1) << fraction_size_c, Nil, fraction_size_c)

  def one(fraction_size_c : Int) : FloatingPoint_B = this.positiveOne(fraction_size_c)

  def positiveZero(fraction_size_c : Int) : FloatingPoint_B = this.apply(false, IntegerNumber(0), NaturalNumber(0), Nil, fraction_size_c)

  def negativeZero(fraction_size_c : Int) : FloatingPoint_B = this.apply(true, IntegerNumber(0), NaturalNumber(0), Nil, fraction_size_c)

  def zero(sign_c : Boolean, fraction_size_c : Int) : FloatingPoint_B = if(sign_c) this.negativeZero(fraction_size_c) else this.positiveZero(fraction_size_c)

  def infinite(sign_c : Boolean, fraction_size_c : Int) : FloatingPoint_B = FloatingPoint_INF(sign_c, fraction_size_c)
}
