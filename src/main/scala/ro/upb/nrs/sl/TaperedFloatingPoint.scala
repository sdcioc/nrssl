package ro.upb.nrs.sl




import scala.annotation.tailrec

/*
We consider TaperedFloatingPoint
(-1)^sign * 2^exponent * ( mantissa / 2^fraction_size )
fraction_size is variable
size is fixed (maximum fraction size)
*/
abstract class TaperedFloatingPoint_B extends NumberRepresentationSystem {
  /*
  sign
  exponent in Z
  mantissa in N (with hidden bit)
  size of the mantissa (variable)
  rest bits
  size (maximum size for fraction) fixed
  */
  val sign : Boolean
  val exponent : IntegerNumber_B
  val mantissa : NaturalNumber_B
  val fraction_size : Int
  val rest_bits : List[Boolean]
  val size : Int
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
  exponent1 >= exponent2
  (-1)^sign * 2^exponent1 * ( mantissa1 / 2^fraction_size1 ) + (-1)^sign * 2^exponent2 * ( mantissa2 / 2^fraction_size2 ) =
  = (-1)^sign * 2^exponent1 * ( (mantissa1 * 2^fraction_size2) / 2^(fraction_size1 + fraction_size2) ) + (-1)^sign * 2^exponent2 * ( (mantissa2 * 2^fraction_size1) / 2^(fraction_size2 + fraction_size1) ) =
  = (-1)^sign * 2^exponent1 * ( (mantissa1 * 2^fraction_size2) + 2^(exponent2 - exponent1) * (mantissa2 * 2^fraction_size1) ) / 2^(fraction_size2 + fraction_size1) =
  = (-1)^sign * 2^exponent1 * ( (mantissa1 << fraction_size2) + ( (mantissa2 << fraction_size1) / 2^(exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
  = (-1)^sign * 2^exponent1 * ( (mantissa1 << fraction_size2) + ( (mantissa2 << fraction_size1) >> (exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
  */
  override def +(that: NumberRepresentationSystem): TaperedFloatingPoint_B = that match {
    case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
    case that : TaperedFloatingPoint_B => {
      if(that.mantissa == NaturalNumber(0)) this
      else if(this.mantissa == NaturalNumber(0)) that
      else {
        if(that.sign != this.sign) this - (-that)
        else if(that.abs <= this.abs) {
          val exponent_diff = (this.exponent - that.exponent).toInt
          val rest_value = (that.mantissa << this.fraction_size).value & ((BigInt(1) << exponent_diff)-1)
          val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(rest_value).binaryEncode).reverse
          val new_mantissa = (this.mantissa << that.fraction_size) + ((that.mantissa << this.fraction_size) >> exponent_diff)
          TaperedFloatingPoint(
            this.sign, this.exponent,
            new_mantissa,
            this.fraction_size + that.fraction_size, rest_bits_computed, size)
        } else {
          that + this
        }
      }
    }
    /*
    case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, _) => {
      if(that_f == NaturalNumber(0)) this
      else {
        if(that_s != this.sign) this - (-that)
        else if(this.abs < that.abs) {
          val exponent_diff = (that_e-this.exponent).toInt
          val rest_value = (this.mantissa << that_fs).value & ((BigInt(1) << exponent_diff)-1)
          val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(rest_value).binaryEncode).reverse
          val new_mantissa = (that_f << this.fraction_size) + ((this.mantissa << that_fs) >> exponent_diff)
          val d = TaperedFloatingPoint(
            this.sign, that_e,
            new_mantissa,
            this.fraction_size + that_fs, rest_bits_computed, size)
          println("ad1:" + d)
          d
        }
        else {
          val exponent_diff = (this.exponent-that_e).toInt
          val rest_value = (that_f << this.fraction_size).value & ((BigInt(1) << exponent_diff)-1)
          val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(rest_value).binaryEncode).reverse
          val new_mantissa = (this.mantissa << that_fs) + ((that_f << this.fraction_size) >> exponent_diff)
          val d = TaperedFloatingPoint(
            this.sign, this.exponent,
            new_mantissa,
            this.fraction_size + that_fs, rest_bits_computed, size)
          println("ad2:" + d)
          d
        }
      }
    }
    */
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint +")
  }
  /*
  we can consider the same case as addition with the same sign and first element with bigger absolute value.
  If the sign differ this - that = this + (-that)
  if the second absolute value is bigger this - that = -(that - this)
  (-1)^sign * 2^exponent1 * ( mantissa1 / 2^fraction_size1 ) - (-1)^sign * 2^exponent2 * ( mantissa2 / 2^fraction_size2 ) =
  = (-1)^sign * 2^exponent1 * ( (mantissa1 * 2^fraction_size2) / 2^(fraction_size1 + fraction_size2) ) - (-1)^sign * 2^exponent2 * ( (mantissa2 * 2^fraction_size1) / 2^(fraction_size2 + fraction_size1) ) =
  = (-1)^sign * 2^exponent1 * ( (mantissa1 * 2^fraction_size2) - 2^(exponent2 - exponent1) * (mantissa2 * 2^fraction_size1) ) / 2^(fraction_size2 + fraction_size1) =
  = (-1)^sign * 2^exponent1 * ( (mantissa1 << fraction_size2) - ( (mantissa2 << fraction_size1) / 2^(exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
  = (-1)^sign * 2^exponent1 * ( (mantissa1 << fraction_size2) - ( (mantissa2 << fraction_size1) >> (exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
  */
  override def -(that: NumberRepresentationSystem): TaperedFloatingPoint_B = that match {
    case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
    case that : TaperedFloatingPoint_B => {
      if(that.mantissa == NaturalNumber(0)) this
      else if(this.mantissa == NaturalNumber(0)) -that
      else {
        if(that.sign != this.sign) this + (-that)
        else if(that.abs <= this.abs) {
          val exponent_diff = (this.exponent - that.exponent).toInt
          val extra_value = BigInt(1) << exponent_diff
          val rest_value = (that.mantissa << this.fraction_size).value & ((BigInt(1) << exponent_diff)-1)
          val rest_bits_computed = auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_diff, NaturalNumber(extra_value - rest_value).binaryEncode).reverse
          val new_mantissa = (this.mantissa << that.fraction_size) - ((that.mantissa << this.fraction_size) >> exponent_diff)
          if(rest_value == 0) {//no extra bits
            //println("rest value ==0")
            TaperedFloatingPoint(
            this.sign, this.exponent,
            new_mantissa,
            this.fraction_size + that.fraction_size, Nil, size)
          } else {
            //println("rest value !0")
            //println("rest bits:" + rest_bits_computed.mkString(" "))
            TaperedFloatingPoint(
            this.sign, this.exponent,
            new_mantissa - NaturalNumber(1),
            this.fraction_size + that.fraction_size,
            rest_bits_computed, //extra bits
            size)
          }
        } else {
          -(that - this)
        }
      }
    }
    /*case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, _) => {
      if(that_f == NaturalNumber(0)) this
      else {
        if(that_s != this.sign) this + (-that)
        //TODO: mayvbe negate rest_bits
        else if(this.abs < that.abs) TaperedFloatingPoint(!this.sign, that_e, (that_f << this.fraction_size) - ((this.mantissa << that_fs) >> (that_e-this.exponent).toInt), this.fraction_size + that_fs, (this.mantissa << that_fs).binaryEncode.take((that_e-this.exponent).toInt).reverse, this.size)
        else TaperedFloatingPoint(this.sign, this.exponent, (this.mantissa << that_fs) - ((that_f << this.fraction_size) >> (this.exponent-that_e).toInt), this.fraction_size + that_fs, (that_f << this.fraction_size).binaryEncode.take((this.exponent-that_e).toInt).reverse, this.size)
      }
    }
    */
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint -")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantissa1 / 2^fraction_size1 ) * (-1)^sign2 * 2^exponent2 * ( mantissa2 / 2^fraction_size2 ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) *  (mantissa1 * mantissa2) / 2^(fraction_size1 + fraction_size2 )
  */
  override def *(that: NumberRepresentationSystem): TaperedFloatingPoint_B = that match {
    case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
    case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, _) => 
      TaperedFloatingPoint(this.sign^that_s, this.exponent+that_e, (this.mantissa * that_f), this.fraction_size + that_fs, Nil, this.size)
    
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint *")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantissa1 / 2^fraction_size1 ) / (-1)^sign2 * 2^exponent2 * ( mantissa2 / 2^fraction_size2 ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) *  ( (mantissa1 * 2^fraction_size2) / mantissa2) / 2^fraction_size1
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) *  ( (mantissa1 << fraction_size2) / mantissa2) / 2^fraction_size1
  */
  override def /(that: NumberRepresentationSystem): TaperedFloatingPoint_B = that match {
    case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
    case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, _) => if(that_f!=NaturalNumber(0)) {
        // we add size extra bits to know for sure we have at least size bits in mantissa
        val eq = (this.mantissa<<(that_fs + this.size)) / that_f
        val er = (this.mantissa<<(that_fs + this.size)) % that_f
        val s = !(er==NaturalNumber(0))
        //eliminate extra bits
        val new_q = eq >> this.size
        TaperedFloatingPoint(this.sign^that_s, this.exponent-that_e, new_q, this.fraction_size, (s :: eq.binaryEncode.take(this.size)).reverse, this.size)
        
    } else TaperedFloatingPoint_NR(this.size)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint /")
  }
  override def /\(that: NumberRepresentationSystem): TaperedFloatingPoint_B = this / that
  override def %(that: NumberRepresentationSystem): TaperedFloatingPoint_B = throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint %")
  /*
  X^Y = X^([Y]+{Y}) where [Y] is integer part of Y and {Y} is fractional part of Y
  X^(-Y) = 1 / X^Y
  */
  override def pow(that: NumberRepresentationSystem): TaperedFloatingPoint_B = that match {
    case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
    case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, that_size) => {
      /*
      if that is negative
      X^(-Y) = 1 / X^Y
      */
      if(that_s)
        this.inverse.pow(-that)
      else {
        //[Y] because Y is positive [Y] is positive
        val integer_value = that.toBigInt
        //{Y} in [0, 1)
        val fractional_value = TaperedFloatingPoint(that_s, that_e, that_f, that_fs, Nil, that_size) - TaperedFloatingPoint(integer_value.toDouble, that_size)
        /*
        X^Y = X^([Y]+{Y}) = X^[Y] * X^{Y} = X^[Y] * X^( 2^exponent * ( mantissa / 2^fraction_size ) )
        mantissa / 2^fraction_size is in [1, 2) so  we can use tha format mantissa / 2^fraction_size = 1 +  fraction / 2^fraction_size
        X^Y = X^[Y] * X^( 2^exponent * ( 1 +  fraction / 2^fraction_size ) ) =
        = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        */
        //X^[Y]
        val x_integerY = mathFunctions.pow_integer(
                        TaperedFloatingPoint(1.0d, size)) (this.abs,
                        integer_value
                      )
        //X^2^exponent //exponent is negative so X^(1/2^exponent.abs)
        val x_exponentY = mathFunctions.pow_fractional(
                          TaperedFloatingPoint(1.0d, size)) (this.abs ,
                          BigInt(1), fractional_value.exponent.abs.toInt
                        )
        //(X^(2^exponent))^( fraction / 2^fraction_size )
        val x_exponentY_fraction = mathFunctions.pow_fractional(
                        TaperedFloatingPoint(1.0d, size)) (
                        x_exponentY,
                        fractional_value.mantissa.value, fractional_value.fraction_size
                      )
        //if fractional value is zero
        // result = X^[Y]
        // else result = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        // if the current number is negative we only do pow for integer numbers
        val result = if(fractional_value == TaperedFloatingPoint(0.0d, that_size))
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
                        TaperedFloatingPoint_NR(this.size)
        result match {
          case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
          case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, that_size) => TaperedFloatingPoint(that_s, that_e, that_f, that_fs, Nil, that_size)
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint pow math")
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint pow")
  }
  override def unary_- : TaperedFloatingPoint_B = TaperedFloatingPoint(!this.sign, this.exponent, this.mantissa, this.fraction_size, this.rest_bits, this.size)
  override def inverse : TaperedFloatingPoint_B = TaperedFloatingPoint(1.0d, this.size) / this
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
    case TaperedFloatingPoint_NR(_) => false
    case that : TaperedFloatingPoint_B => {
      if(this.sign != that.sign) this.sign
      else {
        if(this.mantissa == NaturalNumber(0)) {
          if(that.mantissa == NaturalNumber(0)) false
          else !that.sign //special case when zero
        }
        else if(that.mantissa == NaturalNumber(0)) this.sign //special case when zero
        else {
          if(this.exponent < that.exponent) !this.sign
          else if(this.exponent > that.exponent) this.sign
          else if(this.mantissa < that.mantissa) !this.sign
          else if(this.mantissa > that.mantissa) this.sign
          else false
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case TaperedFloatingPoint_NR(_) => false
    case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, _) => (this.sign == that_s) && (this.exponent == that_e) && (this.mantissa == that_f)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint ==")
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
  override def min(that: NumberRepresentationSystem): TaperedFloatingPoint_B = that match {
    case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
    case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, _) => if (this<that) this else TaperedFloatingPoint(that_s, that_e, that_f, that_fs, Nil, this.size)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint min")
  }
  override def max(that: NumberRepresentationSystem): TaperedFloatingPoint_B = that match {
    case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
    case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, _) => if (this>that) this else TaperedFloatingPoint(that_s, that_e, that_f, that_fs, Nil, this.size)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint max")
  }
  override def abs: TaperedFloatingPoint_B = TaperedFloatingPoint(false, this.exponent, this.mantissa, this.fraction_size, this.rest_bits, this.size)
  override def signum: TaperedFloatingPoint_B = {
    if(this.mantissa == NaturalNumber(0)) this
    else TaperedFloatingPoint(this.sign, IntegerNumber(0), NaturalNumber(1)<<this.fraction_size, this.fraction_size, Nil, this.size)
  }
  override def nth_root (n : Int) : (TaperedFloatingPoint_B, TaperedFloatingPoint_B) = {
    val root = this.nqrt(n)
    (root, this-root.pow(TaperedFloatingPoint(n, this.size)))
  }
  /*
  NQRT( (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) ) =
  = NQRT( (-1)^sign ) * NQRT( 2^( ( n * [exponent / n] ) + ( exponent % n ) ) * ( ( mantissa * 2^( (n - 1) * fraction_size ) ) / 2^( ( n * fraction_size ) ) ) )
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( 2^( exponent % n ) * ( mantissa * 2^( (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantissa * 2^( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantissa * << ( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  */
  override def nqrt (n : Int) : TaperedFloatingPoint_B = {
    val n_ip = IntegerNumber(n)
    // TODO: Does it work on negativ exponent %
    // Yes because is Z TODO: maybe text with math
    // 2*n for the extra 2 bits for g and r
    val (eq, er) = (this.mantissa<<(this.size*n+(n-1)*fraction_size+(this.exponent%n_ip).toInt)).nth_root(n)
    val s = !(er==NaturalNumber(0))
    // eliminate extra bits
    val new_q = eq >> this.size
    val l = new_q.value.testBit(0)
    if(this.sign & n%2==0) TaperedFloatingPoint_NR(this.size)
    else TaperedFloatingPoint(this.sign, this.exponent/n_ip, new_q, this.fraction_size, (s :: eq.binaryEncode.take(this.size)).reverse, this.size)
    
  }
  override def sqrt: TaperedFloatingPoint_B = this.nqrt(2)
  override def exp : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.exp)
  override def ln : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : TaperedFloatingPoint_B = base match {
    case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
    case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, that_size) => this.ln / TaperedFloatingPoint(that_s, that_e, that_f, that_fs, Nil, that_size).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint_B log")
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
  override def sin : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.sin)
  override def cos : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.cos)
  override def tan : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.tan)
  override def cot : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.cot)
  override def sec : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.sec)
  override def csc : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.coth)
  override def sech : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.sech)
  override def csch : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : TaperedFloatingPoint_B = helper_taylor_function(mathFunctions.arccsch)
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
  override def toInternalString: String = "sign=" + sign.toString +"exp=" + exponent.toInternalString + " mantissa=" + mantissa.toInternalString + " fs=" + fraction_size
 
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : TaperedFloatingPoint_B = {
    val precision : Int = 30
    val result = func(
      TaperedFloatingPoint(0.0d, size),
      TaperedFloatingPoint(1.0d, size)) (this,
      TaperedFloatingPoint(precision.toDouble, size))
    result match {
      case TaperedFloatingPoint_NR(_) => TaperedFloatingPoint_NR(this.size)
      case TaperedFloatingPoint(that_s, that_e, that_f, that_fs, _, that_size) => TaperedFloatingPoint(that_s, that_e, that_f, that_fs, Nil, that_size)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint helper_taylor_function")
    }
  }
}

class TaperedFloatingPoint_NR(size_c : Int) extends TaperedFloatingPoint_B {
  override val sign : Boolean = false;
  override val exponent : IntegerNumber_B = IntegerNumber_NR
  override val mantissa : NaturalNumber_B = NaturalNumber_NR
  override val fraction_size : Int = 0
  override val rest_bits : List[Boolean] = Nil
  override val size : Int = size_c 
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
  override def +(that: NumberRepresentationSystem): TaperedFloatingPoint_B = this
  override def -(that: NumberRepresentationSystem): TaperedFloatingPoint_B = this
  override def *(that: NumberRepresentationSystem): TaperedFloatingPoint_B = this
  override def /(that: NumberRepresentationSystem): TaperedFloatingPoint_B = this
  override def pow(that: NumberRepresentationSystem): TaperedFloatingPoint_B = this
  override def unary_- : TaperedFloatingPoint_B = this
  override def inverse : TaperedFloatingPoint_B = this
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
  override def min(that: NumberRepresentationSystem): TaperedFloatingPoint_B = this
  override def max(that: NumberRepresentationSystem): TaperedFloatingPoint_B = this
  override def abs: TaperedFloatingPoint_B = this
  override def signum: TaperedFloatingPoint_B = this
  override def nth_root (n : Int) : (TaperedFloatingPoint_B, TaperedFloatingPoint_B) = (this, this)
  override def nqrt (n : Int) : TaperedFloatingPoint_B = this
  override def sqrt: TaperedFloatingPoint_B = this
  override def exp : TaperedFloatingPoint_B = this
  /*
  ln(+INF)=+INF
  ln(-INF)=-INF
  */
  override def ln : TaperedFloatingPoint_B = this
  override def log(base: NumberRepresentationSystem) : TaperedFloatingPoint_B = this
  /*
  trigonometric functions
  sin
  cos
  tan
  cot
  sec
  csc
  */
  override def sin : TaperedFloatingPoint_B = this
  override def cos : TaperedFloatingPoint_B = this
  override def tan : TaperedFloatingPoint_B = this
  override def cot : TaperedFloatingPoint_B = this
  override def sec : TaperedFloatingPoint_B = this
  override def csc : TaperedFloatingPoint_B = this
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : TaperedFloatingPoint_B = this
  override def arccos : TaperedFloatingPoint_B = this
  override def arctan : TaperedFloatingPoint_B = this
  override def arccot : TaperedFloatingPoint_B = this
  override def arcsec : TaperedFloatingPoint_B = this
  override def arccsc : TaperedFloatingPoint_B = this
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : TaperedFloatingPoint_B = this
  override def cosh : TaperedFloatingPoint_B = this
  override def tanh : TaperedFloatingPoint_B = this
  override def coth : TaperedFloatingPoint_B = this
  override def sech : TaperedFloatingPoint_B = this
  override def csch : TaperedFloatingPoint_B = this
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : TaperedFloatingPoint_B = this
  override def arccosh : TaperedFloatingPoint_B = this
  override def arctanh : TaperedFloatingPoint_B = this
  override def arccoth : TaperedFloatingPoint_B = this
  override def arcsech : TaperedFloatingPoint_B = this
  override def arccsch : TaperedFloatingPoint_B = this
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

object TaperedFloatingPoint_NR {
  def apply(size_c : Int) : TaperedFloatingPoint_B = new TaperedFloatingPoint_NR(size_c)
  def unapply(input : TaperedFloatingPoint_B) = if(input.mantissa.equals(NaturalNumber_NR)) Some(input.size) else None
}

class TaperedFloatingPoint(sign_c : Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, fraction_size_c : Int, rest_bits_c : List[Boolean], size_c : Int) extends TaperedFloatingPoint_B {
  override val sign : Boolean = sign_c;
  override val exponent : IntegerNumber_B = exponent_c
  override val mantissa : NaturalNumber_B = mantissa_c
  override val fraction_size : Int = fraction_size_c
  override val rest_bits : List[Boolean] = rest_bits_c
  override val size : Int = size_c
}


object TaperedFloatingPoint {
  /*
  default sizes and rounding
  */
  var default_size : Int = 32
  /*
  set new sizes and rounding
  */
  def set_size_rounding(size_c : Int) = {
    this.default_size = size_c
  }
  implicit def fromIntToTaperedFloatingPoint(a : Int):TaperedFloatingPoint_B = TaperedFloatingPoint(a)
  implicit def fromLongToTaperedFloatingPoint(a : Long):TaperedFloatingPoint_B = TaperedFloatingPoint(a)
  implicit def fromFloatToTaperedFloatingPoint(a : Float):TaperedFloatingPoint_B = TaperedFloatingPoint(a)
  implicit def fromDoubleToTaperedFloatingPoint(a : Double):TaperedFloatingPoint_B = TaperedFloatingPoint(a)
  
  /*
  TaperedFloatingPoint has normalised mantissa
  */
  @tailrec
  def apply(sign_c: Boolean, exponent_c : IntegerNumber_B, mantissa_c : NaturalNumber_B, fraction_size_c : Int, rest_bits: List[Boolean], size_c : Int) :TaperedFloatingPoint_B = {
      (exponent_c, mantissa_c) match {
        case (IntegerNumber_NR(_), _)  =>TaperedFloatingPoint_NR(size_c)
        case (_, NaturalNumber_NR(_))  =>TaperedFloatingPoint_NR(size_c)
        case (_, NaturalNumber(a)) =>     if(NaturalNumber(a)==NaturalNumber(0)) {
                                        if(rest_bits != Nil)
                                          this.apply(
                                            sign_c,
                                            exponent_c - IntegerNumber(1), 
                                            (mantissa_c << 1) + (if(rest_bits.head) NaturalNumber(1) else NaturalNumber(0)), //add the first bit of rest bits
                                            fraction_size_c,
                                            rest_bits.tail,
                                            size_c
                                          )
                                        else
                                        /*
                                        if mantissa is 0
                                        */
                                        new TaperedFloatingPoint(
                                                  false, // TaperedFloatingPoint has only positive zero
                                                  IntegerNumber(0),
                                                  NaturalNumber(0),
                                                  0,
                                                  rest_bits,
                                                  size_c
                                                )
                                    } else {
                                        /*
                                        println(
                                          "sign:" + sign_c +
                                          " exponent:" + exponent_c +
                                          " mantissa:" + mantissa_c +
                                          " fraction_size:" + fraction_size_c +
                                          " restbits:" + rest_bits.mkString(" ") +
                                          " size:" + size_c
                                        )
                                        */
                                        /*
                                        if mantissa / 2^fraction_size >= 2
                                        we divide mantissa by 2 (shifting right with 1) and increase the exponent by one
                                        (-1)^sign * 2^exponent * ( mantissa / 2^fraction_size ) =
                                        = (-1)^sign * 2^exponent * ( ( 2 * ( mantissa / 2 ) ) / 2^fraction_size )
                                        = (-1)^sign * 2^(exponent + 1) * ( ( mantissa / 2 ) / 2^fraction_size ) OLD WAY
                                        = (-1)^sign * 2^(exponent + 1) * ( mantissa / 2^(fraction_size + 1) ) NEW WAY
                                        */
                                        if(mantissa_c.binaryEncode.length > (fraction_size_c+1)) 
                                            this.apply(
                                              sign_c,
                                              exponent_c + IntegerNumber(1),
                                              mantissa_c,
                                              fraction_size_c + 1,
                                              rest_bits,
                                              size_c
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
                                                  fraction_size_c,
                                                  rest_bits.tail,
                                                  size_c
                                                )
                                            else this.apply(
                                                  sign_c,
                                                  exponent_c - IntegerNumber(1), 
                                                  (mantissa_c << 1),
                                                  fraction_size_c,
                                                  Nil,
                                                  size_c
                                                )
                                        else {
                                            /*if mantissa is in [1,2)
                                            */
                                            //println("a luat pe aici")
                                            new TaperedFloatingPoint(
                                                  sign_c,
                                                  exponent_c,
                                                  mantissa_c,
                                                  fraction_size_c,
                                                  rest_bits,
                                                  size_c
                                                )
                                        }
                                    }
        case _=> throw new NotImplementedError("ro.upb.nrs.sl.TaperedFloatingPoint apply")
      }
  }

  def apply(a : Double, size_c : Int) : TaperedFloatingPoint_B = {
    val bits : Long = java.lang.Double.doubleToRawLongBits(a)
    val negative : Boolean = (bits & 0x8000000000000000L) != 0
    val exponent_bits : Long = ((bits & 0x7ff0000000000000L) >> 52)
    val exponent : Long = exponent_bits - 1023
    val mantissa : Long = if(exponent != -1023) 
                            (bits & 0x000fffffffffffffL) | 0x0010000000000000L
                          else
                            (bits & 0x000fffffffffffffL)
    this.apply(negative, IntegerNumber(exponent), NaturalNumber(mantissa), 52, Nil, size_c)
  }
  
  def apply(a : Int, size_c : Int) : TaperedFloatingPoint_B = this.apply(a.toDouble, size_c)
  def apply(a : Int) : TaperedFloatingPoint_B = this.apply(a.toDouble, this.default_size)
  def apply(a : Long, size_c : Int) : TaperedFloatingPoint_B = this.apply(a.toDouble, size_c)
  def apply(a : Long) : TaperedFloatingPoint_B = this.apply(a.toDouble, this.default_size)
  def apply(a : Double) : TaperedFloatingPoint_B = this.apply(a, this.default_size)
  def apply(a : Float, size_c : Int) : TaperedFloatingPoint_B = this.apply(a.toDouble, size_c)
  def apply(a : Float) : TaperedFloatingPoint_B = this.apply(a.toDouble)

  def unapply(input : TaperedFloatingPoint_B) = Some(input.sign, input.exponent, input.mantissa, input.fraction_size, input.rest_bits, input.size)
}
