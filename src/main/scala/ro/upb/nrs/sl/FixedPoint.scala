package ro.upb.nrs.sl




/*
FixedPoint is saw as a Z number
= value / 2^fraction_size
fraction_size, size and rounding is fixed
*/
abstract class FixedPoint_B extends FixedPrecisionNumberRepresentationSystem {
  /*
  value in Z
  size, fraction size and rounding
  */
  val value : IntegerNumber_B
  val size : Int
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
  (value1 / 2^fraction_size) + (value2 / 2^fraction_size) =
  = (value1 + value2) / 2^fraction_size
  */
  override def +(that: NumberRepresentationSystem): FixedPoint_B = that match {
    case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
    case FixedPoint(x, y, z, t) => FixedPoint(this.value + x, size, fraction_size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint +")
  }
  /*
  (value1 / 2^fraction_size) - (value2 / 2^fraction_size) =
  = (value1 - value2) / 2^fraction_size
  */
  override def -(that: NumberRepresentationSystem): FixedPoint_B = that match {
    case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
    case FixedPoint(x, y, z, t) => FixedPoint(this.value - x, size, fraction_size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint -")
  }
  /*
  (value1 / 2^fraction_size) * (value2 / 2^fraction_size) =
  = (value1 * value2) / 2^(2 * fraction_size) =
  = ( (value1 * value2) / 2^fraction_size) / 2^fraction_size
  = ( (value1 * value2) >> fraction_size) / 2^fraction_size
  */
  override def *(that: NumberRepresentationSystem): FixedPoint_B = that match {
    case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
    case FixedPoint(x, y, z, t) => {
        val q = this.value.value * x.value
        // keep 2 extra bits for g r
        val eq = q >> (fraction_size-2)
        val g  = eq.value.testBit(1)
        val r  = eq.value.testBit(0)
        //rest of the bits
        val s = ((q.value & (1<<(fraction_size-2)-1)) != 0)
        //eliminate the 2 extra bits
        val new_q = eq >> 2
        val l = new_q.value.testBit(0)
        FixedPoint(IntegerNumber(this.value.sign ^ x.sign, new_q), l, g, r, s, size, fraction_size, rounding)
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint *")
  }
  /*
  (value1 / 2^fraction_size) / (value2 / 2^fraction_size) =
  = (value1 / value2) =
  = ( (value1 / value2) * 2^fraction_size) / 2^fraction_size
  = ( ( (value1 * 2^fraction_size) / value2) ) / 2^fraction_size
  = ( ( (value1 << fraction_size) / value2) ) / 2^fraction_size
  */
  override def /(that: NumberRepresentationSystem): FixedPoint_B = that match {
    case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
    case FixedPoint(IntegerNumber(sign, a), y, z, t) => {
        if(a == NaturalNumber(0)) FixedPoint_NR(size, fraction_size, rounding)
        else {
            // take 2 extra bits for g and r bits
            val eq = (this.value.value<<(this.fraction_size + 2)) / a
            val er = (this.value.value<<(this.fraction_size + 2)) % a
            val g  = eq.value.testBit(1)
            val r  = eq.value.testBit(0)
            val s = !(er==NaturalNumber(0))
            // eliminate the extra bits
            val new_q = eq >> 2
            val l = new_q.value.testBit(0)
            FixedPoint(IntegerNumber(this.value.sign ^ sign, new_q), l, g, r, s, size, fraction_size, rounding)
        }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint /")
  }
  override def /\(that: NumberRepresentationSystem): FixedPoint_B = this / that
  override def %(that: NumberRepresentationSystem): FixedPoint_B = throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint %")
  /*
  X^Y = X^([Y]+{Y}) where [Y] is integer part of Y and {Y} is fractional part of Y
  X^(-Y) = 1 / X^Y
  */
  override def pow(that: NumberRepresentationSystem): FixedPoint_B = that match {
    case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
    case FixedPoint(x, y, that_fs, t) => {
      /*
      if that is negative
      X^(-Y) = 1 / X^Y
      */
      if(x.sign)
        this.inverse.pow(-that)
      else {
        //[Y] because Y is positive [Y] is positive
        val integer_value = (x.value >> that_fs).value
        //{Y} in [0, 1)
        val fractional_value = x.value.value & ( (BigInt(1) << fraction_size) - BigInt(1)  )
        /*
        X^Y = X^([Y]+{Y}) = X^[Y] * X^{Y} = 
        [Y] = X >> fractions_size we have to shift and transform in BigInt
        {Y} = X / 2^ fraction_size pow_fractional will take only fraction_size bits
        */
        //X^[Y]
        val x_integerY = mathFunctions.pow_integer(
                        FixedPoint(1.0d, size, fraction_size, rounding)) (this.abs,
                        integer_value
                      )
        //X^{Y}
        val x_fractionalY = mathFunctions.pow_fractional(
                          FixedPoint(1.0d, size, fraction_size, rounding)) (this.abs ,
                          fractional_value, that_fs
                        )
        //if fractional value is zero
        // result = X^[Y]
        // else result = X^[Y] * X^{Y}
        // if the current number is negative we only do pow for integer numbers
        val result = if(fractional_value == 0)
                      if(this.value.sign == false)
                        x_integerY
                      else 
                        if((integer_value % 2) ==0)
                          x_integerY
                        else
                          -x_integerY
                     else 
                      if(this.value.sign == false)
                        x_integerY * x_fractionalY
                      else
                        FixedPoint_NR(size, fraction_size, rounding)
        result match {
          case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
          case FixedPoint(x, y, z, t) => FixedPoint(x, y, z, t)
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint_B pow math")
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldFixedFloatingPoint_B pow")
  }
  override def unary_- : FixedPoint_B = FixedPoint(-value, size, fraction_size, rounding)
  override def inverse : FixedPoint_B = FixedPoint(1, size, fraction_size, rounding) / FixedPoint(value, size, fraction_size, rounding)
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
    case FixedPoint_NR(_, _,_) => false
    case FixedPoint(x, y, z, t) => this.value < x
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case FixedPoint_NR(_, _,_) => false
    case FixedPoint(x, y, z, t) => this.value == x
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint ==")
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
  override def min(that: NumberRepresentationSystem): FixedPoint_B = that match {
    case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
    case FixedPoint(x, y, z, t) => FixedPoint(this.value.min(x), y, z, t)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint min")
  }
  override def max(that: NumberRepresentationSystem): FixedPoint_B = that match {
    case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
    case FixedPoint(x, y, z, t) => FixedPoint(this.value.max(x), y, z, t)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint max")
  }
  override def abs: FixedPoint_B = FixedPoint(this.value.abs, size, fraction_size, rounding)
  override def signum: FixedPoint_B = if(this.value.value == NaturalNumber(0))  FixedPoint(0, size, fraction_size, rounding)
                                         else if(this.value.sign)  FixedPoint(-1, size, fraction_size, rounding)
                                         else FixedPoint(1, size, fraction_size, rounding)
  override def nth_root (n : Int) : (FixedPoint_B, FixedPoint_B) = {
    val root = this.nqrt(n)
    (root, this-root.pow(FixedPoint(n, size, fraction_size, rounding)))
  }
  /*
  NQRT(value / 2^fraction_size) =
  = NQRT( ( value * 2^( (n - 1) * fraction_size ) ) / 2^( ( n * fraction_size ) ) )
  = NQRT( ( value * 2^( (n - 1) * fraction_size ) ) ) / 2^fraction_size
  */
  override def nqrt (n : Int) : FixedPoint_B = {
    // 2*n for 2 extra bits or g and r
    val (eq, er) = (this.value.value<<(2*n+(n-1)*fraction_size)).nth_root(n)
    val g  = eq.value.testBit(1)
    val r  = eq.value.testBit(0)
    val s = !(er==NaturalNumber(0))
    // eliminate 2 extra bits
    val new_q = eq >> 2
    val l = new_q.value.testBit(0)
    if(this.value.sign & n%2==0) FixedPoint_NR(size, fraction_size, rounding)
    else FixedPoint(IntegerNumber(this.value.sign, new_q), l, g, r, s, size, fraction_size, rounding)
  }
  override def sqrt: FixedPoint_B = this.nqrt(2)
  override def exp : FixedPoint_B = helper_taylor_function(mathFunctions.exp)
  override def ln : FixedPoint_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : FixedPoint_B = base match {
    case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
    case FixedPoint(x, y, z, t) => this.ln / FixedPoint(x, y, z, t).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint log")
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
  override def sin : FixedPoint_B = helper_taylor_function(mathFunctions.sin)
  override def cos : FixedPoint_B = helper_taylor_function(mathFunctions.cos)
  override def tan : FixedPoint_B = helper_taylor_function(mathFunctions.tan)
  override def cot : FixedPoint_B = helper_taylor_function(mathFunctions.cot)
  override def sec : FixedPoint_B = helper_taylor_function(mathFunctions.sec)
  override def csc : FixedPoint_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : FixedPoint_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : FixedPoint_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : FixedPoint_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : FixedPoint_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : FixedPoint_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : FixedPoint_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : FixedPoint_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : FixedPoint_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : FixedPoint_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : FixedPoint_B = helper_taylor_function(mathFunctions.coth)
  override def sech : FixedPoint_B = helper_taylor_function(mathFunctions.sech)
  override def csch : FixedPoint_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : FixedPoint_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : FixedPoint_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : FixedPoint_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : FixedPoint_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : FixedPoint_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : FixedPoint_B = helper_taylor_function(mathFunctions.arccsch)
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
  override def toRationalNumber : RationalNumber_B = RationalNumber(this.value) / RationalNumber(2).pow(RationalNumber(fraction_size))
  override def toBigDecimal : BigDecimal = this.value.toBigDecimal / BigDecimal(2).pow(this.fraction_size)
  /*
  Show functions
  toString
  toInternalString
  */
  override def toString: String = if(this.value.toString=="NR") "NR" else this.toBigDecimal.toString
  override def toInternalString: String = this.value.toInternalString + "/2^" + this.fraction_size.toString
  /*
  override def toBinaryString: String = {
    val toDecode : BigInt = if (this.value.sign) -this.value.value.value else this.value.value.value
    val binaryNumber : String = String.format("%" + size + "s", toDecode.toLong.toBinaryString).replace(' ', '0')
    val binaryValue : String = if (this.value.sign) binaryNumber drop (64 - size) else binaryNumber
    binaryValue
  }
  */

  
  override def toBinaryString: String = {
    val toDecode : NaturalNumber_B = if (this.value.sign) this.value.value - NaturalNumber(1) else this.value.value
    val binaryNumber : String = "0" + auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size - 1, toDecode.binaryEncode))
    val binaryValue : String = if (this.value.sign) auxiliaryFunctions.BinaryStringNegate(binaryNumber) else binaryNumber
    binaryValue
  }
  
  
  
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : FixedPoint_B = {
    val min_size : Int = size - fraction_size
    val precision : Int = if(min_size < 16) 5 else if (min_size < 32) 7 else if (min_size < 64) 12 else 20
    val result = func(
      FixedPoint(0, size, fraction_size, rounding),
      FixedPoint(1, size, fraction_size, rounding)) (this,
      FixedPoint(precision, size, fraction_size, rounding))
    result match {
      case FixedPoint_NR(_, _,_) => FixedPoint_NR(size, fraction_size, rounding)
      case FixedPoint(x, y, z, t) => FixedPoint(x, y, z, t)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint helper_taylor_function")
    }
  }
}

class FixedPoint_NR(size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends FixedPoint_B {
  override val value : IntegerNumber_B = IntegerNumber_NR
  override val size : Int = size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
}

object FixedPoint_NR {
  def apply(size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedPoint_B = new FixedPoint_NR(size_c, fraction_size_c, rounding_c)
  def unapply(input : FixedPoint_B) = if(input.value.equals(IntegerNumber_NR)) Some(input.size, input.fraction_size, input.rounding) else None
}

class FixedPoint(value_c : IntegerNumber_B, size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) extends FixedPoint_B {
  override val value : IntegerNumber_B = value_c
  override val size : Int = size_c
  override val fraction_size : Int = fraction_size_c
  override val rounding : RoundingType = rounding_c
}


object FixedPoint {
  /*
  default sizes and rounding
  */
  var default_size : Int = 32
  var default_fraction_size : Int = 16
  var default_rounding : RoundingType = RoundEven
  /*
  set new sizes and rounding
  */
  def set_size_rounding(size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) = {
    this.default_size = size_c
    this.default_rounding = rounding_c
    this.default_fraction_size = fraction_size_c
  }
  /*
  ro.upb.nrs.sl.RoundUp -> if positive and (g|r|s) than this + 1 else this
  ro.upb.nrs.sl.RoundDown -> if negative and (g|r|s) than this - 1 else this
  ro.upb.nrs.sl.RoundZero -> this
  ro.upb.nrs.sl.RoundAwayZero -> if (g|r|s) { if(negative) than this - 1 else this + 1 } else this
  ro.upb.nrs.sl.RoundEven -> if (g and (l|r|s) ) { if(negative) than this - 1 else this + 1 } else this
  */
  def apply(value_c : IntegerNumber_B, l : Boolean, g  : Boolean, r : Boolean, s : Boolean, size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedPoint_B = rounding_c match {
      case RoundUp => if(value_c.sign) this.apply(value_c, size_c, fraction_size_c, rounding_c) else if(g|r|s) this.apply(value_c + IntegerNumber(1), size_c, fraction_size_c, rounding_c) else this.apply(value_c, size_c, fraction_size_c, rounding_c)
      case RoundDown => if(!value_c.sign) this.apply(value_c, size_c, fraction_size_c, rounding_c) else if(g|r|s) this.apply(value_c + IntegerNumber(-1), size_c, fraction_size_c, rounding_c) else this.apply(value_c, size_c, fraction_size_c, rounding_c)
      case RoundZero => this.apply(value_c, size_c, fraction_size_c, rounding_c)
      case RoundAwayZero => if(g|r|s) if(value_c.sign) this.apply(value_c + IntegerNumber(-1), size_c, fraction_size_c, rounding_c) else this.apply(value_c + IntegerNumber(1), size_c, fraction_size_c, rounding_c) else this.apply(value_c, size_c, fraction_size_c, rounding_c)
      case RoundEven => if(g&(r|s|l)) if(value_c.sign) this.apply(value_c + IntegerNumber(-1), size_c, fraction_size_c, rounding_c) else this.apply(value_c + IntegerNumber(1), size_c, fraction_size_c, rounding_c) else this.apply(value_c, size_c, fraction_size_c, rounding_c)
      case _ => this.apply(value_c, size_c, fraction_size_c, rounding_c) 
  }
  
  def apply(value_c : IntegerNumber_B, size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedPoint_B = {
      if(value_c.value.value.bitLength <= size_c-1)  new FixedPoint(value_c, size_c, fraction_size_c, rounding_c) else FixedPoint_NR(size_c, fraction_size_c, rounding_c)
  }

  def apply(a : Double, size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedPoint_B = {
    val bits : Long = java.lang.Double.doubleToRawLongBits(a)
    val negative : Boolean = (bits & 0x8000000000000000L) != 0
    val exponent_bits : Long = ((bits & 0x7ff0000000000000L) >> 52)
    val exponent : Long = exponent_bits - 1023
    val mantissa : Long = if(exponent != -1023) 
                            (bits & 0x000fffffffffffffL) | 0x0010000000000000L
                          else
                            (bits & 0x000fffffffffffffL)
    val shift : Int = exponent.toInt - (52 - fraction_size_c)
    if(shift>=0) 
      this.apply(IntegerNumber(negative, NaturalNumber(mantissa)<<shift), size_c, fraction_size_c, rounding_c)
    else {
        //if we shift right we have to eliminate bits from mantisa
        //so we can calculate g r s l
        val q = NaturalNumber(mantissa)
        val pos_shift = (-shift)
        //this means  we have at least 2 extra bits
        if(pos_shift >=2) {
          // extra 2 bits for g and r
          val eq = q >> (pos_shift-2)
          val g  = eq.value.testBit(1)
          val r  = eq.value.testBit(0)
          val s = ((q.value & (1<<(pos_shift-2)-1)) != 0)
          // eliminate the extra bits
          val new_q = eq >> 2
          val l = new_q.value.testBit(0)
          this.apply(IntegerNumber(negative, new_q), l, g, r, s, size_c, fraction_size_c, rounding_c)
        } else if (pos_shift == 1) {
          // only one extra bit for g bit
          val g  = q.value.testBit(0)
          val r  = false
          val s = false
          // eliminate extra bit
          val new_q = q >> 1
          val l = new_q.value.testBit(0)
          this.apply(IntegerNumber(negative, new_q), l, g, r, s, size_c, fraction_size_c, rounding_c)
        } else {
          //this can not happen
          throw new NotImplementedError("ro.upb.nrs.sl.FixedPoint apply Double")
        }
    }
  }

  def apply(a : Int, size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedPoint_B = {
    if(a>0)
      this.apply(IntegerNumber(false, NaturalNumber(a)<<fraction_size_c), size_c, fraction_size_c, rounding_c)
    else
      this.apply(IntegerNumber(true, NaturalNumber(-a)<<fraction_size_c), size_c, fraction_size_c, rounding_c)
  }
  def apply(a : Int) : FixedPoint_B = this.apply(a, this.default_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Long, size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedPoint_B ={
    if(a>0)
      this.apply(IntegerNumber(false, NaturalNumber(a)<<fraction_size_c), size_c, fraction_size_c, rounding_c)
    else
      this.apply(IntegerNumber(true, NaturalNumber(-a)<<fraction_size_c), size_c, fraction_size_c, rounding_c)
  }
  def apply(a : Long) : FixedPoint_B = this.apply(a, this.default_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Double) : FixedPoint_B = this.apply(a, this.default_size, this.default_fraction_size, this.default_rounding)
  def apply(a : Float, size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedPoint_B = this.apply(a.toDouble, size_c, fraction_size_c, rounding_c)
  def apply(a : Float) : FixedPoint_B = this.apply(a.toDouble)

  def apply(binaryString : String, size_c : Int, fraction_size_c : Int, rounding_c : RoundingType) : FixedPoint_B = {
    val toDecode : BigInt = if(binaryString(0) == '1') {
        BigInt( (~BigInt(binaryString, 2) + 1).toLong.toBinaryString drop (64-size_c), 2)
    } else BigInt(binaryString, 2)
    //println("Decode :", toDecode)
    val toDecodeAbsString : String = String.format("%" + size_c + "s", toDecode.toLong.toBinaryString).replace(' ', '0')
    //println("toDecodeAbsString :", toDecodeAbsString)
    //println("Number :", IntegerNumber( binaryString(0) == '1', NaturalNumber(BigInt(toDecodeAbsString, 2)) ))
    this.apply( IntegerNumber( binaryString(0) == '1', NaturalNumber(BigInt(toDecodeAbsString, 2)) ), size_c, fraction_size_c, rounding_c )
  }


  def unapply(input : FixedPoint_B) = Some(input.value, input.size, input.fraction_size, input.rounding)
}
