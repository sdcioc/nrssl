package ro.upb.nrs.sl




abstract class RationalNumber_B extends NumberRepresentationSystem {
  /*
  numerator and denominator values
  */
  val numerator: IntegerNumber_B
  val denominator : NaturalNumber_B
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
  (a / b) + (c / d) = ( a * d + b * c ) / (b * d)
  */
  override def +(that: NumberRepresentationSystem): RationalNumber_B = that match {
    case RationalNumber_NR(_) => RationalNumber_NR
    case RationalNumber(a, b) => RationalNumber(numerator * IntegerNumber(false,b) + a * IntegerNumber(false,denominator), denominator * b)
    case _ => throw new NotImplementedError("aaaa")
  }
  /*
  (a / b) - (c / d) = ( a * d - b * c ) / (b * d)
  */
  override def -(that: NumberRepresentationSystem): RationalNumber_B = that match {
    case RationalNumber_NR(_) => RationalNumber_NR
    case RationalNumber(a, b) => RationalNumber(numerator * IntegerNumber(false,b) - a * IntegerNumber(false,denominator), denominator * b)
    case _ => throw new NotImplementedError("aaaa")
  }
  /*
  (a / b) * (c / d) = ( a * c ) / (b * d)
  */
  override def *(that: NumberRepresentationSystem): RationalNumber_B = that match {
    case RationalNumber_NR(_) => RationalNumber_NR
    case RationalNumber(a, b) => RationalNumber(numerator * a, denominator * b)
    case _ => throw new NotImplementedError("aaaa")
  }
  /*
  (a / b) / (c / d) = ( a * d ) / (b * c)
  */
  override def /(that: NumberRepresentationSystem): RationalNumber_B = that match {
    case RationalNumber_NR(_) => RationalNumber_NR
    case RationalNumber(a, b) => RationalNumber(numerator * IntegerNumber(a.sign, b), denominator * a.abs.value)
    case _ => throw new NotImplementedError("aaaa")
  }
  override def /\(that: NumberRepresentationSystem): RationalNumber_B = this/that
  /*
  TODO: maybe if we consider that a/b  in Z than we can have ramainder as a Q
  */
  override def %(that: NumberRepresentationSystem): RationalNumber_B = throw new NotImplementedError("ro.upb.nrs.sl.RationalNumber %")
  /*
  (a / b) ^ (c / d) -- d >= 1
  = (a^c / b^c).nqrt(d)
  if (c < 0) = (b^(-c) / a^(-c)).nqrt(d)
  if (d==1) = (a^c / b^c) if c >= 0 or (b^(-c) / a^(-c)) if c < 0
  */
  override def pow(that: NumberRepresentationSystem): RationalNumber_B = that match {
    case RationalNumber_NR(_) => RationalNumber_NR
    // x^y when y < 0 -> (1/x)^(-y)
    // ro.upb.nrs.sl.RationalNumber(ro.upb.nrs.sl.IntegerNumber(numerator.sign, denominator), numerator.value).pow(ro.upb.nrs.sl.RationalNumber(ro.upb.nrs.sl.IntegerNumber(false, a), b))
    case RationalNumber(IntegerNumber(true, a), b) => this.inverse.pow(-that)
    // x^y when y>=0
    case RationalNumber(IntegerNumber(false, a), b) => {
        // x ^ y when y = a/1
        if(b==NaturalNumber(1)) {
          RationalNumber(numerator.pow(IntegerNumber(false, a)), denominator.pow(a))
        // x ^ y when y = 1/b -> x^y = x.nqrt(b)
        } else if (a==NaturalNumber(1)) {
          //precision extra bits
          //val precision : Int = 8
          //ro.upb.nrs.sl.RationalNumber(ro.upb.nrs.sl.IntegerNumber(numerator.sign, numerator.value << (precision*b.toInt())).nth_root(b.toInt())._1, (denominator << (precision*b.toInt())).nth_root(b.toInt())._1)
          this.nqrt(b.toInt)
        }
        // x ^ y when y = a/b -> x^y = x^a.nqrt(b) or (x^(a/1))^(1/b)
        else {
          //this.pow(ro.upb.nrs.sl.RationalNumber(ro.upb.nrs.sl.IntegerNumber(false, a), ro.upb.nrs.sl.NaturalNumber(1))).pow(ro.upb.nrs.sl.RationalNumber(ro.upb.nrs.sl.IntegerNumber(1), b))
          RationalNumber(numerator.pow(IntegerNumber(false, a)), denominator.pow(a)).nqrt(b.toInt)
        }
    }
    case _ => throw new NotImplementedError("aaaa")
  }

  override def unary_- : RationalNumber_B = RationalNumber(-numerator, denominator)
  override def inverse : RationalNumber_B = RationalNumber(IntegerNumber(numerator.sign, denominator), numerator.value) //ro.upb.nrs.sl.RationalNumber(1) / this
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
    case RationalNumber_NR(_) => false
    case RationalNumber(a, b) => numerator * IntegerNumber(false,b) < a * IntegerNumber(false,denominator)
    case _ => throw new NotImplementedError("aaaa")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case RationalNumber_NR(_) => false
    case RationalNumber(a, b) => numerator * IntegerNumber(false,b) == a * IntegerNumber(false,denominator)
    case _ => throw new NotImplementedError("aaaa")
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
  override def min(that: NumberRepresentationSystem): RationalNumber_B = that match {
    case RationalNumber_NR(_) => RationalNumber_NR
    case RationalNumber(x, y) => if(that<this) RationalNumber(x, y) else this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.RationalNumber min")
  }
  override def max(that: NumberRepresentationSystem): RationalNumber_B = that match {
    case RationalNumber_NR(_) => RationalNumber_NR
    case RationalNumber(x, y) => if(that<this) this else RationalNumber(x, y)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.RationalNumber max")
  }
  override def abs: RationalNumber_B = RationalNumber(numerator.abs, denominator)
  override def signum: RationalNumber_B = RationalNumber(numerator.signum, NaturalNumber(1))
  override def nth_root (n : Int) : (RationalNumber_B, RationalNumber_B) = {
    val root = this.nqrt(n)
    // TODO: may break REMAINDER >= 0 for negative numbers
    (root, this-root.pow(RationalNumber(n)))
  }
  /*
  (a / b).nqrt(n) = a.nqrt(n) / b.nqrt(n)
  It is good to add precision for better results a = a << (precision * n)
  */
  override def nqrt(n : Int): RationalNumber_B = {
    // adding extra bits for extra precision
    val precision : Int = 8;
    RationalNumber(IntegerNumber(numerator.sign, numerator.value << (precision*n)).nqrt(n), (denominator << (precision*n)).nqrt(n))
  }
  override def sqrt: RationalNumber_B = this.nqrt(2)
  override def exp : RationalNumber_B = helper_taylor_function(mathFunctions.exp)
  override def ln : RationalNumber_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : RationalNumber_B = base match {
    case RationalNumber_NR(_) => RationalNumber_NR
    case RationalNumber(x, y) => this.ln / RationalNumber(x, y).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.RationalNumber log")
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
  override def sin : RationalNumber_B = helper_taylor_function(mathFunctions.sin)
  override def cos : RationalNumber_B = helper_taylor_function(mathFunctions.cos)
  override def tan : RationalNumber_B = helper_taylor_function(mathFunctions.tan)
  override def cot : RationalNumber_B = helper_taylor_function(mathFunctions.cot)
  override def sec : RationalNumber_B = helper_taylor_function(mathFunctions.sec)
  override def csc : RationalNumber_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : RationalNumber_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : RationalNumber_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : RationalNumber_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : RationalNumber_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : RationalNumber_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : RationalNumber_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : RationalNumber_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : RationalNumber_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : RationalNumber_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : RationalNumber_B = helper_taylor_function(mathFunctions.coth)
  override def sech : RationalNumber_B = helper_taylor_function(mathFunctions.sech)
  override def csch : RationalNumber_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : RationalNumber_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : RationalNumber_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : RationalNumber_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : RationalNumber_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : RationalNumber_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : RationalNumber_B = helper_taylor_function(mathFunctions.arccsch)
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
  override def toRationalNumber : RationalNumber_B = this
  override def toBigDecimal : BigDecimal = this.numerator.toBigDecimal / this.denominator.toBigDecimal
  /*
  extra conversion functions
  toTaperedFloatingPoint
  toIEEE754
  toFloatingPoint
  toFixedP
  */
  def toTaperedFloatingPoint(size : Int) : TaperedFloatingPoint_B = {
    val sign = this.numerator.sign
    val exponent = IntegerNumber(0)
    val denominator_power = this.denominator.value.bitLength - 1 + 1
    val fraction_size = denominator_power + size
    val eq = (this.numerator.value<<(fraction_size + 2)) / this.denominator
    val er = (this.numerator.value<<(fraction_size + 2)) % this.denominator
    val g  = eq.value.testBit(1)
    val r  = eq.value.testBit(0)
    val s = !(er==NaturalNumber(0))
    // the 2 extra bits are eliminated from the final result
    val new_q = eq >> 2
    TaperedFloatingPoint(sign, exponent, new_q, fraction_size, g::r::s::Nil, size) 
  }
  def toOldIEEE754(exponent_size : Int, fraction_size : Int, rounding : RoundingType) : oldIEEE754_B = {
    val sign = this.numerator.sign
    val denominator_power = this.denominator.value.bitLength - 1 + 1
    val precision_size = denominator_power + exponent_size + fraction_size
    val exponent = IntegerNumber(-precision_size)
    val eq = (this.numerator.value<<(precision_size + fraction_size + 2)) / this.denominator
    val er = (this.numerator.value<<(precision_size + fraction_size + 2)) % this.denominator
    val g  = eq.value.testBit(1)
    val r  = eq.value.testBit(0)
    val s = !(er==NaturalNumber(0))
    // the 2 extra bits are eliminated from the final result
    val new_q = eq >> 2
    oldIEEE754(sign, exponent, new_q,  g::r::s::Nil, exponent_size, fraction_size, rounding) 
  }
  def toOldFloatingPoint(exponent_size : Int, fraction_size : Int, rounding : RoundingType) : oldFixedFloatingPoint_B = {
    val sign = this.numerator.sign
    val denominator_power = this.denominator.value.bitLength - 1 + 1
    val precision_size = denominator_power + exponent_size + fraction_size
    val exponent = IntegerNumber(-precision_size)
    val eq = (this.numerator.value<<(precision_size + fraction_size + 2)) / this.denominator
    val er = (this.numerator.value<<(precision_size + fraction_size + 2)) % this.denominator
    val g  = eq.value.testBit(1)
    val r  = eq.value.testBit(0)
    val s = !(er==NaturalNumber(0))
    // the 2 extra bits are eliminated from the final result
    val new_q = eq >> 2
    oldFixedFloatingPoint(sign, exponent, new_q,  g::r::s::Nil, exponent_size, fraction_size, rounding) 
  }
  
  def toFloatingPoint(fraction_size : Int) : FloatingPoint_B = {
    val sign = this.numerator.sign
    val denominator_power = this.denominator.value.bitLength - 1 + 1
    val precision_size = denominator_power + fraction_size
    val exponent = IntegerNumber(-precision_size)
    val eq = (this.numerator.value<<(precision_size + fraction_size + 2)) / this.denominator
    val er = (this.numerator.value<<(precision_size + fraction_size + 2)) % this.denominator
    val g  = eq.value.testBit(1)
    val r  = eq.value.testBit(0)
    val s = !(er==NaturalNumber(0))
    // the 2 extra bits are eliminated from the final result
    val new_q = eq >> 2
    FloatingPoint(sign, exponent, new_q,  g::r::s::Nil, fraction_size) 
  }

  def toFixedP(size : Int, fraction_size : Int, rounding : RoundingType) : FixedPoint_B = {
    val sign = this.numerator.sign
    val denominator_power = this.denominator.value.bitLength - 1 + 1
    val precision_size = denominator_power + size
    val eq = (this.numerator.value<<(precision_size + fraction_size + 2)) / this.denominator
    val er = (this.numerator.value<<(precision_size + fraction_size + 2)) % this.denominator
    val g  = (eq >> precision_size).value.testBit(1)
    val r  = (eq >> precision_size).value.testBit(0)
    val s = !(er==NaturalNumber(0)) && ( (eq.value & ((BigInt(1) << precision_size) - 1) ) != 0)
    // the 2 extra bits are eliminated from the final result
    val new_q = eq >> (precision_size + 2)
    val l = new_q.value.testBit(0)
    FixedPoint(IntegerNumber(sign, new_q), l, g, r, s, size, fraction_size, rounding)
  }
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = {
    numerator.toInternalString + "/" + denominator.toInternalString
  }
  /*
  helper function for taylor functions
  precision = 30
  zero = ro.upb.nrs.sl.RationalNumber(0)
  one = ro.upb.nrs.sl.RationalNumber(1)
  X = this
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : RationalNumber_B = {
    val result = func(RationalNumber(0), RationalNumber(1)) (this, RationalNumber(30))
    result match {
      case RationalNumber_NR(_) => RationalNumber_NR
      case RationalNumber(a, b) => RationalNumber(a, b)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.RationalNumber helper_taylor_function")
    }
  }
}


object RationalNumber_NR extends RationalNumber_B {
  def unapply(input : RationalNumber_B) = if(input.equals(RationalNumber_NR)) Some(None) else None
  /*
  local values
  */
  override val numerator: IntegerNumber_B = IntegerNumber_NR
  override val denominator : NaturalNumber_B = NaturalNumber_NR
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
  override def +(that: NumberRepresentationSystem): RationalNumber_B = this
  override def -(that: NumberRepresentationSystem): RationalNumber_B = this
  override def *(that: NumberRepresentationSystem): RationalNumber_B = this
  override def /(that: NumberRepresentationSystem): RationalNumber_B = this
  override def /\(that: NumberRepresentationSystem): RationalNumber_B = this
  override def %(that: NumberRepresentationSystem): RationalNumber_B = this
  override def pow(that: NumberRepresentationSystem): RationalNumber_B = this
  override def unary_- : RationalNumber_B = this
  override def inverse : RationalNumber_B = this
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
  override def !=(that: NumberRepresentationSystem): Boolean = false
  override def >(that: NumberRepresentationSystem): Boolean = false
  override def >=(that: NumberRepresentationSystem): Boolean = false
  override def <=(that: NumberRepresentationSystem): Boolean = false
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
  override def min(that: NumberRepresentationSystem): RationalNumber_B = this
  override def max(that: NumberRepresentationSystem): RationalNumber_B = this
  override def abs: RationalNumber_B = this
  override def signum: RationalNumber_B = this
  override def nth_root (n : Int) : (RationalNumber_B, RationalNumber_B) = (this, this)
  override def nqrt(n : Int): RationalNumber_B = this
  override def sqrt: RationalNumber_B = this
  override def exp: RationalNumber_B = this
  override def ln: RationalNumber_B = this
  override def log(base: NumberRepresentationSystem) : RationalNumber_B = this
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
  override def toRationalNumber : RationalNumber_B = this
  override def toBigDecimal : BigDecimal = BigDecimal(1)/BigDecimal(0)
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = "NR"
  override def toString : String = "NR"
}


class RationalNumber(numerator_c : IntegerNumber_B, denominator_c : NaturalNumber_B) extends RationalNumber_B {
  override val numerator: IntegerNumber_B = numerator_c
  override val denominator: NaturalNumber_B = denominator_c
}

object RationalNumber {
  def apply(numerator: IntegerNumber_B, denominator : NaturalNumber_B) : RationalNumber_B = (numerator, denominator) match {
    case (IntegerNumber_NR(_), _) => RationalNumber_NR
    case (_, NaturalNumber_NR(_)) => RationalNumber_NR
    case (IntegerNumber(sign, x), y) => {
        if(y==NaturalNumber(0)) RationalNumber_NR
        else if (x==NaturalNumber(0)) new RationalNumber(IntegerNumber(false, x), NaturalNumber(1))
        else {
            val gcd_xy = x.gcd(y)
            new RationalNumber(IntegerNumber(sign, x/gcd_xy), y/gcd_xy)
        }
    }
    case _=> throw new NotImplementedError("ro.upb.nrs.sl.RationalNumber apply")
  }
  def apply(a : Int) : RationalNumber_B = this.apply(IntegerNumber(a), NaturalNumber(1))
  def apply(a : Long) : RationalNumber_B = this.apply(IntegerNumber(a), NaturalNumber(1))
  def apply(a : Int, b :Int) : RationalNumber_B = this.apply(IntegerNumber(a), NaturalNumber(b))
  def apply(a : Long, b :Long) : RationalNumber_B = this.apply(IntegerNumber(a), NaturalNumber(b))
  def apply(numerator: IntegerNumber_B) : RationalNumber_B = this.apply(numerator, NaturalNumber(1))
  def apply(numerator: NaturalNumber_B) : RationalNumber_B = this.apply(IntegerNumber(false, numerator), NaturalNumber(1))

  def apply(a : Double) : RationalNumber_B = {
    val bits : Long = java.lang.Double.doubleToRawLongBits(a)
    //sign bit
    val negative : Boolean = (bits & 0x8000000000000000L) != 0
    val exponent_bits : Long = ((bits & 0x7ff0000000000000L) >> 52)
    // exponent =  exponent_bits-1023 (bias exponent)
    val exponent : Long = exponent_bits - 1023
    // mantisa = 1 << 52 + fraction_bits if not subnormal else fraction_bits
    val mantissa : Long = if(exponent != -1023) 
                            (bits & 0x000fffffffffffffL) | 0x0010000000000000L
                          else
                            (bits & 0x000fffffffffffffL)
    /* 
    D = 2^exponent * (mantisa / 2^52)
    */
    val exponent_Q : RationalNumber_B = RationalNumber(2).pow(RationalNumber(exponent))
    val fraction_size_Q : NaturalNumber_B = NaturalNumber(2).pow(NaturalNumber(52))
    val mantisa_Q : RationalNumber_B = RationalNumber(IntegerNumber(mantissa), fraction_size_Q)
    val result : RationalNumber_B = ( if(negative) RationalNumber(-1) else RationalNumber(1) ) * exponent_Q * mantisa_Q
    result
  }
  
  def apply(a : Float) : RationalNumber_B = this.apply(a.toDouble)

  def fromString(textString : String) : RationalNumber_B = {
    if(textString.count(_ == '.') <= 1) {
      val length = textString.length
      val decimal_point = if (textString.count(_ == '.') == 1) textString.indexOf('.') else length - 1
      val denominator_power = length - decimal_point - 1 //(1 for the point)
      val sign = if(textString.head == '-') true else false
      val numerator = IntegerNumber(sign, NaturalNumber(BigInt(textString.filter(x => (x != '.') && (x != '-')))))
      val denominator = NaturalNumber(10).pow(NaturalNumber(denominator_power))
      this.apply(numerator, denominator)
    } else {
      RationalNumber_NR
    }
  }

  def unapply(input : RationalNumber_B) = Some(input.numerator, input.denominator)
}