package ro.upb.nrs.sl




abstract class FractionalNumber_B extends FixedPrecisionNumberRepresentationSystem {
  /*
  local value, size for numerator, size for denominator and rounding
  */
  val value : RationalNumber_B
  val size_numerator : Int
  val size_denominator : Int
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
  override def +(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value + x, size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber +")
  }
  override def -(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value - x, size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber -")
  }
  override def *(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value * x, size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber *")
  }
  override def /(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value / x, size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber /")
  }
  override def /\(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value /\ x, size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber /\\")
  }
  override def %(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value % x, size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber %")
  }
  override def pow(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value.pow(x), size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber pow")
  }
  override def unary_- : FractionalNumber_B = FractionalNumber(this.value.unary_-, size_numerator, size_denominator, rounding)
  override def inverse : FractionalNumber_B = FractionalNumber(this.value.inverse, size_numerator, size_denominator, rounding)
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
    case FractionalNumber_NR(_, _, _) => false
    case FractionalNumber(x, y, z, t) => this.value < x
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber <")
  }

  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case FractionalNumber_NR(_, _, _) => false
    case FractionalNumber(x, y, z, t) => this.value == x
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber ==")
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
  override def min(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value.min(x), size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber min")
  }
  override def max(that: NumberRepresentationSystem): FractionalNumber_B = that match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => FractionalNumber(this.value.max(x), size_numerator, size_denominator, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber max")
  }
  override def abs: FractionalNumber_B = FractionalNumber(this.value.abs, size_numerator, size_denominator, rounding)
  override def signum: FractionalNumber_B = FractionalNumber(this.value.signum, size_numerator, size_denominator, rounding)
  override def nth_root (n : Int) : (FractionalNumber_B, FractionalNumber_B) = {
    val (root, remainder) = this.value.nth_root(n)
    (FractionalNumber(root, size_numerator, size_denominator, rounding),
     FractionalNumber(remainder, size_numerator, size_denominator, rounding))
  }
  override def nqrt(n : Int): FractionalNumber_B = FractionalNumber(this.value.nqrt(n), size_numerator, size_denominator, rounding)
  override def sqrt: FractionalNumber_B = FractionalNumber(this.value.sqrt, size_numerator, size_denominator, rounding)
  override def exp: FractionalNumber_B = helper_taylor_function(mathFunctions.exp)
  override def ln: FractionalNumber_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem): FractionalNumber_B = base match {
    case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
    case FractionalNumber(x, y, z, t) => this.ln / FractionalNumber(x, y, z, t).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber log")
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
  override def sin : FractionalNumber_B = helper_taylor_function(mathFunctions.sin)
  override def cos : FractionalNumber_B = helper_taylor_function(mathFunctions.cos)
  override def tan : FractionalNumber_B = helper_taylor_function(mathFunctions.tan)
  override def cot : FractionalNumber_B = helper_taylor_function(mathFunctions.cot)
  override def sec : FractionalNumber_B = helper_taylor_function(mathFunctions.sec)
  override def csc : FractionalNumber_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : FractionalNumber_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : FractionalNumber_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : FractionalNumber_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : FractionalNumber_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : FractionalNumber_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : FractionalNumber_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : FractionalNumber_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : FractionalNumber_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : FractionalNumber_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : FractionalNumber_B = helper_taylor_function(mathFunctions.coth)
  override def sech : FractionalNumber_B = helper_taylor_function(mathFunctions.sech)
  override def csch : FractionalNumber_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : FractionalNumber_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : FractionalNumber_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : FractionalNumber_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : FractionalNumber_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : FractionalNumber_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : FractionalNumber_B = helper_taylor_function(mathFunctions.arccsch)
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
  override def toRationalNumber : RationalNumber_B = this.value
  override def toBigDecimal : BigDecimal = this.value.toBigDecimal
  /*
  Show functions
  toString
  toInternalString
  toBinaryString
  */
  override def toInternalString: String = value.toInternalString
  override def toBinaryString: String = {
    val binaryNumerator : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size_numerator, this.value.numerator.value.binaryEncode))
    val binaryDenominator : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size_denominator, this.value.denominator.binaryEncode))
    val binaryValue : String = (if (this.value.numerator.sign) "1" else "0") + binaryNumerator + binaryDenominator
    binaryValue
  }
  
  
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : FractionalNumber_B = {
    val min_size : Int = if(size_denominator < size_numerator) size_denominator else size_numerator
    val precision : Int = if(min_size < 16) 5 else if (min_size < 32) 7 else if (min_size < 64) 12 else 20
    val result = func(
      FractionalNumber(0, size_numerator, size_denominator, rounding),
      FractionalNumber(1, size_numerator, size_denominator, rounding)) (this,
      FractionalNumber(precision, size_numerator, size_denominator, rounding))
    result match {
      case FractionalNumber_NR(_, _, _) => FractionalNumber_NR(size_numerator, size_denominator, rounding)
      case FractionalNumber(x, y, z, t) => FractionalNumber(x, y, z, t)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber helper_taylor_function")
    }
  }
}

class FractionalNumber_NR(size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) extends FractionalNumber_B {
  override val value : RationalNumber_B = RationalNumber_NR
  override val size_numerator : Int = size_numerator_c
  override val size_denominator : Int = size_denominator_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
}

object FractionalNumber_NR {
  def apply(size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = new FractionalNumber_NR(size_numerator_c, size_denominator_c, rounding_c)
  def unapply(input : FractionalNumber_B) = if(input.value.equals(RationalNumber_NR)) Some(input.size_numerator, input.size_denominator, input.rounding) else None
}

class FractionalNumber(value_c : RationalNumber_B, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) extends FractionalNumber_B {
  override val value : RationalNumber_B = value_c
  override val size_numerator : Int = size_numerator_c
  override val size_denominator : Int = size_denominator_c
  override val rounding : RoundingType = rounding_c
}


object FractionalNumber {
  /*
  default sizes and rounding
  */
  var default_size_numerator : Int = 16
  var default_size_denominator : Int = 16
  var default_rounding : RoundingType = RoundEven
  /*
  set new sizes and rounding
  */
  def set_size_rounding(size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) = {
    this.default_size_numerator = size_numerator_c
    this.default_size_denominator = size_denominator_c
    this.default_rounding = rounding_c
  }

  def apply(value_c : RationalNumber_B, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = {
      /*
      If the number can go inside the size we divede both numerator and denominator with 2 until they fit in the sizes given.
      If denominator is too big it will be rounded to zero
      If numerator is too big it will be a NR
      */
      def helper(numerator : IntegerNumber_B, denominator : NaturalNumber_B, size_numerator : Int, size_denominator : Int) : RationalNumber_B = {
        if( (numerator.value.value.bitLength <= (size_numerator-1) ) && (denominator.value.bitLength <= size_denominator) ) {
          RationalNumber(numerator, denominator)
        } else {
          //DISCUSS: numerator/2 denominator/2 instead of shifts -> it makes abs go wrong. it is correct ?
          // here can rounfing have a part ?
          helper(IntegerNumber(numerator.sign, numerator.value>>1), denominator>>1, size_numerator, size_denominator)
        }
      }
      val new_q : RationalNumber_B = helper(value_c.numerator, value_c.denominator, size_numerator_c, size_denominator_c)
      new_q match {
        case RationalNumber_NR(_) => FractionalNumber_NR(size_numerator_c, size_denominator_c, rounding_c)
        case RationalNumber(_, _) => new FractionalNumber(new_q, size_numerator_c, size_denominator_c, rounding_c)
        case _=> throw new NotImplementedError("ro.upb.nrs.sl.FractionalNumber apply")
      }
  }
  def apply(a : Double, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = this.apply(RationalNumber(a), size_numerator_c, size_denominator_c, rounding_c)
  def apply(a : Double) : FractionalNumber_B = this.apply(RationalNumber(a), this.default_size_numerator, this.default_size_denominator, this.default_rounding)
  def apply(a : Float, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = this.apply(RationalNumber(a), size_numerator_c, size_denominator_c, rounding_c)
  def apply(a : Float) : FractionalNumber_B = this.apply(RationalNumber(a), this.default_size_numerator, this.default_size_denominator, this.default_rounding)
  def apply(a : Long, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = this.apply(RationalNumber(a), size_numerator_c, size_denominator_c, rounding_c)
  def apply(a : Long) : FractionalNumber_B = this.apply(RationalNumber(a), this.default_size_numerator, this.default_size_denominator, this.default_rounding)
  def apply(a : Int, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = this.apply(RationalNumber(a), size_numerator_c, size_denominator_c, rounding_c)
  def apply(a : Int) : FractionalNumber_B = this.apply(RationalNumber(a), this.default_size_numerator, this.default_size_denominator, this.default_rounding)
  def apply(a : Int, b : Int) : FractionalNumber_B = this.apply(RationalNumber(a,b), this.default_size_numerator, this.default_size_denominator, this.default_rounding)
  def apply(a : Long, b : Long) : FractionalNumber_B = this.apply(RationalNumber(a,b), this.default_size_numerator, this.default_size_denominator, this.default_rounding)
  def apply(a : Int, b : Int, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = this.apply(RationalNumber(a,b), size_numerator_c, size_denominator_c, rounding_c)
  def apply(a : Long, b : Long, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = this.apply(RationalNumber(a,b), size_numerator_c, size_denominator_c, rounding_c)
  def apply(binaryString : String, size_numerator_c : Int, size_denominator_c : Int, rounding_c : RoundingType) : FractionalNumber_B = {
    val numeratorString :  String = (binaryString drop 1) take size_numerator_c
    val denominatorString : String = binaryString drop ( 1 + size_numerator_c )
    this.apply( RationalNumber(IntegerNumber( binaryString(0) == '1', NaturalNumber(BigInt(numeratorString, 2)) ), NaturalNumber(BigInt(denominatorString, 2))), size_numerator_c, size_denominator_c, rounding_c )
  }
  def unapply(input : FractionalNumber_B) = Some(input.value, input.size_numerator, input.size_denominator, input.rounding)
}
