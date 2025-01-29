package ro.upb.nrs.sl



/*
We consider FixedPoint as a subset of Q where
denominator is a power of two
*/
abstract class InfiniteFixedPoint_B extends NumberRepresentationSystem {
  /*
  underlying value
  */
  val value : RationalNumber_B
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
  override def +(that: NumberRepresentationSystem): InfiniteFixedPoint_B = that match {
    case InfiniteFixedPoint_NR(_) => InfiniteFixedPoint_NR
    case InfiniteFixedPoint(a) => InfiniteFixedPoint(value + a)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint +")
  }
  override def -(that: NumberRepresentationSystem): InfiniteFixedPoint_B = that match {
    case InfiniteFixedPoint_NR(_)  => InfiniteFixedPoint_NR
    case InfiniteFixedPoint(a) => InfiniteFixedPoint(value - a)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint -")
  }
  override def *(that: NumberRepresentationSystem): InfiniteFixedPoint_B = that match {
    case InfiniteFixedPoint_NR(_)  => InfiniteFixedPoint_NR
    case InfiniteFixedPoint(a) => InfiniteFixedPoint(value * a)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint *")
  }
  override def /(that: NumberRepresentationSystem): InfiniteFixedPoint_B = that match {
    case InfiniteFixedPoint_NR(_)  => InfiniteFixedPoint_NR
    case InfiniteFixedPoint(a) => InfiniteFixedPoint(value / a)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint /")
  }
  override def /\(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this/that
  override def %(that: NumberRepresentationSystem): InfiniteFixedPoint_B = throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint %")
  override def pow(that: NumberRepresentationSystem): InfiniteFixedPoint_B = that match {
    case InfiniteFixedPoint_NR(_)  => InfiniteFixedPoint_NR
    case InfiniteFixedPoint(a) => InfiniteFixedPoint(value.pow(a))
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint /\\")
  }
  override def unary_- : InfiniteFixedPoint_B = InfiniteFixedPoint(-value)
  override def inverse : InfiniteFixedPoint_B = InfiniteFixedPoint(value.inverse)
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
    case InfiniteFixedPoint_NR(_)  => false
    case InfiniteFixedPoint(a) => value < a
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case InfiniteFixedPoint_NR(_)  => false
    case InfiniteFixedPoint(a) => value == a
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint ==")
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
  override def min(that: NumberRepresentationSystem): InfiniteFixedPoint_B = that match {
    case InfiniteFixedPoint_NR(_)  => InfiniteFixedPoint_NR
    case InfiniteFixedPoint(a) => if(that<this) InfiniteFixedPoint(a) else this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint min")
  }
  override def max(that: NumberRepresentationSystem): InfiniteFixedPoint_B = that match {
    case InfiniteFixedPoint_NR(_)  => InfiniteFixedPoint_NR
    case InfiniteFixedPoint(a) => if(that<this) this else InfiniteFixedPoint(a)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint max")
  }
  override def abs: InfiniteFixedPoint_B = InfiniteFixedPoint(value.abs)
  override def signum: InfiniteFixedPoint_B = InfiniteFixedPoint(value.signum)

  override def nth_root (n : Int) : (InfiniteFixedPoint_B, InfiniteFixedPoint_B) = {
    val (root, remainder) = this.value.nth_root(n)
    (InfiniteFixedPoint(root), InfiniteFixedPoint(remainder))
  }
  override def nqrt(n : Int): InfiniteFixedPoint_B = InfiniteFixedPoint(value.nqrt(n))
  override def sqrt: InfiniteFixedPoint_B = InfiniteFixedPoint(value.sqrt)
  override def exp : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.exp)
  override def ln : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : InfiniteFixedPoint_B = base match {
    case InfiniteFixedPoint_NR(_)  => InfiniteFixedPoint_NR
    case InfiniteFixedPoint(a) => this.ln / InfiniteFixedPoint(a).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint log")
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
  override def sin : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.sin)
  override def cos : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.cos)
  override def tan : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.tan)
  override def cot : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.cot)
  override def sec : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.sec)
  override def csc : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.coth)
  override def sech : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.sech)
  override def csch : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : InfiniteFixedPoint_B = helper_taylor_function(mathFunctions.arccsch)
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
  */
  override def toInternalString: String = value.toInternalString
  
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : InfiniteFixedPoint_B = {
    val result = func(
      InfiniteFixedPoint(0),
      InfiniteFixedPoint(1)) (this,
      InfiniteFixedPoint(30))
    result match {
      case InfiniteFixedPoint_NR(_)  => InfiniteFixedPoint_NR
      case InfiniteFixedPoint(a) => InfiniteFixedPoint(a)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint helper_taylor_function")
    }
  }
}


object InfiniteFixedPoint_NR extends InfiniteFixedPoint_B {
  def unapply(input : InfiniteFixedPoint_B) = if(input.equals(InfiniteFixedPoint_NR)) Some(None) else None
  /*
  local values
  */
  override val value: RationalNumber_B = RationalNumber_NR
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
  override def +(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def -(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def *(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def /(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def /\(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def %(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def pow(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def unary_- : InfiniteFixedPoint_B = this
  override def inverse : InfiniteFixedPoint_B = this
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
  override def min(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def max(that: NumberRepresentationSystem): InfiniteFixedPoint_B = this
  override def abs: InfiniteFixedPoint_B = this
  override def signum: InfiniteFixedPoint_B = this
  override def nth_root (n : Int) : (InfiniteFixedPoint_B, InfiniteFixedPoint_B) = (this, this)
  override def nqrt(n : Int): InfiniteFixedPoint_B = this
  override def sqrt: InfiniteFixedPoint_B = this
  override def exp: InfiniteFixedPoint_B = this
  override def ln: InfiniteFixedPoint_B = this
  override def log(base: NumberRepresentationSystem) : InfiniteFixedPoint_B = this
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
  override def toRationalNumber : RationalNumber_B = RationalNumber_NR
  override def toBigDecimal : BigDecimal = BigDecimal(1)/BigDecimal(0)
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = "NR"
  override def toString : String = "NR"
}


class InfiniteFixedPoint(value_c : RationalNumber_B) extends InfiniteFixedPoint_B {
  override val value: RationalNumber_B = value_c
}

object InfiniteFixedPoint {
  /*
  We consider Inifinite precision FixedPoint a number that can be writen as ( a / 2^fs ) with a in Z so
  FixedPoint is in fact a subset of Q where denominator is power of two
  */
  def apply(value_c : RationalNumber_B) : InfiniteFixedPoint_B = value_c match {
    case RationalNumber_NR(_) => InfiniteFixedPoint_NR
    case RationalNumber(numerator, denominator) => if(denominator.isPowerOfTwo) new InfiniteFixedPoint(value_c) else InfiniteFixedPoint_NR
    case _=> throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFixedPoint apply")
  }

  def apply(a : Int) : InfiniteFixedPoint_B = this.apply(RationalNumber(a))
  def apply(a : Long) : InfiniteFixedPoint_B = this.apply(RationalNumber(a))
  def apply(a : Float) : InfiniteFixedPoint_B = this.apply(RationalNumber(a))
  def apply(a : Double) : InfiniteFixedPoint_B = this.apply(RationalNumber(a))

  def apply(numerator: IntegerNumber_B) : InfiniteFixedPoint_B = this.apply(RationalNumber(numerator))
  def apply(numerator: NaturalNumber_B) : InfiniteFixedPoint_B = this.apply(RationalNumber(numerator))

  def unapply(input : InfiniteFixedPoint) = Some(input.value)
}