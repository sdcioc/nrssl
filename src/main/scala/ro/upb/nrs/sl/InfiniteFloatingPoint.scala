package ro.upb.nrs.sl



/*
We consider ro.upb.nrs.sl.InfiniteFloatingPoint
as 2^exponent * mantisa
= 2^exponent * ( a / 2^fs )
a / 2^fs is in [1,2) or in (-2,-1]  -- constructor takes care of this
*/
abstract class InfiniteFloatingPoint_B extends NumberRepresentationSystem {
  /*
  exponent and mantisa values
  */
  val exponent : IntegerNumber_B
  val mantisa : InfiniteFixedPoint_B
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
  We consider that the first element has a bigger exponent value if
  not we switch them. we can consider e1>=e2
  2^e1 * mantisa1 + 2^e2 * mantisa2 = 2^e1 * ( mantisa1 + 2^(e2 - e1) * mantisa2 )
  = 2^e1 * ( mantis1 + ( mantisa2 / 2^(e1-e2) ) )
  = 2^e1 * mantisa3 -- the constructor will take care to have mantisa in the right
  range and to modify the exponent accordingly to it
  */
  override def +(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = that match {
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
    case InfiniteFloatingPoint(a, b) => if(exponent<a) InfiniteFloatingPoint(a, b)+this
                               else InfiniteFloatingPoint(exponent, mantisa + (b / InfiniteFixedPoint(RationalNumber(2).pow(RationalNumber(exponent-a))) ) )
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint +")
  }
  /*
  Same as addition
  2^e1 * mantisa1 - 2^e2 * mantisa2 = 2^e1 * ( mantisa1 - 2^(e2 - e1) * mantisa2 )
  = 2^e1 * ( mantis1 - ( mantisa2 / 2^(e1-e2) ) )
  = 2^e1 * mantisa3
  */
  override def -(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = that match {
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
    case InfiniteFloatingPoint(a, b) => if(exponent<a) -(InfiniteFloatingPoint(a, b)-this) 
                               else InfiniteFloatingPoint(exponent, mantisa - (b / InfiniteFixedPoint(RationalNumber(2).pow(RationalNumber(exponent-a))) ) )
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint -")
  }
  /*
  2^e1 * mantisa1 * 2^e2 * mantisa2 = 2^(e1 + e2) * (mantisa1 * mantisa2)
  */
  override def *(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = that match {
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
    case InfiniteFloatingPoint(a, b) => InfiniteFloatingPoint(exponent+a, mantisa *b)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint *")
  }
  /*
  2^e1 * mantisa1 / 2^e2 * mantisa2 = 2^(e1 - e2) * (mantisa1 / mantisa2)
  */
  override def /(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = that match {
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
    case InfiniteFloatingPoint(a, b) => if(b.value.numerator.value != NaturalNumber(0)) InfiniteFloatingPoint(exponent-a, mantisa/b) else InfiniteFloatingPoint_NR
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint /")
  }
  override def /\(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this/that
  override def %(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint %")
  override def pow(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = that match {
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
    case InfiniteFloatingPoint(a, b) => InfiniteFloatingPoint(this.toRationalNumber.pow(InfiniteFloatingPoint(a, b).toRationalNumber))
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint pow")
  }
  /*
  -2^e1 * mantisa1 = 2^e1 * -mantisa1
  */
  override def unary_- : InfiniteFloatingPoint_B = InfiniteFloatingPoint(exponent, -mantisa)
  override def inverse : InfiniteFloatingPoint_B = InfiniteFloatingPoint(1) / this
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
    case InfiniteFloatingPoint_NR(_) => false
    case InfiniteFloatingPoint(a, b) => if(exponent < a) true 
                               else if(exponent > a) false
                               else if(mantisa>b) false else true
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case InfiniteFloatingPoint_NR(_) => false
    case InfiniteFloatingPoint(a, b) => (exponent == a) && (mantisa == b)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint ==")
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
  override def min(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = that match {
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
    case InfiniteFloatingPoint(a, b) => if(that<this) InfiniteFloatingPoint(a, b) else this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint min")
  }
  override def max(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = that match {
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
    case InfiniteFloatingPoint(a, b) => if(that<this) this else InfiniteFloatingPoint(a, b)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint max")
  }
  override def abs: InfiniteFloatingPoint_B = InfiniteFloatingPoint(exponent, mantisa.abs)
  override def signum: InfiniteFloatingPoint_B = InfiniteFloatingPoint(IntegerNumber(0), mantisa.signum)
  override def nth_root (n : Int) : (InfiniteFloatingPoint_B, InfiniteFloatingPoint_B) = {
    require(n >= 2)
    val root = this.nqrt(n)
    (root, this-root.pow(InfiniteFloatingPoint(n)))
  }
  /*
  [X] - integer value of X
  NQRT(2^e1 * mantisa1) = NQRT(2^( n * [e1 / n] + e1 - n * [e1 / n] ) * mantisa1)
  = NQRT(2^( n * [e1 / n] ) * 2^( e1 - n * [e1 / n] ) * mantisa1) =
  = 2^([e1 / n]) * NQRT(2^( e1 - n * [e1 / n] ) * mantisa1)
  */
  override def nqrt (n : Int) : InfiniteFloatingPoint_B = {
    require(n >= 2)
    // maybe to make nqrt without the extra prexision from Q ?
    InfiniteFloatingPoint(
      exponent / IntegerNumber(n),
      InfiniteFixedPoint( 
        (RationalNumber(2).pow( RationalNumber( exponent - ( ( exponent/IntegerNumber(n) ) * IntegerNumber(n) ) ) ) * 
        mantisa.value).nqrt(n)
      )
    )
  }
  override def sqrt: InfiniteFloatingPoint_B = this.nqrt(2)
  override def exp : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.exp)
  override def ln : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : InfiniteFloatingPoint_B = base match {
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
    case InfiniteFloatingPoint(a, b) => this.ln / InfiniteFloatingPoint(a, b).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint log")
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
  override def sin : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.sin)
  override def cos : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.cos)
  override def tan : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.tan)
  override def cot : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.cot)
  override def sec : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.sec)
  override def csc : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.coth)
  override def sech : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.sech)
  override def csch : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : InfiniteFloatingPoint_B = helper_taylor_function(mathFunctions.arccsch)
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
  override def toRationalNumber : RationalNumber_B = RationalNumber(2).pow(RationalNumber(exponent)) * mantisa.value
  override def toBigDecimal : BigDecimal = this.toRationalNumber.toBigDecimal
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = "exp=" + exponent.toInternalString + " mantisa=" + mantisa.toInternalString
  
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : InfiniteFloatingPoint_B = {
    val result = func(
      InfiniteFloatingPoint(0),
      InfiniteFloatingPoint(1)) (this,
      InfiniteFloatingPoint(30)
    )
    result match {
      case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint_NR
      case InfiniteFloatingPoint(a, b) => InfiniteFloatingPoint(a, b)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint helper_taylor_function")
    }
  }
}


object InfiniteFloatingPoint_NR extends InfiniteFloatingPoint_B {
  def unapply(input : InfiniteFloatingPoint_B) = if(input.equals(InfiniteFloatingPoint_NR)) Some(None) else None
  /*
  local values
  */
  override val exponent : IntegerNumber_B = IntegerNumber_NR
  override val mantisa : InfiniteFixedPoint_B = InfiniteFixedPoint_NR
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
  override def +(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def -(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def *(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def /(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def /\(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def %(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def pow(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def unary_- : InfiniteFloatingPoint_B = this
  override def inverse : InfiniteFloatingPoint_B = this
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
  override def min(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def max(that: NumberRepresentationSystem): InfiniteFloatingPoint_B = this
  override def abs: InfiniteFloatingPoint_B = this
  override def signum: InfiniteFloatingPoint_B = this
  override def nth_root (n : Int) : (InfiniteFloatingPoint_B, InfiniteFloatingPoint_B) = (this, this)
  override def nqrt(n : Int): InfiniteFloatingPoint_B = this
  override def sqrt: InfiniteFloatingPoint_B = this
  override def exp: InfiniteFloatingPoint_B = this
  override def ln: InfiniteFloatingPoint_B = this
  override def log(base: NumberRepresentationSystem) : InfiniteFloatingPoint_B = this
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


class InfiniteFloatingPoint(exponent_c : IntegerNumber_B, mantisa_c : InfiniteFixedPoint_B) extends InfiniteFloatingPoint_B {
  override val exponent : IntegerNumber_B = exponent_c
  override val mantisa : InfiniteFixedPoint_B = mantisa_c
}

object InfiniteFloatingPoint {
  def apply(exponent_c : IntegerNumber_B, mantisa_c : InfiniteFixedPoint_B) : InfiniteFloatingPoint_B = (exponent_c, mantisa_c) match {
    case (IntegerNumber_NR(_), _)  => InfiniteFloatingPoint_NR
    case (_, InfiniteFixedPoint_NR(_))  => InfiniteFloatingPoint_NR
    /*
    absolute values
    a.numerator.value > (a.denominator<<1) if a >= 2
    a.numerator.value < a.denominator if a < 1
    */
    case (_, InfiniteFixedPoint(a)) => if(a.numerator.value == NaturalNumber(0)) new InfiniteFloatingPoint(exponent_c, mantisa_c)
                                else if(a.numerator.value >= (a.denominator<<1)) this.apply(exponent_c+IntegerNumber(1), mantisa_c/InfiniteFixedPoint(2))
                                else if (a.numerator.value < a.denominator) this.apply(exponent_c-IntegerNumber(1), mantisa_c*InfiniteFixedPoint(2))
                                else new InfiniteFloatingPoint(exponent_c, mantisa_c)
    case _=> throw new NotImplementedError("ro.upb.nrs.sl.InfiniteFloatingPoint apply")
  }
  def apply(a : Int) : InfiniteFloatingPoint_B = this.apply(IntegerNumber(0), InfiniteFixedPoint(a))
  def apply(a : Long) : InfiniteFloatingPoint_B = this.apply(IntegerNumber(0), InfiniteFixedPoint(a))
  def apply(a : Float) : InfiniteFloatingPoint_B = this.apply(a.toDouble)
  def apply(a : Double) : InfiniteFloatingPoint_B = this.apply(IntegerNumber(0), InfiniteFixedPoint(a))

  def apply(a : RationalNumber_B) : InfiniteFloatingPoint_B = this.apply(IntegerNumber(0), InfiniteFixedPoint(a))

  def unapply(input : InfiniteFloatingPoint) = Some(input.exponent, input.mantisa)
}