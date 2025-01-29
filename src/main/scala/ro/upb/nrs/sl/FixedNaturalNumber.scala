package ro.upb.nrs.sl




abstract class FixedNaturalNumber_B extends FixedPrecisionNumberRepresentationSystem {
  /*
  underlying value, size and rounding
  */
  val value : NaturalNumber_B
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
  override def +(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => FixedNaturalNumber(this.value + x, y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber +")
  }
  override def -(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => FixedNaturalNumber(this.value - x, y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber -")
  }
  override def *(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => FixedNaturalNumber(this.value * x, y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber *")
  }
  override def /(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => if(x != NaturalNumber(0)) {
        /*
        The devident is shifted 2 bits left so we can find more precise
        the g and r bits
        */
        val eq = (this.value<<2) / x
        val er = (this.value<<2) % x
        val g  = eq.value.testBit(1)
        val r  = eq.value.testBit(0)
        val s = !(er==NaturalNumber(0))
        // the 2 extra bits are eliminated from the final result
        val new_q = eq >> 2
        val l = new_q.value.testBit(0)
        FixedNaturalNumber(new_q, l, g, r, s, size, rounding)
    } else FixedNaturalNumber_NR(size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber /")
  }
  override def /\(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => if(x != NaturalNumber(0)) FixedNaturalNumber(this.value / x, y, z) else FixedNaturalNumber_NR(size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber /\\")
  }
  override def %(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => if(x != NaturalNumber(0)) FixedNaturalNumber(this.value % x, y, z) else FixedNaturalNumber_NR(size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber %")
  }
  override def pow(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => FixedNaturalNumber(this.value.pow(x), size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber pow")
  }
  override def unary_- : FixedNaturalNumber_B = if(this.value.value == 0) this else FixedNaturalNumber_NR(size, rounding) 
  override def inverse : FixedNaturalNumber_B = if(this.value.value == 1) this else FixedNaturalNumber_NR(size, rounding)
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
    case FixedNaturalNumber_NR(_, _) => false
    case FixedNaturalNumber(x, y, z) => this.value < x
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case FixedNaturalNumber_NR(_, _) => false
    case FixedNaturalNumber(x, y, z) => this.value == x
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber ==")
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
  override def min(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => FixedNaturalNumber(this.value.min(x), y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber min")
  }
  override def max(that: NumberRepresentationSystem): FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => FixedNaturalNumber(this.value.max(x), y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber max")
  }
  override def abs: FixedNaturalNumber_B = this
  override def signum: FixedNaturalNumber_B = FixedNaturalNumber(this.value.signum, size, rounding)
  override def nth_root(n : Int) : (FixedNaturalNumber_B, FixedNaturalNumber_B) = {
    val (eq, er) = this.value.nth_root(n)
    //can be different from nqrt because it does not have rouding
    (FixedNaturalNumber(eq, size, rounding), FixedNaturalNumber(er, size, rounding))
  }
  override def nqrt (n : Int) : FixedNaturalNumber_B = {
    /*
    same as division but for nqrt for every one bit in result
    it needs to add n bits in the devident, so for 2 extra bits
    in result we shifted left the devident with 2*n.
    */
    val (eq, er) = (this.value<<(2*n)).nth_root(n)
    val g  = eq.value.testBit(1)
    val r  = eq.value.testBit(0)
    val s = !(er==NaturalNumber(0))
    // eliminate extra bits
    val new_q = eq >> 2
    val l = new_q.value.testBit(0)
    FixedNaturalNumber(new_q, l, g, r, s, size, rounding)
  }
  override def sqrt: FixedNaturalNumber_B = this.nqrt(2)
  override def exp: FixedNaturalNumber_B = FixedNaturalNumber(this.value.exp, size, rounding)
  override def ln: FixedNaturalNumber_B = FixedNaturalNumber(this.value.ln, size, rounding)
  override def log(base : NumberRepresentationSystem): FixedNaturalNumber_B = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber log")
  /*
  trigonometric functions
  sin
  cos
  tan
  cot
  sec
  csc
  */
  override def sin : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber sin")
  override def cos : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber cos")
  override def tan : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber tan")
  override def cot : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber cot")
  override def sec : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber sec")
  override def csc : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber csc")
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arcsin")
  override def arccos : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arccos")
  override def arctan : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arctan")
  override def arccot : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arccot")
  override def arcsec : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arcsec")
  override def arccsc : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arccsc")
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber sinh")
  override def cosh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber cosh")
  override def tanh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber tanh")
  override def coth : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber coth")
  override def sech : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber sech")
  override def csch : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber csch")
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arcsinh")
  override def arccosh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arccosh")
  override def arctanh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arctanh")
  override def arccoth : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arccoth")
  override def arcsech : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arcsech")
  override def arccsch : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber arccsch")
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
  override def toRationalNumber : RationalNumber_B = RationalNumber(this.value)
  override def toBigDecimal : BigDecimal = this.value.toBigDecimal
  override def toBigInt : BigInt = this.value.toBigInt
  override def toInt : Int = this.value.toInt
  override def toLong : Long = this.value.toLong
  override def toFloat : Float = this.value.toFloat
  override def toDouble : Double = this.value.toDouble
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = value.toInternalString
  override def toBinaryString : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size, this.value.binaryEncode))
  /*
  extra functions
  */
  def gcd(that: NumberRepresentationSystem) : FixedNaturalNumber_B = that match {
    case FixedNaturalNumber_NR(_, _) => FixedNaturalNumber_NR(size, rounding)
    case FixedNaturalNumber(x, y, z) => FixedNaturalNumber(this.value.gcd(x), y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedNaturalNumber gcd")
  }
}

class FixedNaturalNumber_NR(size_c : Int, rounding_c : RoundingType) extends FixedNaturalNumber_B {
  override val value : NaturalNumber_B = NaturalNumber_NR
  override val size : Int = size_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
}

object FixedNaturalNumber_NR {
  def apply(size_c : Int, rounding_c : RoundingType) : FixedNaturalNumber_B = new FixedNaturalNumber_NR(size_c, rounding_c)
  def unapply(input : FixedNaturalNumber_B) = if(input.value.equals(NaturalNumber_NR)) Some(input.size, input.rounding) else None
}

class FixedNaturalNumber(value_c : NaturalNumber_B, size_c : Int, rounding_c : RoundingType) extends FixedNaturalNumber_B {
  override val value : NaturalNumber_B = value_c
  override val size : Int = size_c
  override val rounding : RoundingType = rounding_c
}


object FixedNaturalNumber {
  /*
  default size and rounding
  */
  var default_size : Int = 32
  var default_rounding : RoundingType = RoundEven

  /*
  set new size and rounding
  */
  def set_size_rounding(size_c : Int, rounding_c : RoundingType) = {
    this.default_size = size_c
    this.default_rounding = rounding_c
  }

  def apply(value_c : NaturalNumber_B, l : Boolean, g  : Boolean, r : Boolean, s : Boolean, size_c : Int, rounding_c : RoundingType) : FixedNaturalNumber_B = rounding_c match {
      case RoundUp => if(g|r|s) this.apply(value_c + NaturalNumber(1), size_c, rounding_c) else this.apply(value_c, size_c, rounding_c)
      case RoundDown => this.apply(value_c, size_c, rounding_c)
      case RoundZero => this.apply(value_c, size_c, rounding_c)
      case RoundAwayZero => if(g|r|s) this.apply(value_c + NaturalNumber(1), size_c, rounding_c) else this.apply(value_c, size_c, rounding_c)
      case RoundEven => if(g&(r|s|l)) this.apply(value_c + NaturalNumber(1), size_c, rounding_c) else this.apply(value_c, size_c, rounding_c)
      case _ => this.apply(value_c, size_c, rounding_c) 
  }


  def apply(value_c : NaturalNumber_B, size_c : Int, rounding_c : RoundingType) : FixedNaturalNumber_B = {
      if(value_c.value.bitLength <= size_c) new FixedNaturalNumber(value_c, size_c, rounding_c) else FixedNaturalNumber_NR(size_c, rounding_c)
  }
  
  def apply(a : Int, size_c : Int, rounding_c : RoundingType) : FixedNaturalNumber_B = {
    require(a>=0)
    this.apply(NaturalNumber(a), size_c, rounding_c)
  }
  
  def apply(a : Int) : FixedNaturalNumber_B = {
    require(a>=0)
    this.apply(NaturalNumber(a), this.default_size, this.default_rounding)
  }
  def apply(a : Long, size_c : Int , rounding_c : RoundingType) : FixedNaturalNumber_B = {
    require(a>=0)
    this.apply(NaturalNumber(a), size_c, rounding_c)
  }
  def apply(a : Long) : FixedNaturalNumber_B = {
    require(a>=0)
    this.apply(NaturalNumber(a), this.default_size, this.default_rounding)
  }
  def apply(a : Float, size_c : Int , rounding_c : RoundingType) : FixedNaturalNumber_B = {
    require(a>=0)
    this.apply(NaturalNumber(a), size_c, rounding_c)
  }
  def apply(a : Float) : FixedNaturalNumber_B = {
    require(a>=0)
    this.apply(NaturalNumber(a), this.default_size, this.default_rounding)
  }
  def apply(a : Double, size_c : Int , rounding_c : RoundingType) : FixedNaturalNumber_B = {
    require(a>=0)
    this.apply(NaturalNumber(a), size_c, rounding_c)
  }
  def apply(a : Double) : FixedNaturalNumber_B = {
    require(a>=0)
    this.apply(NaturalNumber(a), this.default_size, this.default_rounding)
  }
  def unapply(input : FixedNaturalNumber_B) = Some(input.value, input.size, input.rounding)
}