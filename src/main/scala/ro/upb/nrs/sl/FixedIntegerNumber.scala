package ro.upb.nrs.sl




abstract class FixedIntegerNumber_B extends FixedPrecisionNumberRepresentationSystem {
 /*
  underlying value, size and rounding
  */
  val value : IntegerNumber_B
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
  override def +(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => FixedIntegerNumber(this.value + x, y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber +")
  }
  override def -(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => FixedIntegerNumber(this.value - x, y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber -")
  }
  override def *(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => FixedIntegerNumber(this.value * x, y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber *")
  }
  override def /(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => if(x!=IntegerNumber(0)) {
        //we add two bits to deviident so we can get exactly  g and r
        // bit value
        val eq = (this.value.value<<2) / x.value
        val er = (this.value.value<<2) % x.value
        val g  = eq.value.testBit(1)
        val r  = eq.value.testBit(0)
        val s = !(er==NaturalNumber(0))
        val new_q = eq >> 2
        val l = new_q.value.testBit(0)
        FixedIntegerNumber(IntegerNumber(this.value.sign^x.sign,  new_q), l, g, r, s, size, rounding)
    } else FixedIntegerNumber_NR(size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber /")
  }
  override def /\(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => if(x!=IntegerNumber(0)) FixedIntegerNumber(this.value / x, y, z) else FixedIntegerNumber_NR(size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber /\\")
  }
  override def %(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => if(x!=IntegerNumber(0))  FixedIntegerNumber(this.value % x, y, z) else FixedIntegerNumber_NR(size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber %")
  }
  override def pow(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => FixedIntegerNumber(this.value.pow(x), size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber pow")
  }
  override def unary_- : FixedIntegerNumber_B = FixedIntegerNumber(this.value.unary_-, size, rounding)
  override def inverse : FixedIntegerNumber_B = FixedIntegerNumber(this.value.inverse, size, rounding)
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
    case FixedIntegerNumber_NR(_, _) => false
    case FixedIntegerNumber(x, y, z) => this.value < x
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case FixedIntegerNumber_NR(_, _) => false
    case FixedIntegerNumber(x, y, z) => this.value == x
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber ==")
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
  override def min(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => FixedIntegerNumber(this.value.min(x), y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber min")
  }
  override def max(that: NumberRepresentationSystem): FixedIntegerNumber_B = that match {
    case FixedIntegerNumber_NR(_, _) => FixedIntegerNumber_NR(size, rounding)
    case FixedIntegerNumber(x, y, z) => FixedIntegerNumber(this.value.max(x), y, z)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber max")
  }
  override def abs: FixedIntegerNumber_B = FixedIntegerNumber(this.value.abs, size, rounding)
  override def signum: FixedIntegerNumber_B = FixedIntegerNumber(this.value.signum, size, rounding)
  override def nth_root (n : Int) : (FixedIntegerNumber_B, FixedIntegerNumber_B) = {
    val (q, r) = this.value.nth_root(n)
    (FixedIntegerNumber(q, size, rounding), FixedIntegerNumber(IntegerNumber(false, r), size, rounding))
  }
  override def nqrt (n : Int) : FixedIntegerNumber_B = {
    /*
    we do the n order root for absolute value with 2*n extra
    bits so we can find g r bits. 
    */
    val (pos_q, pos_r) = (this.value.value<<(2*n)).nth_root(n)
    val g  = pos_q.value.testBit(1)
    val r  = pos_q.value.testBit(0)
    val s = !(pos_r==NaturalNumber(0))
    val q = pos_q >> 2
    val l = q.value.testBit(0)
    if(this.value.sign & n%2==0) FixedIntegerNumber_NR(size, rounding)
    else FixedIntegerNumber(IntegerNumber(this.value.sign, q), l, g, r, s, size, rounding)
  }
  override def sqrt: FixedIntegerNumber_B = this.nqrt(2)
  override def exp: FixedIntegerNumber_B = FixedIntegerNumber(this.value.exp, size, rounding)
  override def ln: FixedIntegerNumber_B = FixedIntegerNumber(this.value.ln, size, rounding)
  override def log(base: NumberRepresentationSystem) : FixedIntegerNumber_B = FixedIntegerNumber(this.value.log(base), size, rounding)
  /*
  trigonometric functions
  sin
  cos
  tan
  cot
  sec
  csc
  */
  override def sin : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber sin")
  override def cos : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber cos")
  override def tan : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber tan")
  override def cot : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber cot")
  override def sec : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber sec")
  override def csc : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber csc")
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arcsin")
  override def arccos : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arccos")
  override def arctan : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arctan")
  override def arccot : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arccot")
  override def arcsec : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arcsec")
  override def arccsc : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arccsc")
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber sinh")
  override def cosh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber cosh")
  override def tanh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber tanh")
  override def coth : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber coth")
  override def sech : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber sech")
  override def csch : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber csch")
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arcsinh")
  override def arccosh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arccosh")
  override def arctanh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arctanh")
  override def arccoth : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arccoth")
  override def arcsech : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arcsech")
  override def arccsch : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber arccsch")
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
  override def toRationalNumber : RationalNumber_B = this.value.toRationalNumber
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
  override def toBinaryString : String = if(this.value.sign) "1" else "0" + auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size-1, this.value.value.binaryEncode))
}

class FixedIntegerNumber_NR(size_c : Int, rounding_c : RoundingType) extends FixedIntegerNumber_B {
  override val value : IntegerNumber_B = IntegerNumber_NR
  override val size : Int = size_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
}

object FixedIntegerNumber_NR {
  def apply(size_c : Int, rounding_c : RoundingType) : FixedIntegerNumber_B = new FixedIntegerNumber_NR(size_c, rounding_c)
  def unapply(input : FixedIntegerNumber_B) = if(input.value.equals(IntegerNumber_NR)) Some(input.size, input.rounding) else None
}

class FixedIntegerNumber(value_c : IntegerNumber_B, size_c : Int, rounding_c : RoundingType) extends FixedIntegerNumber_B {
  override val value : IntegerNumber_B = value_c
  override val size : Int = size_c
  override val rounding : RoundingType = rounding_c
}


object FixedIntegerNumber {
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
  implicit def fromIntToFixedIntegerNumber(a : Int):FixedIntegerNumber_B = FixedIntegerNumber(a)
  implicit def fromLongToFixedIntegerNumber(a : Long):FixedIntegerNumber_B = FixedIntegerNumber(a)
  implicit def fromFloatToFixedIntegerNumber(a : Float):FixedIntegerNumber_B = FixedIntegerNumber(a)
  implicit def fromDoubleToFixedIntegerNumber(a : Double):FixedIntegerNumber_B = FixedIntegerNumber(a)

  def apply(value_c : IntegerNumber_B, size_c : Int, rounding_c : RoundingType) : FixedIntegerNumber_B = {
      if(value_c.value.value.bitLength <= size_c-1)
        value_c match {
          case IntegerNumber_NR(_) => FixedIntegerNumber_NR(size_c, rounding_c)
          case IntegerNumber(x, y) => new FixedIntegerNumber(value_c, size_c, rounding_c)
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.FixedIntegerNumber apply")
        }
      else
        FixedIntegerNumber_NR(size_c, rounding_c)
  }
  def apply(value_c : IntegerNumber_B, l : Boolean, g  : Boolean, r : Boolean, s : Boolean,
            size_c : Int, rounding_c : RoundingType) : FixedIntegerNumber_B = rounding_c match {
      case RoundUp => if(value_c.sign) this.apply(value_c, size_c, rounding_c) else if(g|r|s) this.apply(value_c + IntegerNumber(1), size_c, rounding_c) else this.apply(value_c, size_c, rounding_c)
      case RoundDown => if(!value_c.sign) this.apply(value_c, size_c, rounding_c) else if(g|r|s) this.apply(value_c + IntegerNumber(-1), size_c, rounding_c) else this.apply(value_c, size_c, rounding_c)
      case RoundZero => this.apply(value_c, size_c, rounding_c)
      case RoundAwayZero => if(g|r|s) if(value_c.sign) this.apply(value_c + IntegerNumber(-1), size_c, rounding_c) else this.apply(value_c + IntegerNumber(1), size_c, rounding_c) else this.apply(value_c, size_c, rounding_c)
      case RoundEven => if(g&(r|s|l)) if(value_c.sign) this.apply(value_c + IntegerNumber(-1), size_c, rounding_c) else this.apply(value_c + IntegerNumber(1), size_c, rounding_c) else this.apply(value_c, size_c, rounding_c)
      case _ => this.apply(value_c, size_c, rounding_c) 
  }
  def apply(a : Int, size_c : Int, rounding_c : RoundingType) : FixedIntegerNumber_B = this.apply(IntegerNumber(a), size_c, rounding_c)
  def apply(a : Int) : FixedIntegerNumber_B = this.apply(IntegerNumber(a), this.default_size, this.default_rounding)
  def apply(a : Long, size_c : Int, rounding_c : RoundingType) : FixedIntegerNumber_B = this.apply(IntegerNumber(a), size_c, rounding_c)
  def apply(a : Long) : FixedIntegerNumber_B = this.apply(IntegerNumber(a), this.default_size, this.default_rounding)
  def apply(a : Float, size_c : Int, rounding_c : RoundingType) : FixedIntegerNumber_B = this.apply(IntegerNumber(a.toLong), size_c, rounding_c)
  def apply(a : Float) : FixedIntegerNumber_B = this.apply(IntegerNumber(a.toLong), this.default_size, this.default_rounding)
  def apply(a : Double, size_c : Int, rounding_c : RoundingType) : FixedIntegerNumber_B = this.apply(IntegerNumber(a.toLong), size_c, rounding_c)
  def apply(a : Double) : FixedIntegerNumber_B = this.apply(IntegerNumber(a.toLong), this.default_size, this.default_rounding)
  def unapply(input : FixedIntegerNumber_B) = Some(input.value, input.size, input.rounding)
}
