package ro.upb.nrs.sl



trait  NumberRepresentationSystem {
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
  def +(that: NumberRepresentationSystem) : NumberRepresentationSystem
  def -(that: NumberRepresentationSystem) : NumberRepresentationSystem
  def *(that: NumberRepresentationSystem) : NumberRepresentationSystem
  def /(that: NumberRepresentationSystem) : NumberRepresentationSystem
  def /\(that: NumberRepresentationSystem) : NumberRepresentationSystem
  def %(that: NumberRepresentationSystem) : NumberRepresentationSystem
  def pow(that: NumberRepresentationSystem) : NumberRepresentationSystem
  def **(that: NumberRepresentationSystem) : NumberRepresentationSystem = this.pow(that)
  def unary_- : NumberRepresentationSystem
  def inverse : NumberRepresentationSystem

  /*
  logical operation (ordering)
  less
  equal
  not equal
  greater
  greater or equal
  less or equal
  */
  def <(that: NumberRepresentationSystem) : Boolean
  def ==(that: NumberRepresentationSystem) : Boolean
  def !=(that: NumberRepresentationSystem) : Boolean = !(this==that)
  def >(that: NumberRepresentationSystem) : Boolean = !(this<that) && !(this==that)
  def >=(that: NumberRepresentationSystem) : Boolean = !(this<that)
  def <=(that: NumberRepresentationSystem) : Boolean = (this<that) || (this==that)

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
  def min(that: NumberRepresentationSystem) : NumberRepresentationSystem = if(this<that) this else that
  def max(that: NumberRepresentationSystem) : NumberRepresentationSystem = if(this<that) that else this
  def abs : NumberRepresentationSystem = this.signum * this
  def signum : NumberRepresentationSystem
  def nth_root(n : Int) : (NumberRepresentationSystem, NumberRepresentationSystem)
  def nqrt(n : Int) : NumberRepresentationSystem = this.nth_root(n)._1
  def sqrt : NumberRepresentationSystem = this.nqrt(2)
  def exp : NumberRepresentationSystem
  def ln : NumberRepresentationSystem
  def log(base: NumberRepresentationSystem) : NumberRepresentationSystem = this.ln / base.ln
  /*
  trigonometric functions
  sin
  cos
  tan
  cot
  sec
  csc
  */
  def sin : NumberRepresentationSystem
  def cos : NumberRepresentationSystem
  def tan : NumberRepresentationSystem
  def cot : NumberRepresentationSystem
  def sec : NumberRepresentationSystem
  def csc : NumberRepresentationSystem

  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  def arcsin : NumberRepresentationSystem
  def arccos : NumberRepresentationSystem
  def arctan : NumberRepresentationSystem
  def arccot : NumberRepresentationSystem
  def arcsec : NumberRepresentationSystem
  def arccsc : NumberRepresentationSystem

  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  def sinh : NumberRepresentationSystem
  def cosh : NumberRepresentationSystem
  def tanh : NumberRepresentationSystem
  def coth : NumberRepresentationSystem
  def sech : NumberRepresentationSystem
  def csch : NumberRepresentationSystem

  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  def arcsinh : NumberRepresentationSystem
  def arccosh : NumberRepresentationSystem
  def arctanh : NumberRepresentationSystem
  def arccoth : NumberRepresentationSystem
  def arcsech : NumberRepresentationSystem
  def arccsch : NumberRepresentationSystem

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
  def toRationalNumber : RationalNumber_B
  def toBigDecimal : BigDecimal
  def toBigInt : BigInt = BigInt(this.toBigDecimal.toLong)
  def toInt : Int = this.toBigDecimal.toInt
  def toLong : Long = this.toBigDecimal.toLong
  def toFloat : Float = this.toBigDecimal.toFloat
  def toDouble : Double = this.toBigDecimal.toDouble

  /*
  Show functions
  toString
  toInternalString
  */
  override def toString : String = this.toBigDecimal.toString
  def toInternalString : String
}




object NumberRepresentationSystemConversions {
  implicit var numberRepresentationSystemWorkingType : NumberRepresentationSystem = NaturalNumber_NR
  implicit def fromInt(x : Int) : NumberRepresentationSystem = numberRepresentationSystemWorkingType match {
    case NaturalNumber_NR(_) => NaturalNumber(x)
    case IntegerNumber_NR(_) => IntegerNumber(x)
    case RationalNumber_NR(_) => RationalNumber(x)
    case InfiniteFixedPoint_NR(_) => InfiniteFixedPoint(x)
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint(x)
    case FixedNaturalNumber_NR(y, z) => FixedNaturalNumber(x, y, z)
    case FixedIntegerNumber_NR(y, z) => FixedIntegerNumber(x, y, z)
    case FractionalNumber_NR(y, z, t) => FractionalNumber(x, y, z, t)
    case FixedPoint_NR(y, z, t) => FixedPoint(x, y, z, t)
    case oldFixedFloatingPoint_NR(y, z, t) => oldFixedFloatingPoint(x, y, z, t)
    case FixedFloatingPoint_NR(y, z, t) => FixedFloatingPoint(x, y, z, t)
    case oldIEEE754_NR(y, z, t) => oldIEEE754(x, y, z, t)
    case IEEE754_NR(y, z, t) => IEEE754(x, y, z, t)
    case oldPosit_NR(that_es , that_size, that_round) => oldPosit(x, that_es , that_size, that_round)
    case Posit_NR(that_es , that_size, that_round) => Posit(x, that_es , that_size, that_round)
    case Morris_NR(that_g_size , that_size, that_round) => Morris(x, that_g_size , that_size, that_round)
    case MorrisHEB_NR(that_g_size , that_size, that_round) => MorrisHEB(x, that_g_size , that_size, that_round)
    case MorrisBiasHEB_NR(that_g_size , that_size, that_round) => MorrisBiasHEB(x, that_g_size , that_size, that_round)
    case MorrisUnaryHEB_NR(that_size, that_round) => MorrisUnaryHEB(x, that_size, that_round)
    case _ => throw new NotImplementedError("class")
  }
  implicit def fromLong(x : Long) : NumberRepresentationSystem = numberRepresentationSystemWorkingType match {
    case NaturalNumber_NR(_) => NaturalNumber(x)
    case IntegerNumber_NR(_) => IntegerNumber(x)
    case RationalNumber_NR(_) => RationalNumber(x)
    case InfiniteFixedPoint_NR(_) => InfiniteFixedPoint(x)
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint(x)
    case FixedNaturalNumber_NR(y, z) => FixedNaturalNumber(x, y, z)
    case FixedIntegerNumber_NR(y, z) => FixedIntegerNumber(x, y, z)
    case FractionalNumber_NR(y, z, t) => FractionalNumber(x, y, z, t)
    case FixedPoint_NR(y, z, t) => FixedPoint(x, y, z, t)
    case oldFixedFloatingPoint_NR(y, z, t) => oldFixedFloatingPoint(x, y, z, t)
    case FixedFloatingPoint_NR(y, z, t) => FixedFloatingPoint(x, y, z, t)
    case oldIEEE754_NR(y, z, t) => oldIEEE754(x, y, z, t)
    case IEEE754_NR(y, z, t) => IEEE754(x, y, z, t)
    case oldPosit_NR(that_es , that_size, that_round) => oldPosit(x, that_es , that_size, that_round)
    case Posit_NR(that_es , that_size, that_round) => Posit(x, that_es , that_size, that_round)
    case Morris_NR(that_g_size , that_size, that_round) => Morris(x, that_g_size , that_size, that_round)
    case MorrisHEB_NR(that_g_size , that_size, that_round) => MorrisHEB(x, that_g_size , that_size, that_round)
    case MorrisBiasHEB_NR(that_g_size , that_size, that_round) => MorrisBiasHEB(x, that_g_size , that_size, that_round)
    case MorrisUnaryHEB_NR(that_size, that_round) => MorrisUnaryHEB(x, that_size, that_round)
    case _ => throw new NotImplementedError("class")
  }
  implicit def fromFloat(x : Float) : NumberRepresentationSystem = numberRepresentationSystemWorkingType match {
    case NaturalNumber_NR(_) => NaturalNumber(x)
    case IntegerNumber_NR(_) => IntegerNumber(x)
    case RationalNumber_NR(_) => RationalNumber(x)
    case InfiniteFixedPoint_NR(_) => InfiniteFixedPoint(x)
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint(x)
    case FixedNaturalNumber_NR(y, z) => FixedNaturalNumber(x, y, z)
    case FixedIntegerNumber_NR(y, z) => FixedIntegerNumber(x, y, z)
    case FractionalNumber_NR(y, z, t) => FractionalNumber(x, y, z, t)
    case FixedPoint_NR(y, z, t) => FixedPoint(x, y, z, t)
    case oldFixedFloatingPoint_NR(y, z, t) => oldFixedFloatingPoint(x, y, z, t)
    case FixedFloatingPoint_NR(y, z, t) => FixedFloatingPoint(x, y, z, t)
    case oldIEEE754_NR(y, z, t) => oldIEEE754(x, y, z, t)
    case IEEE754_NR(y, z, t) => IEEE754(x, y, z, t)
    case oldPosit_NR(that_es , that_size, that_round) => oldPosit(x, that_es , that_size, that_round)
    case Posit_NR(that_es , that_size, that_round) => Posit(x, that_es , that_size, that_round)
    case Morris_NR(that_g_size , that_size, that_round) => Morris(x, that_g_size , that_size, that_round)
    case MorrisHEB_NR(that_g_size , that_size, that_round) => MorrisHEB(x, that_g_size , that_size, that_round)
    case MorrisBiasHEB_NR(that_g_size , that_size, that_round) => MorrisBiasHEB(x, that_g_size , that_size, that_round)
    case MorrisUnaryHEB_NR(that_size, that_round) => MorrisUnaryHEB(x, that_size, that_round)
    case _ => throw new NotImplementedError("class")
  }
  implicit def fromDouble(x : Double) : NumberRepresentationSystem = numberRepresentationSystemWorkingType match {
    case NaturalNumber_NR(_) => NaturalNumber(x)
    case IntegerNumber_NR(_) => IntegerNumber(x)
    case RationalNumber_NR(_) => RationalNumber(x)
    case InfiniteFixedPoint_NR(_) => InfiniteFixedPoint(x)
    case InfiniteFloatingPoint_NR(_) => InfiniteFloatingPoint(x)
    case FixedNaturalNumber_NR(y, z) => FixedNaturalNumber(x, y, z)
    case FixedIntegerNumber_NR(y, z) => FixedIntegerNumber(x, y, z)
    case FractionalNumber_NR(y, z, t) => FractionalNumber(x, y, z, t)
    case FixedPoint_NR(y, z, t) => FixedPoint(x, y, z, t)
    case oldFixedFloatingPoint_NR(y, z, t) => oldFixedFloatingPoint(x, y, z, t)
    case FixedFloatingPoint_NR(y, z, t) => FixedFloatingPoint(x, y, z, t)
    case oldIEEE754_NR(y, z, t) => oldIEEE754(x, y, z, t)
    case IEEE754_NR(y, z, t) => IEEE754(x, y, z, t)
    case oldPosit_NR(that_es , that_size, that_round) => oldPosit(x, that_es , that_size, that_round)
    case Posit_NR(that_es , that_size, that_round) => Posit(x, that_es , that_size, that_round)
    case Morris_NR(that_g_size , that_size, that_round) => Morris(x, that_g_size , that_size, that_round)
    case MorrisHEB_NR(that_g_size , that_size, that_round) => MorrisHEB(x, that_g_size , that_size, that_round)
    case MorrisBiasHEB_NR(that_g_size , that_size, that_round) => MorrisBiasHEB(x, that_g_size , that_size, that_round)
    case MorrisUnaryHEB_NR(that_size, that_round) => MorrisUnaryHEB(x, that_size, that_round)
    case _ => throw new NotImplementedError("class")
  }
}