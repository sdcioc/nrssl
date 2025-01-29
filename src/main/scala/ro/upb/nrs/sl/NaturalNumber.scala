package ro.upb.nrs.sl




import scala.annotation.tailrec
import scala.language.implicitConversions

abstract class NaturalNumber_B extends NumberRepresentationSystem {
  /*
  underlying value
  */
  val value : BigInt
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
  */
  override def +(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => NaturalNumber(this.value + ys)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber +")
  }
  override def -(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => NaturalNumber(this.value - ys)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber -")
  }
  override def *(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => NaturalNumber(this.value * ys)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber *")
  }
  /*
  BIGINT devident /% divisor returns (quotient, remainder)
  devident = divisor * quotient + remainder
  remainder >= 0
  */
  override def /(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => if(ys != 0) NaturalNumber((this.value /% ys)._1) else NaturalNumber_NR
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber /")
  }
  override def /\(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) =>  if(ys != 0) if((this.value /% ys)._2==0) NaturalNumber((this.value /% ys)._1) else NaturalNumber_NR else NaturalNumber_NR
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber /\\")
  }
  override def %(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => if(ys != 0) NaturalNumber((this.value /% ys)._2)  else NaturalNumber_NR
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber %")
  }
  override def pow(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => if(ys.isValidInt) NaturalNumber(this.value.pow(ys.intValue)) 
                        else if(ys.isValidLong) {
                          val temp1 = this.value.pow(1<<30)
                          val temp2 = temp1.pow(4) // power for bits over first 32 bits
                          val one : Long = 1
                          val longdivider = one << 32
                          val extraLong = ys.toLong / longdivider // MSB 32-bits
                          val extraInt = ys.toLong % longdivider // LSB 32-bits
                          val temp3 = temp2.pow(extraLong.toInt)
                          if(extraInt >= (one<<31)) {
                            NaturalNumber(temp3 * temp1 * temp1 * this.value.pow( (extraInt-(one<<31)).toInt ))
                          } else {
                            NaturalNumber(temp3 * this.value.pow( extraInt.toInt ))
                          }
                        }
                        else throw new NotImplementedError("raised to a power too big to compute even in infinite precision")
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber pow")
  }

  override def unary_- : NaturalNumber_B = if(this.value == 0) this else NaturalNumber_NR
  override def inverse : NaturalNumber_B = if(this.value == 1) this else NaturalNumber_NR
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
    case NaturalNumber_NR(_) => false
    case NaturalNumber(ys) => this.value < ys
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case NaturalNumber_NR(_) => false
    case NaturalNumber(ys) => this.value == ys
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber >")
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
  override def min(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => if (this<=NaturalNumber(ys)) this else NaturalNumber(ys)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber min")
  }
  override def max(that: NumberRepresentationSystem): NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => if (this>=NaturalNumber(ys)) this else NaturalNumber(ys)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber max")
  }
  override def abs: NaturalNumber_B = this
  override def signum: NaturalNumber_B = if (this.value == 0) this else NaturalNumber(1)
  /*
  https://en.wikipedia.org/wiki/Nth_root#Computing_principal_roots
  This is a derivate for binary base
  result = n order root of Devident
  D = current Devident
  P = current result
  X = biggest posibile Digit so Y <= REMAINDER (1 or 0 in binary)
      so we use only 1 to verify because for X = 0 => Y = 0
  B = the numerice base (2 for binary)
  D = Devident
  P = 0
  REMAINDER = 0
  while(D != 0)
    Y = SUM of Base^iterator * PascalaTriangle(n, iterator) * P^iterator * X^(n-iterator)
        with iterator from 0 to n-1
    REMAINDER = REAMINDER << n + MSB n bits of D
    D = D drop MSB n bits
    if(REMAINDER >= Y) {
      REAMINDER = REMAINDER - Y
      P = P << 1 + 1
    } else {
      P = P << 1
    }
  result = (P, REMAINDER)
   */
  def nth_root (n : Int) : (NaturalNumber_B, NaturalNumber_B) = {
    require(n>=2)
    def pascal_triangle(row : Int, col : Int) : Int = {
      require(col<=row)
      (row, col) match {
        case (_, 0) => 1
        case (_, _) => if(col==row) 1 else pascal_triangle(row-1, col-1) + pascal_triangle(row-1, col)
      }
    }
    @tailrec
    def helper(d: List[Boolean], r : NaturalNumber_B, q : NaturalNumber_B) : (NaturalNumber_B, NaturalNumber_B) = d match {
      case Nil => (q, r)
      case _ => {
        /*
        {\displaystyle \sum _{i=0}^{n-1}10^{i}P(n,i)p^{i}x^{n-i}}\sum _{i=0}^{n-1}10^{i}P(n,i)p^{i}x^{n-i}.

        Sum by i from 0 to n-1 of B^i * PascalaTriangle(n, i) * p^i * x^(n-i)

        B = the base in which we make the calculus (for us 2) -> shiting with i to the left
        p = is Q the current result
        x = the maximum digit we can have so R>=Y but for base 2 si only 1
          */
        val y : NaturalNumber_B = (0 to (n-1)).toList map (i=> (NaturalNumber(pascal_triangle(n,i))*q.pow(NaturalNumber(i)))<<i) reduceLeft (_ + _)
        val new_r = (r<<n) + NaturalNumber(auxiliaryFunctions.BinaryEncodetoInt((d take n).reverse))
        if(new_r>y  || new_r==y)  helper(d drop n, new_r - y, (q<<1) + NaturalNumber(1))
        else helper(d drop n, new_r, (q<<1))
      }
    }
    

    val toAdd = n - this.binaryEncode.length % n
    helper((this.binaryEncode++List.fill[Boolean](toAdd)(false)).reverse, NaturalNumber(0), NaturalNumber(0))
  }
  override def nqrt(n : Int): NaturalNumber_B =  this.nth_root(n)._1
  override def sqrt: NaturalNumber_B = this.nqrt(2)
  override def exp: NaturalNumber_B = if(this.value == 0) NaturalNumber(1) else NaturalNumber_NR
  override def ln: NaturalNumber_B = if(this.value == 1) NaturalNumber(0) else NaturalNumber_NR
  override def log(base: NumberRepresentationSystem) : NaturalNumber_B = base match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => {
      if(ys==2)
        if(this.isPowerOfTwo) 
          NaturalNumber(this.value.bitLength - 1)
        else
          NaturalNumber_NR
      else throw new NotImplementedError("only base 2 for ro.upb.nrs.sl.NaturalNumber")
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber log")
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
  override def sin : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber sin")
  override def cos : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber cos")
  override def tan : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber tan")
  override def cot : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber cot")
  override def sec : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber sec")
  override def csc : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber csc")
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arcsin")
  override def arccos : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arccos")
  override def arctan : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arctan")
  override def arccot : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arccot")
  override def arcsec : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arcsec")
  override def arccsc : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arccsc")
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber sinh")
  override def cosh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber cosh")
  override def tanh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber tanh")
  override def coth : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber coth")
  override def sech : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber sech")
  override def csch : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber csch")
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arcsinh")
  override def arccosh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arccosh")
  override def arctanh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arctanh")
  override def arccoth : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arccoth")
  override def arcsech : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arcsech")
  override def arccsch : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber arccsch")
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
  override def toRationalNumber : RationalNumber_B = RationalNumber(this)
  override def toBigDecimal : BigDecimal = BigDecimal(this.value)
  override def toBigInt : BigInt = this.value
  override def toInt : Int = this.value.intValue
  override def toLong : Long = this.value.longValue
  override def toFloat : Float = this.value.floatValue
  override def toDouble : Double = this.value.doubleValue
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = {
    value.toString + " bitLength: " + value.bitLength.toString + " binary:" + value.toString(2)
  }
  /*
  conversions to binary
  */
  def binaryEncode: List[Boolean] = if(this.value.bitLength == 0) List(false)
                                    else ((0 to this.value.bitLength - 1) map this.value.testBit).toList
  /*
  extra functions
  */
  def <<(n : Int) : NaturalNumber_B = NaturalNumber(this.value << n)
  def >>(n : Int) : NaturalNumber_B = NaturalNumber(this.value >> n)
  /*
  https://en.wikipedia.org/wiki/Euclidean_algorithm
  */
  def gcd(that: NumberRepresentationSystem) : NaturalNumber_B = that match {
    case NaturalNumber_NR(_) => NaturalNumber_NR
    case NaturalNumber(ys) => NaturalNumber(this.value.gcd(ys))
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.NaturalNumber gcd")
  }
  //only one bit set
  def isPowerOfTwo : Boolean = this.binaryEncode.init match {
    case Nil => true
    case ys => !(ys.foldLeft(false)(_ | _))
  }
}

case object NaturalNumber_NR extends NaturalNumber_B {
  /*
  unapply method for match case scenarios
  */
  def unapply(input : NaturalNumber_B) = if(input.equals(NaturalNumber_NR)) Some(None) else None
  //local value
  override val value: BigInt = 0
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
  override def +(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def -(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def *(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def /(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def /\(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def %(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def pow(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def unary_- : NaturalNumber_B = this
  override def inverse : NaturalNumber_B = this
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
  override def min(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def max(that: NumberRepresentationSystem): NaturalNumber_B = this
  override def abs: NaturalNumber_B = this
  override def signum: NaturalNumber_B = this
  override def nth_root(n : Int): (NaturalNumber_B,NaturalNumber_B) = (this, this)
  override def log(base: NumberRepresentationSystem) : NaturalNumber_B = this

  override def isPowerOfTwo : Boolean = false
  override def toString : String = "NR"
}

class NaturalNumber(value_constructor: BigInt) extends NaturalNumber_B {
  override val value: BigInt = value_constructor
}


object NaturalNumber {
  def apply(value_constructor: BigInt) :NaturalNumber_B = new NaturalNumber(value_constructor)

  def apply(a : Int) : NaturalNumber_B = {
    require(a>=0)
    this.apply(BigInt(a))
  }
  def apply(a : Long) : NaturalNumber_B = {
    require(a>=0)
    this.apply(BigInt(a))
  }
  def apply(a : Float) : NaturalNumber_B = NaturalNumber(a.toLong)
  def apply(a : Double) : NaturalNumber_B = NaturalNumber(a.toLong)
  def unapply(input : NaturalNumber_B) = Some(input.value)
}