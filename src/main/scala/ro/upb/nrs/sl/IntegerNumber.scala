package ro.upb.nrs.sl




abstract class IntegerNumber_B extends NumberRepresentationSystem {
  /*
  unferlying values
  sign of the number
  and the absolute value
  */
  val sign: Boolean
  val value : NaturalNumber_B
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
  if both have the same sign than their absolute values will be sumed up
  if they have different sign the one will the bigger absolute value
  will give the sign and the abasolute value of the resutl will
  be the difference between them
  */
  override def +(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) =>IntegerNumber_NR
    case IntegerNumber(x, y) => if(x==sign) IntegerNumber(sign, value+y) else {
      if(value>=y) IntegerNumber(sign, value-y)
      else IntegerNumber(x, y-value)
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber +")
  }
  /*
  if both have the same sign if the second one has a bigger absolute
  value than it will change the sign otherwise the sign will be kept
  the absolute value of de result is the absolute value of the difference
  if the have different signs you cand consider that is an addition
  a - (-b) = a + b
  */
  override def -(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) =>IntegerNumber_NR
    case IntegerNumber(x, y) => if(x==sign)  {
      if(value>=y) IntegerNumber(sign, value-y)
      else IntegerNumber(!sign, y-value)
    } else this + IntegerNumber(!x, y)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber -")
  }
  override def *(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) =>IntegerNumber_NR
    case IntegerNumber(x, y) => IntegerNumber(x^sign, y*value)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber *")
  }
  /*
  we use Eculidian division because it has the best mathematical proprities 
  R >= 0 always
  https://en.wikipedia.org/wiki/Modulo_operation
  Only the euclidian division was implemented
  The next algorithm maintains R >= 0 with the respect to formula
  Devident = Divisor * Quotient + Remainder
  if Devident is negative if the quotient has
  the same absolute value as if it was positive than the remainder
  is negative for sure if it is diffent form zero. So when we have
  negative devidents we have to increase the avsolute value of
  quotient with one so we can respect the mathematic formula that
  says REMAINDER >= 0 always
   */
  def quout_rest(that: NumberRepresentationSystem): (IntegerNumber_B, NaturalNumber_B)  = that match {
    case IntegerNumber_NR(_) => (IntegerNumber_NR, NaturalNumber_NR)
    case IntegerNumber(x, y) => if (y!=NaturalNumber(0)){
            /*
            make the operations as the both numbers were positive
            between (their absolute values)
            */
            val (pos_quotient, pos_remainder) : (NaturalNumber_B, NaturalNumber_B) = (value / y, value % y)
            /*
             if divident is negative and the positive remainder is different from zero the absolute value of
             quotient is incrase by one and the remainder become divisor absolute value minus the
             positivve remainder if both the number were positive
             */
            val absolute_quotient : NaturalNumber_B = if(sign && (pos_remainder != NaturalNumber(0))) 
                                                  pos_quotient + NaturalNumber(1)
                                                else
                                                  pos_quotient
            val remainder : NaturalNumber_B = if(sign && (pos_remainder != NaturalNumber(0)))
                                          y - pos_remainder
                                        else
                                          pos_remainder
            (IntegerNumber(x^sign, absolute_quotient), remainder)
          } else (IntegerNumber_NR, NaturalNumber_NR)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber quotient rest")
  }
  override def /(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) => IntegerNumber_NR
    case IntegerNumber(x, y) => if (y!=NaturalNumber(0)) this.quout_rest(that)._1 else IntegerNumber_NR
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber /")
  }
  override def /\(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) => IntegerNumber_NR
    case IntegerNumber(x, y) => if (y!=NaturalNumber(0)) if(this.quout_rest(that)._2==NaturalNumber(0)) this.quout_rest(that)._1 else IntegerNumber_NR else IntegerNumber_NR
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber /\\")
  }
  override def %(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) =>IntegerNumber_NR
    case IntegerNumber(x, y) => if (y!=NaturalNumber(0)) IntegerNumber(false, this.quout_rest(that)._2) else IntegerNumber_NR
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber %")
  }
  /*
  a^(-b) = 1 / a^b
  (-a)^2k = a^2k
  (-a)^(2k+1) = -a^(2k+1)
  */
  override def pow(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) =>IntegerNumber_NR
    case IntegerNumber(x, y) => {
      if(x) {
        val a:IntegerNumber_B = IntegerNumber(1)
        val b:IntegerNumber_B = this.pow(IntegerNumber(false, y))
        a/b
      }
      else {
        if(sign) {
          if(y%NaturalNumber(2)==NaturalNumber(0)) IntegerNumber(false, value.pow(y))
          else IntegerNumber(true, value.pow(y))
        }
        else IntegerNumber(false, value.pow(y))
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber pow")
  }
  override def unary_- : IntegerNumber_B = if(value == NaturalNumber(0)) this else IntegerNumber(!sign, value)
  override def inverse : IntegerNumber_B = IntegerNumber(!sign, value.inverse)
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
    case IntegerNumber_NR(_) => false
    case IntegerNumber(x, y) => if(sign) {
      if(x) y<value
      else true
    } else {
      if(x) false
      else value<y
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case IntegerNumber_NR(_) => false
    case IntegerNumber(x, y) => (x==sign) && (y==value)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber ==")
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
  override def min(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) =>IntegerNumber_NR
    case IntegerNumber(x, y) => if(that<this) IntegerNumber(x, y) else this
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber min")
  }
  override def max(that: NumberRepresentationSystem): IntegerNumber_B = that match {
    case IntegerNumber_NR(_) => IntegerNumber_NR
    case IntegerNumber(x, y) => if(that<this) this else IntegerNumber(x, y)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber max")
  }
  override def abs: IntegerNumber_B = IntegerNumber(false, value)
  override def signum: IntegerNumber_B = if(value == NaturalNumber(0)) this else IntegerNumber(sign, NaturalNumber(1))
  // SAME as division REMAINDER >= 0 always
  override def nth_root (n : Int) : (IntegerNumber_B, NaturalNumber_B) = {
    //if negative and root order is pozitive -> complex number-> NR
    if(sign && n%2==0) (IntegerNumber_NR, NaturalNumber_NR)
    else {
        //make the n root for absolute value
        val (pos_q, pos_r) = this.value.nth_root(n)
        //if the number is negative and the remainder si different from zero
        // the absolute value of the result is increased by one and the
        // remainder is recalculated by the next formula (pos_q+1)^n-pos_q^n-pos_r
        //positive as ussualy
        //negative   (pos_q, pos_r)=nth_rooth(-a)
        // q = if(pos_r!=0) -(pos_q + 1) else -pos_q
        // r = if(pos_r!=0) (pos_q+1)^n-pos_q^n-pos_r else 0
        // new_r hopw much you need to add to -(q+1)^n to get a
        if(!sign)
          (IntegerNumber(false, pos_q), pos_r)
        else
          if(pos_r != NaturalNumber(0)) 
            (IntegerNumber(true, pos_q + NaturalNumber(1)), (pos_q + NaturalNumber(1)).pow(NaturalNumber(n)) - pos_q.pow(NaturalNumber(n)) - pos_r)
          else
            (IntegerNumber(true, pos_q), pos_r)
    }
  }
  override def nqrt(n : Int): IntegerNumber_B = this.nth_root(n)._1
  override def sqrt: IntegerNumber_B = this.nqrt(2)
  override def exp: IntegerNumber_B = if(value == NaturalNumber(0)) IntegerNumber(false, NaturalNumber(1)) else IntegerNumber_NR
  override def ln: IntegerNumber_B = {
    if(sign) IntegerNumber_NR
    else IntegerNumber(false, value.ln)
  }
  override def log(base: NumberRepresentationSystem) : IntegerNumber_B = base match {
    case IntegerNumber_NR(_) =>IntegerNumber_NR
    case IntegerNumber(x, y) => {
      if(sign) IntegerNumber_NR
      else IntegerNumber(false, value.log(y))
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber log")
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
  override def sin : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber sin")
  override def cos : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber cos")
  override def tan : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber tan")
  override def cot : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber cot")
  override def sec : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber sec")
  override def csc : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber csc")
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arcsin")
  override def arccos : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arccos")
  override def arctan : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arctan")
  override def arccot : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arccot")
  override def arcsec : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arcsec")
  override def arccsc : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arccsc")
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber sinh")
  override def cosh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber cosh")
  override def tanh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber tanh")
  override def coth : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber coth")
  override def sech : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber sech")
  override def csch : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber csch")
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arcsinh")
  override def arccosh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arccosh")
  override def arctanh : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arctanh")
  override def arccoth : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arccoth")
  override def arcsech : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arcsech")
  override def arccsch : NumberRepresentationSystem = throw new NotImplementedError("ro.upb.nrs.sl.IntegerNumber arccsch")
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
  override def toBigDecimal : BigDecimal = (if(sign) -1 else 1) * this.value.toBigDecimal
  override def toBigInt : BigInt = (if(sign) -1 else 1) * this.value.toBigInt
  override def toInt : Int = (if(sign) -1 else 1) * this.value.toInt
  override def toLong : Long = (if(sign) -1 else 1) * this.value.toLong
  override def toFloat : Float = (if(sign) -1 else 1) * this.value.toFloat
  override def toDouble : Double = (if(sign) -1 else 1) * this.value.toDouble
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = {
    (if(sign) "-" else "") + value.toInternalString
  }
}


object IntegerNumber_NR extends IntegerNumber_B {
  def unapply(input : IntegerNumber_B) = if(input.equals(IntegerNumber_NR)) Some(None) else None
  /*
  local values
  */
  override val sign: Boolean = false
  override val value: NaturalNumber_B = NaturalNumber_NR
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
  override def +(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def -(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def *(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def /(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def /\(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def %(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def pow(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def unary_- : IntegerNumber_B = this
  override def inverse : IntegerNumber_B = this
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
  override def min(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def max(that: NumberRepresentationSystem): IntegerNumber_B = this
  override def abs: IntegerNumber_B = this
  override def signum: IntegerNumber_B = this
  override def nth_root (n : Int) : (IntegerNumber_B, NaturalNumber_B) = (IntegerNumber_NR, NaturalNumber_NR)
  override def nqrt(n : Int): IntegerNumber_B = this
  override def sqrt: IntegerNumber_B = this
  override def exp: IntegerNumber_B = this
  override def ln: IntegerNumber_B = this
  override def log(base: NumberRepresentationSystem) : IntegerNumber_B = this
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


class IntegerNumber(sign_c : Boolean, value_c : NaturalNumber_B) extends IntegerNumber_B {
  override val sign: Boolean = sign_c
  override val value: NaturalNumber_B = value_c
}

object IntegerNumber {
  def apply(sign : Boolean, value : NaturalNumber_B) : IntegerNumber_B = value match {
    case NaturalNumber_NR(_) => IntegerNumber_NR
    case _=> {
      if(value == NaturalNumber(0)) new IntegerNumber(false, value)
      else new IntegerNumber(sign, value)
    } 
  }
  def apply(a : Int) : IntegerNumber_B = {
    if(a<0) this.apply(true, NaturalNumber(-a))
    else this.apply(false, NaturalNumber(a))
  }
  def apply(a : Long) : IntegerNumber_B = {
    if(a<0) this.apply(true, NaturalNumber(-a))
    else this.apply(false, NaturalNumber(a))
  }
  def apply(a : Float) : IntegerNumber_B = this.apply(a.toLong)
  def apply(a : Double) : IntegerNumber_B = this.apply(a.toLong)

  def unapply(input : IntegerNumber) = Some(input.sign, input.value)
}