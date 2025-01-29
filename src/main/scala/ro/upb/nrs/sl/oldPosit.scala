package ro.upb.nrs.sl




import scala.annotation.tailrec

/*
We consider Posit
(-1)^sign * 2^( (2^exponent_size)^regime + binary_exponent ) * ( mantisa / 2^fraction_size )
(-1)^sign * 2^exponent * ( mantisa / 2^fraction_size )
mantisa / 2^fraction_size in [1, 2)
size, exponent_size and rounding are fixed
mantisa has the hiddent bit that is always 1
*/
abstract class oldPosit_B extends NumberRepresentationSystem {
  /*
  sign
  exponent in Z
  mantisa in N (with hidden bit)
  size of the mantisa (variable)
  size of the exponent
  size of the number
  rounding
  */
  val sign : Boolean
  val exponent : IntegerNumber_B
  val mantisa : NaturalNumber_B
  val fraction_size : Int
  val exponent_size : Int
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
  /*
  We can consider for this operation that both operands have the same sign and
  the first one has a bigger absolute value which means that is exponent is bigger.
  For the other cases: if they have different signs we can do substraction with the same sign
  this + that = this - (-that) // (negate second operand sign).
  If second operand has a bigger absolute value we can swap them. this + that = that + this
  exponent1 >= exponent2
  (-1)^sign * 2^exponent1 * ( mantisa1 / 2^fraction_size1 ) + (-1)^sign * 2^exponent2 * ( mantisa2 / 2^fraction_size2 ) =
  = (-1)^sign * 2^exponent1 * ( (mantisa1 * 2^fraction_size2) / 2^(fraction_size1 + fraction_size2) ) + (-1)^sign * 2^exponent2 * ( (mantisa2 * 2^fraction_size1) / 2^(fraction_size2 + fraction_size1) ) =
  = (-1)^sign * 2^exponent1 * ( (mantisa1 * 2^fraction_size2) + 2^(exponent2 - exponent1) * (mantisa2 * 2^fraction_size1) ) / 2^(fraction_size2 + fraction_size1) =
  = (-1)^sign * 2^exponent1 * ( (mantisa1 << fraction_size2) + ( (mantisa2 << fraction_size1) / 2^(exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
  = (-1)^sign * 2^exponent1 * ( (mantisa1 << fraction_size2) + ( (mantisa2 << fraction_size1) >> (exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
  */
  override def +(that: NumberRepresentationSystem): oldPosit_B = that match {
    case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
    case oldPosit(that_s, that_e, that_f, that_fs, _ , _, _) => {
      if(that_s != this.sign) this - (-that)
      else if(this.abs < that.abs) oldPosit(this.sign, that_e, (that_f << this.fraction_size) + ((this.mantisa << that_fs) >> (that_e-this.exponent).toInt), (this.mantisa << that_fs).binaryEncode.take((that_e-this.exponent).toInt).reverse, this.fraction_size + that_fs, this.exponent_size, this.size, this.rounding   )
      else oldPosit(this.sign, this.exponent, (this.mantisa << that_fs) + ((that_f << this.fraction_size) >> (this.exponent-that_e).toInt), (that_f << this.fraction_size).binaryEncode.take((this.exponent-that_e).toInt).reverse, this.fraction_size + that_fs, this.exponent_size, this.size, this.rounding   )
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit +")
  }
  /*
  we can consider the same case as addition with the same sign and first element with bigger absolute value.
  If the sign differ this - that = this + (-that)
  if the second absolute value is bigger this - that = -(that - this)
  (-1)^sign * 2^exponent1 * ( mantisa1 / 2^fraction_size1 ) - (-1)^sign * 2^exponent2 * ( mantisa2 / 2^fraction_size2 ) =
  = (-1)^sign * 2^exponent1 * ( (mantisa1 * 2^fraction_size2) / 2^(fraction_size1 + fraction_size2) ) - (-1)^sign * 2^exponent2 * ( (mantisa2 * 2^fraction_size1) / 2^(fraction_size2 + fraction_size1) ) =
  = (-1)^sign * 2^exponent1 * ( (mantisa1 * 2^fraction_size2) - 2^(exponent2 - exponent1) * (mantisa2 * 2^fraction_size1) ) / 2^(fraction_size2 + fraction_size1) =
  = (-1)^sign * 2^exponent1 * ( (mantisa1 << fraction_size2) - ( (mantisa2 << fraction_size1) / 2^(exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
  = (-1)^sign * 2^exponent1 * ( (mantisa1 << fraction_size2) - ( (mantisa2 << fraction_size1) >> (exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
  */
  override def -(that: NumberRepresentationSystem): oldPosit_B = that match {
     case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
    case oldPosit(that_s, that_e, that_f, that_fs, _ , _, _) => {
      if(that_s != this.sign) this + (-that)
      //TODO: mayvbe negate rest_bits
      else if(this.abs < that.abs) oldPosit(!this.sign, that_e, (that_f << this.fraction_size) - ((this.mantisa << that_fs) >> (that_e-this.exponent).toInt), (this.mantisa << that_fs).binaryEncode.take((that_e-this.exponent).toInt).reverse, this.fraction_size + that_fs, this.exponent_size, this.size, this.rounding   )
      else oldPosit(this.sign, this.exponent, (this.mantisa << that_fs) - ((that_f << this.fraction_size) >> (this.exponent-that_e).toInt), (that_f << this.fraction_size).binaryEncode.take((this.exponent-that_e).toInt).reverse, this.fraction_size + that_fs, this.exponent_size, this.size, this.rounding   )
   }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit -")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantisa1 / 2^fraction_size1 ) * (-1)^sign2 * 2^exponent2 * ( mantisa2 / 2^fraction_size2 ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) *  (mantisa1 * mantisa2) / 2^(fraction_size1 + fraction_size2 )
  */
  override def *(that: NumberRepresentationSystem): oldPosit_B = that match {
    case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
    case oldPosit(that_s, that_e, that_f, that_fs, _ , _, _) => 
      oldPosit(this.sign^that_s, this.exponent+that_e, (this.mantisa * that_f), Nil, this.fraction_size + that_fs, this.exponent_size, this.size, this.rounding   )
    
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit *")
  }
  /*
  (-1)^sign1 * 2^exponent1 * ( mantisa1 / 2^fraction_size1 ) / (-1)^sign2 * 2^exponent2 * ( mantisa2 / 2^fraction_size2 ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) *  ( (mantisa1 * 2^fraction_size2) / mantisa2) / 2^fraction_size1
  = (-1)^(sign1 xor sign2) * 2^(exponent1 - exponent2) *  ( (mantisa1 << fraction_size2) / mantisa2) / 2^fraction_size1
  */
  override def /(that: NumberRepresentationSystem): oldPosit_B = that match {
    case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
    case oldPosit(that_s, that_e, that_f, that_fs, _ , _, _) => if(that_f!=NaturalNumber(0)) {
        /* OLD WAY
        // we add two extra bits to find g and r
        val eq = (this.mantisa<<(that_fs + 2)) / that_f
        // for finding s value
        val er = (this.mantisa<<(that_fs + 2)) % that_f
        val g  = eq.value.testBit(1)
        val r  = eq.value.testBit(0)
        val s = !(er==ro.upb.nrs.sl.NaturalNumber(0))
        // eliminate the extra bits
        val new_q = eq >> 2
        val l = new_q.value.testBit(0)
        ro.upb.nrs.sl.oldPosit(this.sign^that_s, this.exponent-that_e, new_q, g::r::s::Nil, this.fraction_size, this.exponent_size, this.size, this.rounding)
        */
        // we add size extra bits to know for sure we have at least size bits in mantisa
        val eq = (this.mantisa<<(that_fs + size)) / that_f
        val er = (this.mantisa<<(that_fs + size)) % that_f
        val s = !(er==NaturalNumber(0))
        //eliminate extra bits
        val new_q = eq >> size
        oldPosit(this.sign^that_s, this.exponent-that_e, new_q, (s :: eq.binaryEncode.take(size)).reverse, this.fraction_size, this.exponent_size, this.size, this.rounding)
        
    } else oldPosit_NR(exponent_size, size, rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit /")
  }
  override def /\(that: NumberRepresentationSystem): oldPosit_B = this / that
  override def %(that: NumberRepresentationSystem): oldPosit_B = throw new NotImplementedError("ro.upb.nrs.sl.oldPosit %")
  /*
  X^Y = X^([Y]+{Y}) where [Y] is integer part of Y and {Y} is fractional part of Y
  X^(-Y) = 1 / X^Y
  */
  override def pow(that: NumberRepresentationSystem): oldPosit_B = that match {
    case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
    case oldPosit(that_s, that_e, that_f, that_fs, that_es , that_size, that_round) => {
      /*
      if that is negative
      X^(-Y) = 1 / X^Y
      */
      if(that_s)
        this.inverse.pow(-that)
      else {
        //[Y] because Y is positive [Y] is positive
        val integer_value = that.toBigInt
        //{Y} in [0, 1)
        val fractional_value = oldPosit(that_s, that_e, that_f, Nil, that_fs, that_es , that_size, that_round) - oldPosit(integer_value.toDouble, that_es , that_size, that_round)
        /*
        X^Y = X^([Y]+{Y}) = X^[Y] * X^{Y} = X^[Y] * X^( 2^exponent * ( mantisa / 2^fraction_size ) )
        mantisa / 2^fraction_size is in [1, 2) so  we can use tha format mantisa / 2^fraction_size = 1 +  fraction / 2^fraction_size
        X^Y = X^[Y] * X^( 2^exponent * ( 1 +  fraction / 2^fraction_size ) ) =
        = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        */
        //X^[Y]
        val x_integerY = mathFunctions.pow_integer(
                        oldPosit(1.0d, exponent_size, size, rounding)) (this.abs,
                        integer_value
                      )
        //X^2^exponent //exponent is negative so X^(1/2^exponent.abs)
        val x_exponentY = mathFunctions.pow_fractional(
                          oldPosit(1.0d, exponent_size, size, rounding)) (this.abs ,
                          BigInt(1), fractional_value.exponent.abs.toInt
                        )
        //(X^(2^exponent))^( fraction / 2^fraction_size )
        val x_exponentY_fraction = mathFunctions.pow_fractional(
                        oldPosit(1.0d, exponent_size, size, rounding)) (
                        x_exponentY,
                        fractional_value.mantisa.value, fractional_value.fraction_size
                      )
        //if fractional value is zero
        // result = X^[Y]
        // else result = X^[Y] * X^2^exponent * (X^(2^exponent))^( fraction / 2^fraction_size )
        // if the current number is negative we only do pow for integer numbers
        val result = if(fractional_value == oldPosit(0.0d, that_es , that_size, that_round))
                      if(this.sign == false)
                        x_integerY
                      else 
                        if((integer_value % 2) ==0)
                          x_integerY
                        else
                          -x_integerY
                     else 
                      if(this.sign == false)
                        x_integerY * x_exponentY * x_exponentY_fraction
                      else
                        oldPosit_NR(exponent_size, size, rounding)
        result match {
          case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
          case oldPosit(that_s, that_e, that_f, that_fs, that_es , that_size, that_round) => oldPosit(that_s, that_e, that_f, Nil, that_fs, that_es , that_size, that_round)
          case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit pow math")
        }
      }
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit pow")
  }
  override def unary_- : oldPosit_B = oldPosit(!this.sign, this.exponent, this.mantisa, Nil, this.fraction_size, this.exponent_size, this.size, this.rounding)
  override def inverse : oldPosit_B = oldPosit(1.0d, this.exponent_size, this.size, this.rounding) / this
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
    case oldPosit_NR(_, _,_) => false
    case oldPosit(that_s, that_e, that_f, that_fs, _ , _, _) => {
      if(this.sign != that_s) this.sign
      else if(this.exponent<that_e) !this.sign
      else if(this.exponent>that_e) this.sign
      else if(this.mantisa<that_f) !this.sign
      else if(this.mantisa>that_f) this.sign
      else false
    }
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit <")
  }
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case oldPosit_NR(_, _,_) => false
    case oldPosit(that_s, that_e, that_f, that_fs, _ , _, _) => (this.sign == that_s) && (this.exponent == that_e) && (this.mantisa == that_f)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit ==")
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
  override def min(that: NumberRepresentationSystem): oldPosit_B = that match {
    case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
    case oldPosit(that_s, that_e, that_f, that_fs, _ , _, _) => if (this<that) this else oldPosit(that_s, that_e, that_f, Nil, that_fs, this.exponent_size, this.size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit min")
  }
  override def max(that: NumberRepresentationSystem): oldPosit_B = that match {
    case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
    case oldPosit(that_s, that_e, that_f, that_fs, _ , _, _) => if (this>that) this else oldPosit(that_s, that_e, that_f, Nil, that_fs, this.exponent_size, this.size, this.rounding)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit max")
  }
  override def abs: oldPosit_B = oldPosit(false, this.exponent, this.mantisa, Nil,this.fraction_size,  this.exponent_size, this.size, this.rounding)
  override def signum: oldPosit_B = {
    if(this.mantisa == NaturalNumber(0)) this
    else oldPosit(this.sign, IntegerNumber(0), NaturalNumber(1)<<this.fraction_size, Nil, this.fraction_size, this.exponent_size, this.size, this.rounding)
  }
  override def nth_root (n : Int) : (oldPosit_B, oldPosit_B) = {
    val root = this.nqrt(n)
    (root, this-root.pow(oldPosit(n, this.exponent_size, this.size, this.rounding)))
  }
  /*
  NQRT( (-1)^sign * 2^exponent * ( mantisa / 2^fraction_size ) ) =
  = NQRT( (-1)^sign ) * NQRT( 2^( ( n * [exponent / n] ) + ( exponent % n ) ) * ( ( mantisa * 2^( (n - 1) * fraction_size ) ) / 2^( ( n * fraction_size ) ) ) )
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( 2^( exponent % n ) * ( mantisa * 2^( (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantisa * 2^( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  = NQRT( (-1)^sign ) * 2^([exponent / n]) * NQRT( mantisa * << ( ( exponent % n ) + (n - 1) * fraction_size ) ) / 2^fraction_size
  */
  override def nqrt (n : Int) : oldPosit_B = {
    /* OLD WAY
    val n_ip = ro.upb.nrs.sl.IntegerNumber(n)
    // TODO: Does it work on negativ exponent %
    // Yes because is Z TODO: maybe text with math
    // 2*n for the extra 2 bits for g and r
    val (eq, er) = (this.mantisa<<(2*n+(n-1)*fraction_size+(this.exponent%n_ip).toInt)).nth_root(n)
    val g  = eq.value.testBit(1)
    val r  = eq.value.testBit(0)
    val s = !(er==ro.upb.nrs.sl.NaturalNumber(0))
    // eliminate extra bits
    val new_q = eq >> 2
    val l = new_q.value.testBit(0)
    if(this.sign & n%2==0) ro.upb.nrs.sl.oldPosit_NR(exponent_size, size, rounding)
    else ro.upb.nrs.sl.oldPosit(this.sign, this.exponent/n_ip, new_q, g::r::s::Nil, this.fraction_size, this.exponent_size, this.size, this.rounding)
    */
    
    val n_ip = IntegerNumber(n)
    // TODO: Does it work on negativ exponent %
    // Yes because is Z TODO: maybe text with math
    // 2*n for the extra 2 bits for g and r
    val (eq, er) = (this.mantisa<<(size*n+(n-1)*fraction_size+(this.exponent%n_ip).toInt)).nth_root(n)
    val s = !(er==NaturalNumber(0))
    // eliminate extra bits
    val new_q = eq >> size
    val l = new_q.value.testBit(0)
    if(this.sign & n%2==0) oldPosit_NR(exponent_size, size, rounding)
    else oldPosit(this.sign, this.exponent/n_ip, new_q, (s :: eq.binaryEncode.take(size)).reverse, this.fraction_size, this.exponent_size, this.size, this.rounding)
    
  }
  override def sqrt: oldPosit_B = this.nqrt(2)
  override def exp : oldPosit_B = helper_taylor_function(mathFunctions.exp)
  override def ln : oldPosit_B = helper_taylor_function(mathFunctions.ln)
  override def log(base: NumberRepresentationSystem) : oldPosit_B = base match {
    case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
    case oldPosit(that_s, that_e, that_f, that_fs, that_es , that_size, that_round) => this.ln / oldPosit(that_s, that_e, that_f, Nil, that_fs, that_es , that_size, that_round).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit_B log")
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
  override def sin : oldPosit_B = helper_taylor_function(mathFunctions.sin)
  override def cos : oldPosit_B = helper_taylor_function(mathFunctions.cos)
  override def tan : oldPosit_B = helper_taylor_function(mathFunctions.tan)
  override def cot : oldPosit_B = helper_taylor_function(mathFunctions.cot)
  override def sec : oldPosit_B = helper_taylor_function(mathFunctions.sec)
  override def csc : oldPosit_B = helper_taylor_function(mathFunctions.csc)
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  override def arcsin : oldPosit_B = helper_taylor_function(mathFunctions.arcsin)
  override def arccos : oldPosit_B = helper_taylor_function(mathFunctions.arccos)
  override def arctan : oldPosit_B = helper_taylor_function(mathFunctions.arctan)
  override def arccot : oldPosit_B = helper_taylor_function(mathFunctions.arccot)
  override def arcsec : oldPosit_B = helper_taylor_function(mathFunctions.arcsec)
  override def arccsc : oldPosit_B = helper_taylor_function(mathFunctions.arccsc)
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  override def sinh : oldPosit_B = helper_taylor_function(mathFunctions.sinh)
  override def cosh : oldPosit_B = helper_taylor_function(mathFunctions.cosh)
  override def tanh : oldPosit_B = helper_taylor_function(mathFunctions.tanh)
  override def coth : oldPosit_B = helper_taylor_function(mathFunctions.coth)
  override def sech : oldPosit_B = helper_taylor_function(mathFunctions.sech)
  override def csch : oldPosit_B = helper_taylor_function(mathFunctions.csch)
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  override def arcsinh : oldPosit_B = helper_taylor_function(mathFunctions.arcsinh)
  override def arccosh : oldPosit_B = helper_taylor_function(mathFunctions.arccosh)
  override def arctanh : oldPosit_B = helper_taylor_function(mathFunctions.arctanh)
  override def arccoth : oldPosit_B = helper_taylor_function(mathFunctions.arccoth)
  override def arcsech : oldPosit_B = helper_taylor_function(mathFunctions.arcsech)
  override def arccsch : oldPosit_B = helper_taylor_function(mathFunctions.arccsch)
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
  /*
  (-1)^sign * 2^exponent * ( mantisa / 2^fraction_size )
  */
  override def toRationalNumber : RationalNumber_B = (if(sign) RationalNumber(-1) else RationalNumber(1)) * (RationalNumber(2).pow(RationalNumber(exponent)-RationalNumber(fraction_size)) * mantisa.toRationalNumber)
  override def toBigDecimal : BigDecimal = this.toRationalNumber.toBigDecimal
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = "sign=" + sign.toString +"exp=" + exponent.toInternalString + " mantisa=" + mantisa.toInternalString + " fs=" + fraction_size
  def toBinaryString: String = {
    val exponent_power : IntegerNumber_B = IntegerNumber(2).pow(IntegerNumber(exponent_size))
    // K = exponent / 2^exponent_size
    val regime : IntegerNumber_B = exponent / exponent_power
    // e = exponent - exponent / 2^exponent_size
    // e = exponent % 2^exponent_size
    val exponentWithoutRegime : IntegerNumber_B = exponent % exponent_power
    // rs = if(K >= 0) K + 2 else -K + 1
    val regime_size : Int = if(regime>=IntegerNumber(0)) (regime.toInt + 2) else (-regime.toInt + 1)
    val binaryRegime : String = auxiliaryFunctions.BinaryEncodetoBinaryString( ((if(regime>=IntegerNumber(0)) List.fill(regime_size-1)(true) ::: List(false) else List.fill(regime_size-1)(false) ::: List(true))).reverse )
    val binaryExponent : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(exponent_size, exponentWithoutRegime.value.binaryEncode))
    val mantisaWithoutHiddenBit = this.mantisa.binaryEncode take fraction_size
    val binaryMantisa : String = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(fraction_size, mantisaWithoutHiddenBit))
    val binaryAbsolute : String = (binaryRegime + binaryExponent + binaryMantisa) take (size - 1)
    val absoluteString : String = "0" + auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size - 1, (NaturalNumber(BigInt(binaryAbsolute, 2)) - (if (this.sign) NaturalNumber(1) else NaturalNumber(0))).binaryEncode  ))
    val binaryValue : String = (if (this.sign) auxiliaryFunctions.BinaryStringNegate(absoluteString) else absoluteString)
    binaryValue
  }
  /*
  helper function for taylor functions
  */
  def helper_taylor_function(func : (NumberRepresentationSystem, NumberRepresentationSystem) => (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem) : oldPosit_B = {
    val maximumExponent : Int = oldPosit.maximum_exponent(exponent_size, size).toInt
    val precision : Int = if(maximumExponent >= 108) 30 else if (maximumExponent >= 62) 20 else if(maximumExponent >= 30) 12 else if(maximumExponent >= 16) 8 else if(maximumExponent >= 7) 5 else 2
    val result = func(
      oldPosit(0.0d, exponent_size, size, rounding),
      oldPosit(1.0d, exponent_size, size, rounding)) (this,
      oldPosit(precision.toDouble, exponent_size, size, rounding))
    result match {
      case oldPosit_NR(_, _,_) => oldPosit_NR(exponent_size, size, rounding)
      case oldPosit(that_s, that_e, that_f, that_fs, that_es , that_size, that_round) => oldPosit(that_s, that_e, that_f, Nil, that_fs, that_es , that_size, that_round)
      case _ => throw new NotImplementedError("ro.upb.nrs.sl.oldPosit helper_taylor_function")
    }
  }
}

class oldPosit_NR(exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) extends oldPosit_B {
  override val sign : Boolean = false;
  override val exponent : IntegerNumber_B = IntegerNumber_NR
  override val mantisa : NaturalNumber_B = NaturalNumber_NR
  override val fraction_size : Int = 0
  override val exponent_size : Int = exponent_size_c
  override val size : Int = size_c
  override val rounding : RoundingType = rounding_c
  override def toString : String = "NR"
}

object oldPosit_NR {
  def apply(exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) : oldPosit_B = new oldPosit_NR(exponent_size_c, size_c, rounding_c)
  def unapply(input : oldPosit_B) = if(input.mantisa.equals(NaturalNumber_NR)) Some(input.exponent_size, input.size, input.rounding) else None
}

class oldPosit(sign_c : Boolean, exponent_c : IntegerNumber_B, mantisa_c : NaturalNumber_B, fraction_size_c : Int, exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) extends oldPosit_B {
  override val sign : Boolean = sign_c;
  override val exponent : IntegerNumber_B = exponent_c
  override val mantisa : NaturalNumber_B = mantisa_c
  override val fraction_size : Int = fraction_size_c
  override val exponent_size : Int = exponent_size_c
  override val size : Int = size_c
  override val rounding : RoundingType = rounding_c
}


object oldPosit {
  /*
  default sizes and rounding
  */
  var default_exponent_size : Int = 2
  var default_size : Int = 32
  var default_rounding : RoundingType = RoundEven
  /*
  set new sizes and rounding
  */
  def set_size_rounding(exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) = {
    this.default_exponent_size = exponent_size_c
    this.default_size = size_c
    this.default_rounding = rounding_c
  }
  implicit def fromIntTooldPosit(a : Int):oldPosit_B = oldPosit(a)
  implicit def fromLongTooldPosit(a : Long):oldPosit_B = oldPosit(a)
  implicit def fromFloatTooldPosit(a : Float):oldPosit_B = oldPosit(a)
  implicit def fromDoubleTooldPosit(a : Double):oldPosit_B = oldPosit(a)
  

  def minimum_exponent(exponent_size_c : Int, size_c : Int): IntegerNumber_B = {
    IntegerNumber(true, NaturalNumber(size_c - 2) << exponent_size_c )
  }
  def maximum_exponent(exponent_size_c : Int, size_c : Int): IntegerNumber_B = {
    IntegerNumber(false, NaturalNumber(size_c - 2) << exponent_size_c )
  }

  /*
  Posit has normally only even rounding
  Rounding has effect on mantisa or exponent depending on case
  down we use mantisa + 1 but but on the case can be exponent 1,2,...,2^exponent_size
  ro.upb.nrs.sl.RoundUp -> if positive and (g|r|s) than mantisa + 1 else mantisa
  ro.upb.nrs.sl.RoundDown -> if negative and (g|r|s) than mantisa + 1 else mantisa
  ro.upb.nrs.sl.RoundZero -> mantisa
  ro.upb.nrs.sl.RoundAwayZero -> if (g|r|s) mantisa + 1 else mantisa
  ro.upb.nrs.sl.RoundEven -> if (g and (l|r|s) ) mantisa + 1 else mantisa
  */
  @tailrec
  def apply(sign_c: Boolean, exponent_c : IntegerNumber_B, mantisa_c : NaturalNumber_B, rest_bits: List[Boolean], fraction_size_c : Int, exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) :oldPosit_B = {
      (exponent_c, mantisa_c) match {
        case (IntegerNumber_NR(_), _)  =>oldPosit_NR(exponent_size_c, size_c, rounding_c)
        case (_, NaturalNumber_NR(_))  =>oldPosit_NR(exponent_size_c, size_c, rounding_c)
        case (_, NaturalNumber(a)) =>     if(NaturalNumber(a)==NaturalNumber(0)) {
                                        /*
                                        if mantisa is 0
                                        if there are no rest bits than is a hard 0
                                        else it does rounding
                                        */
                                        val l = false
                                        rest_bits match {
                                            case Nil => {
                                                new oldPosit(
                                                  false, // Posit has only positive zero
                                                  minimum_exponent(exponent_size_c, size_c),
                                                  NaturalNumber(0),
                                                  0,
                                                  exponent_size_c, size_c, rounding_c)
                                            }
                                            case x::xs => {
                                              val g = x
                                              val r = xs match {case Nil => false case _ => xs.head}
                                              val s = xs match {case Nil => false case y::ys => ys match {case Nil => false case _ => ys.foldLeft(false)((a,b)=>(a|b))}}
                                              //TODO: maybe hard 0
                                              rounding_c match {
                                                  case RoundUp => if(!sign_c & (g|r|s)) this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c + NaturalNumber(1), Nil, fraction_size_c, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c, Nil, fraction_size_c, exponent_size_c, size_c, rounding_c)
                                                  case RoundDown => if(sign_c & (g|r|s)) this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c + NaturalNumber(1), Nil, fraction_size_c, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c, Nil, fraction_size_c, exponent_size_c, size_c, rounding_c)
                                                  case RoundZero => this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c, Nil, fraction_size_c, exponent_size_c, size_c, rounding_c)
                                                  case RoundAwayZero => if(g|r|s) this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c + NaturalNumber(1), Nil, fraction_size_c, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c, Nil, fraction_size_c, exponent_size_c, size_c, rounding_c)
                                                  case RoundEven => if(g&(r|s|l)) this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c + NaturalNumber(1), Nil, fraction_size_c, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c, Nil, fraction_size_c, exponent_size_c, size_c, rounding_c)
                                                  case _ => this.apply(sign_c, minimum_exponent(exponent_size_c, size_c), mantisa_c, Nil, fraction_size_c, exponent_size_c, size_c, rounding_c) 
                                              }
                                            }
                                        }
                                    } else {
                                        /*
                                        if mantisa / 2^fraction_size >= 2
                                        we divide mantisa by 2 (shifting right with 1) and increase the exponent by one
                                        (-1)^sign * 2^exponent * ( mantisa / 2^fraction_size ) =
                                        = (-1)^sign * 2^exponent * ( ( 2 * ( mantisa / 2 ) ) / 2^fraction_size )
                                        = (-1)^sign * 2^(exponent + 1) * ( ( mantisa / 2 ) / 2^fraction_size ) OLD WAY
                                        = (-1)^sign * 2^(exponent + 1) * ( mantisa / 2^(fraction_size + 1) ) NEW WAY
                                        */
                                        if(mantisa_c.binaryEncode.length > (fraction_size_c+1)) 
                                            this.apply(
                                              sign_c,
                                              exponent_c + IntegerNumber(1),
                                              /* OLD WAY
                                              mantisa_c >> 1,
                                              mantisa_c.binaryEncode.head :: rest_bits,
                                              fraction_size_c,
                                              */
                                              mantisa_c,
                                              rest_bits,
                                              fraction_size_c + 1,
                                              exponent_size_c, size_c, rounding_c
                                            )
                                          /*
                                          if mantisa / 2^fraction_size < 1
                                          we multiply mantisa by 2 (shifting left with 1) and decrease the exponent by one
                                          Because we have rest bit to protect the value we have to add the first bit from the rest bits
                                          (-1)^sign * 2^exponent * ( mantisa / 2^fraction_size ) =
                                          = (-1)^sign * 2^exponent * ( ( ( 2 * mantisa ) / 2 ) / 2^fraction_size )
                                          = (-1)^sign * 2^(exponent - 1) * ( ( 2 * mantisa ) / 2^fraction_size )
                                          */
                                        else if (mantisa_c.binaryEncode.length < (fraction_size_c+1)) 
                                            if(rest_bits != Nil)
                                                this.apply(
                                                  sign_c,
                                                  exponent_c - IntegerNumber(1), 
                                                  (mantisa_c << 1) + (if(rest_bits.head) NaturalNumber(1) else NaturalNumber(0)), //add the first bit of rest bits
                                                  rest_bits.tail,
                                                  fraction_size_c,
                                                  exponent_size_c, size_c, rounding_c
                                                )
                                            else this.apply(
                                                  sign_c,
                                                  exponent_c - IntegerNumber(1), 
                                                  (mantisa_c << 1),
                                                  Nil,
                                                  fraction_size_c,
                                                  exponent_size_c, size_c, rounding_c
                                                )
                                        else {
                                            /*if mantisa is in [1,2)
                                            if the exponent is smaller than minimum value
                                            posit will take the minimum value possible. It will not
                                            underflow to zero
                                            */
                                            if(exponent_c < minimum_exponent(exponent_size_c, size_c))
                                                new oldPosit(
                                                  sign_c,
                                                  minimum_exponent(exponent_size_c, size_c),
                                                  NaturalNumber(1),
                                                  0,
                                                  exponent_size_c, size_c, rounding_c
                                                )
                                            /*
                                            if the exponent is bigger than maximum value
                                            posit will take the maximum value possible. It will not
                                            overflow to infinite
                                            */
                                            else if (exponent_c >= maximum_exponent(exponent_size_c, size_c))
                                                new oldPosit(
                                                  sign_c,
                                                  maximum_exponent(exponent_size_c, size_c),
                                                  NaturalNumber(1),
                                                  0,
                                                  exponent_size_c, size_c, rounding_c
                                                )
                                            else {
                                                /*
                                                if exponent and mantisa are in right ranges than we
                                                calculate regime exponent and real size for exponent and fraction
                                                */
                                                // exponent_power = 2^exponent_size
                                                // es = exponent_size
                                                // K = regime value
                                                // e = exponent value
                                                // f = fraction value
                                                // fs = fraction size
                                                // rs = regime size
                                                // res = real exponent size
                                                // rfs = real fraction size
                                                val exponent_power : IntegerNumber_B = IntegerNumber(2).pow(IntegerNumber(exponent_size_c))
                                                // K = exponent / 2^exponent_size
                                                val regime : IntegerNumber_B = exponent_c / exponent_power
                                                // e = exponent - exponent / 2^exponent_size
                                                // e = exponent % 2^exponent_size
                                                val exponent : IntegerNumber_B = exponent_c % exponent_power
                                                // rs = if(K >= 0) K + 1 else -K + 1
                                                val regime_size : Int = if(regime>=IntegerNumber(0)) (regime.toInt + 2) else (-regime.toInt + 1)
                                                /*
                                                -1 for sign bit
                                                res = max(0, min(size - 1 - rs, es))
                                                */
                                                val real_exponent_size : Int = if( (size_c - 1 - regime_size) > 0 )
                                                                                  if((size_c - 1 - regime_size) >= exponent_size_c)
                                                                                    exponent_size_c
                                                                                  else size_c - 1 - regime_size
                                                                                else 0
                                                /*
                                                rfs =  = max(0, size - 1 - rs - es)
                                                */
                                                val real_fraction_size : Int = if( (size_c - 1 - regime_size - exponent_size_c) > 0 )
                                                                                (size_c - 1 - regime_size - exponent_size_c)
                                                                               else 0
                                                /*
                                                ediff = es - res
                                                */
                                                val exponent_diff : Int = exponent_size_c-real_exponent_size
                                                /*
                                                If the difference between (real) representable exponent size and exponent size is
                                                bigger or equal with 2 it means that g and r are taken from  the first 2 exponent bits that
                                                will not be reprezentable
                                                */
                                                if(exponent_diff>=2) {
                                                    // take g and r from exponent bits
                                                    val g : Boolean = (exponent.value >> (exponent_diff-1)).value.testBit(0)
                                                    val r : Boolean = (exponent.value >> (exponent_diff-2)).value.testBit(0)
                                                    // calcualte s from the other exponent bits, mantisa bits and any other rest bits
                                                    val s : Boolean = ((exponent.value.value & ((1<<(exponent_diff-2))-1)) != 0) || (mantisa_c != NaturalNumber(1<<fraction_size_c)) || rest_bits.foldLeft(false)((a,b)=>(a|b))
                                                    // because the entire exponent can not be representable will calculate the most fit representable exponent
                                                    // from the current regime and the reprezentable exponent bits (not representable bits will be considered zeros)
                                                    val new_exponent = regime * exponent_power + IntegerNumber(false, ( (exponent.value >> exponent_diff) << exponent_diff ) )
                                                    /*
                                                     we calculate l bit. if the res is zeri than the bit is taken from regime and there are five cases
                                                     infinite NR treated 
                                                     maximum value that is alreafy treated exponent >= maximum_exponent is 1 we do not do rounding here
                                                     second to maximum value is 0
                                                     minimum value is 1 -- the only case possible
                                                     zero value already treated
                                                    */
                                                    val l : Boolean = if(real_exponent_size == 0) 
                                                                        if (regime < IntegerNumber(0))
                                                                          true
                                                                        else {
                                                                          false
                                                                        }
                                                                          
                                                                      else
                                                                        (exponent.value >> exponent_diff).value.testBit(0)
                                                    // if we do not have any rounding bits
                                                    if(!g && !r && !s)
                                                      new oldPosit(
                                                        sign_c,
                                                        new_exponent,
                                                        NaturalNumber(1),
                                                        0, //real_fraction_size rfs for sure zero
                                                        exponent_size_c, size_c, rounding_c
                                                      )
                                                    else 
                                                        rounding_c match { //real_fraction_size rfs for sure zero
                                                            case RoundUp => if(!sign_c & (g|r|s)) this.apply(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case RoundDown => if(sign_c & (g|r|s)) this.apply(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case RoundZero => this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case RoundAwayZero => if(g|r|s) this.apply(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case RoundEven => if(g&(r|s|l)) this.apply(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case _ => this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) 
                                                        }
                                                } else if (exponent_diff == 1) {
                                                    /*
                                                    If the difference between (real) representable exponent size and exponent size is
                                                    1 it means that g is taken from the exponent bit that
                                                    will not be representable
                                                    */
                                                    val g : Boolean = exponent.value.value.testBit(0)
                                                    // r first mantisa bit  if we have fraction size or first rest bit
                                                    // if we do not have mantisa bits
                                                    val r : Boolean = if(fraction_size_c >= 1)
                                                                        ( ( mantisa_c.value & ( 1 << (fraction_size_c - 1) ) ) != 0 )
                                                                      else if(rest_bits != Nil)
                                                                        rest_bits.head
                                                                      else false
                                                    // s are all the remaining mantisa bits execpt first one and all the rest bits
                                                    // or if we do not have mantisa bits is the rest bits except the first one
                                                    val s : Boolean = if(fraction_size_c >= 1) 
                                                                        ( ( mantisa_c.value & ( ( 1 << (fraction_size_c - 1) ) - 1 ) ) != 0 ) || rest_bits.foldLeft(false)( (a, b) => (a | b) )
                                                                      else if(rest_bits != Nil)
                                                                        rest_bits.tail.foldLeft(false)( (a, b) => (a | b) )
                                                                      else
                                                                        false 
                                                    /*
                                                     we calculate l bit. if the res is zeri than the bit is taken from regime and there are five cases
                                                     infinite NR treated 
                                                     maximum value that is alreafy treated exponent >= maximum_exponent is 1 we do not do rounding here
                                                     second to maximum value is 0
                                                     minimum value is 1 -- the only case possible
                                                     zero value already treated
                                                    */
                                                    val new_exponent = regime * exponent_power + IntegerNumber(false, ( (exponent.value >> exponent_diff) << exponent_diff ) )
                                                    val l : Boolean = if(real_exponent_size == 0) 
                                                                        if (regime < IntegerNumber(0))
                                                                          true
                                                                        else
                                                                          false
                                                                      else
                                                                        (exponent.value >> exponent_diff).value.testBit(0)
                                                    // if we do not have any rounding bits
                                                    if(!g && !r && !s)
                                                      new oldPosit(
                                                        sign_c,
                                                        new_exponent,
                                                        NaturalNumber(1),
                                                        0, //real_fraction_size rfs for sure zero
                                                        exponent_size_c, size_c, rounding_c
                                                      )
                                                    else 
                                                        rounding_c match {
                                                            case RoundUp => if(!sign_c & (g|r|s)) this.apply(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case RoundDown => if(sign_c & (g|r|s)) this.apply(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case RoundZero => this.apply(sign_c, new_exponent, mantisa_c, Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case RoundAwayZero => if(g|r|s) this.apply(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case RoundEven => if(g&(r|s|l)) this.apply(sign_c, new_exponent + IntegerNumber(false, NaturalNumber(1) << exponent_diff), NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, new_exponent, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                            case _ => this.apply(sign_c, exponent_c, NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) 
                                                        }
                                                } else {
                                                    /*
                                                    all the exponent bits are representable so the exponent has the exponent value
                                                    we have to calculate ho many bits of fraction are representable
                                                    fraction_diff = fs - rfs (fraction bits that are not representable)
                                                    */
                                                    val fraction_diff = fraction_size_c - real_fraction_size
                                                    if(fraction_diff < 0) {
                                                      val rest_bits_mantissa = auxiliaryFunctions.BinaryEncodeFixedWidth(-fraction_diff, rest_bits.take(-fraction_diff)).reverse
                                                      val add_mantissa_binaryString = auxiliaryFunctions.BinaryEncodetoBinaryString(rest_bits_mantissa)
                                                      val add_mantissa_value = NaturalNumber(BigInt(add_mantissa_binaryString, 2))
                                                      this.apply(
                                                        sign_c,
                                                        exponent_c,
                                                        ( mantisa_c << (-fraction_diff) ) + add_mantissa_value, // take the necesary bits for mantisa from rest bits
                                                        rest_bits drop (-fraction_diff),
                                                        real_fraction_size,
                                                        exponent_size_c, size_c, rounding_c
                                                      )
                                                    } else {
                                                      /*
                                                      calculate g r s bits
                                                      g is the first not reprsentable mantisa bit or if all bits are repsentable the first rest bit
                                                      r is next bit of not reprsentable mantisa or from rest bit
                                                      */
                                                      val g : Boolean = if(fraction_diff>=1)
                                                                          ((mantisa_c.value & (1<<(fraction_diff-1))) != 0)
                                                                        else if(rest_bits != Nil)
                                                                          rest_bits.head
                                                                        else false
                                                      val r : Boolean = if(fraction_diff>=2)
                                                                          ((mantisa_c.value & (1<<(fraction_diff-2))) != 0)
                                                                        else if(fraction_diff==1) {
                                                                          if(rest_bits != Nil)
                                                                            rest_bits.head
                                                                          else false
                                                                        } else 
                                                                          if(rest_bits != Nil)
                                                                            if(rest_bits.tail != Nil)
                                                                              rest_bits.tail.head
                                                                            else false
                                                                          else false
                                                      val s : Boolean = if(fraction_diff>=2) 
                                                                          ( ( mantisa_c.value & ( (1 << ( fraction_diff - 1 ) ) - 1 ) ) != 0) || rest_bits.foldLeft(false)( (a, b)=>(a | b) )
                                                                        else if(fraction_diff==1) {
                                                                          if(rest_bits != Nil)
                                                                            rest_bits.tail.foldLeft(false)( (a, b)=>(a | b) )
                                                                          else
                                                                            false
                                                                        } else
                                                                          if(rest_bits != Nil)
                                                                            if(rest_bits.tail != Nil)
                                                                              rest_bits.tail.tail.foldLeft(false)( (a, b) => (a | b) )
                                                                            else false
                                                                          else false
                                                      // reprsentable mantisa
                                                      val mantisa : NaturalNumber_B = mantisa_c >> fraction_diff
                                                      // calculate l
                                                      // if we do not have mantisa we try exponent and fi wwe do not have
                                                      // exponent we try regime
                                                      val l : Boolean = if(real_fraction_size == 0) 
                                                                          if(real_exponent_size == 0)
                                                                            if (regime < IntegerNumber(0))
                                                                              true
                                                                            else
                                                                              false
                                                                          else exponent.value.value.testBit(0)
                                                                        else mantisa.value.testBit(0)
                                                      if(!g && !r && !s)
                                                        new oldPosit(
                                                          sign_c,
                                                          exponent_c,
                                                          mantisa,
                                                          real_fraction_size,
                                                          exponent_size_c, size_c, rounding_c
                                                        )
                                                      else 
                                                          rounding_c match {
                                                              case RoundUp => if(!sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantisa + NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, exponent_c, mantisa, Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                              case RoundDown => if(sign_c & (g|r|s)) this.apply(sign_c, exponent_c, mantisa + NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, exponent_c, mantisa, Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                              case RoundZero => this.apply(sign_c, exponent_c, mantisa, Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                              case RoundAwayZero => if(g|r|s) this.apply(sign_c, exponent_c, mantisa + NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, exponent_c, mantisa, Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                              case RoundEven => if(g&(r|s|l)) this.apply(sign_c, exponent_c, mantisa + NaturalNumber(1), Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) else this.apply(sign_c, exponent_c, mantisa, Nil, real_fraction_size, exponent_size_c, size_c, rounding_c)
                                                              case _ => this.apply(sign_c, exponent_c, mantisa, Nil, real_fraction_size, exponent_size_c, size_c, rounding_c) 
                                                          }

                                                    }
                                                }
                                            }
                                        }
                                     }
        case _=> throw new NotImplementedError("ro.upb.nrs.sl.oldPosit apply")
      }
  }

  def apply(a : Double, exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) : oldPosit_B = {
    val bits : Long = java.lang.Double.doubleToRawLongBits(a)
    val negative : Boolean = (bits & 0x8000000000000000L) != 0
    val exponent_bits : Long = ((bits & 0x7ff0000000000000L) >> 52)
    val exponent : Long = exponent_bits - 1023
    val mantissa : Long = if(exponent != -1023) 
                            (bits & 0x000fffffffffffffL) | 0x0010000000000000L
                          else
                            (bits & 0x000fffffffffffffL)
    this.apply(negative, IntegerNumber(exponent), NaturalNumber(mantissa), Nil, 52, exponent_size_c, size_c, rounding_c)
  }
  
  def apply(a : Int, exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) : oldPosit_B = this.apply(a.toDouble, exponent_size_c, size_c, rounding_c)
  def apply(a : Int) : oldPosit_B = this.apply(a.toDouble, this.default_exponent_size, this.default_size, this.default_rounding)
  def apply(a : Long, exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) : oldPosit_B = this.apply(a.toDouble, exponent_size_c, size_c, rounding_c)
  def apply(a : Long) : oldPosit_B = this.apply(a.toDouble, this.default_exponent_size, this.default_size, this.default_rounding)
  def apply(a : Double) : oldPosit_B = this.apply(a, this.default_exponent_size, this.default_size, this.default_rounding)
  def apply(a : Float, exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) : oldPosit_B = this.apply(a.toDouble, exponent_size_c, size_c, rounding_c)
  def apply(a : Float) : oldPosit_B = this.apply(a.toDouble)
  def apply(binaryString : String, exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) : oldPosit_B = {
    //special cases
    val zeroBinaryString = auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(size_c)(false) )
    val nrBinaryString = "1" + auxiliaryFunctions.BinaryEncodetoBinaryString( List.fill(size_c-1)(false) )
    if(binaryString == zeroBinaryString) {
      new oldPosit(
        false, // Posit has only positive zero
        minimum_exponent(exponent_size_c, size_c),
        NaturalNumber(0),
        0,
        exponent_size_c, size_c, rounding_c)
    } else if(binaryString == nrBinaryString) {
        new oldPosit_NR(exponent_size_c, size_c, rounding_c)
    } else {
      val sign : Boolean = binaryString(0) == '1'
      val toDecode : String = (
                                if(sign) 
                                  auxiliaryFunctions.BinaryStringNegate(auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size_c, NaturalNumber((BigInt(binaryString, 2) - 1)).binaryEncode)))
                                else
                                  auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size_c, NaturalNumber((BigInt(binaryString, 2))).binaryEncode))
                              ) drop 1 //sign bit
      val regimeString : String = toDecode takeWhile ( x => x == toDecode(0) )
      val regime : BigInt = if(toDecode(0) == '1') BigInt(regimeString.length - 1) else BigInt(-regimeString.length)
      val exponentString : String = ((toDecode dropWhile ( x => x == toDecode(0) )) drop 1) take exponent_size_c
      val exponent : BigInt = if (exponentString.length > 0) BigInt(exponentString, 2) << (exponent_size_c - exponentString.length) else 0
      val fractionString : String =  ((toDecode dropWhile ( x => x == toDecode(0) )) drop 1) drop exponent_size_c
      val fraction_size : Int = fractionString.length
      val fraction : BigInt =  (if(fraction_size > 0) BigInt(fractionString, 2) else BigInt(0)) + ( BigInt(1) << fraction_size )
      val finalExponent : BigInt = (regime << exponent_size_c) + exponent
      this.apply(sign, IntegerNumber(finalExponent<0, NaturalNumber(finalExponent.abs)), NaturalNumber(fraction), Nil, fraction_size, exponent_size_c, size_c , rounding_c)
    }
  }

  def apply(value_c : TaperedFloatingPoint_B, exponent_size_c : Int, size_c : Int, rounding_c : RoundingType) :oldPosit_B = this.apply(value_c.sign, value_c.exponent, value_c.mantissa, value_c.rest_bits, value_c.fraction_size, exponent_size_c, size_c, rounding_c)


  def unapply(input : oldPosit_B) = Some(input.sign, input.exponent, input.mantisa, input.fraction_size, input.exponent_size, input.size, input.rounding)
}
