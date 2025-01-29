package ro.upb.nrs.sl




import scala.annotation.tailrec

import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

/*
We consider Complex
x = real + i * imaginary
x =  |x| * e^(phi * i)
*/
abstract class Complex_B extends NumberRepresentationSystem {
  /*
  real part
  imaginary part
  */
  val real : NumberRepresentationSystem
  val imaginary : NumberRepresentationSystem
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
  real1 + i * imaginary1 + real2 + imaginary2 = (real1 +  real2) + i * (imaginary1 +  imaginary2)
  */
  override def +(that: NumberRepresentationSystem): Complex_B = that match {
    case Complex_NR(_) => Complex_NR
    case Complex(that_real, that_imaginary) =>  Complex(this.real + that_real, this.imaginary + that_imaginary)
    case _ => Complex(this.real + that, this.imaginary)
  }
  /*
  real1 + i * imaginary1 - ( real2 + imaginary2 ) = (real1 -  real2) + i * (imaginary1 -  imaginary2)
  */
  override def -(that: NumberRepresentationSystem): Complex_B = that match {
    case Complex_NR(_) => Complex_NR
    case Complex(that_real, that_imaginary) =>  Complex(this.real - that_real, this.imaginary - that_imaginary)
    case _ => Complex(this.real - that, this.imaginary)
  }
  /*
  ( real1 + i * imaginary1 ) * ( real2 + i * imaginary2 ) =
  ( (real1 * real2) - ( imaginary1 * imaginary2 ) ) + i * ( ( real1 * imaginary2 ) + ( real2 * imaginary1 ) )
  */
  override def *(that: NumberRepresentationSystem): Complex_B = that match {
    case Complex_NR(_) => Complex_NR
    case Complex(that_real, that_imaginary) =>  Complex(
                                                        (this.real * that_real) - (this.imaginary * that_imaginary),
                                                        (this.real * that_imaginary) + (that_real * this.imaginary)
                                                       )
    case _ => Complex(this.real * that, this.imaginary * that)
  }
  /*
  ( real1 + i * imaginary1 ) / ( real2 + i * imaginary2 ) =
  ( ( real1 + i * imaginary1 ) * ( real2 - i * imaginary2 ) ) / (real2^2 -  imaginary2^2) =
   ( ( (real1 * real2) + ( imaginary1 * imaginary2 ) ) + i * ( ( real2 * imaginary1 ) - ( real1 * imaginary2 ) ) ) / (real2^2 +  imaginary2^2)
   TODO: verify for zero
  */
  override def /(that: NumberRepresentationSystem): Complex_B = that match {
    case Complex_NR(_) => Complex_NR
    case Complex(that_real, that_imaginary) => {
                                                       val denominator = (that_real * that_real) + (that_imaginary * that_imaginary)
                                                        Complex(
                                                        ( (this.real * that_real) + (this.imaginary * that_imaginary) ) / denominator,
                                                        ( (that_real * this.imaginary) - (this.real * that_imaginary) ) / denominator
                                                       )
    }
    case _ => {
      Complex(this.real / that, this.imaginary / that)
    }
  }
  override def /\(that: NumberRepresentationSystem): Complex_B = this / that
  override def %(that: NumberRepresentationSystem): Complex_B = throw new NotImplementedError("ro.upb.nrs.sl.Complex %")
  /*
    X ^ Y
    ( real1 + i * imaginary1 ) ^ ( real2 + i * imaginary2 ) =
    ( ( real1 + i * imaginary1 ) ^ real2 ) * ( ( real1 + i * imaginary1 ) ^ ( i * imaginary2 ) )
    ( (|x1| * e^(phi1 * i) ) ^ real2 ) * ( ( |x1| * e^(phi1 * i) ) ^ ( i * imaginary2 ) ) 
    = ( |x1|^real2 * (e^(phi1 * i))^real2 ) * ( ( |x1|^( i * imaginary2 ) * (e^(phi1 * i))^( i * imaginary2 ) ) )
    = ( |x1|^real2 * e^(real2 * phi1 * i) ) * ( ( e^ln( |x1| ) )^( i * imaginary2 ) * ( e^( -1 * phi1 * imaginary2 ) )
    = ( |x1|^real2 * e^(real2 * phi1 * i) ) * ( ( e^( ln( |x1| ) * imaginary2 * i ) * ( e^( -phi1 * imaginary2 ) )
  */
  override def pow(that: NumberRepresentationSystem): Complex_B = that match {
    case Complex_NR(_) => Complex_NR
    case Complex(that_real, that_imaginary) => {
        val zero = this.real - this.real
        //|x1|^real2
        val element1 = Complex(this.abs.pow(that_real), zero)
        //e^(real2 * phi1 * i)
        val element2 = Complex(zero, that_real * this.phi).exp
        //e^( ln( |x1| ) * imaginary2 * i ) 
        val element3 = Complex(zero, this.abs.ln * that_imaginary).exp
        // e^( -phi1 * imaginary2 )
        val element4 = Complex( (this.phi * that_imaginary).exp, zero)
        val result = element1 * element2 * element3 * element4
        result
    }
    case _ => this.pow(Complex(that))
  }
  override def unary_- : Complex_B = Complex(-this.real, -this.imaginary)
  //TODO: verify for zero
  override def inverse : Complex_B = {
        val denominator = (this.real * this.real) + (this.imaginary * this.imaginary)
        Complex(
            this.real / denominator,
            -this.imaginary / denominator
        )
    }
  /*
  logical operation (ordering)
  less
  equal
  not equal
  greater
  greater or equal
  less or equal
  */
  //No order in complex numbers
  override def <(that: NumberRepresentationSystem): Boolean = throw new NotImplementedError("ro.upb.nrs.sl.Complex <")
  override def ==(that: NumberRepresentationSystem): Boolean = that match {
    case Complex_NR(_) => false
    case Complex(that_real, that_imaginary) =>  (this.real == that_real) && (this.imaginary == that_imaginary)
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Complex ==")
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
  override def min(that: NumberRepresentationSystem): Complex_B = throw new NotImplementedError("ro.upb.nrs.sl.Complex min")
  override def max(that: NumberRepresentationSystem): Complex_B = throw new NotImplementedError("ro.upb.nrs.sl.Complex max")
  /*
  sqrt(real*real + imaginary*imaginary)
  */
  override def abs: NumberRepresentationSystem = ( (this.real * this.real) + (this.imaginary * this.imaginary) ).sqrt
  def phi: NumberRepresentationSystem = {
      val zero = this.real - this.real
      val one = zero.exp
      if(this.real == zero) {
          one.arcsin
      } else  {
          (this.imaginary / this.real).arctan
      }
  }
  override def signum: Complex_B = Complex( this.real.signum, this.imaginary.signum)
  override def nth_root (n : Int) : (Complex_B, Complex_B) = throw new NotImplementedError("ro.upb.nrs.sl.Complex nth_root")
  /*
  NQRT( |x| * e^(phi * i) ) = NQRT(|x|) * NQRT(e^(phi * i)) =
  =  NQRT(|x|) * e^( (phi / n) * i)
  */
  override def nqrt (n : Int) : Complex_B = {
    val zero = this.real - this.real
    //NQRT(|x|)
    val element1 = Complex(this.abs.nqrt(n), zero)
    //e^( (phi / n) * i)
    // TODO make something terrible with n
    val element2 = Complex(zero, (this.phi / n).exp)
    val result = element1 * element2
    result
  }
  override def sqrt: Complex_B = this.nqrt(2)
  /*
  e^(a + bi) = e^a * e^(b * i) = e^a * ( cos(b) + i * sin(b) ) =
  = e^a * cos(b) + e^a * sin(b) * i
  */
  override def exp : Complex_B = Complex( this.real.exp * this.imaginary.cos, this.real.exp * this.imaginary.sin)
  /*
  ln(|x| * e^(phi * i)) = ln(|x|) + ln(e^(phi * i)) = ln(|x|) + phi * i
  */
  override def ln : Complex_B = Complex( this.abs.ln, this.phi)
  override def log(base: NumberRepresentationSystem) : Complex_B = base match {
    case Complex_NR(_) => Complex_NR
    case Complex(that_real, that_imaginary) =>  this.ln / Complex(that_real, that_imaginary).ln
    case _ => throw new NotImplementedError("ro.upb.nrs.sl.Complex log")
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
  //https://ximera.osu.edu/math/complexBook/complexBook/complexMappings/complexMappings
  /*
  sin(z) = ( e^iz - e^(-iz) ) / (2 * i)
  */
  override def sin : Complex_B = {
      val zero = this.real - this.real
      val one = zero.exp
      val i_simple = Complex(zero, one)
      //( e^iz - e^(-iz) )
      val element = (this * i_simple).exp - (-this * i_simple).exp
      element / (i_simple + i_simple)
  }
  /*
  cos(z) = ( e^iz + e^(-iz) ) / 2
  */
  override def cos : Complex_B = {
      val zero = this.real - this.real
      val one = zero.exp
      val i_simple = Complex(zero, one)
      //( e^iz - e^(-iz) )
      val element = (this * i_simple).exp + (-this * i_simple).exp
      element / (one + one)
  }
  /*
  https://ximera.osu.edu/math/complexBook/complexBook/complexTrig/complexTrig
  */
  override def tan : Complex_B = this.sin / this.cos
  override def cot : Complex_B = this.cos / this.sin
  override def sec : Complex_B = this.cos.inverse
  override def csc : Complex_B = this.sin.inverse
  /*
  inverse trigonometric functions
  arcsin
  arccos
  arctan
  arccot
  arcsec
  arccsc
  */
  /*
  https://en.wikipedia.org/wiki/Inverse_trigonometric_functions
  logarithmic form
  */
  /*
  arcsin(z) = -i * ln( SQRT(1 - z^2) + i * z )
  */
  override def arcsin : Complex_B = {
      val zero = this.real - this.real
      val one = zero.exp
      val one_comlex = Complex(one, zero)
      val i_simple = Complex(zero, one)
      i_simple.unary_- * ( ( one_comlex - this * this  ).sqrt + i_simple * this ).ln
  }
  /*
  arcsos(z) = -i * ln( i * SQRT(1 - z^2) + z )
  */
  override def arccos : Complex_B = {
      val zero = this.real - this.real
      val one = zero.exp
      val one_comlex = Complex(one, zero)
      val i_simple = Complex(zero, one)
      i_simple.unary_- * ( i_simple * ( one_comlex - this * this  ).sqrt +  this ).ln
  }
  /*
  arctan(z) = (-i / 2) * ln( (i - z) / (i + z) )
  */
  override def arctan : Complex_B = {
      val zero = this.real - this.real
      val one = zero.exp
      val one_comlex = Complex(one, zero)
      val i_simple = Complex(zero, one)
      (i_simple.unary_- / (one_comlex + one_comlex) ) * ( (i_simple - this) / (i_simple + this) ).ln
  }
  /*
  arccot(z) = arctan(1 / z)
  */
  override def arccot : Complex_B = this.inverse.arctan
  /*
  arcsec(z) = arccos(1 / z)
  */
  override def arcsec : Complex_B = this.inverse.arccos
  /*
  arccsc(z) = arcsin(1 / z)
  */
  override def arccsc : Complex_B = this.inverse.arcsin
  /*
  hyperbolic functions
  sinh
  cosh
  tanh
  coth
  sech
  csch
  */
  /*
    FOR X in (-infinite, infinite)
    SINH = ( e^(2*X) - 1 ) / ( 2 * e^X )
         = ( (e^X - 1) * (e^X + 1) ) / (2 * e^X)   // a^2 - b^2 = (a - b) * (a + b)
         = ( (exp(X) - 1) * (exp(X) + 1) ) / (2 * exp(X)) 
         DEFINITION
    */
  override def sinh : Complex_B = {
      val e_x = this.exp
      val zero = this.real - this.real
      val one = zero.exp
      ( (e_x - one) * (e_x + one) ) / ( ( one + one ) * e_x )
  }
  /*
  COSH = e^X - sinh
  */
  override def cosh : Complex_B = {
      val e_x = this.exp
      e_x - this.sinh
  }
  override def tanh : Complex_B = this.sinh / this.cosh
  override def coth : Complex_B = this.cosh / this.sinh
  override def sech : Complex_B = this.cosh.inverse
  override def csch : Complex_B = this.sinh.inverse
  /*
  inverse hyperbolic functions
  arcsinh
  arccosh
  arctanh
  arccoth
  arcsech
  arccsch
  */
  //http://scipp.ucsc.edu/~haber/archives/physics116A10/arc_10.pdf
  /*
  arcsinh(z) = ln(x + SQRT(x^2 + 1) )
  */
  override def arcsinh : Complex_B = {
      val zero = this.real - this.real
      val one = zero.exp
      val one_comlex = Complex(one, zero)
      val i_simple = Complex(zero, one)
      ( this + (this * this + one_comlex).sqrt ).ln
  }
  /*
  arccosh(z) = ln(x + SQRT(x^2 - 1) )
  */
  override def arccosh : Complex_B = {
      val zero = this.real - this.real
      val one = zero.exp
      val one_comlex = Complex(one, zero)
      val i_simple = Complex(zero, one)
      ( this + (this * this - one_comlex).sqrt ).ln
  }
  /*
  arctanh(z) = (1 / 2) * ln( (i + z) / (i - z) )
  */
  override def arctanh : Complex_B = {
      val zero = this.real - this.real
      val one = zero.exp
      val one_comlex = Complex(one, zero)
      val i_simple = Complex(zero, one)
      ( one_comlex / (one_comlex + one_comlex) ) * ( (i_simple + this) / (i_simple - this) ).ln
  }
  /*
  arccoth(z) = arctanh(1/z)
  */
  override def arccoth : Complex_B = this.inverse.arctanh
  override def arcsech : Complex_B = this.inverse.arccosh
  override def arccsch : Complex_B = this.inverse.arcsinh
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
  not implemented
  */
  override def toRationalNumber : RationalNumber_B = throw new NotImplementedError("ro.upb.nrs.sl.Complex toRationalNumber")
  override def toBigDecimal : BigDecimal = throw new NotImplementedError("ro.upb.nrs.sl.Complex toBigDecimal")
  def toRationalNumber_tuple : (RationalNumber_B, RationalNumber_B) = (this.real.toRationalNumber, this.imaginary.toRationalNumber)
  def toBigDecimal_tuple : (BigDecimal, BigDecimal) = (this.real.toBigDecimal, this.imaginary.toBigDecimal)
  /*
  Show functions
  toString
  toInternalString
  */
  override def toInternalString: String = "x = " + this.real.toInternalString + " + i * " + this.imaginary.toInternalString
  override def toString: String = this.real.toString + " + i * " + this.imaginary.toString
  
}

object Complex_NR extends Complex_B {
  // TODO: something more generic
  override val real : NumberRepresentationSystem = NaturalNumber_NR
  override val imaginary : NumberRepresentationSystem = NaturalNumber_NR
  override def toString : String = "NR"
  def unapply(input : Complex_B) = if(input.toString.equals("NR")) Some(None) else None
}

class Complex(real_c : NumberRepresentationSystem, imaginary_c : NumberRepresentationSystem) extends Complex_B {
  override val real : NumberRepresentationSystem = real_c;
  override val imaginary : NumberRepresentationSystem = imaginary_c
}

object Complex {
  def apply(real_c : NumberRepresentationSystem, imaginary_c : NumberRepresentationSystem) : Complex_B = new  Complex(real_c, imaginary_c)
  def apply(real_c : NumberRepresentationSystem) : Complex_B = new  Complex(real_c, real_c - real_c)
  def unapply(input :  Complex_B) = Some(input.real, input.imaginary)
}
