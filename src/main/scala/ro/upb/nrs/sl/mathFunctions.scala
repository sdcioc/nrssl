package ro.upb.nrs.sl



import scala.annotation.tailrec

object mathFunctions {
  /*
    TAYLOR series for mathematical functions
    https://www.efunda.com/math/taylor_series/taylor_series.cfm
    A Taylor series is infinite sum of elements.
    Most time is similar with a polynom.
    TS = SUM of ScalarElement(iterator) * XDepedentElement(iterator)
         with iterator from zero to precision
    We make it finite and make the calculation until a given precision.
    Every element has an iterator (current) which gives its position in sum.
    elementAcc last scalar element acccumulator
    xAcc last X dependent element
    acc the sum until current iterator
    We have 4 functions
    nextX that takes iterator value, x value and last X depedent element and
          returns the current X depedent element
    nextElem calculate the next scalar element from the iterator value and last scalar element value
    nextAcc calucaltes the next sum from the current sum, current scalar element and curent x depedent element
    nextCurrent calculates next value of the iterator
    */
  def taylor_series(nextX: (NumberRepresentationSystem, NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem,
                    nextElem: (NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem,
                    nextAcc: (NumberRepresentationSystem, NumberRepresentationSystem, NumberRepresentationSystem) => NumberRepresentationSystem,
                    nextCurrent: (NumberRepresentationSystem) => NumberRepresentationSystem)(zero: NumberRepresentationSystem)
                   (x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    @tailrec
    def helper(current: NumberRepresentationSystem, elementAcc: NumberRepresentationSystem, xAcc: NumberRepresentationSystem, acc: NumberRepresentationSystem): NumberRepresentationSystem = {
      if (current >= precision) acc
      else {
        //println("current=" + current.toInternalString + " value=" + current.toString)
        //println("elementAcc=" + elementAcc.toInternalString + " value=" + elementAcc.toString)
        //println("xAcc=" + xAcc.toInternalString + " value=" + xAcc.toString)
        //println("acc=" + acc.toInternalString + " acc=" + acc.toString)
        val a = nextElem(current, elementAcc)
        val b = nextX(current, x, xAcc)
        val c = nextAcc(a, b, acc)
        helper(nextCurrent(current), a, b, c)
      }
    }

    helper(zero, zero, zero, zero)

  }

  /*
    exponential
    EXP = SUM of (1/(iterator!))*X^iterator
    */
  def exp(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = taylor_series(
    (a: NumberRepresentationSystem, x: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else acc * x,
    (a: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else acc * a,
    (sc_elem: NumberRepresentationSystem, x_elem: NumberRepresentationSystem, acc: NumberRepresentationSystem) => acc + x_elem / sc_elem,
    (a: NumberRepresentationSystem) => a + one
  )(zero)(x, precision)

  /*
    natural logarithm
        for X in (0,2]
        LN = SUM of (1/iterator) * (X-1)^iterator
    */
  def ln2(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    val ln_taylor = (x, y) => taylor_series(
      (a: NumberRepresentationSystem, x: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) x - one else acc * (x - one),
      (a: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else -acc.signum * (acc.abs + one),
      (sc_elem: NumberRepresentationSystem, x_elem: NumberRepresentationSystem, acc: NumberRepresentationSystem) => acc + x_elem / sc_elem,
      (a: NumberRepresentationSystem) => a + one
    )(zero)(x, y)

    @tailrec
    def helper(currentX: NumberRepresentationSystem, acc: NumberRepresentationSystem): NumberRepresentationSystem = {
      if (currentX <= zero) throw new NotImplementedError("ln of x <= 0")
      else if (currentX <= two) ln_taylor(currentX, precision) + acc * ln_taylor(two, precision)
      else helper(currentX / two, acc + one)
    }

    helper(x, zero)
  }

  /*
    natural logarithm
        for X > 0
        LN = 2 * SUM of (1/(2*iterator+1)) * ((X-1)/(X+1))^(2*iterator+1) with iterator from 0 to precision
        LN = 2 * SUM of (1/iterator) * ((X-1)/(X+1))^(iterator)
             with iterator from 1 to 2 * precison with next iterator = iterator + 2
    */
  def ln(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    two * taylor_series(
      (a: NumberRepresentationSystem, x: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) (x - one) / (x + one) else acc * ((x - one) / (x + one)) * ((x - one) / (x + one)),
      (a: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else acc + two,
      (sc_elem: NumberRepresentationSystem, x_elem: NumberRepresentationSystem, acc: NumberRepresentationSystem) => acc + x_elem / sc_elem,
      (a: NumberRepresentationSystem) => a + one
    )(zero)(x, two * precision)
  }

  /*
    logarithm
    log_base(X) = ln(X)/ln(base)
    */
  def log(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(base: NumberRepresentationSystem, x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    ln(zero, one)(x, precision) / ln(zero, one)(base, precision)
  }

  /*
    trigonometric functions
    FOR X in (-infinite, infinite)
    SIN = SUM of -1^((iterator-1)/2) * (1/iterator!) * X^iterator
          with iterator from 1 to 2 * precison with next iterator = iterator + 2
    */
  def sin(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    taylor_series(
      (a: NumberRepresentationSystem, x: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) x else acc * x * x,
      (a: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else -acc * a * (a + one),
      (sc_elem: NumberRepresentationSystem, x_elem: NumberRepresentationSystem, acc: NumberRepresentationSystem) => acc + x_elem / sc_elem,
      (a: NumberRepresentationSystem) => a + two
    )(zero)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    COS = SUM of -1^((iterator)/2) * (1/iterator!) * X^iterator
          with iterator from 0 to 2 * precison with next iterator = iterator + 2
    */
  def cos(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    taylor_series(
      (a: NumberRepresentationSystem, x: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else acc * x * x,
      (a: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else -acc * a * (a - one),
      (sc_elem: NumberRepresentationSystem, x_elem: NumberRepresentationSystem, acc: NumberRepresentationSystem) => acc + x_elem / sc_elem,
      (a: NumberRepresentationSystem) => a + two
    )(zero)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    TAN = SIN / COS (DEFINITION)
    */
  def tan(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    sin(zero, one)(x, precision) / cos(zero, one)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    COT = COS / SIN (DEFINITION)
    */
  def cot(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    cos(zero, one)(x, precision) / sin(zero, one)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    SEC = 1 / COS (DEFINITION)
    */
  def sec(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    one / cos(zero, one)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    CSC = 1 / SIN (DEFINITION)
    */
  def csc(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    one / sin(zero, one)(x, precision)
  }

  /*
    inverse trigonometric functions
    FOR X in (-1, 1)
    ARCTAN = SUM of -1^((iterator-1)/2) * (1/iterator) * X^iterator
             with iterator from 1 to 2 * precison with next iterator = iterator + 2
    FOR X with |X|>=1
    ARCTAN = 2 * ARCTAN ( X / ( 1 + SQRT(1+X^2) ) )
            (HALF ANGLE FORMULA tan(phi/2) = sin(phi) / ( 1 + cos(phi) ) )
            It works because  | X / ( 1 + SQRT(1+X^2) ) | < 1
    */
  def arctan(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    if (x.abs < one)
      taylor_series(
        (a: NumberRepresentationSystem, x: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) x else acc * x * x,
        (a: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else -(acc / (a - one)) * (a + one),
        (sc_elem: NumberRepresentationSystem, x_elem: NumberRepresentationSystem, acc: NumberRepresentationSystem) => acc + x_elem / sc_elem,
        (a: NumberRepresentationSystem) => a + two
      )(zero)(x, precision)
    else
      two * arctan(zero, one)(x / (one + (one + x * x).sqrt), precision) // LESS than 1 for sure no tailrec
  }

  /*
    FOR X in (-infinite, infinite) with X != 0
    ARCCOT = ARCTAN(1/X)
    FOR X = 0
    ARCCOT(0) = ARCCOS(0) = 2 * ARCTAN(1)
                DEFINITION + HALF ANGLE FORMULA see ARCCOS
    */
  def arccot(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    if (x == zero) two * arctan(zero, one)(one, precision) //arcos(0)
    else arctan(zero, one)(one / x, precision)
  }

  /*
    FOR X in [-1, 1]
    ARCSIN = 2 * ARCTAN( X / ( SQRT(1 - X^2) + 1 ) )
             HALF ANGLE FORMULA
    FOR |X| > 1 ARCSIN - NR (not representable, undefined)
    */
  def arcsin(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    if (x.abs <= one) two * arctan(zero, one)(x / (one + (one - x * x).sqrt), precision)
    else one / zero
  }

  /*
    FOR X in [-1, 1] and X != 0
    ARCCOS = ARCTAN( SQRT(1 - X^2) / X )
             DEFINITION
    FOR X = 0 HALF ANGLE FORMULA 2 * ARCTAN( SQRT(1 - X^2) / (1 + X) ) = 2 * ARCTAN(1)
    FOR |X| > 1 ARCCOS - NR (not representable, undefined)
    */
  def arccos(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    if (x.abs <= one)
      if (x == zero) two * arctan(zero, one)(one, precision) //arcos(0)
      else arctan(zero, one)((one - x * x).sqrt / x, precision)
    else one / zero
  }

  /*
    FOR |X|>=1
    ARCSEC = ARCCOS(1 / X)
             DEFINITION
    FOR |X| < 1 ARCSEC - NR (not representable, undefined)
    */
  def arcsec(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    if (x.abs >= one) arccos(zero, one)(one / x, precision)
    else one / zero
  }

  /*
    FOR |X|>=1
    ARCCSC = ARCSIN(1 / X)
             DEFINITION
    FOR |X| < 1 ARCCSC - NR (not representable, undefined)
    */
  def arccsc(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    if (x.abs >= one) arcsin(zero, one)(one / x, precision)
    else one / zero
  }

  /*
    hyperbolic functions
    FOR X in (-infinite, infinite)
    SINH = ( e^(2*X) - 1 ) / ( 2 * e^X )
         = ( (e^X - 1) * (e^X + 1) ) / (2 * e^X)   // a^2 - b^2 = (a - b) * (a + b)
         = ( (exp(X) - 1) * (exp(X) + 1) ) / (2 * exp(X)) // e^X = exp(X)
         DEFINITION
    */
  def sinh(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    val e_x = exp(zero, one)(x, precision)
    (e_x - one) * (e_x + one) / (two * e_x)
  }

  /*
    FOR X in (-infinite, infinite)
    COSH = e^X - SINH
         DEFINITION
    */
  def cosh(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    val e_x = exp(zero, one)(x, precision)
    e_x - sinh(zero, one)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    TANH = SINH / COSH
         DEFINITION
    */
  def tanh(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    sinh(zero, one)(x, precision) / cosh(zero, one)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    COTH = COSH / SINH
         DEFINITION
    */
  def coth(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    cosh(zero, one)(x, precision) / sinh(zero, one)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    SECH = 1 / COSH
         DEFINITION
    */
  def sech(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    one / cosh(zero, one)(x, precision)
  }

  /*
    FOR X in (-infinite, infinite)
    CSCH = 1 / SINH
         DEFINITION
    */
  def csch(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    one / sinh(zero, one)(x, precision)
  }

  /*
    inverse hyperbolic functions
    FOR X in (-1, 1)
    ARCTANH = SUM of (1/iterator) * X^iterator
             with iterator from 1 to 2 * precison with next iterator = iterator + 2
    FOR X with |X|>=1 ARCTANH - NR (not representable, undefined)
    */
  def arctanh(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    val two = one + one
    if (x.abs < one)
      taylor_series(
        (a: NumberRepresentationSystem, x: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) x else acc * x * x,
        (a: NumberRepresentationSystem, acc: NumberRepresentationSystem) => if (a == zero) one else (acc / (a - two)) * a,
        (sc_elem: NumberRepresentationSystem, x_elem: NumberRepresentationSystem, acc: NumberRepresentationSystem) => acc + x_elem / sc_elem,
        (a: NumberRepresentationSystem) => a + two
      )(zero)(x, two * precision)
    else
      one / zero
  }

  /*
    FOR |X|>1
    ARCCOTH = ARCTANH(1/X)
              DEFINITION
    FOR X with |X|<=1 ARCCOTH - NR (not representable, undefined)
    */
  def arccoth(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    if (x.abs > one) arctanh(zero, one)(one / x, precision)
    else one / zero //NR
  }

  /*
    TO UNDERSTAND ARCSINH and ARCCOSH
    LN(x) = ARCTANH( (X^2 - 1) / (X^2 + 1) )
          = ARCSINH( (X^2 - 1) / (2 * X) )
          = +/- ARCCOSH( (X^2 + 1) / (2 * X) )
    */
  /*
    FOR X in (-infinite, infinite)
    ARCSINH = ARCTANH( X / SQRT(1 + X^2) )
              LN(X) formula
    */
  def arcsinh(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    arctanh(zero, one)(x / (one + x * x).sqrt, precision)
  }

  /*
    FOR |X|>=1
    ARCCOSH = |ARCTANH( SQRT(X^2 - 1) / X )|
            = ABSOLUTE( ARCTANH( SQRT(X^2 - 1) / X ) )
              LN(X) formula
    FOR X with |X|<1 ARCCOSH - NR (not representable, undefined)
    */
  def arccosh(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    if (x >= one) (arctanh(zero, one)((x * x - one).sqrt / x, precision)).abs
    else one / zero //NR
  }

  /*
    FOR X in (0, 1]
    ARCSECH = ARCCOSH(1 / X)
              DEFINITION
    FOR X not in (0, 1] X <= 0 or X > 1 ARCSECH - NR (not representable, undefined)
    */
  def arcsech(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    if (x > zero)
      if (x <= one)
        arccosh(zero, one)(one / x, precision)
      else one / zero
    else one / zero //NR
  }

  /*
    FOR X in (-infinite, infinite) X != 0
    ARCCSCH = ARCSINH(1 / X)
              DEFINITION
    FOR X = 0 ARCCSCH - NR (not representable, undefined)
    */
  def arccsch(zero: NumberRepresentationSystem, one: NumberRepresentationSystem)(x: NumberRepresentationSystem, precision: NumberRepresentationSystem): NumberRepresentationSystem = {
    if (x == zero) one / zero // NR
    else arcsinh(zero, one)(one / x, precision)
  }

  /*
    power functions
    */
  /*
    pow_integer power function base^exponent with exponent integer
    it sees the exponent in binary EXAMPLE 01001000 (LSB first bit - 18 decimal)
    base is X -- for every i bit you have to multiply with X^(2^i) if the i bit is set
    X^(2^i) = X^(2 * 2 * 2 ... 2) i times = (...((X^2)^2)^2)...)^2) so you have to raise to
    square at every step when you go through binary representation bits of the exponent
    if the bit is set you multiply the current xAcc value with the acccumulator
    Exponent >=0
    */
  def pow_integer(one: NumberRepresentationSystem)(base: NumberRepresentationSystem, exponent: BigInt): NumberRepresentationSystem = {
    require(exponent >= 0)

    @tailrec
    def helper(current: BigInt, xAcc: NumberRepresentationSystem, acc: NumberRepresentationSystem): NumberRepresentationSystem = {
      if (current == 0)
        acc
      else
        helper(
          current >> 1,
          xAcc * xAcc,
          (if (current.testBit(0)) acc * xAcc else acc)
        )
    }

    helper(exponent, base, one)
  }

  /*
    pow_fractional function base^exponent with exponent in [0,1)
    X is base
    base^exponent = X^(exponent_mantisa/2^exponent_fraction_size) =
    = X^( (exponent_mantisa(exponent_fraction_size-1) / 2) + (exponent_mantisa(exponent_fraction_size-2) / 4) + ...
    (exponent_mantisa(0) / 2^exponent_fraction_size)) )
    most semnificant bit of exponent_mantisa gives 1/2 which is X.sqrt next bit gives 1/4
    which is X.nqrt(4) = X.sqrt.sqrt and so on. So in xAcc will be the square root of previous element
    if the bit is set in mantisa than we multiply the current value with the acccumulator
    we go in reverse through bits from MSB to LSB (exponent_fraction_size-1 to 0)
    */
  def pow_fractional(one: NumberRepresentationSystem)(
    base: NumberRepresentationSystem, exponent_mantisa: BigInt, exponent_fraction_size: Int
  ): NumberRepresentationSystem = {
    require(exponent_mantisa >= 0)

    @tailrec
    def helper(current: Int, xAcc: NumberRepresentationSystem, acc: NumberRepresentationSystem): NumberRepresentationSystem = {
      if (current < 0)
        acc
      else
        helper(
          current - 1,
          xAcc.sqrt,
          (if (exponent_mantisa.testBit(current)) acc * xAcc else acc)
        )
    }

    helper(exponent_fraction_size - 1, base.sqrt, one)
  }
}
