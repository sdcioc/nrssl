import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class Posit_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = Posit_NR(Posit.default_exponent_size, Posit.default_size, Posit.default_rounding)

  test("toString ro.upb.nrs.sl.Posit") {
    assert(Posit(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.Posit") {
    assert(Posit(2.0d) + Posit(3.0d) == Posit(5.0d))
    assert(Posit(2) + Posit(3) == Posit(5))
  }

  test("Subraction ro.upb.nrs.sl.Posit") {
    assert(Posit(3.0d) - Posit(2.0d) == Posit(1.0d))
    assert(Posit(3) - Posit(2) == Posit(1))
  }

  test("Mul ro.upb.nrs.sl.Posit") {
    assert(Posit(3.0d) * Posit(2.0d) == Posit(6.0d))
    assert(Posit(3) * Posit(2) == Posit(6))
  }

  test("Div ro.upb.nrs.sl.Posit") {
    assert(Posit(6.0d) / Posit(2.0d) == Posit(3.0d))
    assert(Posit(6) / Posit(2) == Posit(3))
  }

  test("Pow ro.upb.nrs.sl.Posit") {
    assert(Posit(2.0d).pow(2) == Posit(4.0d))
    assert(Posit(6).pow(2) == Posit(36))
  }

  test("Nqrt ro.upb.nrs.sl.Posit") {
    assert(Posit(16.0d).nqrt(2) == Posit(4.0d))
    assert(Posit(8).nqrt(3) == Posit(2))
  }

  test("Abs ro.upb.nrs.sl.Posit") {
    assert(Posit(-16.0d).abs== Posit(16.0d))
    assert(Posit(-8).abs == Posit(8))
  }

  test("Signum ro.upb.nrs.sl.Posit") {
    assert(Posit(-16.0d).signum== Posit(-1.0d))
    assert(Posit(8).signum == Posit(1))
  }

  test("Equals ro.upb.nrs.sl.Posit") {
    assert(Posit(16.0d) == Posit(16.0d))
    assert(Posit(16) == Posit(16))
  }

  test("Complex ro.upb.nrs.sl.Posit") {
    // Asta trebuie fixata ca nici suma cu numere mici nu merge
    val x = Posit(1135984.7834d)
    val y = Posit(3423456.11345d)

    assert(Posit(11) + Posit(34)==Posit(45))

    val sum = x + y
    assert(sum > Posit(4559440.0d))
    val dif = x - y
    assert(dif == Posit(-2287471.25d)) //right answer -2287471.33005
    val mul = Posit(11359.7834d) * Posit(34234.11345d)
    assert(mul == Posit(388892113.0d))  //388892113.683
    val div = Posit(11359.7834d) / Posit(34234.11345d)
    assert(div >= Posit(0.331d))
    val pow = Posit(11359.7834d).pow(2)
    assert(pow >= Posit(129044678.0d))  //129044678.895
    val root = x.nqrt(2)
    assert(root >= Posit(1065.825d))
    val nthroot = x.nqrt(3)
    assert(nthroot >= Posit(104.341d))
  }


  test("Positive Zero only ro.upb.nrs.sl.Posit") {
    val positiveZero = Posit(0.0d)
    val negativeZero = Posit(0.0d) * Posit(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (Posit(-1.0d) + Posit(1.0d)) == negativeZero )
    assert( (Posit(-1.0d) + Posit(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    //Posit has only one zero (positive)
    assert(positiveZero.value.sign == negativeZero.value.sign)
  }

  
}
