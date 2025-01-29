import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class FixedNaturalNumber_Tests extends FunSuite {

  numberRepresentationSystemWorkingType=FixedNaturalNumber_NR(FixedNaturalNumber.default_size, FixedNaturalNumber.default_rounding)

  test("Addition 1+2==3") {
    assert(1+FixedNaturalNumber(2)==FixedNaturalNumber(3))
  }

  test("Addition 1+2==3 v2") {
    assert(FixedNaturalNumber(1)+2==FixedNaturalNumber(3))
  }

  test("Subtraction ro.upb.nrs.sl.FixedNaturalNumber") {
    assert(FixedNaturalNumber(3)-2==FixedNaturalNumber(1))
    assert(3 - FixedNaturalNumber(2)==FixedNaturalNumber(1))
  }

  test("Multiplication ro.upb.nrs.sl.FixedNaturalNumber") {
    assert(FixedNaturalNumber(3) * 2==FixedNaturalNumber(6))
    assert(2 * FixedNaturalNumber(3)==FixedNaturalNumber(6))
    assert(FixedNaturalNumber(2) * FixedNaturalNumber(3)==FixedNaturalNumber(6))
  }

  test("Divison ro.upb.nrs.sl.FixedNaturalNumber") {
    assert(FixedNaturalNumber(4) / 2==FixedNaturalNumber(2))
    assert(FixedNaturalNumber(6) / FixedNaturalNumber(2)==FixedNaturalNumber(3))
    assert(6 / FixedNaturalNumber(3)==FixedNaturalNumber(2))
  }

  test("Modulo ro.upb.nrs.sl.FixedNaturalNumber") {
    assert(FixedNaturalNumber(3) % FixedNaturalNumber(2)==FixedNaturalNumber(1))
    assert(FixedNaturalNumber(3) % 2==FixedNaturalNumber(1))
    assert(3 % FixedNaturalNumber(3)==FixedNaturalNumber(0))
  }

  test("Power ro.upb.nrs.sl.FixedNaturalNumber") {
    assert(FixedNaturalNumber(64).pow(FixedNaturalNumber(2))==FixedNaturalNumber(4096))
    assert(FixedNaturalNumber(64).pow(2)==FixedNaturalNumber(4096))
  }

  test("Nqrt ro.upb.nrs.sl.FixedNaturalNumber") {
    assert(FixedNaturalNumber(64).nqrt(2)==FixedNaturalNumber(8))
    assert(FixedNaturalNumber(1000).nqrt(3)==FixedNaturalNumber(10))
  }

  test("Abs ro.upb.nrs.sl.FixedNaturalNumber") {
    assert(FixedNaturalNumber(64).abs==FixedNaturalNumber(64))
  }

  test("Signum ro.upb.nrs.sl.FixedNaturalNumber") {
    assert(FixedNaturalNumber(64).signum==FixedNaturalNumber(1))
  }

  
  test("gcd 4 16 == 4") {
    assert(FixedNaturalNumber(16).gcd(FixedNaturalNumber(4))==FixedNaturalNumber(4))
  }

  test("gcd 4 16 +1 == 5") {
    assert((FixedNaturalNumber(16).gcd(FixedNaturalNumber(4)) + 1)==FixedNaturalNumber(5))
  }

  test("Complex ro.upb.nrs.sl.FixedNaturalNumber") {
    val x = FixedNaturalNumber(346372547, 64, RoundEven)
    val y = FixedNaturalNumber(274957638, 64, RoundEven)


    val sum = x + y
    assert(sum == FixedNaturalNumber(621330185, 64, RoundEven))
    val dif = x - y
    assert(dif == FixedNaturalNumber(71414909, 64, RoundEven))
    val mul = FixedNaturalNumber(34637, 32, RoundEven) * FixedNaturalNumber(27495, 32, RoundEven)
    assert(mul == FixedNaturalNumber(952344315, 128, RoundEven))
    val div = x / y
    assert(div == FixedNaturalNumber(1))
    assert(FixedNaturalNumber(346372542, 64, RoundEven) / 2 == FixedNaturalNumber(173186271, 64, RoundEven))

    val pow = FixedNaturalNumber(3463, 64, RoundEven).pow(2)
    assert(pow == FixedNaturalNumber(11992369, 64, RoundEven))

    val root = FixedNaturalNumber(71503936, 64, RoundEven).nqrt(2)
    assert(root == FixedNaturalNumber(8456, 64, RoundEven))

    val nthroot = FixedNaturalNumber(603351125, 64, RoundEven).nqrt(3)
    assert(nthroot == FixedNaturalNumber(845))

  }
  
  test("SQRT 16") {
    assert(FixedNaturalNumber(16).sqrt==FixedNaturalNumber(4))
  }
}