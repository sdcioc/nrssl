import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class FixedIntegerNumber_Tests extends FunSuite {

  numberRepresentationSystemWorkingType = FixedIntegerNumber_NR(FixedIntegerNumber.default_size, FixedIntegerNumber.default_rounding)

  test("Addition 1+2==3") {
    assert(1+FixedIntegerNumber(2)==FixedIntegerNumber(3))
  }

  test("Addition 1+2==3 v2") {
    assert(FixedIntegerNumber(1)+2==FixedIntegerNumber(3))
  }

  test("Subtraction ro.upb.nrs.sl.FixedIntegerNumber") {
    assert(FixedIntegerNumber(3)-2==FixedIntegerNumber(1))
    assert(FixedIntegerNumber(2)-3==FixedIntegerNumber(-1))
    assert(3 - FixedIntegerNumber(2)==FixedIntegerNumber(1))
    assert(2 - FixedIntegerNumber(3)==FixedIntegerNumber(-1))
    assert(FixedIntegerNumber(3) - FixedIntegerNumber(2)==FixedIntegerNumber(1))
    assert(FixedIntegerNumber(2) - FixedIntegerNumber(3)==FixedIntegerNumber(-1))
  }

  test("Multiplication ro.upb.nrs.sl.FixedIntegerNumber") {
    assert(FixedIntegerNumber(3) * 2==FixedIntegerNumber(6))
    assert(2 * FixedIntegerNumber(3)==FixedIntegerNumber(6))
    assert(FixedIntegerNumber(-2) * FixedIntegerNumber(3)==FixedIntegerNumber(-6))
  }

  test("Divison ro.upb.nrs.sl.IntegerNumber") {
    assert(FixedIntegerNumber(4) / 2==FixedIntegerNumber(2))
    assert(FixedIntegerNumber(6) / FixedIntegerNumber(2)==FixedIntegerNumber(3))
    assert(-6 / FixedIntegerNumber(3)==FixedIntegerNumber(-2))
  }

  test("Modulo ro.upb.nrs.sl.IntegerNumber") {
    assert(FixedIntegerNumber(3) % FixedIntegerNumber(2)==FixedIntegerNumber(1))
    assert(FixedIntegerNumber(-3) % FixedIntegerNumber(2)==FixedIntegerNumber(1))
    assert(FixedIntegerNumber(3) % 2==FixedIntegerNumber(1))
    assert(3 % FixedIntegerNumber(2)==FixedIntegerNumber(1))
  }

  test("Power ro.upb.nrs.sl.FixedIntegerNumber") {
    assert(FixedIntegerNumber(64).pow(FixedIntegerNumber(2))==FixedIntegerNumber(4096))
    assert(FixedIntegerNumber(64).pow(2)==FixedIntegerNumber(4096))
    assert(FixedIntegerNumber(-10).pow(2)==FixedIntegerNumber(100))
    assert(FixedIntegerNumber(-10).pow(FixedIntegerNumber(3))==FixedIntegerNumber(-1000))
    assert(FixedIntegerNumber(-10).pow(3)==FixedIntegerNumber(-1000))
  }

  test("Nqrt ro.upb.nrs.sl.FixedIntegerNumber") {
    assert(FixedIntegerNumber(64).nqrt(2)==FixedIntegerNumber(8))
    assert(FixedIntegerNumber(1000).nqrt(3)==FixedIntegerNumber(10))
  }

  test("Abs ro.upb.nrs.sl.FixedIntegerNumber") {
    assert(FixedIntegerNumber(64).abs==FixedIntegerNumber(64))
    assert(FixedIntegerNumber(-64).abs==FixedIntegerNumber(64))
  }

  test("Signum ro.upb.nrs.sl.FixedIntegerNumber") {
    assert(FixedIntegerNumber(64).signum==FixedIntegerNumber(1))
    assert(FixedIntegerNumber(-64).signum==FixedIntegerNumber(-1))
  }

  test("Equals ro.upb.nrs.sl.FixedIntegerNumber") {
    assert(FixedIntegerNumber(8)==FixedIntegerNumber(8))
    assert(FixedIntegerNumber(-8)==FixedIntegerNumber(-8))
  }

  test("Complex ro.upb.nrs.sl.FixedIntegerNumber") {
    val x = FixedIntegerNumber(346372547, 64, RoundEven)
    val y = FixedIntegerNumber(274957638, 64, RoundEven)


    val sum = x + y
    assert(sum == FixedIntegerNumber(621330185, 64, RoundEven))
    val dif = x - y
    assert(dif == FixedIntegerNumber(71414909, 64, RoundEven))
    val mul = FixedIntegerNumber(34637, 32, RoundEven) * FixedIntegerNumber(27495, 32, RoundEven)
    assert(mul == FixedIntegerNumber(952344315, 128, RoundEven))
    assert(FixedIntegerNumber(-3360, 64, RoundEven) * FixedIntegerNumber(34345, 64, RoundEven) == FixedIntegerNumber(-115399200, 64, RoundEven))
    val div = x / y
    assert(div == FixedIntegerNumber(1))
    assert(FixedIntegerNumber(346372542, 64, RoundEven) / 2 == FixedIntegerNumber(173186271, 64, RoundEven))
    assert(FixedIntegerNumber(346372542, 64, RoundEven) / -2 == FixedIntegerNumber(-173186271, 64, RoundEven))
    assert(FixedIntegerNumber(336065825, 64, RoundEven) / FixedIntegerNumber(34345, 64, RoundEven) == FixedIntegerNumber(9785, 64, RoundEven))

    val pow = FixedIntegerNumber(3463, 64, RoundEven).pow(2)
    assert(pow == FixedIntegerNumber(11992369, 64, RoundEven))

    val root = FixedIntegerNumber(71503936, 64, RoundEven).nqrt(2)
    assert(root == FixedIntegerNumber(8456, 64, RoundEven))

    val nthroot = FixedIntegerNumber(603351125, 64, RoundEven).nqrt(3)
    assert(nthroot == FixedIntegerNumber(845))

  }

  test("SQRT 16") {
    assert(FixedIntegerNumber(16).sqrt==FixedIntegerNumber(4))
  }
}