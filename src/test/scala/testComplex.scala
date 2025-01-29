import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class Complex_Tests extends FunSuite {

  numberRepresentationSystemWorkingType = RationalNumber_NR

  test("Addition 1+2==3") {
    assert( (Complex(1, 0) + Complex(2, 0)) == Complex(3, 0) )
  }


  test("Addition 1+2i==1+2i") {
    assert( (Complex(1, 0) + Complex(0, 2)) == Complex(1, 2) )
  }


  test("Multiplication 1*2i==2i") {
    assert( (Complex(1, 0) * Complex(0, 2)) == Complex(0, 2) )
  }

}