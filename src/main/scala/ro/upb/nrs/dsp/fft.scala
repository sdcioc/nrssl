package ro.upb.nrs.dsp


import ro.upb.nrs.sl.NumberRepresentationSystemConversions._
import ro.upb.nrs.sl._

object FFT {
    def _fft(cSeq: Seq[Complex_B], direction: Complex_B, scalar: NumberRepresentationSystem): Seq[Complex_B] = {
        if (cSeq.length == 1) {
            return cSeq
        }
        val n = cSeq.length
        assume(n % 2 == 0, "The Cooley-Tukey FFT algorithm only works when the length of the input is even.")
    
        val evenOddPairs = cSeq.grouped(2).toSeq
        val evens = _fft(evenOddPairs map (_(0)), direction, scalar)
        val odds  = _fft(evenOddPairs map (_(1)), direction, scalar)
    
        def leftRightPair(k: Int): (Complex_B, Complex_B) = {
            val base = evens(k) / scalar
            val exponent = direction * (3.14 * k / n)
            val offset = exponent.exp * odds(k) / scalar
            //println(exponent.exp());
            (base + offset, base - offset)
        }
    
        val pairs = (0 until n/2) map leftRightPair
        val left  = pairs map (_._1)
        val right = pairs map (_._2)
        left ++ right
    }
    
    def  fft(cSeq: Seq[Complex_B]): Seq[Complex_B] = {
        val old_conversion : NumberRepresentationSystem = NumberRepresentationSystemConversions.numberRepresentationSystemWorkingType
        val new_conversion : NumberRepresentationSystem = cSeq(0).real match {
            case a : NaturalNumber_B => NaturalNumber_NR
            case a : IntegerNumber_B => IntegerNumber_NR
            case a : RationalNumber_B => RationalNumber_NR
            case a : InfiniteFixedPoint_B => InfiniteFixedPoint_NR
            case a : InfiniteFloatingPoint_B => InfiniteFloatingPoint_NR
            case a : FixedNaturalNumber_B => FixedNaturalNumber_NR(a.size, a.rounding)
            case a : FixedIntegerNumber_B => FixedIntegerNumber_NR(a.size, a.rounding)
            case a : FractionalNumber_B => FractionalNumber_NR(a.size_numerator, a.size_denominator, a.rounding)
            case a : FixedPoint_B => FixedPoint_NR(a.size, a.fraction_size, a.rounding)
            case a : oldFixedFloatingPoint_B => oldFixedFloatingPoint_NR(a.exponent_size, a.fraction_size, a.rounding)
            case a : oldIEEE754_B => oldIEEE754_NR(a.exponent_size, a.fraction_size, a.rounding)
            case a : oldPosit_B => oldPosit_NR(a.exponent_size, a.size, a.rounding)
            case _ => throw new NotImplementedError("ro.upb.nrs.dsp class fft")
        }
        NumberRepresentationSystemConversions.numberRepresentationSystemWorkingType = new_conversion
        val logarithm2Length = Math.log(cSeq.length) / Math.log(2)
        val new_cSeq : Seq[Complex_B] = if(cSeq.length == Math.pow(2, logarithm2Length.toInt))
                                            cSeq
                                        else
                                            cSeq ++ Seq.fill( (Math.pow(2, logarithm2Length.toInt + 1) - cSeq.length).toInt )(Complex(0))
        println(new_cSeq)
        val returnValue = _fft(new_cSeq, Complex(0,  -2), 1)
        NumberRepresentationSystemConversions.numberRepresentationSystemWorkingType = old_conversion
        returnValue
    }
    def ifft(cSeq: Seq[Complex_B]): Seq[Complex_B] = {
        
        val old_conversion : NumberRepresentationSystem = NumberRepresentationSystemConversions.numberRepresentationSystemWorkingType
        val new_conversion : NumberRepresentationSystem = cSeq(0).real match {
            case a : NaturalNumber_B => NaturalNumber_NR
            case a : IntegerNumber_B => IntegerNumber_NR
            case a : RationalNumber_B => RationalNumber_NR
            case a : InfiniteFixedPoint_B => InfiniteFixedPoint_NR
            case a : InfiniteFloatingPoint_B => InfiniteFloatingPoint_NR
            case a : FixedNaturalNumber_B => FixedNaturalNumber_NR(a.size, a.rounding)
            case a : FixedIntegerNumber_B => FixedIntegerNumber_NR(a.size, a.rounding)
            case a : FractionalNumber_B => FractionalNumber_NR(a.size_numerator, a.size_denominator, a.rounding)
            case a : FixedPoint_B => FixedPoint_NR(a.size, a.fraction_size, a.rounding)
            case a : oldFixedFloatingPoint_B => oldFixedFloatingPoint_NR(a.exponent_size, a.fraction_size, a.rounding)
            case a : oldIEEE754_B => oldIEEE754_NR(a.exponent_size, a.fraction_size, a.rounding)
            case a : oldPosit_B => oldPosit_NR(a.exponent_size, a.size, a.rounding)
            case _ => throw new NotImplementedError("ro.upb.nrs.dsp class fft")
        }
        NumberRepresentationSystemConversions.numberRepresentationSystemWorkingType = new_conversion
        val returnValue = _fft(cSeq, Complex(0, 2), 2)
        NumberRepresentationSystemConversions.numberRepresentationSystemWorkingType = old_conversion
        returnValue
    } 
}