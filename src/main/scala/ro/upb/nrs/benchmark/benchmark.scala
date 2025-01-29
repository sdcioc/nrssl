package ro.upb.nrs.benchmark



import scala.annotation.tailrec
import ro.upb.nrs.sl._
import ro.upb.nrs.dsp._

object basicBenchmarkFunctions {
    //converting from binary string to the given NRS
    def fromBinaryString(binaryString : String, nrs : FixedPrecisionNumberRepresentationSystem) : FixedPrecisionNumberRepresentationSystem = {
        nrs match {
            case a : FixedNaturalNumber_B => FixedNaturalNumber(BigInt(binaryString, 2).toLong, a.size, a.rounding)
            case a : FixedIntegerNumber_B => FixedIntegerNumber(BigInt(binaryString, 2).toLong, a.size, a.rounding)
            case a : FractionalNumber_B => FractionalNumber(binaryString, a.size_numerator, a.size_denominator, a.rounding)
            case a : FixedPoint_B => FixedPoint(binaryString, a.size, a.fraction_size, a.rounding)
            //case a : oldFixedFloatingPoint_B => oldFixedFloatingPoint(binaryString, a.exponent_size, a.fraction_size, a.rounding)
            case a : FixedFloatingPoint_B => FixedFloatingPoint(binaryString, a.exponent_size, a.fraction_size, a.rounding)
            //case a : oldIEEE754_B => oldIEEE754(binaryString, a.exponent_size, a.fraction_size, a.rounding)
            case a : IEEE754_B => IEEE754(binaryString, a.exponent_size, a.fraction_size, a.rounding)
            //case a : oldPosit_B => oldPosit(binaryString, a.exponent_size, a.size, a.rounding)
            case a : Posit_B => Posit(binaryString, a.exponent_size, a.size, a.rounding)
            case a : Morris_B => Morris(binaryString, a.g_size, a.size, a.rounding)
            case a : MorrisHEB_B => MorrisHEB(binaryString, a.g_size, a.size, a.rounding)
            case a : MorrisBiasHEB_B => MorrisBiasHEB(binaryString, a.g_size, a.size, a.rounding)
            case a : MorrisUnaryHEB_B => MorrisUnaryHEB(binaryString, a.size, a.rounding)
            case _ => throw new NotImplementedError("ro.upb.nrs.benchmark basicBenchmarkFunctions fromBinaryString not such NRS")
        }
    }
    //converting from text string to the given NRS
    def fromTextString(textString : String, nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        nrs match {
            case a : NaturalNumber_B => NaturalNumber(BigInt(textString).toLong)
            case a : IntegerNumber_B => IntegerNumber(BigInt(textString).toLong)
            case a : RationalNumber_B => RationalNumber.fromString(textString)
            case a : FixedNaturalNumber_B => FixedNaturalNumber(BigInt(textString).toLong, a.size, a.rounding)
            case a : FixedIntegerNumber_B => FixedIntegerNumber(BigInt(textString).toLong, a.size, a.rounding)
            case a : FractionalNumber_B => FractionalNumber(RationalNumber.fromString(textString), a.size_numerator, a.size_denominator, a.rounding)
            case a : FixedPoint_B => RationalNumber.fromString(textString).toFixedP(a.size, a.fraction_size, a.rounding)
            case a : oldFixedFloatingPoint_B => RationalNumber.fromString(textString).toOldFloatingPoint(a.exponent_size, a.fraction_size, a.rounding)
            case a : FixedFloatingPoint_B => FixedFloatingPoint(RationalNumber.fromString(textString).toFloatingPoint(a.fraction_size), a.exponent_size, a.fraction_size, a.rounding)
            case a : oldIEEE754_B => RationalNumber.fromString(textString).toOldIEEE754(a.exponent_size, a.fraction_size, a.rounding)
            case a : IEEE754_B => IEEE754(RationalNumber.fromString(textString).toFloatingPoint(a.fraction_size), a.exponent_size, a.fraction_size, a.rounding)
            case a : oldPosit_B => oldPosit(RationalNumber.fromString(textString).toTaperedFloatingPoint(a.size), a.exponent_size, a.size, a.rounding)
            case a : Posit_B => Posit(RationalNumber.fromString(textString).toTaperedFloatingPoint(a.size), a.exponent_size, a.size, a.rounding)
            case a : Morris_B => Morris(RationalNumber.fromString(textString).toTaperedFloatingPoint(a.size), a.g_size, a.size, a.rounding)
            case a : MorrisHEB_B => MorrisHEB(RationalNumber.fromString(textString).toTaperedFloatingPoint(a.size), a.g_size, a.size, a.rounding)
            case a : MorrisBiasHEB_B => MorrisBiasHEB(RationalNumber.fromString(textString).toTaperedFloatingPoint(a.size), a.g_size, a.size, a.rounding)
            case a : MorrisUnaryHEB_B => MorrisUnaryHEB(RationalNumber.fromString(textString).toTaperedFloatingPoint(a.size), a.size, a.rounding)
            case _ => throw new NotImplementedError("ro.upb.nrs.benchmark basicBenchmarkFunctions fromBinaryString not such NRS")
        }
    }
    //converting from RationalNumber to the given NRS
    def fromRationalNumber(value : RationalNumber_B, nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        nrs match {
            case a : NaturalNumber_B => NaturalNumber(value.toLong)
            case a : IntegerNumber_B => IntegerNumber(value.toLong)
            case a : RationalNumber_B => value
            case a : FixedNaturalNumber_B => FixedNaturalNumber(value.toLong, a.size, a.rounding)
            case a : FixedIntegerNumber_B => FixedIntegerNumber(value.toLong, a.size, a.rounding)
            case a : FractionalNumber_B => FractionalNumber(value, a.size_numerator, a.size_denominator, a.rounding)
            case a : FixedPoint_B => value.toFixedP(a.size, a.fraction_size, a.rounding)
            case a : oldFixedFloatingPoint_B => value.toOldFloatingPoint(a.exponent_size, a.fraction_size, a.rounding)
            case a : FixedFloatingPoint_B => FixedFloatingPoint(value.toFloatingPoint(a.fraction_size), a.exponent_size, a.fraction_size, a.rounding)
            case a : oldIEEE754_B => value.toOldIEEE754(a.exponent_size, a.fraction_size, a.rounding)
            case a : IEEE754_B => IEEE754(value.toFloatingPoint(a.fraction_size), a.exponent_size, a.fraction_size, a.rounding)
            case a : oldPosit_B => oldPosit(value.toTaperedFloatingPoint(a.size), a.exponent_size, a.size, a.rounding)
            case a : Posit_B => Posit(value.toTaperedFloatingPoint(a.size), a.exponent_size, a.size, a.rounding)
            case a : Morris_B => Morris(value.toTaperedFloatingPoint(a.size), a.g_size, a.size, a.rounding)
            case a : MorrisHEB_B => MorrisHEB(value.toTaperedFloatingPoint(a.size), a.g_size, a.size, a.rounding)
            case a : MorrisBiasHEB_B => MorrisBiasHEB(value.toTaperedFloatingPoint(a.size), a.g_size, a.size, a.rounding)
            case a : MorrisUnaryHEB_B => MorrisUnaryHEB(value.toTaperedFloatingPoint(a.size), a.size, a.rounding)
            case _ => throw new NotImplementedError("ro.upb.nrs.benchmark basicBenchmarkFunctions fromBinaryString not such NRS")
        }
    }
    def getNRS_size(nrs : NumberRepresentationSystem) : Int = {
        nrs match {
            case a : FixedNaturalNumber_B => a.size
            case a : FixedIntegerNumber_B => a.size
            case a : FractionalNumber_B => a.size_denominator + a.size_numerator
            case a : FixedPoint_B => a.size
            case a : oldFixedFloatingPoint_B => a.exponent_size + a.fraction_size + 1
            case a : FixedFloatingPoint_B => a.exponent_size + a.fraction_size + 1
            case a : oldIEEE754_B => a.exponent_size + a.fraction_size + 1
            case a : IEEE754_B => a.exponent_size + a.fraction_size + 1
            case a : oldPosit_B => a.size
            case a : Posit_B => a.size
            case a : Morris_B => a.size
            case a : MorrisHEB_B => a.size
            case a : MorrisBiasHEB_B => a.size
            case a : MorrisUnaryHEB_B => a.size
            case _ => throw new NotImplementedError("ro.upb.nrs.benchmark basicBenchmarkFunctions fromBinaryString not such NRS")
        }
    }
    //generating the string of the NRS
    def toStringFromNRS(nrs : NumberRepresentationSystem) : String = {
        def roundToString(rounding : RoundingType) = rounding match {
            case RoundUp => "RU"
            case RoundDown => "RD"
            case RoundZero => "RZ"
            case RoundAwayZero => "RAZ"
            case RoundEven => "RE"
            case _ => "NR"
        }
        nrs match {
            case a : NaturalNumber_B => "NaturalNumber"
            case a : IntegerNumber_B => "IntegerNumber"
            case a : RationalNumber_B => "RationalNumber"
            case a : FixedNaturalNumber_B => "FixedNaturalNumber_" + a.size.toString + "_" + roundToString(a.rounding)
            case a : FixedIntegerNumber_B => "FixedIntegerNumber_" + a.size.toString + "_" + roundToString(a.rounding)
            case a : FractionalNumber_B => "FractionalNumber_" + a.size_numerator.toString + "_" + a.size_denominator.toString + "_" + roundToString(a.rounding)
            case a : FixedPoint_B => "FixedP_" + a.size.toString + "_" + a.fraction_size.toString + "_" + roundToString(a.rounding)
            case a : oldFixedFloatingPoint_B => "FloatingPoint_" + a.exponent_size.toString + "_" + a.fraction_size.toString + "_" + roundToString(a.rounding)
            case a : FixedFloatingPoint_B => "FixedFloatingPoint_" + a.exponent_size.toString + "_" + a.fraction_size.toString + "_" + roundToString(a.rounding)
            case a : oldIEEE754_B => "OldIEEE754_" + a.exponent_size.toString + "_" + a.fraction_size.toString + "_" + roundToString(a.rounding)
            case a : IEEE754_B => "IEEE754_" + a.exponent_size.toString + "_" + a.fraction_size.toString + "_" + roundToString(a.rounding)
            case a : oldPosit_B => "OldPosit_" + a.exponent_size.toString + "_" + a.size.toString + "_" + roundToString(a.rounding)
            case a : Posit_B => "Posit_" + a.exponent_size.toString + "_" + a.size.toString + "_" + roundToString(a.rounding)
            case a : Morris_B => "Morris_" + a.g_size.toString + "_" + a.size.toString + "_" + roundToString(a.rounding)
            case a : MorrisHEB_B => "MorrisHEB_" + a.g_size.toString + "_" + a.size.toString + "_" + roundToString(a.rounding)
            case a : MorrisBiasHEB_B => "MP_" + a.g_size.toString + "_" + a.size.toString + "_" + roundToString(a.rounding)
            case a : MorrisUnaryHEB_B => "PM_" + a.size.toString + "_" + roundToString(a.rounding)
            case _ => throw new NotImplementedError("ro.upb.nrs.benchmark basicBenchmarkFunctions fromBinaryString not such NRS")
        }
    }
    /*
    Decimal error = log10(x_computed/x_exact)
    */
    def decimalError(computed : RationalNumber_B, exact : RationalNumber_B) : RationalNumber_B = if(exact==RationalNumber(0)) {
        if(computed==exact)
            RationalNumber(0)
        else
            throw throw new NotImplementedError("ro.upb.nrs.sl.benchmarkFunctions decimalError for exact value 0")
    } else {
        (computed/exact).log(RationalNumber(10))
    }
    /*
    Decimal Accuracy = log10(1/decimal error)
    */
    def decimalAccuracy(computed : RationalNumber_B, exact : RationalNumber_B) : RationalNumber_B = {
        val currentDecimalError = decimalError(computed, exact)
        if(currentDecimalError == RationalNumber(0))
            throw throw new NotImplementedError("ro.upb.nrs.sl.benchmarkFunctions decimalAccuracy Decimal error=0")
        else
            currentDecimalError.inverse.log(RationalNumber(10))
    }
    /*
    Absolute error = |x_computed-x_exact|
    */
    def absoluteError(computed : RationalNumber_B, exact : RationalNumber_B) : RationalNumber_B = (computed - exact).abs
    /*
    Relative error = Absolute error / exact
    */
    def relativeError(computed : RationalNumber_B, exact : RationalNumber_B) : RationalNumber_B = if(exact==NaturalNumber(0)) {
        if(computed==exact)
            RationalNumber(0)
        else
            throw throw new NotImplementedError("ro.upb.nrs.sl.benchmarkFunctions relativeError for exact value 0")
    } else {
        absoluteError(computed, exact)/exact
    }
    /*
    answer quality = 1/Absolute error
    */
    def answerQuality(computed : RationalNumber_B, exact : RationalNumber_B) : RationalNumber_B = {
        val currentAbsoluteError = absoluteError(computed, exact)
        if(currentAbsoluteError==RationalNumber(0))
            throw throw new NotImplementedError("ro.upb.nrs.sl.benchmarkFunctions answerQuality Absolute error=0")
        else
            currentAbsoluteError.inverse
    }
    /*
    information per bit = answer quality / number of bits
    */
    def bitInformation(computed : RationalNumber_B, exact : RationalNumber_B, numberOfBits : RationalNumber_B) : RationalNumber_B = {
        val currentAnswerQuality = answerQuality(computed, exact)
        currentAnswerQuality/numberOfBits
    }
    
    //auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(32, NaturalNumber(0).binaryEncode))
}