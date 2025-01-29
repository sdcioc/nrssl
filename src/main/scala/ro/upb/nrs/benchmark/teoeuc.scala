package ro.upb.nrs.benchmark


import java.io.PrintWriter
import java.io.File

import scala.annotation.tailrec
import ro.upb.nrs.sl._
import ro.upb.nrs.dsp._

object endOfErorrUnumComputing {
    /*
    The end of error unum computing pg 151 5.9604644775390625^0.875
    Correct 4.76837158203125
    */
    def thoeuc151_pow(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val base = basicBenchmarkFunctions.fromTextString("5.9604644775390625", nrs)
        val pow = basicBenchmarkFunctions.fromTextString("0.875", nrs)
        base.pow(pow)
    }
    /*
    The end of error unum computing pg 153 e^(pi * sqrt(163))=262537412640768743.9999999999999999999...
    */
    def thoeuc153_exp(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val pi = basicBenchmarkFunctions.fromTextString("3.14159265359", nrs)
        val a = basicBenchmarkFunctions.fromTextString("163", nrs)
        val euler = basicBenchmarkFunctions.fromTextString("2.7182818284590452353602874", nrs)
        //(pi * a.sqrt).exp
        euler.pow(pi * a.sqrt)
    }
    /*
    The end of error unum computing pg 170 john wallis product pi=2*(2*2/(1*3)x(4*4)/(3*5)x...)
    pi
    */
    def thoeuc170_john_wallis_product(nrs : NumberRepresentationSystem, precision : Int) : NumberRepresentationSystem = {
        val nrs_precision = basicBenchmarkFunctions.fromRationalNumber(RationalNumber(precision), nrs)
        val a =  basicBenchmarkFunctions.fromTextString("2", nrs)
        val b =  basicBenchmarkFunctions.fromTextString("1", nrs)
        def helper(acc : NumberRepresentationSystem, xAcc : NumberRepresentationSystem) : NumberRepresentationSystem = {
            if(xAcc > nrs_precision) acc
            else helper((acc * (xAcc * xAcc)) / ((xAcc - b) * (xAcc + b)), xAcc + a)
        }
        helper(a, a)
    }
    /*
    The end of error unum computing pg 173 wrath of kahan
    u_{i+2}=111-1130/u_{i+1}+3000/(u_{i}*u_{i+1})
    */
    def thoeuc173_wrath_of_kahan(nrs : NumberRepresentationSystem, precision : Int) : NumberRepresentationSystem = {
        val a = basicBenchmarkFunctions.fromTextString("111", nrs)
        val b = basicBenchmarkFunctions.fromTextString("1130", nrs)
        val c = basicBenchmarkFunctions.fromTextString("3000", nrs)
        val u0 = basicBenchmarkFunctions.fromTextString("2", nrs)
        val u1 = basicBenchmarkFunctions.fromTextString("-4", nrs)
        def helper(prev1 : NumberRepresentationSystem, prev2 : NumberRepresentationSystem, iterator : Int) : NumberRepresentationSystem = {
            val current = a - (b / prev1) + (c / (prev2*prev1))
            if(iterator > precision) current
            else helper(current, prev1, iterator + 1)
        }
        helper(u1, u0, 2)
    }
    /*
    The end of error unum computing pg 176 Jean-Michel Muller
    E(0)=1 E(z)=(exp(z)-1)/z, Q(x)=|x-sqrt(x^2+1)|-1/(x+sqrt(x^2+1))
    H(x)=E(Q(x)^2)
    X = 15.0, 16.0, 17.0, 9999.0
    1 1 1 1
    */
    def thoeuc176_JeanMichealMuller(x : String, nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val a = basicBenchmarkFunctions.fromTextString("0", nrs)
        val b = basicBenchmarkFunctions.fromTextString("1", nrs)
        val x_nrs = basicBenchmarkFunctions.fromTextString(x, nrs)
        def functionE(z : NumberRepresentationSystem) : NumberRepresentationSystem = if(z==a) b else (z.exp-b) / z
        def functionQ(z : NumberRepresentationSystem) : NumberRepresentationSystem = (z - (z * z + b).sqrt ).abs - (b / (z + (z * z + b).sqrt ) )
        def functionH(z : NumberRepresentationSystem) : NumberRepresentationSystem = functionE(functionQ(z) * functionQ(z))
        functionH(x_nrs)
    }
    /*
    The end of error unum computing pg 179 Siegried Rump
    333.75 * Y^6 + X^2 * (11 * X^2 * Y^2 - Y^6 - 121 * Y^4 - 2) + 5.5 * Y^8 + X / (2 * Y)
    X = 77617 Y = 33096
    -0.827
    */
    def thoeuc179_SiegriedRump(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val x = basicBenchmarkFunctions.fromTextString("77617", nrs)
        val y = basicBenchmarkFunctions.fromTextString("33096", nrs)
        val val_333_75 = basicBenchmarkFunctions.fromTextString("333.75", nrs)
        val val_11 = basicBenchmarkFunctions.fromTextString("11", nrs)
        val val_121 = basicBenchmarkFunctions.fromTextString("121", nrs)
        val val_2 = basicBenchmarkFunctions.fromTextString("2", nrs)
        val val_5_5 = basicBenchmarkFunctions.fromTextString("5.5", nrs)
        val y_pow2 = y * y
        val y_pow4 = y_pow2 * y_pow2
        val y_pow6 = y_pow4 * y_pow2
        val y_pow8 = y_pow6 * y_pow2
        val x_pow2 = x * x
        val_333_75 * y_pow6 + x_pow2 * (val_11 * x_pow2 * y_pow2 - y_pow6 - val_121 * y_pow4 - val_2) + val_5_5 * y_pow8 + x / (val_2 * y)
    }
    /*
    The end of error unum computing pg 181 quadratic
    r_1 = (-b+sqrt(b^2-4*a*c)) / (2 * a)
    r_2 = (-b-sqrt(b^2-4*a*c)) / (2 * a)
    a = 3
    b = 100
    c = 2
    -0.02001
    -33.31332
    */
    def thoeuc181_Quadratic(nrs : NumberRepresentationSystem) : (NumberRepresentationSystem, NumberRepresentationSystem) = {
        val a = basicBenchmarkFunctions.fromTextString("3", nrs)
        val b = basicBenchmarkFunctions.fromTextString("100", nrs)
        val c = basicBenchmarkFunctions.fromTextString("2", nrs)
        val val_2 = basicBenchmarkFunctions.fromTextString("2", nrs)
        val quadraticSqrt = (b * b - val_2 * val_2 * a * c).sqrt
        ( (-b + quadraticSqrt) / (val_2 * a), (-b - quadraticSqrt) / (val_2 * a) )
    }
    /*
    The end of error unum computing pg 184 bailey
    0.25510582*x+0.52746197*y=0.79981812
    0.80143857*x+1.65707065*y=2.51270273
    x = -1 y = 2
    */
    def thoeuc184_Bailey(
        nrs : NumberRepresentationSystem 
        ) : (NumberRepresentationSystem, NumberRepresentationSystem) = {
        val val_a11 = basicBenchmarkFunctions.fromTextString("0.25510582", nrs)
        val val_a12 = basicBenchmarkFunctions.fromTextString("0.52746197", nrs)
        val val_b1 = basicBenchmarkFunctions.fromTextString("0.79981812", nrs)
        val val_a21 = basicBenchmarkFunctions.fromTextString("0.80143857", nrs)
        val val_a22 = basicBenchmarkFunctions.fromTextString("1.65707065", nrs)
        val val_b2 = basicBenchmarkFunctions.fromTextString("2.51270273", nrs)
        val det = val_a11 * val_a22 - val_a12 * val_a21
        val x = (val_b1 * val_a22 - val_b2 * val_a12) / det
        val y = (val_b2 * val_a11 - val_b1 * val_a21) / det
        (x, y)
    }
    /*
    The end of error unum computing pg 189 CFFT
    1K CFFT
    1024 complex FFT
    */
    def thoeuc189_FFT(
        test : Seq[Complex_B]
        ) : Seq[Complex_B] = {
        FFT.fft(test)
    }

    def testOperation(nrs : NumberRepresentationSystem) = {
        val filePath = "results/" + "thoeuc" + "/" + basicBenchmarkFunctions.toStringFromNRS(nrs) + ".txt"
        val fileAccuracy = new File(filePath)
        val filePrinterAccuracy = new PrintWriter(fileAccuracy)
        var totalTime = 0.0d
        val beforeTime = System.nanoTime
        //val result_151 = endOfErorrUnumComputing.thoeuc151_pow(nrs)
        //val result_153 = endOfErorrUnumComputing.thoeuc153_exp(nrs)
        val result_170 = endOfErorrUnumComputing.thoeuc170_john_wallis_product(nrs, 30)
        val result_173 = endOfErorrUnumComputing.thoeuc173_wrath_of_kahan(nrs, 30)
        //X = 15.0, 16.0, 17.0, 9999.0
        val result_176_15 = endOfErorrUnumComputing.thoeuc176_JeanMichealMuller("15", nrs)
        val result_176_16 = endOfErorrUnumComputing.thoeuc176_JeanMichealMuller("16", nrs)
        val result_176_17 = endOfErorrUnumComputing.thoeuc176_JeanMichealMuller("17", nrs)
        val result_176_9999 = endOfErorrUnumComputing.thoeuc176_JeanMichealMuller("9999", nrs)
        val result_179 = endOfErorrUnumComputing.thoeuc179_SiegriedRump(nrs)
        val result_181 = endOfErorrUnumComputing.thoeuc181_Quadratic(nrs)
        val result_184 = endOfErorrUnumComputing.thoeuc184_Bailey(nrs)
        val afterTime = System.nanoTime
        totalTime = totalTime + (afterTime-beforeTime) / 1e9d
        filePrinterAccuracy.write(
        //    result_151 + " " + "\n" +
        //    result_153 + " " + "\n" +
            result_170 + " " + "\n" +
            result_173 + " " + "\n" +
            result_176_15 + " " +
            result_176_16 + " " +
            result_176_17 + " " +
            result_176_9999 + " " + "\n" +
            result_179 + " " + "\n" +
            //basicBenchmarkFunctions.decimalAccuracy(result_181._1.toRationalNumber, RationalNumber.fromString("-0.02018229166666666666666666666666667") ) +
            result_181._1 + " " +
            result_181._2 + " " +
            "\n" +
            result_184._1 + " " +
            result_184._2 + " " +
            "\n")
        filePrinterAccuracy.write("Time:" + totalTime)
        filePrinterAccuracy.close()
    }
    
    def testOperations = {
        val size = 32
        
        val nrssList = List(
            Posit(0.0d, 2, size, RoundEven),
            MorrisUnaryHEB(1.0d, size, RoundEven),
            MorrisBiasHEB(1.0d, 4, size, RoundEven),
            Morris(1.0d, 4, size, RoundZero),
            MorrisHEB(1.0d, 4, size, RoundZero),
            FixedPoint(1.0d, size, 16, RoundEven),
            IEEE754(0.0d, 8, 23, RoundEven),
            FixedFloatingPoint(0.0d, 8, 23, RoundEven)
        )
        /*
        val nrssList = List(
            Posit(0.0d, 2, size, RoundEven),
            MorrisUnaryHEB(1.0d, size, RoundEven),
            MorrisBiasHEB(1.0d, 5, size, RoundEven),
            Morris(1.0d, 5, size, RoundZero),
            MorrisHEB(1.0d, 5, size, RoundZero),
            FixedPoint(1.0d, size, 32, RoundEven),
            IEEE754(0.0d, 11, 52, RoundEven),
            FixedFloatingPoint(0.0d, 11, 52, RoundEven),
            RationalNumber_NR
        )
        */
        var threadList = for ( nrs <- nrssList) yield (new endOfErorrUnumComputingBenchmarkThread(nrs))
        for(thread <- threadList) {
            thread.start()
        }
        for(thread <- threadList) {
            thread.join()
        }
    }
}


class endOfErorrUnumComputingBenchmarkThread(nrs : NumberRepresentationSystem) extends Thread {
    override def run(): Unit = {
        endOfErorrUnumComputing.testOperation(nrs)
    }
}