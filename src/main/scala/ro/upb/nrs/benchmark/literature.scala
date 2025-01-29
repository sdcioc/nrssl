package ro.upb.nrs.benchmark


import java.io.PrintWriter
import java.io.File

import scala.annotation.tailrec
import ro.upb.nrs.sl._
import ro.upb.nrs.dsp._

object litareatureBenchmarks {
    //Beating Floating Point at its Own Game: Posit Arithmetic
    def thinTriangleBFP(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val a = basicBenchmarkFunctions.fromTextString("7", nrs)
        val bc : RationalNumber_B = (RationalNumber.fromString("7") / RationalNumber.fromString("2") ) + RationalNumber.fromString("3") * RationalNumber.fromString("2").pow(RationalNumber.fromString("-26"))
        val b = basicBenchmarkFunctions.fromRationalNumber(bc, nrs)
        val c = basicBenchmarkFunctions.fromRationalNumber(bc, nrs)
        val s = (a+b+c) / basicBenchmarkFunctions.fromTextString("2", nrs)
        val result = (s * (s - a) * (s - b) * (s - c)).sqrt
        result
    }
    /* cancelation formula Beating Floating Point at its Own Game: Posit Arithmetic
    */
    def cancelationBFP(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val pi = basicBenchmarkFunctions.fromTextString("3.14159265359", nrs)
        val euler = basicBenchmarkFunctions.fromTextString("2.7182818284590452353602874", nrs)
        val a = basicBenchmarkFunctions.fromTextString("27", nrs) / basicBenchmarkFunctions.fromTextString("10", nrs)
        val b = basicBenchmarkFunctions.fromTextString("2", nrs).sqrt + basicBenchmarkFunctions.fromTextString("3", nrs).sqrt
        val c = basicBenchmarkFunctions.fromTextString("67", nrs) / basicBenchmarkFunctions.fromTextString("16", nrs)
        val result = ((a-euler)/(pi - b)).pow(c)
        result
    }
    /*
    x^n/n! Posits: the good, the bad and the ugly
    x=7 n=20
    x=25 n=30
    */
    def powfactGBU(nrs : NumberRepresentationSystem) : (NumberRepresentationSystem, NumberRepresentationSystem) = {
        val n1 = basicBenchmarkFunctions.fromTextString("20", nrs)
        val n2 = basicBenchmarkFunctions.fromTextString("30", nrs)
        val x1_pow = basicBenchmarkFunctions.fromTextString("7", nrs).pow(n1)
        val x2_pow = basicBenchmarkFunctions.fromTextString("25", nrs).pow(n2)
        val one = basicBenchmarkFunctions.fromTextString("1", nrs)
        def factorial(acc: NumberRepresentationSystem, current : NumberRepresentationSystem) : NumberRepresentationSystem = {
            if(current == one) acc
            else factorial(current * acc, current - one) 
        }
        (x1_pow / factorial(one, n1), x2_pow / factorial(one, n2) )
    }
    /*
    constants Posits: the good, the bad and the ugly
    Planck constant h 6.626070150 · 10^{−34}
    */
    def constanthGBU(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val a = RationalNumber.fromString("6.626070150")
        val power = RationalNumber.fromString("10").pow(RationalNumber.fromString("-34"))
        basicBenchmarkFunctions.fromRationalNumber(a * power, nrs)
    }
    /*
    constants Posits: the good, the bad and the ugly
    Avogadro number NA 6.02214076 · 10^{23}
    */
    def constantNAGBU(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val a = RationalNumber.fromString("6.02214076")
        val power = RationalNumber.fromString("10").pow(RationalNumber.fromString("23"))
        basicBenchmarkFunctions.fromRationalNumber(a * power, nrs)
    }
    /*
    constants Posits: the good, the bad and the ugly
    Speed of light c 299792458
    */
    def constantcGBU(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val a = RationalNumber.fromString("299792458")
        basicBenchmarkFunctions.fromRationalNumber(a, nrs)
    }
    /*
    constants Posits: the good, the bad and the ugly
    charge of e− 1.602176634 · 10e−19
    */
    def constantceGBU(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val a = RationalNumber.fromString("1.602176634")
        val power = RationalNumber.fromString("10").pow(RationalNumber.fromString("-19"))
        basicBenchmarkFunctions.fromRationalNumber(a * power, nrs)
    }
    /*
    constants Posits: the good, the bad and the ugly
    Boltzmann constant k 1.380649 · 10−23
    */
    def constantkGBU(nrs : NumberRepresentationSystem) : NumberRepresentationSystem = {
        val a = RationalNumber.fromString("1.380649")
        val power = RationalNumber.fromString("10").pow(RationalNumber.fromString("-23"))
        basicBenchmarkFunctions.fromRationalNumber(a * power, nrs)
    }


    def testOperation(nrs : NumberRepresentationSystem) = {
        val filePath = "results/" + "literature" + "/" + basicBenchmarkFunctions.toStringFromNRS(nrs) + ".txt"
        val fileAccuracy = new File(filePath)
        val filePrinterAccuracy = new PrintWriter(fileAccuracy)
        var totalTime = 0.0d
        val beforeTime = System.nanoTime
        
        val result_ttBFP = litareatureBenchmarks.thinTriangleBFP(nrs)
        val result_ccBFP = litareatureBenchmarks.cancelationBFP(nrs)
        
        val result_pfGBU = litareatureBenchmarks.powfactGBU(nrs)
        val result_chGBU = litareatureBenchmarks.constanthGBU(nrs)
        val result_naGBU = litareatureBenchmarks.constantNAGBU(nrs)
        val result_cGBU = litareatureBenchmarks.constantcGBU(nrs)
        val result_ceGBU = litareatureBenchmarks.constantceGBU(nrs)
        val result_kGBU = litareatureBenchmarks.constantkGBU(nrs)
        val afterTime = System.nanoTime
        totalTime = totalTime + (afterTime-beforeTime) / 1e9d
        filePrinterAccuracy.write(
            result_ttBFP + " " + "\n" +
            result_ccBFP + " " + "\n" +
            result_pfGBU._1 + " " +
            result_pfGBU._2 + " " +
            "\n" +
            result_chGBU + " " + "\n" +
            result_naGBU + " " + "\n" +
            result_cGBU + " " + "\n" +
            result_ceGBU + " " + "\n" +
            result_kGBU + " " +
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
        )/*
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
        )*/
        var threadList = for ( nrs <- nrssList) yield (new literatureBenchmarkThread(nrs))
        for(thread <- threadList) {
            thread.start()
        }
        for(thread <- threadList) {
            thread.join()
        }
    }
}


class literatureBenchmarkThread(nrs : NumberRepresentationSystem) extends Thread {
    override def run(): Unit = {
        litareatureBenchmarks.testOperation(nrs)
    }
}