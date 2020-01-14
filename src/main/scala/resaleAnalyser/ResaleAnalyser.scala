package resaleAnalyser

import scala.annotation.tailrec
import scala.util._
import scala.io.StdIn.readLine
import scala.math.sqrt

trait Statistic{
  def name: String
  def results(flats: List[Flat]): Double
}

final case class Flat(
    month:String,
    town:String,
    flatType:String,
    block:String,
    streetName:String,
    floorAreaSqm:String,
    flatModel:String,
    remainingLease:Int,
    resalePrice: Double)

final case class Percentile (n: Int) extends Statistic {
  val name = s"Percentile $n"

  def results(flats: List[Flat]): Double= {

    if (flats.nonEmpty) {
      //Sort the flats by HDB prices
      val sorted = flats.sortBy(_.resalePrice).map(_.resalePrice)
      //part of formula to find percentile
      val formula : Double =  (n.toDouble / 100) * flats.size

      //if statement to get percentile. "Counting"
      if (formula % 1 == 0) {
        // -1 as list starts from 0. return is retained for readability
        return sorted(formula.toInt - 1)}
      else
        return (sorted(formula.toInt - 1) + sorted(formula.toInt)) * (formula % 1)}
    else Double.NaN
  }
}


object Mean extends Statistic {
  val name = "Mean"

  def results(flats: List[Flat]): Double = {
    if (flats.nonEmpty){
      flats.map(_.resalePrice).sum / flats.size
    }
    else Double.NaN
  }
}

object Min extends Statistic {
  val name = "Min"

  def results(flats: List[Flat]): Double = {
    if (flats.nonEmpty) flats.minBy(_.resalePrice).resalePrice
    else Double.NaN
  }
}

object Max extends Statistic {
  val name = "Max"

  def results(flats: List[Flat]): Double = {
    if (flats.nonEmpty) flats.maxBy(_.resalePrice).resalePrice
    else Double.NaN
  }
}

object StdDev extends  Statistic{
  val name = "Standard Deviation"

  def results(flats: List[Flat]): Double = {
    if (flats.nonEmpty) {

      val mean = Mean.results(flats)
      val sqrMean = (flats.map(_.resalePrice - mean) , flats.map(_.resalePrice - mean)).zipped.map(_*_)
      return sqrt(sqrMean.sum/sqrMean.size)

    } else Double.NaN
  }
}




object ResaleAnalyser {

  //all is a list of stats
  val all = List[Statistic](Mean,Max,Min,StdDev,Percentile(25),Percentile(50),Percentile(75))

  def main(args: Array[String]): Unit = {

    println("Running ResaleAnalyser...")
    val src = io.Source.fromFile("resalePriceAfterJan2015.csv")

    //Reading the CSV file, removed the header
    val flats = src.getLines.zipWithIndex.drop(1).flatMap { case (line, idx) => load(idx, line.split(",")) }.toList
    //Closed source after loading into script
    src.close
    println(s"Total number of resale flats sold after Jan 2015 is: ${flats.size}")

    //Basic data exploration from flats instances
    val highestResale = flats.maxBy(_.resalePrice)
    val lowestResale = flats.minBy(_.resalePrice)
    println(s"\nHighest resale price is : ${highestResale.resalePrice}. It's flat type is ${highestResale.flatType}")
    println(s"Lowest resale price is : ${lowestResale.resalePrice}. It's flat type is ${lowestResale.flatType}")
    println(s"The most common HDB flat sold is ${flats.groupBy(_.flatType).maxBy(_._2.size)._1}")

    prompt(flats)
  }

  @tailrec
  def prompt(flats: List[Flat]) : Unit = {
    //User decides which flat type resale price average to display. Invalid inputs will be checked at calcflats()
    val hdbType = readLine("\nInput [t] to check a table of relevant stats by room type \nAverage price of ? Flat Type ([1-5] or e/m/t): ")
    hdbType match {
      case "t" => printStatisticsTable(flats , all)
      case "m" => calcFlats(flats.filter(_.flatType == "MULTI-GENERATION") , "Multi-generation")
      case "e" | "E" => calcFlats(flats.filter(_.flatType == "EXECUTIVE") , "Executive")
      case _ => calcFlats(flats.filter(_.flatType == s"$hdbType ROOM") , s"$hdbType room")
    }
    //checks to see if user wants to explore data of other flat types
    if (reCheck("\nCheck out more info on other flat types? (y/n): ")) prompt(flats)
  }

  def load(lineNo: Int, cols: Array[String]): Option[Flat] = {
    Try(Flat(cols(0),cols(1),cols(2),cols(3),cols(4),cols(6),cols(8),cols(9).toInt,cols(10).toDouble)) match {
      case Success(f) =>
        Some(f)
      case Failure(ex) =>
        println(s"Error on line $lineNo: $ex")
        None
    }
  }

  //To check if user wants to re run the script
  @tailrec
  def reCheck(printer: String): Boolean = {
    val userInput = readLine(printer)
    userInput match {
      case "y" => true
      case "n" => println("Goodbye SpaceCowboy......")
                  false
      case _ => println("Invalid Input, try again")
                reCheck(printer)
    }
  }

  def calcFlats(flats: List[Flat], rmType: String): Unit ={
    if (flats.isEmpty) println(s"$rmType unavailable/does not exist")
    else {
      //counts total number of flats sold of a particular type
      println(s"\nThere are a total of ${flats.size} $rmType flat sold from 2015")
      //most expensive and cheapest incl estate
      println(s"The most expensive $rmType flat sold for : ${flats.maxBy(_.resalePrice).resalePrice} & it's located at ${flats.maxBy(_.resalePrice).town}")
      println(s"The cheapest $rmType flat sold for : ${flats.minBy(_.resalePrice).resalePrice} & it's located at ${flats.minBy(_.resalePrice).town}")
      //avg
      println(s"\nThe average price of a $rmType flat is ${flats.map(_.resalePrice).sum / flats.size}")
    }
  }

  def printStatisticsTable(flats: List[Flat], stats: List[Statistic]): Unit ={
    println("\nDisplaying table relevant stats...")
    print(("Flat Type"::stats.map(_.name)).mkString(", "))
    flats.groupBy(_.flatType).toList.sortBy(_._1).foreach { case (flatType, list) => println("\n" + (flatType :: stats.map(_.results(list))).mkString(", "))}
    println()
  }
}