package scalatutorial.app

import org.scalatra._
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.json._
import scalatutorial.app.service.WeatherService

class MyScalatraServlet extends ScalatraServlet with JacksonJsonSupport {

  protected implicit val jsonFormats: Formats = DefaultFormats

  before() {
    contentType = formats("json")
  }

  //SETUP
  get("/") {
    "{" +
      "\"hello\": \"OSCON\"" +
      "}"
  }

  //CALLING A METHOD
  get("/hello") {
    val who = params("who")

    "{" +
      "\"hello\": \"" + who + "\"" +
      "}"
  }

  //COLLECTIONS
  get("/cloud") {
    val allClouds = List("cirrus", "altostratus", "cumulus")
    allClouds(0)
  }

  //CASE CLASS
  /**
   * TASK: Add a field of your choosing to the Cloud case class.
   * NOTE: here I've added the field someOtherfield to the Cloud case class. It has a default value
   * of "hello!!!". See the "/clouds" end point for an example of how to specify a non-default value for someOtherField.
   */
  case class Cloud(name: String, heightInFeet: Int, someOtherField: String = "hello!!!") {
    /**
     * TASK: Add a method to Cloud that uses the object's method to return the cloud's height in meters
     */
    def getHeightInMeters() = Foo.convertFeedToMeters(heightInFeet)
  }

  get("/clouds") {
    val cirrus = Cloud("cirrus", 18000, "I AM CIRRUS!") //NOTE: someOtherField has a non-default value here
    val altostratus = Cloud("altostratus", 10000)
    val cumulus = Cloud("cumulus", 6000)

    val allClouds = List(cirrus, altostratus, cumulus)

    /**
     * TASK: Change /clouds to print out all the cloud names in uppercase using .map
     */
    allClouds.map(_.name.toUpperCase)
  }

  case class ChitChat(opener: String)

  //EXPRESSIONS
  get("/smalltalk") {
    val weather = params("weather")

    if (weather == "hot") {
      ChitChat("Hot enough for ya?")
    } else {
      ChitChat("Brrrr!")
    }

    weather match {
      case "hot" => ChitChat("Don't forget the sunscreen!")
      case "nice" => ChitChat("Couldn't ask for a nicer day, huh?")
      case "rain" => ChitChat("It's coming down cats & dogs")
      case "cold" => ChitChat("It's colder than a polar bear's toenails out there!")
    }
  }

  /*
   * TASK:
   * Scalatra's multiparams returns a List of params.
   * Write an endpoint in the form: http://localhost:8080/highlow?temps=45,70
   *   1. Use pattern matching with value binding to extract the high and low temp and return them in a sentence
   *   2. Use guards to validate the data
   *
   *   NOTE: we are expecting the following behavior
   *   /highlow?temps=45,70 should output ["the high is 70 and the low is 45"]
   *
   *   /highlow?temps=100,120&temps=600,70
   *     should output
   *   ["the high is 100 and the low is 120", "the high is 600 and the low is 70"]
   **/

  // This method takes in a a string of the form "[temp1],[temp2]" and returns
  // a tuple of integers: (higher_temp, lower_temp)
  // Note that Strings not abiding by the "[temp1],[temp2]" format will throw an exception
  def parseTemperaturePair(temps: String): (Int, Int) = {
    val tempValues: Array[Int] = temps.split(",").map(_.toInt)
    tempValues match {
      // Note: added safe guard to ensure that we know which value is the high and which is the low.
      case Array(high, low) if high >= low => (high, low)
      case Array(low, high) => (high, low)
    }
  }

  get("/highlow") {

    // We will use multiParams to obtain the parameters.
    // This will return us something of the form: Seq("10,20", "30,40", "50,60")
    // Note: you can think of the Seq type as a more generic form of a List
    val temps: Seq[String] = multiParams("temps")

    // convert elements of temps to (high,low) - Tuples of integers
    val tempPairs: Seq[(Int, Int)] = temps.map(parseTemperaturePair)

    // for each (high,low) pair, convert it to some informative string
    tempPairs.map{case (high, low) =>
      // we are using Scala's string interpolation here
      s"the high is $high and the low is $low"
    }

  }



  //FUNCTIONS
  def fToC(fahrenheit: Int): Double = {
    (fahrenheit - 32.0) * (5.0/9.0)
  }

  get("/convertToCelsius") {
    val f = params("temp").toInt
    fToC(f)
  }

  /* NOTE: helper method for /convertToFahrenheit */
  def cToF(celsius: Int): Double = {
    (celsius * (9.0/5.0)) + 32.0
  }

  /*Task: Complete the /convertToFahrenheit end point*/
  get("/convertToFahrenheit") {
   val c = params("temp").toInt
   cToF(c)
  }

  /* Note: IMPORTANT
   * We are unable to call WeatherService.getRecords() more than once since it will close
   * the weather's file stream. As a result, I will store the records into the weatheRecords variable
   * so that we can access it as many times as we'd like.
   */
  val weatherRecords = WeatherService.getRecords()

  /*
   * NOTE: THIS ENDPOINT WAS MODIFIED -- see previous note
   */
  get("/weatherRecords") {
    // WeatherService.getRecords  <--- NO LONGER USING THIS
    weatherRecords
  }

  //FIRST CLASS FUNCTIONS

  /**
   * TASK: Implement /averageTemp:city
   */
  get("/averageTemp/:city") {
    val city = params("city")
    // obtain records for the specified city
    val records = weatherRecords.filter(_.city == city)
    // obtain the number of records associated with the city
    val nRecords = records.length.toDouble
    // compute average high and low temperatures
    val averageHigh = records.map(_.high).sum / nRecords
    val averageLow = records.map(_.low).sum / nRecords

    Map("average_high"->averageHigh, "average_low"->averageLow)
  }

  /**
   * Task: Implement /maxTemp (maximum temperature for a given date)
   */
  get("/maxTemp") {
    // obtain date input
    val date = params("date")
    // parse the date input into an actual Date class
    val dateObj = WeatherService.dateFormat.parse(date)
    // obtain records that match the target date
    val records = weatherRecords.filter(_.date.equals(dateObj))

    // return the record with the maximum temperature by finding the record with the largest high temp.
    // (note: we can ignore low temps)
    records.maxBy(_.high)
  }


  /**
   * TASK: Come up with a query of your choice on the weather data and implement an end point for it.
   * NOTE: Here we are implementing the /stdTemp end point which computes the standard deviation of
   * high and low temperatures.
   */
  def std(xs: Seq[Int]): Double = {
    val n = xs.length.toDouble
    val mu = xs.sum / n
    Math.sqrt((xs.map(x => (x-mu)*(x-mu)).sum)/n)
  }

  get("/stdTemp/:city") {
    val city = params("city")
    // obtain records for the specified city
    val records = weatherRecords.filter(_.city == city)
    // obtain high temperatures and low temperatures of records
    val highs = records.map(_.high)
    val lows = records.map(_.low)

    Map("std of lows"->std(lows), "std of highs"->std(highs))
  }

}
