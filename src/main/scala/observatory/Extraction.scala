package observatory

import observatory.Extraction.getClass
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.types.*
import org.apache.spark.sql.{DataFrame, Dataset, Encoder, Encoders, SQLContext, SparkSession}

import java.nio.file.Paths
import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface:

  import Helpers.toCelsius
  import scala.io.Source
  import org.apache.spark.sql.types._
  import scala3encoders.given

  val conf: SparkConf = new SparkConf().setAppName("capstone").setMaster("local")
  val spark: SparkSession = SparkSession.builder.config(conf).getOrCreate
  import spark.implicits.localSeqToDatasetHolder

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] =

    val stations: RDD[String] = spark.sparkContext.parallelize(getLines(stationsFile))
    val stationRDD: RDD[StationSchema] = stations.map(_.split(",", -1))
      .map(row => StationSchema(
        toOption(row(0)).map(_.toInt), // stnId
        toOption(row(1)).map(_.toInt), // wbanId
        toOption(row(3)), // long
        toOption(row(2)) // lat
      ))

    val temperatures: RDD[String] = spark.sparkContext.parallelize(getLines(temperaturesFile))
    val temperatureRDD: RDD[TemperatureSchema] = temperatures.map(_.split(",", -1))
      .map(row => TemperatureSchema(
        toOption(row(0)).map(_.toInt), // stnId
        toOption(row(1)).map(_.toInt), // wbanId
        row(2).toInt, // month
        row(3).toInt, // day
        toOption(row(4)).map(_.toDouble) // temperature
      ))

    val validStationRDD = stationRDD
      .filter(station => station.latitude.isDefined && station.longitude.isDefined)
      .map(station => (
        (station.stnId, station.wbanId), Location(station.latitude.get.toDouble, station.longitude.get.toDouble)
      ))

    val validTemperatureRDD = temperatureRDD
      .filter(temp => temp.temperature.isDefined && !temp.temperature.contains(9999.9))
      .map(temp => temp.copy(temperature = temp.temperature.map(toCelsius)))
      .map(temp => (
        (temp.stnId, temp.wbanId),
        (LocalDate.of(year, temp.month, temp.day), temp.temperature.get))
      )

    validStationRDD.join(validTemperatureRDD).mapValues {
      case (location: Location, (localDate: LocalDate, temperature: Temperature)) => (localDate, location, temperature)
    }.values.collect()

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
    records.groupBy(_._2).view.mapValues(rowsPerLocation => rowsPerLocation.map(_._3).sum / rowsPerLocation.size).toSeq

  private def getLines(resource: String): List[String] =
    Source.fromInputStream(getClass.getResourceAsStream(resource), "utf-8").getLines().toList

  private def toOption(string: String): Option[String] =
    Option(string).filter(_.trim.nonEmpty)
