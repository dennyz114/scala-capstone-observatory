package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given

import scala.math.{Pi, abs, asin, cos, pow, sin, sqrt, toRadians}
import scala.collection.parallel.CollectionConverters.given

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  val height = 180
  val width = 360
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
    val disTemp: Iterable[(Double, Temperature)] = temperatures.map{case (loc, temp) => (getDistance(loc, location), temp)}
    val closest: (Double, Temperature) = disTemp.reduce((first, second) => if first._1 < second._1 then first else second)
    val p = 2

    if(closest._1 < 1)
      closest._2
    else
      // interpolate
      val weightsAndTemp = disTemp.map(entry => (1 / pow(entry._1, p), entry._2))
      weightsAndTemp.map(entry => entry._2 * entry._1).sum / weightsAndTemp.map(_._1).sum

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    val sameTemperatureAndColor = points.find(_._1 == value)
    sameTemperatureAndColor match {
      case Some(tempAndColor) => tempAndColor._2
      case _ =>
        val (smallers, biggers) = points.partition(_._1 < value)
        if (smallers.isEmpty)
          biggers.minBy(_._1)._2
        else if (biggers.isEmpty)
          smallers.maxBy(_._1)._2
        else
          val closestSmaller = smallers.maxBy(_._1)
          val closestBigger = biggers.minBy(_._1)
          val ws = 1 / abs(closestSmaller._1 - value)
          val wb = 1 / abs(closestBigger._1 - value)
          def interpolate(x: Int, y: Int): Int = ((x*ws + y*wb) / (ws + wb)).round.toInt

          val colorX = closestSmaller._2
          val colorY = closestBigger._2
          Color(interpolate(colorX.red, colorY.red), interpolate(colorX.green, colorY.green), interpolate(colorX.blue, colorY.blue))
    }


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): ImmutableImage =
    val posAndLocations = for {
      x <- 0 until height
      y <- 0 until width
    } yield (x, y, toLocation(x, y))
    val pixels = posAndLocations
      .map(v => (v._1, v._2, predictTemperature(temperatures, v._3)))
      .map(v => (v._1, v._2, interpolateColor(colors, v._3)))
      .map(v => Pixel(v._1, v._2, v._3.red, v._3.green, v._3.blue, 255))
    ImmutableImage.wrapPixels(width, height, pixels.toArray, ImageMetadata.empty)

  def getDistance(a: Location, b: Location): Double =
    // inverse distance weighting
    def areAntipodes(a: Location, b: Location): Boolean =
      (a.lat == -b.lat) && (abs(a.lon - b.lon) == 180)

    if (a == b)
      0
    else if (areAntipodes(a, b))
      earthRadius * Pi
    else
      val deltaLon = toRadians(abs(a.lon - b.lon))
      val aLat = toRadians(a.lat)
      val bLat = toRadians(b.lat)
      val deltaTeta = abs(aLat - bLat)
      val deltaDist = 2 * asin(sqrt( pow(sin(deltaTeta/2), 2) + cos(aLat) * cos(bLat) * pow(sin(deltaLon/2), 2) ))
      earthRadius * deltaDist

  def toLocation(x: Int, y: Int): Location =
    val lon = (y - width / 2) * (360 / width)
    val lat = -(x - height / 2) * (180 / height)
    Location(lat, lon)
