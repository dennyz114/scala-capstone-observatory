package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import scala.collection.parallel.CollectionConverters.given

import observatory.Visualization.{interpolateColor, predictTemperature}
/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface:

  val initialZoom = 8
  val width = 256
  val heihgt = 256

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = tile.toLocation
  
  def getTilesWithLocation(tile: Tile): Seq[(Int, Int, Location)] =
    val xOffset = tile.x * width
    val yOffset = tile.y * heihgt

    for {
      x <- 0 until width
      y <- 0 until heihgt
    } yield (x + xOffset, y + yOffset, Tile(
      x + xOffset,
      y + yOffset,
      initialZoom + tile.zoom
    ).toLocation)
    
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): ImmutableImage =
    val allLocations = getTilesWithLocation(tile)
    val allColors: Seq[(Int, Int, Color)] = allLocations
      .map(loc => (loc._1, loc._2, loc._3, predictTemperature(temperatures, loc._3)))
      .map(tp => (tp._1, tp._2, tp._3, interpolateColor(colors, tp._4)))
      .sortBy(-_._3.lat)
      .map(v => (v._1, v._2, v._4))

    ImmutableImage.wrapPixels(
      width,
      heihgt,
      allColors.map(v => Pixel(v._1, v._2, v._3.red, v._3.green, v._3.blue, 127)).toArray,
      ImageMetadata.empty
    )

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit =
    val tiles = for {
      zoom <- 0 until 4
      x <- 0 until (math.pow(2, zoom)).toInt
      y <- 0 until (math.pow(2, zoom)).toInt
      data <- yearlyData
    } yield generateImage(data._1, Tile(x, y, zoom), data._2)