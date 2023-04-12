package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface:

  val width = 256
  val height = 256
  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature =
    val x = point.x
    val y = point.y
    val x1 = 1 - point.x
    val y1 = 1 - point.y
    d00 * x1 * y1 + d10 * x * y1 + d01 * x1 * y + d11 * x * y

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): ImmutableImage =
    val locations = Interaction.getTilesWithLocation(tile)
    val pointsAndColors = locations
      .map(v => {
        val loc = v._3
        val lat = loc.lat.toInt
        val lon = loc.lon.toInt
        (v._1, v._2, bilinearInterpolation(
          CellPoint(loc.lon - lon, loc.lat - lat),
          grid(GridLocation(lat, lon)),
          grid(GridLocation(lat + 1, lon)),
          grid(GridLocation(lat, lon + 1)),
          grid(GridLocation(lat + 1, lon + 1))
        ))
      }).map(v => (v._1, v._2, Visualization.interpolateColor(colors, v._3)))
      .toArray

    ImmutableImage.wrapPixels(width, height, pointsAndColors.map(x => Pixel(x._1, x._2, x._3.red, x._3.green, x._3.blue, 255)), ImageMetadata.empty)
