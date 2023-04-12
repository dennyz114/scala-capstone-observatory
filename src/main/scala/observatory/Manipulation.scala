package observatory

import scala.collection.parallel.CollectionConverters.given
import scala.collection.mutable.{Map => MMap}

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface:

  def memoizeFnc[K, V](dict: MMap[K, V])(f: K => V): K => V = {
    k =>
      dict.getOrElse(k, {
        dict.update(k, f(k))
        dict(k)
      })
  }
  val GRID_MAP: MMap[Int, MMap[GridLocation, Temperature]] = MMap.empty
  
  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature =
    val hash = temperatures.hashCode()
    if !GRID_MAP.contains(hash) then
      GRID_MAP.update(hash, MMap.empty)
    
    memoizeFnc(GRID_MAP(hash))(
      (gridLoc: GridLocation) => Visualization.predictTemperature(temperatures, Location(gridLoc.lat, gridLoc.lon))
    )

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature =
    (gridLoc: GridLocation) => {
      val a = temperaturess.map(avgs => makeGrid(avgs)(gridLoc)).toList
      a.sum / a.length
    }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature =
    (gridLoc: GridLocation) => {
      makeGrid(temperatures)(gridLoc) - normals(gridLoc)
    }



