package observatory

import java.time.LocalDate

object Helpers {

  def toCelsius(fahrenheit: Double): Double =
    (fahrenheit - 32) * 5/9
  def toLocalDate(year: Year, month: Int, day: Int): LocalDate =
    LocalDate.of(year, month, day)

}
