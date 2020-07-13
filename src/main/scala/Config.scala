import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.decode
import scalaz.Scalaz._
import com.github.nscala_time.time.Imports._

import scala.util.{Try, Success, Failure}

object Config {
  var now = DateTime.now()
  import Utils._

  import scala.io.Source._

  val filename = "config.txt"
  def writeFile(s: String, filename: String = filename): Unit = {
    import java.io._
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(s)
    bw.close()
  }


  case class ConfigData(
                         city:     String,
                         categories: List[String],
                         mon:      List[List[String]],
                         tue:      List[List[String]],
                         wed:      List[List[String]],
                         thu:      List[List[String]],
                         fri:      List[List[String]],
                         sat:      List[List[String]],
                         sun:      List[List[String]],
                         days:     Int)

  case class ConfigInternal (
                              city: String,
                              categories: List[String],
                              dates: List[(DateTime, DateTime)]
                            )

  def showConfigData(configData: ConfigData): String = {
    val city = configData.city
    val cat = configData.categories
    val mon = configData.mon
    val tue = configData.tue
    val wed = configData.wed
    val thu = configData.thu
    val fri = configData.fri
    val sat = configData.sat
    val sun = configData.sun
    val days = configData.days
    val outputList = List(
      "\n City       = " ++ city.toString,
      "\n Categories = " ++ (cat mkString ", "),
      "\n Monday     = " ++ (mon.map(_ mkString " - ") mkString "; " ),
      "\n Tuesday    = " ++ (tue.map(_ mkString " - ") mkString "; " ),
      "\n Wednesday  = " ++ (wed.map(_ mkString " - ") mkString "; " ),
      "\n Thursday   = " ++ (thu.map(_ mkString " - ") mkString "; " ),
      "\n Friday     = " ++ (fri.map(_ mkString " - ") mkString "; " ),
      "\n Saturday   = " ++ (sat.map(_ mkString " - ") mkString "; " ),
      "\n Sunday     = " ++ (sun.map(_ mkString " - ") mkString "; " ),
      "\n Days       = " ++ days.toString)
    outputList mkString ""
  }


  def config(): Try[ConfigData] = {
    val config = fromFile(filename) mkString ""
    implicit val decoder: Decoder[ConfigData] = deriveDecoder
    decode[ConfigData](config) |> toTry

  }
  def getDayTimetable(week_num: Int, config: ConfigData): List[List[String]] = {
    week_num match {
      case 1 => config.mon
      case 2 => config.tue
      case 3 => config.wed
      case 4 => config.thu
      case 5 => config.fri
      case 6 => config.sat
      case 7 => config.sun
    }
  }
  def updateTime(time: List[String], dateTime: DateTime): (DateTime, DateTime) = {
    time match {
      case start :: end :: Nil => {
        val (h1, m1) = parseTime(start)
        val (h2, m2) = parseTime(end)
        val dateTime1 = dateTime.hour(h1).minute(m1).withZone(DateTimeZone.UTC)
        val dateTime2 = dateTime.hour(h2).minute(m2).withZone(DateTimeZone.UTC)
        (dateTime1, dateTime2)
      }
    }
  }
  def timeConvert(config: ConfigData): List[(DateTime, DateTime)] = {
    var startDate = now.second(0)
    val periods =
      for (i <- 1 to config.days) yield {
        val dayOfWeek: Int = startDate.getDayOfWeek
        val lstPeriods = getDayTimetable(dayOfWeek, config)
        val list = lstPeriods.map(updateTime(_, startDate))
        startDate = startDate.plusDays(1)
        list
      }
    List(periods).flatten.flatten
  }
  def configDataToInternal(configData: ConfigData): ConfigInternal = {
    val city = configData.city
    val categories = configData.categories
    val dates = timeConvert(configData)
    ConfigInternal(city, categories, dates)
  }
  def dayChange(configData: ConfigData, day: String, value: List[List[String]]) : ConfigData = {
    day match {
      case "mon" => configData.copy(mon = value)
      case "tue" => configData.copy(tue = value)
      case "wed" => configData.copy(wed = value)
      case "thu" => configData.copy(thu = value)
      case "fri" => configData.copy(fri = value)
      case "sat" => configData.copy(sat = value)
      case "sun" => configData.copy(sun = value)
    }
  }
  def interactiveSet(configData : ConfigData, field: String, value: String) : Tuple2[ConfigData, String] = {
    val api = API
    field match {
      case "city" =>
        val cities = api.cities() match {
          case Success(v) => v.map(_.slug)
          case Failure(e) => return (configData, "Error when trying access city from API")
        }
        if (cities.contains(value)) {
          return (configData.copy(city = value), "")
        } else {
          return (configData, value ++ " is not in the list of cities")
        }
      case "days" =>
        Try(value.toInt) match {
          case Success(v) => {
            if (v > 0) {
              return (configData.copy(days = v), "")
            } else {
              return (configData, "Error, days is lower than 0 or equal 0")
            }
          }
        }
      case "categories" =>
        val cat = decode[List[String]](value) |> toTry

        cat match {
          case Failure(e) => return (configData, "Error when trying to decode categories")
          case Success(catV) => {
            val categories = api.categories match {
              case Success(v) => v.map(_.slug)
              case Failure(e) => return (configData, "Error when trying access categories from API")
            }
            if (catV.forall(categories.contains(_))) {
              return (configData.copy(categories = catV), "")
            } else {
              return (configData, catV.toString ++ " is not in the list of categories")
            }
          }
        }
      case dayOfWeek if List("mon", "tue", "wed", "thu", "fri", "sat", "sun").exists(_ == dayOfWeek) =>
        val time = decode[List[List[String]]](value) |> toTry
        time match {
          case Success(v) => {
            if (v.forall(_.length == 2)) {
              return (dayChange(configData, dayOfWeek, v), "")
            }
            else {
              return (configData, "Error, you input not 2 time fields")
            }
          }
          case Failure(e) => return (configData, "Error when trying to decode time")
        }

        return (configData, "")
    }

  }

}
