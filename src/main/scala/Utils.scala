import scala.util.{Failure, Success, Try}

object Utils {
  import com.github.nscala_time.time.Imports._
  def toTry[A](either : Either[io.circe.Error, A]) : Try[A] =
    either match {
      case Right(categories) => Success(categories)
      case Left(error) => Failure(new RuntimeException (error.getMessage()))
    }
  def parseTime(str: String): (Int, Int) = {
    val time = DateTimeFormat.forPattern("HH:mm").parseDateTime(str)
    (time.getHourOfDay, time.getMinuteOfHour)
  }
}