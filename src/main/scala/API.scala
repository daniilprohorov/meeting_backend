import Config._
import com.github.nscala_time.time.Imports._
import scalaj.http._
import scala.util.{Failure, Success, Try}
import Utils.toTry
import io.circe.parser.decode
import io.circe.{Decoder, Json}
import scalaz.Scalaz._
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.auto._

object API {
  case class Category(id: Int, slug: String, name: String)
  type Categories = List[Category]
  def categories() : Try[Categories] = {
    val jsonCategories =
      Http("https://kudago.com/public-api/v1.4/event-categories/")
        .param("order_by", "id")
        .asString
    val str : String = jsonCategories.body
    decode[Categories](str) |> toTry
  }

  case class City(slug: String, name: String)
  type Cities = List[City]

  def cities() : Try[Cities] = {
    val jsonCategories =
      Http("https://kudago.com/public-api/v1.4/locations/?lang=ru")
        .asString
    val str : String = jsonCategories.body
    implicit val decoder: Decoder[City] = deriveDecoder
    decode[Cities](str) |> toTry
  }

  case class Event(id: Int, title: String, slug: String)
  case class EventsInternal(count: Int, next: Option[String], previous: Option[String], results: List[Event])
  def events(config: ConfigInternal): String = {
    val city = config.city
    val categories = config.categories
    val dates = config.dates
    val events =
      for((start, end) <- dates)
        yield {
          val str = Http("https://kudago.com/public-api/v1.4/events/")
            .param("page_size", "100")
            .param("location", city)
            .param("actual_since", start.toString)
            .param("actual_until", end.toString)
            .param("categories", categories mkString ",")
            .asString
            .body
          var eventsInternal = decode[EventsInternal](str) |> toTry
          if (eventsInternal.isSuccess) {
            var result = List(eventsInternal.get.results)
            while(eventsInternal.get.next != None) {
              eventsInternal = decode[EventsInternal](Http(eventsInternal.get.next.get).asString.body) |> toTry
              if (eventsInternal.isSuccess) {
                result = eventsInternal.get.results :: result
              }
              else {
                return eventsInternal match {
                  case Failure(e) => e.toString
                }
              }
            }
            result.flatten
          }
          else {
            eventsInternal match {
              case Failure(e) => e.toString
            }
          }
        }

    val output =
      for(((start, end), eventsList: List[Event]) <- dates.zip(events)) yield {
        val startL = start.withZone(DateTimeZone.forOffsetHours(3))
        val endL = end.withZone(DateTimeZone.forOffsetHours(3))
        val startS = startL.toString("EEE HH:mm dd-MM-YYYY")
        val endS = endL.toString("EEE HH:mm dd-MM-YYYY")
        val startEnd = s"\n\nStart: $startS; End: $endS; \n    - "
        val eventsNames = eventsList.map(x => {val id = x.id; val title = x.title; s"$id - $title"}) mkString "\n    - "
        startEnd ++ eventsNames
      }
    output mkString ""
  }

}
