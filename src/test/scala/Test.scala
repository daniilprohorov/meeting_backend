import org.joda.time.DateTime
import org.scalatest._
import com.github.nscala_time.time.Imports._

import scala.util.{Success, Try}
class Test extends FlatSpec with Matchers {
  "Config filename" should "be named config.txt" in {
    Config.filename shouldEqual "config.txt"
  }

  "parseTime" should "return Tuple of hours and minutes" in {
    Utils.parseTime("11:12") shouldEqual (11, 12)
    Utils.parseTime("0:12") shouldEqual (0, 12)
    Utils.parseTime("0:0") shouldEqual (0, 0)
    intercept[java.lang.IllegalArgumentException] {
      Utils.parseTime("-1:0")
    }
    intercept[java.lang.IllegalArgumentException] {
      Utils.parseTime("-1:-1")
    }
    intercept[java.lang.IllegalArgumentException] {
      Utils.parseTime("25:1")
    }
    intercept[java.lang.IllegalArgumentException] {
      Utils.parseTime("12:60")
    }
    intercept[java.lang.IllegalArgumentException] {
      Utils.parseTime("12:61")
    }
  }
  "interactive set" should "correct set cities" in {
    import Config._
    val configData = ConfigData("", List(), List(), List(), List(), List(), List(), List(), List(), 1)
    interactiveSet(configData, "city", "spb") shouldEqual (configData.copy(city = "spb"), "")
    interactiveSet(configData, "city", "ekb") shouldEqual (configData.copy(city = "ekb"), "")
    interactiveSet(configData, "city", "krasnoyarsk") shouldEqual (configData.copy(city = "krasnoyarsk"), "")
    interactiveSet(configData, "city", "krd") shouldEqual (configData.copy(city = "krd"), "")
    interactiveSet(configData, "city", "kzn") shouldEqual (configData.copy(city = "kzn"), "")
    interactiveSet(configData, "city", "msk") shouldEqual (configData.copy(city = "msk"), "")
    interactiveSet(configData, "city", "nnv") shouldEqual (configData.copy(city = "nnv"), "")
    interactiveSet(configData, "city", "nsk") shouldEqual (configData.copy(city = "nsk"), "")
    interactiveSet(configData, "city", "online") shouldEqual (configData.copy(city = "online"), "")
    interactiveSet(configData, "city", "sochi") shouldEqual (configData.copy(city = "sochi"), "")
    interactiveSet(configData, "city", "lol") shouldEqual (configData, "lol is not in the list of cities")
  }
  "interactive set" should "correct set categories" in {
    import Config._
    val configData = ConfigData("", List(), List(), List(), List(), List(), List(), List(), List(), 1)
    interactiveSet(configData, "categories", "[]") shouldEqual (configData.copy(categories = List()), "")
    interactiveSet(configData, "categories", "[\"theater\"]") shouldEqual (configData.copy(categories = List("theater")), "")
    interactiveSet(configData, "categories", "[\"theater\", \"concert\"]") shouldEqual (configData.copy(categories = List("theater", "concert")), "")
    val resultList = List("theater", "concert", "education", "party", "exhibition", "tour", "festival", "cinema", "fashion", "holiday", "social-activity", "yarmarki-razvlecheniya-yarmarki", "stock", "shopping", "quest", "kids", "other", "photo", "business-events", "recreation", "entertainment")
    val inputList : String = "[" ++ (resultList.map("\"" ++ _ ++ "\"") mkString ", ") ++ "]"
    print(inputList)
    interactiveSet(configData, "categories", inputList) shouldEqual (configData.copy(categories = resultList), "")
    interactiveSet(configData, "categories", "[\"lol\"]") shouldEqual (configData, "List(lol) is not in the list of categories")
  }

  "interactive set" should "correct set days count" in {
    import Config._
    val configData = ConfigData("", List(), List(), List(), List(), List(), List(), List(), List(), 1)
    interactiveSet(configData, "days", "10") shouldEqual (configData.copy(days = 10), "")
    interactiveSet(configData, "days", "80") shouldEqual (configData.copy(days = 80), "")
    interactiveSet(configData, "days", "1") shouldEqual (configData.copy(days = 1), "")
    interactiveSet(configData, "days", "-1") shouldEqual (configData, "Error, days is lower than 0 or equal 0")
    interactiveSet(configData, "days", "0") shouldEqual (configData, "Error, days is lower than 0 or equal 0")
  }
  "interactive set" should "correct set days" in {
    import Config._

    var startDate = DateTime.now().second(0)
    val configData = ConfigData("", List(), List(), List(), List(), List(), List(), List(), List(), 1)
    interactiveSet(configData, "mon", "[]") shouldEqual (configData.copy(mon = List()), "")
    interactiveSet(configData, "mon", "[\"asdf\"]") shouldEqual (configData, "Error when trying to decode time")
    interactiveSet(configData, "mon", "[[\"10:00\", \"12:00\"], [\"14:00\", \"18:00\"]]") shouldEqual (configData.copy(mon = List(List("10:00", "12:00"), List("14:00", "18:00"))), "")

    interactiveSet(configData, "tue", "[]") shouldEqual (configData.copy(tue = List()), "")
    interactiveSet(configData, "tue", "[\"asdf\"]") shouldEqual (configData, "Error when trying to decode time")
    interactiveSet(configData, "tue", "[[\"10:00\", \"12:00\"], [\"14:00\", \"18:00\"]]") shouldEqual (configData.copy(tue = List(List("10:00", "12:00"), List("14:00", "18:00"))), "")

    interactiveSet(configData, "wed", "[]") shouldEqual (configData.copy(wed = List()), "")
    interactiveSet(configData, "wed", "[\"asdf\"]") shouldEqual (configData, "Error when trying to decode time")
    interactiveSet(configData, "wed", "[[\"10:00\", \"12:00\"], [\"14:00\", \"18:00\"]]") shouldEqual (configData.copy(wed = List(List("10:00", "12:00"), List("14:00", "18:00"))), "")

    interactiveSet(configData, "thu", "[]") shouldEqual (configData.copy(thu = List()), "")
    interactiveSet(configData, "thu", "[\"asdf\"]") shouldEqual (configData, "Error when trying to decode time")
    interactiveSet(configData, "thu", "[[\"10:00\", \"12:00\"], [\"14:00\", \"18:00\"]]") shouldEqual (configData.copy(thu = List(List("10:00", "12:00"), List("14:00", "18:00"))), "")

    interactiveSet(configData, "fri", "[]") shouldEqual (configData.copy(fri = List()), "")
    interactiveSet(configData, "fri", "[\"asdf\"]") shouldEqual (configData, "Error when trying to decode time")
    interactiveSet(configData, "fri", "[[\"10:00\", \"12:00\"], [\"14:00\", \"18:00\"]]") shouldEqual (configData.copy(fri = List(List("10:00", "12:00"), List("14:00", "18:00"))), "")

    interactiveSet(configData, "sat", "[]") shouldEqual (configData.copy(sat = List()), "")
    interactiveSet(configData, "sat", "[\"asdf\"]") shouldEqual (configData, "Error when trying to decode time")
    interactiveSet(configData, "sat", "[[\"10:00\", \"12:00\"], [\"14:00\", \"18:00\"]]") shouldEqual (configData.copy(sat = List(List("10:00", "12:00"), List("14:00", "18:00"))), "")

    interactiveSet(configData, "sun", "[]") shouldEqual (configData.copy(sun = List()), "")
    interactiveSet(configData, "sun", "[\"asdf\"]") shouldEqual (configData, "Error when trying to decode time")
    interactiveSet(configData, "sun", "[[\"10:00\", \"12:00\"], [\"14:00\", \"18:00\"]]") shouldEqual (configData.copy(sun = List(List("10:00", "12:00"), List("14:00", "18:00"))), "")
  }

  "API categories" should "be equal" in {
    import API._
    val api = API
    val categoriesRes = Success(List(Category(1,"concert","Концерты"), Category(2,"theater","Спектакли"), Category(3,"education","Обучение"), Category(4,"party","Вечеринки"), Category(6,"exhibition","Выставки"), Category(7,"tour","Экскурсии"), Category(8,"festival","Фестивали"), Category(9,"cinema","Кино (Развлечения)"), Category(10,"fashion","Мода и стиль"), Category(12,"holiday","Праздники"), Category(13,"social-activity","Благотворительность"), Category(14,"yarmarki-razvlecheniya-yarmarki","Ярмарки (Развлечения, Ярмарки)"), Category(25,"stock","Акции и скидки"), Category(27,"shopping","Шопинг (Магазины)"), Category(28,"quest","Квесты"), Category(36,"kids","Детям"), Category(39,"other","Разное"), Category(40,"photo","Фотография"), Category(45,"business-events","События для бизнеса"), Category(46,"recreation","Отдых"), Category(47,"entertainment","Развлечения")))
    api.categories() shouldEqual categoriesRes
  }
  "API cities" should "be equal" in {
    import API._
    val api = API
    val citiesRes = Success(List(City("ekb","Екатеринбург"), City("krasnoyarsk","Красноярск"), City("krd","Краснодар"), City("kzn","Казань"), City("msk","Москва"), City("nnv","Нижний Новгород"), City("nsk","Новосибирск"), City("online","Онлайн"), City("sochi","Сочи"), City("spb","Санкт-Петербург")))
    api.cities() shouldEqual citiesRes
  }

  "Events" should "work" in {
    import Config._
    import API._
    val cnf = Config
    val nowOverride = DateTime.now().hour(2).minute(45).second(10).withDate(2020, 10, 1)
    cnf.now = nowOverride
    val config = cnf.config()
    var configData = ConfigData("spb",List("quest"),List(List("12:00", "14:00")),List(),List(),List(),List(),List(),List(),5)
    val configInternal = configDataToInternal(configData)
    events(configInternal) shouldEqual "\n\nStart: Mon 12:00 05-10-2020; End: Mon 14:00 05-10-2020; \n    - 187583 - Онлайн-квест «Москва в советских комедиях»"
  }

  // Interface testing

  "printTryList" should "print and return Try[List[Smth]]" in {
    import Interface._
    val list = List(1, 2, 3)
    val tryList = Try(list)
    printTryList(list) shouldEqual tryList
  }

  "printTryCities" should "print and return Try[List[City]]" in {
    import Interface._
    import API.City
    val list = List(City(slug = "spb", name = "spb"), City(slug = "msk", name = "msk"))
    val tryList = Try(list)
    printTryCities(list) shouldEqual tryList
  }

  "printTryCategories" should "print and return Try[List[Category]]" in {
    import Interface._
    import API.Category
    val list = List(Category(id = 0, slug = "theater", name = "theater"), Category(id = 1, slug = "quest", name = "quest"))
    val tryList = Try(list)
    printTryCategories(list) shouldEqual tryList
  }

  "startPrompt" should "exit" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = ":q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include (">> "))
  }

  "startPrompt" should "printConfig" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "printConfig\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include (">> ") and include ("City") and include ("Categories") and include ("Monday"))
  }

  "startPrompt" should "importConfig" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "importConfig\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include (">> ") and include ("City") and include ("Categories") and include ("Monday"))
  }

  "startPrompt" should "setDayOfWeekTimetable" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "set mon [[\"12:00\", \"14:00\"]]\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include (">> ") and include ("City") and include ("12:00") and include ("14:00"))
  }

  "startPrompt" should "help" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "help\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include (">> ") and include ("help") and include ("exit") and include ("set"))
  }

  "startPrompt" should "unknown command" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "asdfasdfasf\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include (">> ") and include ("Unknown"))
  }

  "startPrompt" should "error set mon with one time field" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "set mon [[\"12:00\"]]\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include ("Error"))
  }

  "startPrompt" should "error set day timetable" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "set mon asdffasdf\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include ("Error"))
  }

  "startPrompt" should "error set city" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "set city asdffasdf\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include ("is not in the list of cities"))
  }

  "startPrompt" should "error set categories" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "set categories [\"asbsdfasdf\"]\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include ("is not in the list of categories"))
  }

  "startPrompt" should "export config is ok" in {
    import Interface._
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = "exportConfig\n:q"
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        startPrompt()
      }
    }
    out.toString should (include (">>"))
  }

  "main" should "work" in {
    import Interface._
    import Main._
    val main = Main
    import java.io.{ByteArrayOutputStream, StringReader}
    val inputStr = ""
    val in = new StringReader(inputStr)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        main
      }
    }
    out.toString should (include(""))
  }


    "CityClass" should "getName" in {
      import API.City
      val city = City(name = "msk", slug = "msk")
      city.name shouldEqual "msk"
    }

    "CityClass" should "getSlug" in {
      import API.City
      val city = City(name = "msk", slug = "msk")
      city.slug shouldEqual "msk"
    }
}
