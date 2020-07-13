import API._
import Config._
import io.circe.syntax._

import io.circe.generic.auto._

import scala.util.{Failure, Success, Try}

object Interface {
  val api = API
  def printTryList[A](list : List[A]) : Try[List[A]] = {
    println()
    for(elem <- list) {
      println(elem)
    }
    Try(list)
  }
  def printTryCities(list : List[City]) : Try[List[City]] = {
    println()
    for(city <- list) {
      val slug = city.slug
      val name = city.name
      println(s"\t- $name - $slug ")
    }
    Try(list)
  }

  def printTryCategories(list : List[Category]) : Try[List[Category]] = {
    println()
    for(cat <- list) {
      val slug = cat.slug
      val name = cat.name
      println(s"\t- $name - $slug ")
    }
    Try(list)
  }

  val cnf = Config

  def startPrompt(): Unit = {
    var configData = ConfigData("",List(),List(),List(),List(),List(),List(),List(),List(),1)
    while(true) {
      val cmd = scala.io.StdIn.readLine(">> ")
      if (cmd == ":q") {
        return ()
      }
      val arrayArgs = cmd.split(" ")

      val args = if (arrayArgs.length > 1) {
        Array(arrayArgs(0), arrayArgs(1), arrayArgs.slice(2, arrayArgs.length).mkString(""))
      } else {
        arrayArgs
      }
      args(0) match {
        case ":q"           => return ()
        case "find"         => println(events(configDataToInternal(configData)))
        case "printConfig"  => println(showConfigData(configData))
        case "exportConfig" => cnf.writeFile(configData.asJson.toString)
        case "importConfig" => {
          cnf.config match {
            case Success(v) => {configData = v; println(showConfigData(configData))}
            case Failure(e) => println("Error when trying to read config file")
          }
        }
        case "categories"  => api.categories.flatMap(printTryCategories)
        case "cities"      => api.cities.flatMap(printTryCities)
        case "set"         => {
          val res = cnf.interactiveSet(configData, args(1), args(2))
          configData = res._1
          if (res._2 == "") {
            println(showConfigData(configData))
          } else {
            println(res._2)
          }

        }
        case "help"       => {
          println("find                - try to find events with configurated before parameters in config")
          println("printConfig         - print current state of configuration")
          println("importConfig        - load config from config.txt file")
          println("exportConfig        - save config to config.txt file")
          println("categories          - to get list of categories")
          println("cities              - to get list of cities")
          println("set <field> <value> - set value for config\n  Examples:\n    set mon [[\"12:00\", \"14:00\"]]\n    set city msk\n    set categories [\"theater\"]\n    set days 5")
          println("help                - to get this message")
          println(":q                  - to exit")
        }
        case _            => println("Unknown command")
      }
    }

  }
}
