import java.net.{URI, InetSocketAddress}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import scala.io.Source
import scala.annotation.tailrec
import scala.util.Try

class FactorialHandler extends HttpHandler {

  override def handle(httpExchange: HttpExchange): Unit = {
    System.out.print("Requesting factorial with params: ")
    val params = Util.parseParams(httpExchange.getRequestURI)
    System.out.println(params)
    val response = params.get("n") match {
      case Some(x) => Try(Util.factorial(BigInt(x)).toString()).getOrElse("Enter a number")
      case _ => "Enter a number"
    }
    httpExchange.sendResponseHeaders(200, response.getBytes().length)
    val os = httpExchange.getResponseBody
    os.write(response.getBytes)
    os.close()
    System.out.println("Factorial response: " + response)
  }

}

class IndexPageHandler extends HttpHandler {

  override def handle(httpExchange: HttpExchange): Unit = {
    System.out.println("Requesting index page")
    val os = httpExchange.getResponseBody
    val filename = "index.html"
    val file = Source.fromFile(filename).mkString
    httpExchange.sendResponseHeaders(200, file.getBytes().length)
    os.write(file.getBytes())
    os.close()
    System.out.println("Index page response: \n" + file)
  }

}

object Server extends App{
  System.out.println("Starting server...")
  val server = HttpServer.create(new InetSocketAddress(8089), 0)
  server.createContext("/", new IndexPageHandler)
  server.createContext("/calc", new FactorialHandler)
  server.setExecutor(null)
  server.start()
  System.out.println("Server started")
}

object Util{
  def parseParams(uri: URI): Map[String, String] ={
    Option(uri.getQuery).getOrElse("").split("&").collect{
      case str:String if str.contains("=") => {
        val tmp = str.split("=")
        (tmp(0), tmp(1))
      }
    }.toMap
  }


  def factorial(x:BigInt): BigInt ={
    val one = BigInt(1)
    @tailrec
    def accumulatedFactorial(x:BigInt, acc: BigInt): BigInt = {
      if (x == one)
        acc
      else
        accumulatedFactorial(x - one, acc * x)
    }
    accumulatedFactorial(x, one)
  }

}