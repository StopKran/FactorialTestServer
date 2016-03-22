import java.net.{URI, InetSocketAddress}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import scala.annotation.tailrec

class FactorialHandler extends HttpHandler {

  override def handle(httpExchange: HttpExchange): Unit = {
    val params = Util.parseParams(httpExchange.getRequestURI)
    val response = if (params.contains("n")){
      Util.factorial(BigInt(params("n"))).toString()
    }else{
      "Abacabadabacaba"
    }

    httpExchange.sendResponseHeaders(200, response.length())
    val os = httpExchange.getResponseBody
    os.write(response.getBytes)
    os.close()
  }

}

object Server extends App{
  val server = HttpServer.create(new InetSocketAddress(8089), 0)
  server.createContext("/", new FactorialHandler)
  server.setExecutor(null); // creates a default executor
  server.start()
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