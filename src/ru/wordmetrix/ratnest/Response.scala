package ru.wordmetrix.ratnest

abstract trait Status {
    val status = 200
    val message = "Ok"
    def headerStatus = {
        "HTTP/1.1 %d %s\n" format (status, message)
    }
}

trait StatusSuccess extends Status

trait StatusAbort extends Status {
    override val status = 500
    override val message = "Server Error"
}

trait StatusNotFound extends Status {
    override val status = 404
    override val message = "Not Found"
}

abstract class Response(val cookie: Map[String, String], val body: String) {

    val date = new java.text.SimpleDateFormat("E, dd MMM yyyy hh:mm:ss z", new java.util.Locale("C")).format(new java.util.Date())
    val status: Int
    def headerStatus: String

    /*def this(body : String) = {
     this(Map(), body)
   }*/

    var cookieHeader = {
        "Set-Cookie: " + (cookie map { case (x, y) => x + "=" + y } mkString (", ")) + ";  Path=/\n\n"
    }

    override def toString() = {
        headerStatus +
            "Server: RatNest v.NaN (NaN Also Number)\n" +
            "Date: " + date + "\n" +
            "Content-Type: text/html; charset=UTF-8\n" +
            "Connection: close\n" +
            "Cache-Control: no-cache,no-store,max-age=0,must-revalidate\n" +
            //"Content-Length: 108703\n" +
            "Expires: " + date + "\n" +
            "Last-Modified: " + date + "\n" +
            { if (0 == cookie.size) "" else "Set-Cookie: " + (cookie map { case (x, y) => x + "=" + y } mkString (", ")) + ";  Path=/\n" } +
            "\n" +
            body

    }
}

class ResponseSuccess(cookie: Map[String, String], body: String) extends Response(cookie, body) with StatusSuccess {
    def +(x: Tuple2[String, String]) = new ResponseSuccess(cookie + x, body)
}

class ResponseAbort(cookie: Map[String, String], body: String) extends Response(cookie, body) with StatusAbort {
    def this(cookie: Map[String, String]) {
        this(cookie, "<html><body>Error 500</body></html>")
    }
    def +(x: Tuple2[String, String]) = new ResponseAbort(cookie + x, body)
}

class ResponseNotFound(val cookie: Map[String, String], val body: String) extends StatusNotFound {
    def this(cookie: Map[String, String]) {
        this(cookie, "<html><body>NotFound</body></html>")
    }
    def +(x: Tuple2[String, String]) = new ResponseNotFound(cookie + x, body)
}
