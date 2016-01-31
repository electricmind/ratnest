package ru.wordmetrix.ratnest

case class Request(method: String, path: String, query: Map[String, String], cookie: Map[String, String], headers: Map[String, String])

object Request {
    def apply(socket: java.io.InputStream) = {

        val rMethod = "(GET|POST)\\s+/(\\S+).*".r

        val rHead = "(.*):\\s+(.*)".r

        val rUrl1 = "([^? ]*)?(.*)".r

        val rUrl2 = "([^? ]*)".r

        val rEqual = "(.*)=(.*)".r

        val stream = io.Source.fromInputStream(socket).getLines

        val (method, path, querystring) = stream.next match {
            case rMethod(method, url) => {
                url match {
                    case rUrl1(path, query) => (method, path, query)
                    //case rUrl2(path) => (method, path, "")
                    case url                => (method, url, "")
                }
            }
            case _ => throw new Exception("Invalid Method Line")
        }

        val headers = (
            for {
                s <- stream.takeWhile(_.length > 0)
                pair <- s match {
                    case rHead(k, v) => { /* println(k + ": " + v ); */ Some(k -> (v.trim)); }
                    case _           => None
                }
            } yield (pair)).toMap

        lazy val query = (method match {
            case "GET"  => querystring split "&" map { case rEqual(x, y) => x -> y; case x => x -> "" } toMap //split ("?") map { _ split "&"} map {case rEqual(x,y) => x->y; case x => x->""}; Map("x"->"2") }
            case "POST" => Map("data" -> stream.mkString("\n"))
        })

        lazy val cookie = headers.getOrElse("Cookie", "").split(",") map (_.trim.split("=")) map { case Array(x, y) => Some(x -> y); case x => None } filter (_ != None) map (_.get) toMap

        new Request(method, path, query, cookie, headers)
    }
}
