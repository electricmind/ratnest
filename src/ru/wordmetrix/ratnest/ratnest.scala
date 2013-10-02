package ru.wordmetrix.ratnest

import java.net.ServerSocket
import scala.actors.Actor
import scala.actors.Actor._
import scala.math._
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.immutable.Stream.consWrapper
// import java.net.URLEncoder.encode

// http://stackoverflow.com/questions/5564074/scala-http-operations
object RatNest {
    type GoodsId = String
    type Site = String
    type Url = String
    type SessionId = String

    class Log(prefix: String, message: Any*) {
        def apply(message: Any*) = println(prefix + ": " + (message mkString " "))
        this(message mkString " ")
    }

    val random = new util.Random()
    def randomId: SessionId = 1 to 10 map (x => "abcdefghijklmnopqrstuvwxyz"(math.abs(random.nextInt) % 25)) mkString ""

    class Goods(val site: Site, val id: GoodsId) extends Serializable {
        val stage = 0
        val url = id

        override def toString = site + "!! " + id + "!"
    }

    class IndexOfGoods(val site: String) {
        lazy val map: Map[GoodsId, Goods] = Map()
    }

    class GoodsSimpleMeta(site: Site, id: GoodsId, override val url: Url) extends Goods(site, id) {
        override val stage = 1
        def this(goods: Goods, url: String) {
            this(goods.site, goods.id, url)
        }
        override def toString = "<div><a href=\"" + url + "\">" + url + "</a></div>"
    }

    class GoodsMeta(site: Site, id: GoodsId, url: Url, val title: String, announce: String) extends GoodsSimpleMeta(site, id, url) {
        override val stage = 2
        def this(goods: GoodsSimpleMeta, title: String, announce: String) {
            this(goods.site, goods.id, goods.url, title, announce)
        }
        override def toString = "<div><a href=\"" + url + "\" title=\"" + announce + "\">" + title + "</a><div class=\"ui-helper-hidden\">" + announce + "</div></div>"
    }

    case class Sample(val site: String, val id: String, val basket: Set[GoodsId]) {
        override def toString = {
            site + " " +
                id + " " +
                (basket mkString " ") + " "
        }
        def +(goods: GoodsId) = new Sample(this.site, this.id, this.basket + goods)
    }

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

    val root = "html/"

    class Goodss(val metagor: Metagor, val database: String) extends Actor {
        /*
     *  The Idea: Goods Storage stores goods. Storage receives goods and store them by id. Also, Storage receives Set[GoodsId] set and reduce it into
     *  Set[Goods]. Each time Storage  receives goods it sends it to MetaGor. 
     *
     */
        var goodss: Map[GoodsId, Goods] = Map[GoodsId, Goods]()

        def md5hex(x: String) = java.security.MessageDigest.getInstance("MD5").digest(x.getBytes).map("%2h" format math.abs(_)).mkString.replace(" ", "0")

        def goodsfilename(filename: Url) =
            if (filename.length < 50) filename.replaceAll("/", "-") else {
                val rFilename = "http://([^/]+)/(.*)".r
                filename match {
                    case rFilename(host, path) => host + "-" + md5hex(path)
                    case x                     => md5hex(x)
                }
            }

        def act() = {
            val log = new Log("Goodss", "started")
            loop {
                react {
                    case goods: Goods => {

                        def askmetagor(goods: Goods) = goods match {
                            case goods: GoodsMeta => try { // !!! filename too long 
                                new java.io.ObjectOutputStream(new java.io.FileOutputStream(database + goodsfilename(goods.id))).writeObject(goods)
                            } catch {
                                case x: java.io.FileNotFoundException => log(x)
                            }
                            case goods: Goods => { log("Ask metagor"); metagor ! goods }
                        }

                        goodss = goodss get goods.id match {
                            case Some(gs) if gs.stage < goods.stage => {
                                askmetagor(goods)
                                goodss + (goods.id -> goods)
                            }

                            case Some(gs) => {
                                log(goods.id, "has been already received")
                                goodss
                            }

                            case None => try {
                                log("Load...", database + goodsfilename(goods.id))
                                goodss + (goods.id -> new java.io.ObjectInputStream(new java.io.FileInputStream(database + goodsfilename(goods.id))).readObject().asInstanceOf[Goods])
                            } catch {
                                case x: java.io.FileNotFoundException => {
                                    askmetagor(goods)
                                    goodss
                                }
                            }
                        }
                    }

                    case goodsids: Set[GoodsId] => {
                        log("Fill GoodsSet")

                        sender ! (
                            goodsids map (goodsid =>
                                goodss.get(goodsid) match {
                                    case Some(goods) => goods
                                    case None => {
                                        log("Read goods", goodsid)
                                        try {
                                            val goods = new java.io.ObjectInputStream(new java.io.FileInputStream(database + goodsid.replaceAll("/", "-"))).readObject().asInstanceOf[Goods]
                                            goodss = goodss + (goodsid -> goods)
                                            goods
                                        } catch {
                                            case x: java.io.FileNotFoundException => {
                                                this ! new Goods("", goodsid)
                                                None
                                            }
                                        }
                                    }
                                }) filter (_ != None))
                    }
                }
            }
        }
    }

    class Metagor extends Actor {
        /*
     *  Meta Server make from goods more detalized version.
     */

        def act() = {
            val log = new Log("Metagor", "started")

            loop {
                react {
                    case goods: GoodsMeta => None //new java.io.ObjectOutputStream(new java.io.FileOutputStream(database + goods.id)).writeObject(goods)

                    case goods: Goods => {
                        log("Metagor recombine goods", goods)
                        try {
                            sender ! (goods match {
                                case goods: GoodsSimpleMeta => {
                                    val rTitle = "<title>([^<>]*)</title>".r
                                    val rDescription = "<meta\\s+name=\"Description\"\\s+content=\"([^\"]*)\">".r
                                    val url = goods.url

                                    log("Eat HTML from", url)

                                    val page = if (url contains "ach-fci") io.Source.fromURL(url, "koi8-r").mkString("") else io.Source.fromURL(url).mkString("")
                                    val title = rTitle.findFirstIn(page) match {
                                        case Some(rTitle(title)) => title
                                        case None                => url
                                    }

                                    val description = rDescription.findFirstIn(page) match {
                                        case Some(rDescription(description)) => description
                                        case None                            => ""
                                    }

                                    new GoodsMeta(goods, title = title, announce = description)
                                }
                                case goods: Goods => new GoodsSimpleMeta(goods, url = goods.id) // Make request a map GoodsId -> Url and save It
                            })
                        } catch {
                            case x: java.io.IOException => log(x)
                        }
                    }
                }
            }
        }
    }

    class Samples(reports: Reports, goodss: Goodss, database: String) extends Actor {
        /*
    *  The Idea: 
    *    - Every time Samples receive a sample it save one
    *    - Every time Samples receive a tutor it get him samples back
    */
        val log = new Log("Samples", "started")
        var samples = (try {
            (new java.io.ObjectInputStream(new java.io.FileInputStream(database + "samples"))).readObject().asInstanceOf[List[Sample]]
        } catch {
            case x /*: java.io.FileNotFoundException*/ => { log(x); List[Sample]() }
        }) // map ( x=>new Sample(x.site,x.id,x.basket.filter ( ! _.endsWith("@statistic"))))

        def act() = {
            reports ! samples
            loop {
                react {
                    case sample: Sample => sample :: samples
                    case tutor: Tutor => {
                        log("Send sampling with size of", samples.size, "to tutor")

                        tutor ! samples

                        new java.io.ObjectOutputStream(new java.io.FileOutputStream(database + "samples")).writeObject(samples)

                        def collect(samples: List[Sample] = List()): List[Sample] =
                            if (samples.size > 50) samples
                            else collect(receive {
                                case sample: Sample => sample
                            } :: samples)

                        samples = collect() ++ samples
                        reports ! samples
                    }
                    case "reindex" => {
                        for (sample <- samples.foldRight(Set[GoodsId]())((x: Sample, y: Set[GoodsId]) => x.basket ++ y)) {
                            goodss ! new Goods("", sample)
                        }
                    }
                }
            }
        }
    }

    abstract class Estimator extends Actor

    class EstimatorSimple extends Estimator {
        /*
    *  The Idea: 
    *    - Every time Estimator receives a Basket it genereate Recomendation by means of Index of Recomendation
    *    - Every time Estimator recieves a RecomendationIndex it save them instead of previoues one
    */

        var matrix: Map[GoodsId, Map[GoodsId, Int]] = Map[GoodsId, Map[GoodsId, Int]]() withDefaultValue (Map())

        def act() = {
            val log = new Log("Estimator", "started")

            def vadd(x: Map[GoodsId, Int], y: Set[(GoodsId, Int)]): Map[GoodsId, Int] = {
                x.toList ++ y.toList groupBy (_._1) map (x => x._1 -> (x._2 map (_._2) reduce (_ + _)))
            }

            loop {
                react {
                    case Sample(site, id, basket) => {
                        log("Estimate sample")
                        val ws = (basket.map(matrix).fold(Map())((x, y) => vadd(x, y.toSet)))
                        val ranged = ws.toList.sortBy(_._2).reverse.filter(x => !basket.contains(x._1)).take(max(ws.size / 2, 4))

                        sender ! ranged.map(_._1).toSet
                    }
                    case m: Map[GoodsId, Map[GoodsId, Int]] => {
                        log("Set new matrix")
                        matrix = m
                    }
                }
            }
        }
    }

    abstract class Tutor extends Actor

    class Reports(goodss: Goodss, reports: String) extends Actor {
        /*
    * The idea:
    * 
    * Upon receiving samples save them as report for octave or gephi
    */

        def reportOctave(samples: List[Sample]) {
            log("A report is generated for an octave")
            val datafile = new java.io.OutputStreamWriter(new java.io.FileOutputStream(reports + "sample.dat"))
            lazy val goodsids = samples.toSet.map((x: Sample) => x.basket).flatten.toList
            goodss ! goodsids.toSet

            datafile.write(
                //"# " + goodsids.mkString(" ") + "\n"  +
                "# name: sample\n" +
                    "# type: matrix\n" +
                    "# rows: " + goodsids.size + "\n" +
                    "# columns: " + samples.size + "\n")

            for (sample <- samples) {
                datafile.write((goodsids map (x => if (sample.basket contains x) "1" else "0") mkString " ") + "\n")
            }

            val index: Map[GoodsId, String] = receive {
                case goodss: Set[GoodsMeta] => (goodss map ({
                    case x: GoodsMeta => x.id -> x.title;
                }) toMap).withDefaultValue("NoName")
            }

            datafile.write(
                "\n\n" +
                    "# name: title\n" +
                    "# type: cell\n" +
                    "# rows: 1\n" +
                    "# columns: " + goodsids.size + "\n")

            for {
                goodsid <- goodsids
            } {
                val title = "[[\\s][^a-zA-Z\\-,;=0-9А-Яа-я]]+".r.replaceAllIn(index(goodsid), " ").trim

                datafile.write(
                    "# name: <cell-element>\n" +
                        "# type: string\n" +
                        "# elements: 1\n" +
                        "# length: " + title.length + "\n" +
                        title +
                        "\n\n\n")
            }

            datafile.close()
        }

        def reportGML(samples: List[Sample]) = {
            println("A report is generated in a GML file")

            if (samples.size < 1) log("U gotta gonna have at very least 1 sample") else {
                val gmlfile = new java.io.OutputStreamWriter(new java.io.FileOutputStream(reports + "sample.gml"))

                //      lazy val index : Stream[Int] =  0 #:: index map (_ + 1)
                //      val i = index.toIterator

                def genindex(i: Int): Stream[Int] = i #:: genindex(i + 1)
                val index = genindex(1).toIterator

                var vertexes = (samples map (_.basket) reduce (_ ++ _) toSet).map((x: GoodsId) => (x -> ("", index.next, List[GoodsId]()))).toMap

                for {
                    sample <- samples
                    vertex1 <- sample.basket
                    vertex2 <- sample.basket
                    if (vertex1 != vertex2)
                } {
                    vertexes = vertexes ++ {
                        vertexes(vertex1) match {
                            case (name, id, edges) => Map(vertex1 -> (name, id, vertex2 :: edges))
                        }
                    }
                }

                gmlfile.write("graph\n[\ndirected 0\n")

                for {
                    vertex <- vertexes
                } {
                    vertex match {
                        case (vertex, (label, id, edges)) =>
                            goodss ! Set(vertex)
                            gmlfile.write(
                                "node [\n" +
                                    "id " + id + "\n" +
                                    "label " + (receive { case x: Set[GoodsMeta] => { if (x.isEmpty) "NoName" else "[[\\s][^a-zA-Z\\-,;=0-9А-Яа-я]]+".r.replaceAllIn(x.head.title, " ").trim.replace(" ", "-") match { case "" => "NoName"; case x => x } } }) + "\n" +
                                    "]\n")
                    }
                }

                for {
                    vertex <- vertexes
                } {
                    vertex match {
                        case (vertex, (label, id, edges)) =>
                            for {
                                (edge, weight) <- edges groupBy (x => x) map (x => x._1 -> x._2.size)
                            } gmlfile.write(
                                "edge [\n" +
                                    "source " + id + "\n" +
                                    "target " + vertexes(edge)._2 + "\n" +
                                    "weight " + weight + "\n" +
                                    "]\n")
                    }
                }

                gmlfile.write("]\n")
                gmlfile.close()
            }

        }

        val log = new Log("Report", "started")
        def act() = {
            loop {
                react {
                    case samples: List[Sample] => {
                        reportGML(samples)
                        reportOctave(samples)
                    }
                }
            }
        }
    }

    class TutorSimple(samples: Samples, estimator: EstimatorSimple, goodss: Goodss, database: String) extends Tutor {
        /*
    * The Idea: 
    *
    *  - Every time Tutor recieves Set of Sample it computes Index and sends one to Estimator. Then it sends himself to Samples
    *
    */
        def act() = {
            val log = new Log("Tutor", "started")
            samples ! this
            estimator ! (try {
                new java.io.ObjectInputStream(new java.io.FileInputStream(database + "matrix")).readObject().asInstanceOf[Map[GoodsId, Set[Goods]]].withDefaultValue(Set[GoodsId]())
            } catch {
                case x: java.io.FileNotFoundException => Map[GoodsId, Set[Goods]]().withDefaultValue(Set[GoodsId]())
            })

            loop {
                react {
                    case samples: List[Sample] => {
                        log("save samples")
                        val datafile = new java.io.OutputStreamWriter(new java.io.FileOutputStream(database + "sample.dat"))
                        lazy val goodsids = samples.toSet.map((x: Sample) => x.basket).flatten.toList

                        datafile.write(
                            "# " + goodsids.mkString(" ") + "\n" +
                                "# name: samples\n" +
                                "# type: matrix\n" +
                                "# rows: " + goodsids.size + "\n" +
                                "# columns: " + samples.size + "\n")

                        for (sample <- samples) {
                            datafile.write((goodsids map (x => if (sample.basket contains x) "1" else "0") mkString " ") + "\n")
                        }

                        datafile.close()

                        log("learn")

                        def vadd(x: Map[GoodsId, Int], y: Set[(GoodsId, Int)]): Map[GoodsId, Int] = {
                            x.toList ++ y.toList groupBy (_._1) map (x => x._1 -> (x._2 map (_._2) reduce (_ + _)))
                        }

                        val matrix = ((for {
                            sample <- samples
                            goods <- sample.basket
                        } yield (goods -> (sample.basket map (x => (x -> 1))))).toList.foldRight(Map[GoodsId, Map[GoodsId, Int]]().withDefaultValue(Map[GoodsId, Int]()))(
                            (y, x) => x + (y._1 -> vadd(x(y._1), y._2))))

                        new java.io.ObjectOutputStream(new java.io.FileOutputStream(database + "matrix")).writeObject(matrix.toList.toMap)

                        estimator ! matrix
                        sender ! this
                    }
                }
            }
        }
    }

    abstract trait kMeans {
        type V = Map[GoodsId, Double]
        val log: Log

        def vadd(vx: V, vy: V): V = {
            //log("vadd")
            ((vx.keys ++ vy.keys) map (x => (x -> (vy(x) + vx(x)))) toMap) withDefaultValue (0.0)
        }
        def vsub(vx: V, vy: V): V = {
            //log("vsub")
            ((vx.keys ++ vy.keys) map (x => (x -> (vy(x) - vx(x)))) toMap) withDefaultValue (0.0)
        }

        def vmuln(vx: V, n: Double) = vx map (x => x._1 -> (x._2 * n)) withDefaultValue (0.0)

        def vnorm(vx: V): Double = if (vx.isEmpty) 0.0 else vx map (x => x._2 * x._2) reduce (_ + _)

        def vort(vx: V): V = vmuln(vx, 1.0 / math.pow(vnorm(vx), 0.5))

        def closest(centroids: List[V], x: V): (V, Double) = (centroids map (centroid => (centroid, vnorm(vsub(centroid, x)))) minBy (_._2))

        def kMeans(size: Int, centroids: Option[List[V]], samples: List[Sample], energy: Double = Double.PositiveInfinity) = {
            def kMeansStep(centroids: List[V], bunch: List[V], count: Int, energy1: Double = Double.PositiveInfinity, energy2: Double = Double.PositiveInfinity): (Option[List[V]], Double) = {
                log("kMeans Step " + count + " Energy " + energy1 + " " + energy2)
                if (count == 0 || math.abs(energy1 - energy2) < 0.01) (Some(centroids), energy1) else {
                    val (energy2, neighbourhood: Map[V, List[V]]) = bunch.foldLeft(
                        (0.0, Map[V, List[V]]() withDefaultValue (List[V]())))(
                            (y: (Double, Map[V, List[V]]), x: V) => {
                                val (centroid, weight) = closest(centroids, x)
                                (y._1 + weight, y._2 + (centroid -> (x :: y._2(centroid))))
                            })

                    kMeansStep(
                        neighbourhood.values.toList.map(x => vmuln(x.reduce((x, y) => vadd(x, y)), 1.0 / x.length)),
                        bunch,
                        count - 1,
                        energy2 / bunch.size,
                        energy1)
                }
            }

            val batches = samples map (x => vort(x.basket map (x => x -> 1.0) toMap).withDefaultValue(0.0))

            log("Samples #" + batches.size, "Goods #" + (batches reduce (_ ++ _) size), "Centroids #" + (centroids match { case Some(x) => x.size; case None => size }))

            kMeansStep(
                centroids match {
                    case Some(x) => x
                    case None    => (new util.Random).shuffle(batches).take(size)
                }, batches, 100, energy) match { case (centroids, energy) => (centroids.filter(_.size > 0), energy) }
        }

        def kMeansSize(centroids: Option[List[V]], samples: List[Sample], size: Int = 10, energy: Double = Double.PositiveInfinity): (Option[List[V]], Double) = {
            val (c, e) = kMeans(size, centroids, samples, energy)
            log("Sizes:" + (c.get map (_.size) mkString (" ")))

            val max = (c.get map (_.size) reduce (_ + _)) / c.get.size
            if (max > 15) {

                val batches = samples map (x => vort(x.basket map (x => x -> 1.0) toMap).withDefaultValue(0.0))

                kMeansSize(Some((new util.Random).shuffle(batches).take(c.get.size /*max / 15*/ ) ++ c.get), samples, size + 1, e)
            } else (c, e)
        }
    }

    class EstimatorKMeans extends Estimator with kMeans {
        /*
    *  The Idea: 
    *    - Every time Estimator receives a Basket it genereate Recomendation by means of Index of Recomendation
    *    - Every time Estimator recieves a RecomendationIndex it save them instead of previoues one
    */

        override val log = new Log("EstimatorKMeans", "started")

        def act() = {
            loop {
                react {
                    case cs: List[V] => {
                        log("Set new centroids")
                        var centroids: List[V] = cs
                        loop {
                            react {
                                case Sample(site, id, basket) => {
                                    log("Estimate sample")
                                    val (ws, error) = closest(centroids, basket.map(_ -> 1.0).toMap.withDefaultValue(0.0))
                                    val ranged = ws.toList.sortBy(_._2).reverse.filter(x => !basket.contains(x._1)).take(min(ws.size / 2, 10))
                                    sender ! ranged.map(_._1).toSet
                                }
                                case cs: List[V] => {
                                    log("Set new centroids")
                                    centroids = cs
                                }
                            }
                        }
                    }

                    case Sample(site, id, basket) => {
                        log("Estimator is empty")
                        sender ! Set[GoodsId]()
                    }

                }
            }
        }
    }

    class TutorKMeans(samples: Samples, estimator: EstimatorKMeans, goodss: Goodss, database: String) extends Tutor with kMeans {
        /*
    * The Idea: 
    *   - Every time Tutor recieves Set of Sample it computes Index and sends one to Estimator. Then it sends himself to Samples
    */

        var centroids: Option[List[Map[GoodsId, Double]]] = None
        var energy: Double = Double.PositiveInfinity
        override val log = new Log("TutorKMeans", "started")

        def act() = {
            samples ! this
            estimator ! (try {
                new java.io.ObjectInputStream(new java.io.FileInputStream(database + "matrix")).readObject().asInstanceOf[Map[GoodsId, Set[Goods]]].withDefaultValue(Set[GoodsId]())
            } catch {
                case x: java.io.FileNotFoundException => Map[GoodsId, Set[Goods]]().withDefaultValue(Set[GoodsId]())
            })

            loop {
                react {
                    case samples: List[Sample] => {
                        //log("learn")
                        if (0 == samples.size) log("A zero-length sampling received and should be ignored") else {
                            val (c, e) = kMeansSize(centroids, samples filter (_.basket.size > 1) filter (_.basket.size < 6), 20, energy)

                            centroids = c
                            energy = e

                            for {
                                titles <- for {
                                    centroid <- centroids.get
                                } yield ({
                                    goodss ! (centroid filter (_._2 > 0.0001) keys)
                                    receive {
                                        case c: Set[GoodsMeta] => c.map(_.title.trim)
                                    }
                                })
                            } log("###", (titles mkString (" | ")))

                            //!!!new java.io.ObjectOutputStream(new java.io.FileOutputStream(database + "centroids")).writeObject(centroids.toList.toMap)
                            estimator ! centroids.get
                        }
                        sender ! this
                    }
                }
            }
        }
    }

    /* 
    Create session & basket
    Add url
    make simple suggestion
  */

    class Sessions extends Actor {
        // !!! Some assignment is wrong
        var storage: Map[SessionId, Sample] = Map()

        def act() = {
            val log = new Log("Sessions", "started")

            loop {
                react {
                    case session: SessionId => sender ! (storage.get(session) match {
                        case Some(sample) => sample
                        case None         => new Sample("", session, Set())
                    })

                    case (session: SessionId, goods: GoodsId) => {
                        log("Session accepted", session)
                        storage = storage ++ Map(storage.get(session) match {
                            case Some(sample) => {
                                log("session found" + sample)

                                val s = sample + goods
                                log("send sample")
                                sender ! s
                                // I changed statement below to reject old goods from session and include into sample only last two goods
                                session -> new Sample("", session, Set(goods)) //s
                            }

                            case None => {
                                val sample: Sample = new Sample("", session, Set(goods))
                                log("session not found")
                                sender ! sample
                                session -> sample
                            }
                        })
                    }
                }
            }
        }
    }

    /*

 react {
   case q : QQ => {
     react {
       case e : EE => {
         acceptor ! (q,e)  // do something with q & e 
       }
     } 
   }
 }*/

    class Server(val n: Int, dispatcher: Dispatcher, basket: Sessions, goodss: Goodss, samples: Samples, estimator: Estimator) extends Actor {

        def act() = {
            val log = new Log("Server #" + n, "started")

            dispatcher ! this
            loop {
                react {
                    case socket: java.net.Socket => {
                        val start = new java.util.Date()
                        log("socket accepted", new java.text.SimpleDateFormat("yyyy-MM-dd hh:mm:ss", new java.util.Locale("C")).format(start))
                        val request = try { Some(Request(socket.getInputStream())) } catch { case _ => None }
                        socket.getOutputStream.write((request match {
                            case Some(request) => request match {
                                case Request(_, f, query, cookie, headers) => {
                                    val (responseCookie, session) = {
                                        request.cookie.get("session") match {
                                            case Some(session) => (Map[String, String](), session)
                                            case None => {
                                                var session = randomId
                                                (Map("session" -> session), session)
                                            }
                                        }
                                    }

                                    log(f, "accepted")

                                    val ready = f match {
                                        case "add" => { // Create basket or add current Url
                                            // Make goods id from request
                                            // Or make gooods id from Reference

                                            (query get "goodsid" match {
                                                case Some(x) => Some(x)
                                                case None => headers get "Referer" match {
                                                    case Some(x) if x endsWith "@statistic" => None
                                                    case Some(x)                            => Some(x)
                                                    case None                               => None
                                                }
                                            }) match {
                                                case Some(x) => {
                                                    basket ! (session, x)
                                                    goodss ! new Goods(headers.withDefaultValue("http://example.net")("Host"), x) // !!! I could have write error handler here
                                                }
                                                case None => basket ! session
                                            }
                                            log("add matched")
                                            true
                                        }

                                        case "suggest" => {
                                            // Make goods id from request
                                            // Or make gooods id from Reference
                                            basket ! session
                                            log("suggest matched")
                                            true
                                        }

                                        case "reindex.html" => {
                                            log("Reload initiated")
                                            samples ! "reindex"
                                            false
                                        }

                                        case f => {
                                            log("file matched")
                                            false
                                        }
                                    }

                                    try { // !!! We need to make something different and avoid try - catch
                                        log("Waiting sample")
                                        new ResponseSuccess(
                                            responseCookie,
                                            ready match {
                                                case true =>
                                                    receive {
                                                        // We return dump of current basket instead of suggestion. 
                                                        // Just to check if basket storage idea is right
                                                        case sample: Sample => {
                                                            // estimator used instead goodss ! sample.basket //to fill prediction by goods
                                                            samples ! sample // to learn estimator
                                                            estimator ! sample // to estimate 

                                                            try {
                                                                log("Wait estimator")
                                                                goodss ! receive { //Within(1000) {
                                                                    case goodsids: Set[GoodsId] => goodsids
                                                                }

                                                                log("Wait goodss")

                                                                val qq = "suggestion = [" + (try {
                                                                    receive {
                                                                        case g: Set[Goods] => {
                                                                            g map ("'" + _.toString + "'") mkString (", ")
                                                                        }

                                                                    }
                                                                } catch {
                                                                    case x => { log("Goodss run away", x); ""; }
                                                                }) + "]"

                                                                log("1")
                                                                qq
                                                            } catch {
                                                                case x => { log("Estimator run away", x); "suggestion=[]"; }
                                                            }
                                                        }
                                                    }
                                                case false => {
                                                    val file = new java.io.FileInputStream(
                                                        root + (f match {
                                                            case "" => "index.html"
                                                            case x  => x
                                                        }))
                                                    val buffer = new Array[Byte](file.available)
                                                    file.read(buffer)
                                                    buffer.map(_.asInstanceOf[Char]).mkString("")
                                                }
                                            })
                                    } catch {
                                        case x: java.io.FileNotFoundException => {
                                            log(x)
                                            new ResponseNotFound(responseCookie)
                                        }
                                    }
                                }
                            }
                            case None => new ResponseAbort(Map(), "")
                        }).toString.getBytes)

                        socket.close()

                        val finish = new java.util.Date()
                        log("request took", (finish.getTime - start.getTime) / 1000, "seconds to be completed")

                        //log("sleep")
                        // Thread.sleep(50000)
                        //log("awake")

                        dispatcher ! this
                    }
                }
            }
        }
    }

    /*
                 // Get id from query
                 //  ... or Get id && url from reference 
                 // Request goods from storage
                 // Save goods into storage (storage ! new Goods(id, url))

                 // Make a basket from query
                 //  .. or get basket from session
                 //  .. or make empty basket
                 // Add id into basket, ( and shedule basket for future save
                 // respond with standart response (from file "/client/add.js"
                 // If request has commit key commit basket
                 // If request has no commit key ;) don't commit basket
                 // If request has every time commit key :) commit basket


                 val respond = new ResponseSuccess(Map(),io.Source.fromInputStream(new java.io.FileInputStream(root + f)).getLines.mkString(""))
                 if (cookie.contains("session")) respond else respond //+ (session,randomId)
               }

               case Some(Request(_,"/suggest",query,cookie)) => { // Ask for suggestion
                 // Make suggestion from index
                 // .. or compute suggestion
                 // .. or send suggestion to recompute and make simple suggestion
                 // .. generate standart suggestion report 
                 // .. List() toJSON

                 val respond = new ResponseSuccess(Map(),io.Source.fromInputStream(new java.io.FileInputStream(root + f)).getLines.mkString(""))
                 if (cookie.contains("session")) respond else respond //+ (session,randomId)
               }


               case Some(Request(_,f,query,cookie)) => {  // Accomplish request of file
                 val respond = new ResponseSuccess(Map(),io.Source.fromInputStream(new java.io.FileInputStream(root + f)).getLines.mkString(""))
                 if (cookie.contains("session")) respond else respond //+ (session,randomId)
               }

*/

    class Dispatcher extends Actor {
        val log = new Log("Dispatcher")
        def act() = {

            def queue(sockets: List[java.net.Socket], servers: List[Server]) {
                log("Servers:", servers.length, "Sockets:", sockets.length)
                react {
                    case server: Server => {
                        sockets match {
                            case socket :: sockets => {
                                server ! socket
                                queue(sockets, servers)
                            }
                            case List() => queue(sockets, server :: servers)
                        }
                    }
                    case socket: java.net.Socket => {
                        servers match {
                            case server :: servers => {
                                server ! socket
                                queue(sockets, servers)
                            }
                            case List() => { queue(socket :: sockets, servers) }
                        }
                    }

                }
            }
            queue(List(), List())
        }
    }

    /*class MyClass[T](implicit man: Manifest[T]) {
    def getNewInstance =  man.erasure.newInstance
    def getClassT = man.erasure.asInstanceOf[Class[T]]
  }*/

    def main(args: Array[String]) = {
        val (host, port, file) = args match {
            case Array()     => ("0.0.0.0", 8080, None)
            case Array(p)    => if (p.startsWith("-")) ("0.0.0.0", 8080, Some(p.substring(1))) else ("0.0.0.0", p toInt, None)
            case Array(h, p) => (h, p toInt, None)
        }

        val dispatcher = new Dispatcher
        val sessions = new Sessions
        var metagor = new Metagor
        var goodss = new Goodss(metagor, "database/")
        var reports = new Reports(goodss, root)
        var samples = new Samples(reports, goodss, "database/")
        /*var estimator = new EstimatorSimple()
    var tutor = new TutorSimple(samples, estimator, goodss, "database/")
    */

        var estimator = new EstimatorKMeans()
        var tutor = new TutorKMeans(samples, estimator, goodss, "database/")

        dispatcher.start
        sessions.start
        metagor.start
        goodss.start
        estimator.start
        samples.start
        tutor.start
        reports.start
        //samples ! "reindex"

        file match {
            case None => {
                for (i <- 1 to 10) {
                    new Server(i, dispatcher, sessions, goodss, samples, estimator) start
                }

                val serverSocket = new ServerSocket(port)
                try {
                    while (true) {
                        dispatcher ! serverSocket.accept()
                    }
                } finally {
                    println("Exit")
                }
            }

            case Some(file) => {
                println("------------------------------")
                for {
                    goodsids <- io.Source.fromFile(file).getLines.map(_.split(" ")).filter(_.size == 3).map(x => (x(0), x(2))).toList.groupBy(_._1).map(x => x._1 -> (x._2.map(_._2).toSet)).map(_._2)
                } {
                    val id = randomId
                    for {
                        goodsid <- goodsids
                    } {
                        println(goodsid)
                        try {
                            sessions ! (id, goodsid.tail.reverse.tail.reverse)
                            receive {
                                // We return dump of current basket instead of suggestion. 
                                // Just to check if basket storage idea is right
                                case sample: Sample => {
                                    // estimator used instead goodss ! sample.basket //to fill prediction by goods
                                    println("getin")
                                    samples ! sample // to learn estimator
                                }
                            }

                        } catch {
                            case x => println(x)
                        }
                    }
                }

            }

        }
    }
}

