package ru.wordmetrix.ratnest

import scala.actors.Actor

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
                                            var session = RatNest.randomId
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
                                                                case goodsids: Set[RatNest.GoodsId] => goodsids
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
                                                    RatNest.root + (f match {
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