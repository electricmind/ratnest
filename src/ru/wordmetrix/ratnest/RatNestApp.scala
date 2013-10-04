package ru.wordmetrix.ratnest

import java.net.ServerSocket
import scala.actors.Actor._

object RatNestApp extends App {

    override def main(args: Array[String]) = {
        val (host, port, file) = args match {
            case Array()     => ("0.0.0.0", 8080, None)
            case Array(p)    => if (p.startsWith("-")) ("0.0.0.0", 8080, Some(p.substring(1))) else ("0.0.0.0", p toInt, None)
            case Array(h, p) => (h, p toInt, None)
        }

        val dispatcher = new Dispatcher
        val sessions = new Sessions
        var metagor = new Metagor
        var goodss = new Goodss(metagor, "database/")
        var reports = new Reports(goodss, RatNest.root)
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
                    val id = RatNest.randomId
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