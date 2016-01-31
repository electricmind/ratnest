package ru.wordmetrix.ratnest

import scala.actors.Actor

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

        val index: Map[RatNest.GoodsId, String] = receive {
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

            var vertexes = (samples map (_.basket) reduce (_ ++ _) toSet).map((x: RatNest.GoodsId) => (x -> ("", index.next, List[RatNest.GoodsId]()))).toMap

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
