package ru.wordmetrix.ratnest


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
            new java.io.ObjectInputStream(new java.io.FileInputStream(database + "matrix")).readObject().asInstanceOf[Map[RatNest.GoodsId, Set[Goods]]].withDefaultValue(Set[RatNest.GoodsId]())
        } catch {
            case x: java.io.FileNotFoundException => Map[RatNest.GoodsId, Set[Goods]]().withDefaultValue(Set[RatNest.GoodsId]())
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

                    def vadd(x: Map[RatNest.GoodsId, Int], y: Set[(RatNest.GoodsId, Int)]): Map[RatNest.GoodsId, Int] = {
                        x.toList ++ y.toList groupBy (_._1) map (x => x._1 -> (x._2 map (_._2) reduce (_ + _)))
                    }

                    val matrix = ((for {
                        sample <- samples
                        goods <- sample.basket
                    } yield (goods -> (sample.basket map (x => (x -> 1))))).toList.foldRight(Map[RatNest.GoodsId, Map[RatNest.GoodsId, Int]]().withDefaultValue(Map[RatNest.GoodsId, Int]()))(
                        (y, x) => x + (y._1 -> vadd(x(y._1), y._2))))

                    new java.io.ObjectOutputStream(new java.io.FileOutputStream(database + "matrix")).writeObject(matrix.toList.toMap)

                    estimator ! matrix
                    sender ! this
                }
            }
        }
    }
}
