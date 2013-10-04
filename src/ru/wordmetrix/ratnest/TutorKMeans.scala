package ru.wordmetrix.ratnest

class TutorKMeans(samples: Samples, estimator: EstimatorKMeans, goodss: Goodss, database: String) extends Tutor with kMeans {
    /*
    * The Idea: 
    *   - Every time Tutor recieves Set of Sample it computes Index and sends one to Estimator. Then it sends himself to Samples
    */

    var centroids: Option[List[Map[RatNest.GoodsId, Double]]] = None
    var energy: Double = Double.PositiveInfinity
    override val log = new Log("TutorKMeans", "started")

    def act() = {
        samples ! this
        estimator ! (try {
            new java.io.ObjectInputStream(new java.io.FileInputStream(database + "matrix")).readObject().asInstanceOf[Map[RatNest.GoodsId, Set[Goods]]].withDefaultValue(Set[RatNest.GoodsId]())
        } catch {
            case x: java.io.FileNotFoundException => Map[RatNest.GoodsId, Set[Goods]]().withDefaultValue(Set[RatNest.GoodsId]())
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
