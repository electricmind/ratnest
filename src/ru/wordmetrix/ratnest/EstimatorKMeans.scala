package ru.wordmetrix.ratnest

import Math._

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
                    sender ! Set[RatNest.GoodsId]()
                }

            }
        }
    }
}
