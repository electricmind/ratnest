package ru.wordmetrix.ratnest
import scala.math.max


class EstimatorSimple extends Estimator {
    /*
    *  The Idea: 
    *    - Every time Estimator receives a Basket it genereate Recomendation by means of Index of Recomendation
    *    - Every time Estimator recieves a RecomendationIndex it save them instead of previoues one
    */

    var matrix: Map[RatNest.GoodsId, Map[RatNest.GoodsId, Int]] = Map[RatNest.GoodsId, Map[RatNest.GoodsId, Int]]() withDefaultValue (Map())

    def act() = {
        val log = new Log("Estimator", "started")

        def vadd(x: Map[RatNest.GoodsId, Int], y: Set[(RatNest.GoodsId, Int)]): Map[RatNest.GoodsId, Int] = {
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
                case m: Map[RatNest.GoodsId, Map[RatNest.GoodsId, Int]] => {
                    log("Set new matrix")
                    matrix = m
                }
            }
        }
    }
}

