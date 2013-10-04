package ru.wordmetrix.ratnest

abstract trait kMeans {
    type V = Map[RatNest.GoodsId, Double]
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
