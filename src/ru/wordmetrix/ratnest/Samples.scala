package ru.wordmetrix.ratnest

import scala.actors.Actor

case class Sample(val site: String, val id: String, val basket: Set[RatNest.GoodsId]) {
    override def toString = {
        site + " " +
            id + " " +
            (basket mkString " ") + " "
    }
    def +(goods: RatNest.GoodsId) = new Sample(this.site, this.id, this.basket + goods)
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
                    for (sample <- samples.foldRight(Set[RatNest.GoodsId]())((x: Sample, y: Set[RatNest.GoodsId]) => x.basket ++ y)) {
                        goodss ! new Goods("", sample)
                    }
                }
            }
        }
    }
}
