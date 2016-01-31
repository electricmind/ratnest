package ru.wordmetrix.ratnest

import scala.actors.Actor

class Log(prefix: String, message: Any*) {
    def apply(message: Any*) = println(prefix + ": " + (message mkString " "))
    this(message mkString " ")
}

abstract class Estimator extends Actor

abstract class Tutor extends Actor

object RatNest {
    type GoodsId = String
    type Site = String
    type Url = String
    type SessionId = String
   val root = "html/"

    val random = new util.Random()
    def randomId: SessionId = 1 to 10 map (x => "abcdefghijklmnopqrstuvwxyz"(math.abs(random.nextInt) % 25)) mkString ""
}

