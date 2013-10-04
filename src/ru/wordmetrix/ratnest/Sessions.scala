package ru.wordmetrix.ratnest

import scala.actors.Actor

/* 
    Create session & basket
    Add url
    make simple suggestion
  */

class Sessions extends Actor {
    // !!! Some assignment is wrong
    var storage: Map[RatNest.SessionId, Sample] = Map()

    def act() = {
        val log = new Log("Sessions", "started")

        loop {
            react {
                case session: RatNest.SessionId => sender ! (storage.get(session) match {
                    case Some(sample) => sample
                    case None         => new Sample("", session, Set())
                })

                case (session: RatNest.SessionId, goods: RatNest.GoodsId) => {
                    log("Session accepted", session)
                    storage = storage ++ Map(storage.get(session) match {
                        case Some(sample) => {
                            log("session found" + sample)

                            val s = sample + goods
                            log("send sample")
                            sender ! s
                            // I changed statement below to reject old goods from session and include into sample only last two goods
                            session -> new Sample("", session, Set(goods)) //s
                        }

                        case None => {
                            val sample: Sample = new Sample("", session, Set(goods))
                            log("session not found")
                            sender ! sample
                            session -> sample
                        }
                    })
                }
            }
        }
    }
}
