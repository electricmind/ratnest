package ru.wordmetrix.ratnest

import scala.actors.Actor

class Metagor extends Actor {
    /*
     *  Meta Server make from goods more detalized version.
     */

    def act() = {
        val log = new Log("Metagor", "started")

        loop {
            react {
                case goods: GoodsMeta => None //new java.io.ObjectOutputStream(new java.io.FileOutputStream(database + goods.id)).writeObject(goods)

                case goods: Goods => {
                    log("Metagor recombine goods", goods)
                    try {
                        sender ! (goods match {
                            case goods: GoodsSimpleMeta => {
                                val rTitle = "<title>([^<>]*)</title>".r
                                val rDescription = "<meta\\s+name=\"Description\"\\s+content=\"([^\"]*)\">".r
                                val url = goods.url

                                log("Eat HTML from", url)

                                val page = if (url contains "ach-fci") io.Source.fromURL(url, "koi8-r").mkString("") else io.Source.fromURL(url).mkString("")
                                val title = rTitle.findFirstIn(page) match {
                                    case Some(rTitle(title)) => title
                                    case None                => url
                                }

                                val description = rDescription.findFirstIn(page) match {
                                    case Some(rDescription(description)) => description
                                    case None                            => ""
                                }

                                new GoodsMeta(goods, title = title, announce = description)
                            }
                            case goods: Goods => new GoodsSimpleMeta(goods, url = goods.id) // Make request a map GoodsId -> Url and save It
                        })
                    } catch {
                        case x: java.io.IOException => log(x)
                    }
                }
            }
        }
    }
}
