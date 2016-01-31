package ru.wordmetrix.ratnest

import scala.actors.Actor
import scala.actors.Actor._

class Goods(val site: RatNest.Site, val id: RatNest.GoodsId) extends Serializable {
    val stage = 0
    val url = id

    override def toString = site + "!! " + id + "!"
}

class IndexOfGoods(val site: String) {
    lazy val map: Map[RatNest.GoodsId, Goods] = Map()
}

class GoodsSimpleMeta(site: RatNest.Site, id: RatNest.GoodsId, override val url: RatNest.Url) extends Goods(site, id) {
    override val stage = 1
    def this(goods: Goods, url: String) {
        this(goods.site, goods.id, url)
    }
    override def toString = "<div><a href=\"" + url + "\">" + url + "</a></div>"
}

class GoodsMeta(site: RatNest.Site, id: RatNest.GoodsId, url: RatNest.Url, val title: String, announce: String) extends GoodsSimpleMeta(site, id, url) {
    override val stage = 2
    def this(goods: GoodsSimpleMeta, title: String, announce: String) {
        this(goods.site, goods.id, goods.url, title, announce)
    }
    override def toString = "<div><a href=\"" + url + "\" title=\"" + announce + "\">" + title + "</a><div class=\"ui-helper-hidden\">" + announce + "</div></div>"
}

class Goodss(val metagor: Metagor, val database: String) extends Actor {
    /*
     *  The Idea: Goods Storage stores goods. Storage receives goods and store them by id. Also, Storage receives Set[RatNest.GoodsId] set and reduce it into
     *  Set[Goods]. Each time Storage  receives goods it sends it to MetaGor. 
     *
     */
    var goodss: Map[RatNest.GoodsId, Goods] = Map[RatNest.GoodsId, Goods]()

    def md5hex(x: String) = java.security.MessageDigest.getInstance("MD5").digest(x.getBytes).map("%2h" format math.abs(_)).mkString.replace(" ", "0")

    def goodsfilename(filename: RatNest.Url) =
        if (filename.length < 50) filename.replaceAll("/", "-") else {
            val rFilename = "http://([^/]+)/(.*)".r
            filename match {
                case rFilename(host, path) => host + "-" + md5hex(path)
                case x                     => md5hex(x)
            }
        }

    def act() = {
        val log = new Log("Goodss", "started")
        loop {
            react {
                case goods: Goods => {

                    def askmetagor(goods: Goods) = goods match {
                        case goods: GoodsMeta => try { // !!! filename too long 
                            new java.io.ObjectOutputStream(new java.io.FileOutputStream(database + goodsfilename(goods.id))).writeObject(goods)
                        } catch {
                            case x: java.io.FileNotFoundException => log(x)
                        }
                        case goods: Goods => { log("Ask metagor"); metagor ! goods }
                    }

                    goodss = goodss get goods.id match {
                        case Some(gs) if gs.stage < goods.stage => {
                            askmetagor(goods)
                            goodss + (goods.id -> goods)
                        }

                        case Some(gs) => {
                            log(goods.id, "has been already received")
                            goodss
                        }

                        case None => try {
                            log("Load...", database + goodsfilename(goods.id))
                            goodss + (goods.id -> new java.io.ObjectInputStream(new java.io.FileInputStream(database + goodsfilename(goods.id))).readObject().asInstanceOf[Goods])
                        } catch {
                            case x: java.io.FileNotFoundException => {
                                askmetagor(goods)
                                goodss
                            }
                        }
                    }
                }

                case googsids: Set[RatNest.GoodsId] => {
                    log("Fill GoodsSet")

                    sender ! (
                        googsids map (goodsid =>
                            goodss.get(goodsid) match {
                                case Some(goods) => goods
                                case None => {
                                    log("Read goods", goodsid)
                                    try {
                                        val goods = new java.io.ObjectInputStream(new java.io.FileInputStream(database + goodsid.replaceAll("/", "-"))).readObject().asInstanceOf[Goods]
                                        goodss = goodss + (goodsid -> goods)
                                        goods
                                    } catch {
                                        case x: java.io.FileNotFoundException => {
                                            this ! new Goods("", goodsid)
                                            None
                                        }
                                    }
                                }
                            }) filter (_ != None))
                }
            }
        }
    }
}
