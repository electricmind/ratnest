package ru.wordmetrix.ratnest
import ru.wordmetrix.ratnest.Server
import scala.actors.Actor

/*

 react {
   case q : QQ => {
     react {
       case e : EE => {
         acceptor ! (q,e)  // do something with q & e 
       }
     } 
   }
 }*/

/*
                 // Get id from query
                 //  ... or Get id && url from reference 
                 // Request goods from storage
                 // Save goods into storage (storage ! new Goods(id, url))

                 // Make a basket from query
                 //  .. or get basket from session
                 //  .. or make empty basket
                 // Add id into basket, ( and shedule basket for future save
                 // respond with standart response (from file "/client/add.js"
                 // If request has commit key commit basket
                 // If request has no commit key ;) don't commit basket
                 // If request has every time commit key :) commit basket


                 val respond = new ResponseSuccess(Map(),io.Source.fromInputStream(new java.io.FileInputStream(root + f)).getLines.mkString(""))
                 if (cookie.contains("session")) respond else respond //+ (session,randomId)
               }

               case Some(Request(_,"/suggest",query,cookie)) => { // Ask for suggestion
                 // Make suggestion from index
                 // .. or compute suggestion
                 // .. or send suggestion to recompute and make simple suggestion
                 // .. generate standart suggestion report 
                 // .. List() toJSON

                 val respond = new ResponseSuccess(Map(),io.Source.fromInputStream(new java.io.FileInputStream(root + f)).getLines.mkString(""))
                 if (cookie.contains("session")) respond else respond //+ (session,randomId)
               }


               case Some(Request(_,f,query,cookie)) => {  // Accomplish request of file
                 val respond = new ResponseSuccess(Map(),io.Source.fromInputStream(new java.io.FileInputStream(root + f)).getLines.mkString(""))
                 if (cookie.contains("session")) respond else respond //+ (session,randomId)
               }

*/

class Dispatcher extends Actor {
    val log = new Log("Dispatcher")
    def act() = {

        def queue(sockets: List[java.net.Socket], servers: List[Server]) : Unit = {
            log("Servers:", servers.length, "Sockets:", sockets.length)
            react({
                case server: Server => {
                    sockets match {
                        case socket :: sockets => {
                            server ! socket
                            queue(sockets, servers)
                        }
                        case List() => queue(sockets, server :: server :: servers)
                    }
                }
                case socket: java.net.Socket => {
                    servers match {
                        case server :: servers => {
                            server ! socket
                            queue(sockets, servers)
                        }
                        case List() => { queue(socket :: socket :: sockets, servers) }
                    }
                }

            })
        }
        queue(List(), List())
    }
}

