/*
 * NoiseServer.scala
 * textnoise
 * 
 * Created by Илья Михальцов on 2014-05-14.
 * Copyright 2014 Илья Михальцов. All rights reserved.
 */

package com.morphe.noise.server

import akka.actor._
import com.morphe.noise.backend.core.NoiseCore
import scala.concurrent.duration._
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import akka.io.IO
import spray.can.Http
import spray.http._

object NoiseServer {

    implicit val system = ActorSystem()
    
    private val props = Props[NoiseServer]
    val noiseServer: ActorRef = system.actorOf(props)

    def exit () {
        noiseServer ! ServerShutdown()
    }
    
    case class ServerStart(core: NoiseCore)

    case class ServerShutdown()
}

class NoiseServer extends Actor {
    private val logger = LoggerFactory.getLogger(classOf[NoiseServer])
    
    import NoiseServer._

    // override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
    //   case _: ArithmeticException      => Resume
    //   case _: NullPointerException     => Restart
    //   case _: IllegalArgumentException => Stop
    //   case _: Exception                => Escalate
    // }
    
    var core: NoiseCore = null
    
    var lastTrack: java.io.File = null

    def receive = {
        case ServerStart(c)           => {
            core = c
            start
        }

        case _: ServerShutdown        => {
            stop
            context.stop(self)
        }
        
        case _: Http.Connected        => sender ! Http.Register(self)
        
        case HttpRequest(HttpMethods.POST, Uri.Path("/analyze"), _, entity, _) => {
            logger.info("Processing remote request...")
            
            val m = core.musicForText(entity.asString)
            lastTrack = m
            
            val e = HttpEntity(ContentType(MediaTypes.`audio/mpeg`), HttpData(m))

            logger.info("Sending response...")
            
            sender ! HttpResponse(entity = e)
        }
        
        case HttpRequest(HttpMethods.GET, Uri.Path("/like"), _, _, _) => {
            logger.info("\'Liking\' last remote request...")
            
            core.confirmMusicTrack(lastTrack)
            
            sender ! HttpResponse()
        }
        
        case HttpRequest(HttpMethods.GET, Uri.Path("/dislike"), _, _, _) => {
            logger.info("\'Disliking\' last remote request...")
            
            core.discardMusicTrack(lastTrack)
            
            sender ! HttpResponse()
        }
        
        case r: HttpRequest => {
            logger.warn("Invalid remote request")
            logger.trace(r.headers.toString)
            logger.trace(r.uri.toString)
            sender ! HttpResponse(status = 404, entity = "Unknown resource!")
        }
    }
    
    def start {
        logger.info("Starting server...")
        IO(Http) ! Http.Bind(self, interface = "0.0.0.0", port = 23413)
    }
    
    def stop {
        logger.info("Stopping server...")
        IO(Http) ! Http.Unbind(5 second)
    }

}
