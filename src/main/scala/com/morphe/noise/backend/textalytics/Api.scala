/*
 * Api.scala
 * textnoise
 * 
 * Created by Илья Михальцов on 2014-05-13.
 * Copyright 2014 Илья Михальцов. All rights reserved.
 */


package com.morphe.noise.backend.textalytics

import scala.collection.immutable._
import spray.json._
import dispatch._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

// https://textalytics.com/api/media/1.0/analyze
// apiKey = 082570fbf0c4b31514cefaa11b6da751

/*
import com.morphe.noise.backend.textalytics._
val api = new Api("082570fbf0c4b31514cefaa11b6da751")
val r = api.analyze("Hello world")
*/


private object ApiJsonProtocol extends DefaultJsonProtocol {
  implicit val category = rootFormat(jsonFormat3(Category))
  implicit val outDocument = rootFormat(jsonFormat3(OutputDocument))
  implicit val inDocument = rootFormat(jsonFormat3(InputDocument))
  implicit val status = rootFormat(jsonFormat2(Status))
  implicit val response = jsonFormat2(Response)
}

case class OutputDocument (
    id:         String,
    sentiment:  String,
    categorization: Seq[Category]
) {
    def sentimentScore () = {
        sentiment match {
            case "P" => 2.0
            case "NEU" => 0.2
            case "NONE" => -0.2
            case "N" => -2.0
        }
    }
}

case class Category (code: String, labels: Seq[String], relevance: Int)

case class InputDocument (
    id:         String,
    txt:        String,
    language:   String
)

case class Status (
    code:       String,
    message:    String
)

case class Response (
    status:     Status,
    result:     OutputDocument
)


class Api (apiKey: String) {
    import ApiJsonProtocol._

    private val VERSION = "1.0"
    private val textalyticsService = host("textalytics.com") / "api" / "media" /
        VERSION / "analyze"
    
    
    def analyze (text: String) = {
        val request = textalyticsService << Map(
            "key" -> apiKey,
            "fields" -> "sentiment|categories",
            "doc" -> Map("document" -> InputDocument(
                java.util.UUID.randomUUID.toString,
                // "0",
                text,
                "en")).toJson.compactPrint
            )
        Http(request OK as.String) map { _.parseJson.convertTo[Response].result
        }
    }
}

