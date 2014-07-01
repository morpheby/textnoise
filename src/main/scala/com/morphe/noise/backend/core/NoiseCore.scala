/*
 * NoiseCore.scala
 * textnoise
 * 
 * Created by Илья Михальцов on 2014-05-13.
 * Copyright 2014 Илья Михальцов. All rights reserved.
 */


package com.morphe.noise.backend.core

import com.morphe.noise.backend.neurals.{Network, Neuron}
import com.morphe.noise.backend.textalytics.Api
import spray.json._
import scala.collection.immutable._
import scala.collection.parallel.ParSeq
import scala.concurrent.duration._
import org.slf4j.Logger
import org.slf4j.LoggerFactory

private object CoreJsonProtocol extends DefaultJsonProtocol {
    
    implicit object NeuronJsonFormat extends RootJsonFormat[Neuron] {
      def write(n: Neuron) = JsObject(
        "conns" -> JsArray(n.connections.map(_.toJson).toList),
        "layer" -> JsNumber(n.currentLayer),
        "i" -> JsNumber(n.index)
      )
      def read(value: JsValue) = {
        value.asJsObject.getFields("conns", "layer", "i") match {
          case scala.Seq(JsArray(connections), JsNumber(layer), JsNumber(i)) =>
            new Neuron(connections.par.map(_.convertTo[Double]), layer.toInt, i.toInt)
          case _ => throw new DeserializationException("Neuron expected")
        }
      }
    }
    

    implicit def parVectorFormat[T :JsonFormat] = new RootJsonFormat[ParSeq[T]] {
      def write(seq: ParSeq[T]) = JsArray(seq.map(_.toJson).toList)
      def read(value: JsValue) = value match {
        case JsArray(elements) => elements.par.map(_.convertTo[T])
        case x => deserializationError("Expected ParSeq as JsArray, but got " + x)
      }
    }
}

import CoreJsonProtocol._

class NoiseCore {
    private val logger = LoggerFactory.getLogger(classOf[NoiseCore])
    
    private val positiveScore = 1.0
    private val negativeScore = 0.0
    private val penaltyCoeff = 0.01
    private val degradeScore = 0.5
    
    private def indexForCategory (category: String) = {
        // Transforms category string (as of
        // https://textalytics.com/core/classmodels?model=IPTC_en) to
        // the according network index
        
        val categories = Map(
            1 -> (28, Map(
                5  -> 1,
                7  -> 1,
                10 -> 2,
                11 -> 7,
                15 -> 1,
                16 -> 1,
                17 -> 1,
                21 -> 1,
                22 -> 1,
                26 -> 4
                )),
            2 -> (16, Map(
                1  -> 10,
                2  -> 3,
                3  -> 3,
                4  -> 2,
                6  -> 2,
                7  -> 1,
                8  -> 3,
                9  -> 2,
                11 -> 2,
                12 -> 7
                )),
            3 -> (17, Map(
                6  -> 1,
                7  -> 1,
                10 -> 4,
                14 -> 1,
                15 -> 2
                )),
            4 -> (19, Map(
                1  -> 6,
                2  -> 7,
                3  -> 9,
                4  -> 7,
                5  -> 14,
                6  -> 22,
                7  -> 12,
                8  -> 36,
                9  -> 4,
                10 -> 11,
                11 -> 9,
                12 -> 5,
                13 -> 8,
                14 -> 5,
                15 -> 4,
                16 -> 57,
                17 -> 1,
                18 -> 1,
                19 -> 1
                )),
            5 -> (11, Map(
                5  -> 3,
                10 -> 4,
                11 -> 4
                )),
            6 -> (13, Map(
                2  -> 2,
                5  -> 2,
                6  -> 9,
                7  -> 1
                )),
            7 -> (19, Map(
                1  -> 9,
                3  -> 5,
                6  -> 2,
                7  -> 4,
                8  -> 1,
                11 -> 2,
                13 -> 1,
                14 -> 5,
                17 -> 3
                )),
            8 -> (8, Map(
                3  -> 5,
                5  -> 5,
                6  -> 1
                )),
            9 -> (16, Map(
                2  -> 3,
                3  -> 4,
                11 -> 2
                )),
            10 -> (18, Map(
                1  -> 4,
                3  -> 1,
                4  -> 3,
                7  -> 1
                )),
            11 -> (28, Map(
                1  -> 9,
                2  -> 4,
                3  -> 10,
                5  -> 1,
                6  -> 13,
                9  -> 2,
                10 -> 1,
                13 -> 1,
                16 -> 7,
                24 -> 3
                )),
            12 -> (27, Map(
                2  -> 2,
                6  -> 2,
                9  -> 13,
                14 -> 5,
                15 -> 1,
                23 -> 3,
                25 -> 4
                )),
            13 -> (23, Map(
                1  -> 4,
                2  -> 1,
                3  -> 5,
                4  -> 8,
                6  -> 1,
                10 -> 2
                )),
            14 -> (27, Map(
                3  -> 4,
                5  -> 1,
                6  -> 6, 
                10 -> 2,
                17 -> 1,
                24 -> 5,
                25 -> 5
                )),
            15 -> (0, Map[Int, Int]()),
            16 -> (12, Map(
                3  -> 5,
                5  -> 2,
                6  -> 1,
                9  -> 3,
                10 -> 1
                )),
            17 -> (5, Map(
                3  -> 1
                ))
            )
        
        val catNum = category.toInt
        val catDef = (catNum / 1000000, (catNum % 1000000) / 1000, catNum % 1000)
        catDef._1 - 1 + (1 to catDef._1 - 1).map(categories(_)).map({ case (c, m) => c + m.map(_._2).sum
            }).sum + (categories(catDef._1)._1 min catDef._2) +
            categories(catDef._1)._2.filter({ case (i, c) => i < catDef._2 }).map(_._2).sum +
            ((categories(catDef._1)._2 getOrElse (catDef._2, 0)) min catDef._3) + 1
    }
    
    private val totalCategories = indexForCategory("017005000") + 1
    
    private var newStorageCreatedMark = false
    
    private val storageFile = new java.io.File("storage.bin")
    if (!storageFile.exists) {
        storageFile.createNewFile
        newStorageCreatedMark = true
    }
    
    private val audioDirectory = new java.io.File("audio/")
    if (!audioDirectory.exists) {
        audioDirectory.mkdir
    }
    
    private val music = audioDirectory.listFiles
    
    logger.info("There are " ++ music.size.toString ++ " tracks in \'" ++ audioDirectory.toString ++ "\' directory")
    
    private val network: Network = new Network(List(totalCategories, 500, 500, 500, music.size))
    
    if (newStorageCreatedMark) {
        logger.info("Initializing new network...")
        network.cleanNetwork
    } else {
        try {
            logger.info("Attempting to read network...")
            // val input = new scala.io.BufferedSource(new java.io.FileInputStream(storageFile))
            // val data = input.getLines.fold("")(_++ "\n" ++_).parseJson.convertTo[ParSeq[ParSeq[Neuron]]]
            val input = new java.io.FileInputStream(storageFile)
            val data = resources.generated.BrainPack.parseFrom(input).layers.toVector.par.map(_.neurons.toVector.par.map(n =>
                new Neuron(n.connections.par, n.layer, n.index)))
            if (data.last.size != music.size) {
                logger.warn("Count of tracks doesn't match count of network outputs")
                logger.info("Initializing new network...")
                network.cleanNetwork
            } else network.initFromStorage(data)
        } catch {
            case e: Exception => {
                logger.error("Can't read network")
                logger.debug(e.toString)
                e.getStackTrace.map(_.toString).foreach(logger.trace(_))
                logger.info("Initializing new network...")
                network.cleanNetwork
            }
            case e: OutOfMemoryError => {
                logger.error("Unable to read network: not enough memory")
                logger.error("Please, increase heap memory for JVM")
                logger.info("Memory usage:\n\tTotal = " ++ (Runtime.getRuntime.totalMemory/(1024*1024)).toString ++
                    "MB\n\tMax = " ++ (Runtime.getRuntime.maxMemory/(1024*1024)).toString ++
                    "MB\n\tFree = " ++ (Runtime.getRuntime.freeMemory/(1024*1024)).toString ++ "MB")
                throw e
            }
        }
    }
    
    def save () {
        try {
            logger.info("Saving network...")
            // val output = new java.io.FileWriter(storageFile)
            // val net: ParSeq[ParSeq[Neuron]] = network.forStorage match { case n: ParSeq[ParSeq[Neuron]] => n }
            // val data = net.toJson.prettyPrint
            // output.write(data)
            val output = new java.io.FileOutputStream(storageFile)
            val net = network.forStorage
            val data = resources.generated.BrainPack(net.map({l: scala.collection.GenSeq[Neuron] => resources.generated.LayerPack(l.map(n =>
                new resources.generated.NeuronPack(n.connections.toVector, n.currentLayer, n.index)).toVector)}).toVector).writeTo(output)
            output.flush
            output.close
        } catch {
            case e: Exception => {
                logger.error("Unable to save network")
                logger.debug(e.toString)
                e.getStackTrace.map(_.toString).foreach(logger.trace(_))
            }
            case e: OutOfMemoryError => {
                logger.error("Unable to save network: not enough memory")
                logger.error("Please, increase heap memory for JVM")
                logger.info("Memory usage:\n\tTotal = " ++ (Runtime.getRuntime.totalMemory/(1024*1024)).toString ++
                    "MB\n\tMax = " ++ (Runtime.getRuntime.maxMemory/(1024*1024)).toString ++
                    "MB\n\tFree = " ++ (Runtime.getRuntime.freeMemory/(1024*1024)).toString ++ "MB")
                throw e
            }
        }
    }
    
    private val textalyticsApi = new Api("082570fbf0c4b31514cefaa11b6da751")
    private val prng = new scala.util.Random()
    
    def resetNetwork {
        logger.info("Resetting network...")
        network.cleanNetwork
    }
    
    def musicForText (text: String) = {
        logger.info("Sending text for analysis...")
        val result = scala.concurrent.Await.result(textalyticsApi.analyze(text), 10.second)
        logger.info("Processing data...")
        logger.trace(result.toString)
        val categories = result.categorization.map({ c => indexForCategory(c.code) -> (c.relevance/50.0) }).toMap
        val inputForNet = (0 to totalCategories).par.map({
            case 0 => result.sentimentScore
            case i => categories getOrElse (i, 0.0)
            })
        logger.trace("Updating network with new input: " ++ inputForNet.toString)
        network.updateWithInput(inputForNet)
        val output = network.output

        logger.trace("Network output: " ++ output.toString)
        val selections = (output.zipWithIndex.groupBy(_._1).maxBy(_._1)._2)
        
        val selection = selections(prng.nextInt(selections.size min music.size))
        logger.debug("Selection made (prob. " ++ selection._1.toString ++ "): " ++ music(selection._2).toString)
        music(selection._2)
    }
    
    def confirmMusicTrack (track: java.io.File) = updateWithTrackAndScore(track, positiveScore, -penaltyCoeff)

    // Note mistake: we don't account for outputs of concrete neurons
    private def updateWithTrackAndScore(track: java.io.File, score: Double, penalty: Double) {
        logger.info("Updating network data...")
        val trackIndex = music indexOf track
        val output = network.output.zipWithIndex.map({
            case (p, `trackIndex`) => score
            case (p, _) => p + degradeScore*penaltyCoeff
             })
         logger.trace(output.toString)
         network.updateWithOutput(output)
    }
    
    def discardMusicTrack (track: java.io.File) = updateWithTrackAndScore(track, negativeScore, penaltyCoeff)
}

