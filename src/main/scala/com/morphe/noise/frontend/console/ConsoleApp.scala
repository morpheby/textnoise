/*
 * ConsoleApp.scala
 * textnoise
 * 
 * Created by Илья Михальцов on 2014-05-14.
 * Copyright 2014 Илья Михальцов. All rights reserved.
 */


package com.morphe.noise.frontend.console

import com.morphe.noise.backend.core.NoiseCore
import scala.Console
import Console._
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.sys.process._
import akka.actor._
import com.morphe.noise.server.NoiseServer


class ConsoleApp {
}


object ConsoleApp {
    
    val header = BLUE ++ """
********************************************************************************
                         ,___   ___   (,) ___   ___
                         / , ) / . ) / / ( __/ / ,_)
                        / / / (___/ /_/ /___) (___/
********************************************************************************

############ Text reader with intellectual background music player #############
""" ++ RESET

    val menu = BOLD ++ """
Main menu:
1. Read text file
2. Reset
3. Input automated training file
4. Save network
5. Save and exit
6. Exit without saving
0. Start server
""" ++ RESET
    
    private val logger = LoggerFactory.getLogger(classOf[ConsoleApp])

    def main(args: Array[String]) {
        logger.info("Starting up...")
        
        val core = new NoiseCore
        
        var exit = false
        
        println(header)
        
        while (!exit) {
            println(menu)
            Console.readLine match {
                case "1" => {
                    val playerLog = new java.io.File("player.log")
                    if (!playerLog.exists) playerLog.createNewFile
                    val f = new java.io.File(Console.readLine("Enter file name: "))
                    if (f.exists) {
                        val input = new scala.io.BufferedSource(new java.io.FileInputStream(f))
                        val data = input.getLines.fold("")(_++ "\n" ++_)
                        Console.println(BOLD ++ data ++ RESET)
                        Console.println("***")
                        
                        val m = core.musicForText(data)
                        
                        val playerLogger = ProcessLogger(playerLog)
                        
                        val player = Process("mpg123", Seq(m.getPath)).run(playerLogger)
                        logger.info("Playing \'" ++ m.toString ++ "\'")
                        
                        Console.println("\nEnter \'y\' to like, \'n\' to dislike, any other key to quit")
                        
                        Console.readLine match {
                            case "y" => core.confirmMusicTrack(m)
                            case "n" => core.discardMusicTrack(m)
                            case _ => ()
                        }
                        
                        player.destroy
                        player.exitValue
                        playerLogger.close
                        
                    } else println(RED ++ "File \'" ++ f.toString ++ "\' not found" ++ RESET)
                }
                case "2" => core.resetNetwork
                case "3" => {
                }
                case "4" => core.save
                case "5" => {
                    core.save
                    exit = true
                }
                case "6" => exit = true
                case "0" => {
                    NoiseServer.noiseServer ! NoiseServer.ServerStart(core)
                }
                case _ => ()
            }
        }
        NoiseServer.exit
    }
}
