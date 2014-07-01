/*
 * Network.scala
 * textnoise
 * 
 * Created by Илья Михальцов on 2014-05-13.
 * Copyright 2014 Илья Михальцов. All rights reserved.
 */


package com.morphe.noise.backend.neurals

import scala.collection._
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class Network (widths: Seq[Int]) {
    private val logger = LoggerFactory.getLogger(classOf[Network])
    private val prng = new scala.util.Random()

    // Deepness is a count of hidden layers plus output layer
    private val deepness = widths.length - 1
    
    private var state: State = null
    private val eta = 0.1
    private def initialWeight () = prng.nextDouble() * (if (prng.nextInt(50) == 0) 0.5 else 0.000001)

    private def newLayerGenerator(input: GenSeq[Double]) = {
        (n: Neuron) => {
            n.sameNeuronWithConnections(n.map({(n1: Neuron, w: Double, i: Int) => w + eta * n1.sumError(input) * n.y}))
        }
    }

    def initWithInput (input: GenSeq[InputNeuron], data:GenSeq[GenSeq[Neuron]]) {
        state = new State(input +: data)
    }
    
    private def newEmptyNeuron (layer: Int, i: Int) = {
        val nextWidth = if (layer == deepness) 0 else widths(layer + 1)
        new Neuron(((1 to nextWidth).par.map(_ => initialWeight)), layer, i)
    }
    
    def forStorage () = {
        state.layers.head.map({ case (n: InputNeuron) => n.basicNeuronWithSameConnections }) +: state.layers.tail
    }

    def initFromStorage (ns: GenSeq[GenSeq[Neuron]]) {
        initWithInput(ns.head.map(_.inputNeuronWithSameConnections(initialWeight)), ns.tail)
    }
    
    def initFromStorageWithInput (ns: GenSeq[GenSeq[Neuron]], input: GenSeq[Double]) {
        initWithInput((ns.head zip input).map({ case (n, i) => n.inputNeuronWithSameConnections(i) }), ns.tail)
    }
    
    def cleanNetworkWithInputs (input: GenSeq[Double]) {
        state = new State(input.zipWithIndex.map({ case (n, i) =>
            new InputNeuron(n, ((1 to widths(1)).par.map(_ => initialWeight)), 0, i)}) +:
            (1 to deepness).par.map({ (j) => (0 to widths(j)-1).par.map({ i => newEmptyNeuron(j, i) }) }))
    }
    
    def cleanNetwork () {
        cleanNetworkWithInputs((1 to widths(0)).par.map(_ => 0.0))
    }
    
    def updateWithInput (input: GenSeq[Double]) {
        state = new State((state.layer(0) zip input).map({ case (n: InputNeuron, i) => n.inputNeuronWithSameConnections(i) }) +:
            state.layers.tail)
    }
    
    def output () = {
        state.layers.last.map(_.y)
    }
    
    def updateWithOutput (output: GenSeq[Double]) = {
        state = state.mapNewLayer({ (ns: GenSeq[Neuron], rns: GenSeq[Neuron]) =>
            ns.map(newLayerGenerator(output))
            })
    }
}
