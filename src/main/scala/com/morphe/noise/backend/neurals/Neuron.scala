/*
 * Neuron.scala
 * textnoise
 * 
 * Created by Илья Михальцов on 2014-05-13.
 * Copyright 2014 Илья Михальцов. All rights reserved.
 */


package com.morphe.noise.backend.neurals

import scala.collection._
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class Neuron (val connections: GenSeq[Double], val currentLayer: Int, val index: Int) {
    private val logger = LoggerFactory.getLogger(classOf[Neuron])
    
    lazy val y: Double = scala.math.tanh(v)
    lazy val v: Double = state.layer(currentLayer-1).map(_.vForNeuron(index)).reduce(_+_)
    
    def map[A] (m: (Neuron, Double) => A) = {
        (state.layer(currentLayer+1) zip connections).map({
            case (n, w) => m(n, w)
            })
    }
    
    def map[A] (m: (Neuron, Double, Int) => A) = {
        (state.layer(currentLayer+1).zipWithIndex zip connections).map({
            case ((n, i), w) => m(n, w, i)
            })
    }
    
    protected var state: State = null
    
    def setState (s: State) {
        state = s
    }
    
    private var cachedError = Double.NaN
    private var cachedData: GenSeq[Double] = null
    
    private def partialError (input: Double) = {
        input * scala.math.pow(1/scala.math.cosh(v), 2.0)
    }
    
    def sumError (data: GenSeq[Double]): Double = {
        if (cachedError.isNaN || cachedData != data) {
            cachedError = if (connections.size > 0) {
                partialError(this.map({ (n, w) => n.sumError(data)*w }).reduce(_+_))
            } else {
                partialError(data(index) - y)
            }
            cachedData = data
        }
        cachedError
    }
    
    def deltaError (data: GenSeq[Double], target: Int): Double = {
        state.layer(currentLayer+1)(target).sumError(data)
    }
    
    def vForNeuron (i: Int) = y * connections(i)
    
    def sameNeuronWithConnections (conns: GenSeq[Double]) = {
        val n = new Neuron(conns, currentLayer, index)
        n.state = state
        n
    }

    def inputNeuronWithSameConnections (newInput: Double) = {
        new InputNeuron(newInput, connections, currentLayer, index)
    }
}
