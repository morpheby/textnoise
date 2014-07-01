/*
 * InputNeuron.scala
 * textnoise
 * 
 * Created by Илья Михальцов on 2014-05-13.
 * Copyright 2014 Илья Михальцов. All rights reserved.
 */


package com.morphe.noise.backend.neurals

import scala.collection._

class InputNeuron (input: Double, connections: GenSeq[Double], currentLayer: Int, index: Int)
extends Neuron(connections, currentLayer, index) {
    override lazy val y: Double = input
    override lazy val v: Double = 0.0
    
    override def sameNeuronWithConnections (conns: GenSeq[Double]) = {
        val n = new InputNeuron(input, conns, currentLayer, index)
        n.state = state
        n
    }
    
    def basicNeuronWithSameConnections () = {
        new Neuron(connections, currentLayer, index)
    }
}
