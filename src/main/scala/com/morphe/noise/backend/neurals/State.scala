/*
 * State.scala
 * textnoise
 * 
 * Created by Илья Михальцов on 2014-05-13.
 * Copyright 2014 Илья Михальцов. All rights reserved.
 */


package com.morphe.noise.backend.neurals

import scala.collection._

class State (val layers: GenSeq[GenSeq[Neuron]]) {
    // Allows layers through 0 (input), 1..deepness-1 of hidden layers
    // and deepness (output)
    def layer(i: Int) = {
        layers(i)
    }
    
    for (ns <- layers) {
        for (n <- ns) {
            n.setState(this)
        }
    }
    
    // Previous is meant in terms of back-propogationm so it is one, that is n+1
    def mapNewLayer(mapper: (GenSeq[Neuron], GenSeq[Neuron]) => GenSeq[Neuron]) = {
        val invLayers = layers.reverse
        val newLayers: GenSeq[GenSeq[Neuron]] = (invLayers.tail zip invLayers.init).map({
            case (xs, ys) => mapper(xs, ys)
            }).reverse
        new State(newLayers :+ layers.last.map( n => n.sameNeuronWithConnections(n.connections) ))
    }
    
    
}
