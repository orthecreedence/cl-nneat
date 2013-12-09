# cl-nneat (generic AI combining NN/GA)

This library is an adaptation of the [NNEAT](http://en.wikipedia.org/wiki/Neuroevolution_of_augmenting_topologies)
algorithm. It follows it loosely, probably to its own detriment, but the results
are still somewhat favorable.

## Overview

NNEAT defines a neural network architecture where a network can be built from
neurons in a feed-forwardish manner. There are a fixed number of inputs and a
fixed number of outputs. The algorithm evolves the network over time. This
includes not only weights between neurons (which strengthens/weakens the 
firing potention) but also the structure itself. Neurons and connections can be
added and removed. Connections can be made backwards as well, meaning a neuron
near the output layer can connect to a neuron near the input layer, creating a
"memory" of sorts.

### Genomes

The genome is what describes a network: its connections/weights and the neurons.
Generally in a genetic algorithm, a genome is a description of the proposed
solution. Because the genome here is potentially very complicated and difficult
to "mate," instead of it being a description of the network, it's a description
of *how to build the network*.

In other words, the genomes in cl-nneat are a set of sequential instructions,
starting from the beginning, on how to create the network: "create connection
between neuron 2 and 3, add neuron 5, connect it to 2 and 6, ..."

Because these instructions start from a base point and complicate the net as
they grow, they can be effectively mated.

## Demo
The demo isn't going to work without including some packages I have laying
around on one of my computers.

## TODO
- Algorithm updates. Specifically, need to penalize genomes that deviate
*without* giving good returns. This will encourage a balance between evolution
and "doing what works."
- Genome compression. If you add a connection, then remove the same connection,
you may as well not have added the connection at all...however, the two
instructions would cound against the genome's complexity (which should penalize
fitness). Compression may be tricky because once you modify the base of one
genome and not another, you risk genetic corruption.
- Working demo. The demo works and it's awesome, but it uses packages you've 
never heard of and uses a bunch of really, really old SDL code. I want to
update it to use GLFW and itnernalize the random packages used.

