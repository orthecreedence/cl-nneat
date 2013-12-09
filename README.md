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

## Demo (SDL required)
```common-lisp
;; cl-nneat must be in your local-projects/ directory
(ql:quickload :cl-nneat-demo)
(cl-nneat-demo:start)
```

The demo consists of a world, populations, and food. There are two population
types: scavengers (who eat the food) and predators who eat the scavengers.

Here's the basic setup (some of this may be slightly incorrect since I haven't
worked on the demo in ages now):

- All animals have "awareness". The have a field of vision and a visual
distance they can see. The are aware of objects in this field of vision, be they
food, predators, etc.
- Animals must chew. When they eat food or another animal, they have to chew for
a certain amount of time.
- Predators can bite scavengers, and this removes fitness. If the prey's fitness
falls below 0, they die and are removed from the gene pool.
- Animals try to be self-aware...they know when they are chewing and when they
are attacked, they know what direction they're going, how fast, about other
animals of the same type in their visual field, etc. This can lead to somehwat
complex behavior where groups of animals will "flock" together.

As animals grow more complex, the number of genetic mutations they have is shown
on their bodies.

### config
Feel free to play the the values in `demo/config.lisp`. You can create some
interesting scenarios and also tweak the general brain settings to try to gain
more realism or performance.

### keymap
- `q`/`esc`: Quit
- `r`: Reset simulation. All populatations will get randomized (simple) brains.
- `p`: Pause simulation.

## TODO
- Algorithm/demo documentation. Really need this because it's hard to know what
the hell is going on without it.
- Algorithm updates. Specifically, need to penalize genomes that deviate
*without* giving good returns. This will encourage a balance between evolution
and "doing what works."
- Genome compression. If you add a connection, then remove the same connection,
you may as well not have added the connection at all...however, the two
instructions would cound against the genome's complexity (which should penalize
fitness). Compression may be tricky because once you modify the base of one
genome and not another, you risk genetic corruption.
- Simulation save. Would be cool if a population did extremely well to save them
to disk.
- Brain visualization. Would be great if you could click an animal and see how
its brain is structured.

## License
MIT, as always. However, improvements to the algorithm or demo settings would be
greatly appreciated =].

