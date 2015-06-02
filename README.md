About
=====

Implements the Copycat (Parallel terraced scan) architecture in Hofstadter's Fluid Concepts and Creative Analogies.

What's here
===========

Defined the datatypes

* Codelets, Coderack
* Slipnet
* World

(Workspace is just w for now.)

Dynamics

* Can pick and execute a codelet.

Todo
====

* World
  * Figure out how to change temperature. Which systems should effect the change?
* Slipnet
  * How to change activations in Slipnet? (Maybe let codelets do this?)
  * How to spread activation in Slipnet?
  * Should concepts be able to produce codelets? Or have codelets do all the work: codelets monitors activation of concepts?
  * Decay of nodes.
* Workspace
  * What's the right data structure? Could depend on domain. A graph? Hypergraph?
    * Have relationships be nodes themselves?
