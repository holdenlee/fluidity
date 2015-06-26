About
=====

Implements the Copycat (Parallel terraced scan) architecture in Hofstadter's Fluid Concepts and Creative Analogies. Applies it towards the "SeekWhence" program, which finds the rule behind a integer sequence given the input.

What's here
===========

Defined the datatypes in PTScan

* Codelets, Coderack
* Slipnet
* World

I'm trying to make this as modular as possible (though the PTScan architecture doesn't lend itself to this so easily). Right now it's missing a lot of the important stuff: activations, relative strengths, etc. It can take an abitrary workspace type w.

This is specialized for SeekWhence. The Workspace is defined there as a graph of subsequences the program has found explanations for.

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
* Codelets
  * Generalizor: how to select the formulas to generalize?
