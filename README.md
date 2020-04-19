# PureScript-Zeta

[![Build Status](https://travis-ci.org/athanclark/purescript-zeta.svg?branch=master)](https://travis-ci.org/athanclark/purescript-zeta)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-zeta/badge)](https://pursuit.purescript.org/packages/purescript-zeta)

> Signals from outer space

This library is a competing Signaling implementation, modeled after
Bodil's [purescript-signal library](https://pursuit.purescript.org/packages/purescript-signal).

It's differences?

- Zeta Signals are traversable, but not functors, because mapping over a signal creates a new one (signals are references)
- There's an included `Foreign.Object.Object`-based signaling system for dynamic handler sets


Generally, I just wanted a pure implementation of Bodil's Signal library - this one
is probably much slower, and will break api compliance, so fair warning!


Philosophically, this library complements [purescript-queue](https://pursuit.purescript.org/packages/purescript-queue/1.1.1)
in the following ways:

- Queues pass messages to handlers, while Signals maintain a single state acknowledged by handlers
- Both Queues and Signals can have multiple handlers
- Signal handlers are informed of a whole value when reassigned, while Queues are informed of intentional message submissions
