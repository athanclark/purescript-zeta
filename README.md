# PureScript-Zeta

> Signals from outer space

This library is a competing Signaling implementation, modeled after
Bodil's [purescript-signal library](https://pursuit.purescript.org/packages/purescript-signal/9.0.0).

It's differences?

- The signal effects for all handlers are explicit in Zeta
- Zeta Signals are traversable, but not functors, because mapping over a signal creates a new one (signals are references)
- There's an included `StrMap`-based signaling system, but it's not that useful


Generally, I just wanted a pure implementation of Bodil's Signal library - this one
is probably much slower, and will break api compliance, so fair warning!


Philosophically, this library competes with [purescript-queue](https://pursuit.purescript.org/packages/purescript-queue/1.1.1)
in the following ways:

- Queues pass messages to handlers, while Signals maintain a single state acknowledged by handlers
- Both Queues and Signals can have multiple handlers
- Signal handlers are informed of a state change, while Queues are informed of message subissions
- Signals can be manipulated by time or rate debouncing easily - signals are meant to be composed into new onces, while Queues aren't merged or composed
