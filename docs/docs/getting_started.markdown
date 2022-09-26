---
layout: page
title: Getting Started
nav_order: 2 
permalink: /getting-started/
---
## Getting Started
In this section you will find the instructions for running a minimal example application of _Phenesthe_. For more complicated applications please read the documentation section.
 
### Running _Phenesthe_ on the included 'Alice & Bob' sample

The ``samples/alice_and_bob/`` folder includes an example usage of _Phenesthe_. The ``definitions.prolog`` contains definitions of temporal phenomena, the ``narrative.prolog`` contains the input phenomena i.e., the input  and ``run.prolog`` loads _Phenesthe_ and preprocesses the phenomena definitions.

0. Have a look at the phenomena definitions in ``definitions.prolog`` and the narrative at ``narrative.prolog`` in the ``samples/alice_and_bob`` folder.

1. Load ``run.prolog`` in SWI-Prolog. Calling ``query(5)`` loads the input phenomena that arrived until '5' and performs a recognition query given that time is '5'.
```sh
cd samples/alice_and_bob
swipl -l run.prolog
?- query(5). % asserts the input (until t=5) and performs recognition of phenomena at t=5
```
2. In order to print the recognised events you will have to use the ``event_instants(X,T)`` predicate. Calling ``event_instants(X,T)`` will print a user defined event (X) and the instant list (T) at which it's true. For example the event ``unchanged(bob)`` is true at the instant(s) included in list ``T=[3]``.
```prolog
?- event_instants(X,T). % outputs the recognised events
X = unchanged(bob),
T = [3] ;
 ...
```
3. ``state_intervals(X,I)`` outputs user defined states (X) and the interval list (I) at which they hold. For example the state ``possess(alice,wallet)`` holds for the interval(s) included in list ``I = [[4, inf]] ``.
```prolog
?- state_intervals(X,I). % outputs the recognised states
X = possess(alice, wallet),
I = [[4, inf]] ;
...
```
4. ``dynamic_phenomena_intervals(X,I)`` outputs the user defined dynamic temporal phenomena (X) and the intervals (I) at which they hold. For example the state ``drops_objects_when_hungry(bob)`` holds for the interval(s) included in list ``I = [[1, inf]] ``.
```prolog
?- dynamic_phenomenon_intervals(X,I). % outputs the recognised dynamic phenomena
X = drops_objects_when_hungry(bob),
I = [[1, inf]].
```

---
