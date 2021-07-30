# Phenesthe
'_Phenesthe_' (orig. φαίνεσθαι) is a system for the representation and processing of instantaneous and durative temporal phenomena. Temporal phenomena may be:
- events (instantaneous)
- states (durative)
- dynamic temporal phenomena (durative)

Given an input stream of input phenomena '_Phenesthe_' will produce the instants or intervals at which user defined temporal phenomena are true or hold. More details can be found in "M. Pitsikalis, A. Lisitsa and Shan Luo. 2021. Representation and Processing of Instantaneous and Durative Temporal Phenomena. (TBA)"
## Features

- A user friendly language that allows the definition of temporal phenomena
- Formal description of the semantics and execution semantics (to be made public soon)
- An engine that allows the recognition of the instants and intervals at which the user defined phenomena are true/hold

## Installation & Getting Started
'_Phenesthe'_ requires a prolog installation for it to run. Specifically it has been tested under [SWI-Prolog](www.swi-prolog.org) 7.6.4 and [YAP](https://www.dcc.fc.up.pt/~vsc/yap/) 6.2.2.

The samples folder includes an example usage of '_Phenesthe'_. ```definitions.prolog``` contains definitions of temporal phenomena, the ```narrative.prolog``` contains the input phenomena and 'run.prolog' loads _Phenesthe_ and preprocesses the phenomena definitions.

Step 0 (optional): Have a look at the phenomena definitions in ```samples/definitions.prolog``` and the narrative at ```samples/narrative.prolog```.

Step 1 (loading): Load ```run.prolog``` in swipl. Calling ```query(5)``` loads the input phenomena that arrived until '5' and performs a recognition query given that time is '5'.
```sh
cd samples
swipl -l run.prolog
?- query(5). % performs recognition of phenomena at t=5
```
Step 2 (events output): ```event_instants(X,T)``` outputs user defined events (X) and the instants (T) at which they are true. For example the event ```unchanged(bob)``` is true at the instant(s) included in list ```T=[3]```.
```prolog
?- query(5). % performs recognition of phenomena at t=5
?- event_instants(X,T). % outputs the recognised events
X = unchanged(bob),
T = [3] ;
 ...
```
Step 3 (states output): ```state_intervals(X,I)``` outputs user defined states (X) and the intervals (I) at which they hold. For example the state ```possess(alice,wallet)``` holds for the interval(s) included in list ```I = [[4, inf]] ```.
```prolog
?- state_intervals(X,I). % outputs the recognised states
X = possess(alice, wallet),
I = [[4, inf]] ;
...
```
Step 3 (dynamic phenomonena output): ```dynamic_phenomena_intervals(X,I)``` outputs the user defined dynamic temporal phenomena (X) and the intervals (I) at which they hold. For example the state ```drops_objects_when_hungry(bob)``` holds for the interval(s) included in list ```I = [[1, inf]] ```.
```prolog
?- dynamic_phenomenon_intervals(X,I). % outputs the recognised dynamic phenomena
X = drops_objects_when_hungry(bob),
I = [[1, inf]].
```
## Disclaimer
While _Phenesthe_ has undergone through testing, it's still under development.  Therefore some bugs may exist :) . 
## License
This project is licensed under the terms of the [GNU General Public License](https://www.gnu.org/licenses/gpl-3.0.html)

