# Phenesthe

_Phenesthe_ (orig. φαίνεσθαι) is a system for the representation and processing of instantaneous and durative temporal phenomena. Temporal phenomena may be:
- events (instantaneous)
- states (durative)
- dynamic temporal phenomena (durative)

Given an input stream of input phenomena _Phenesthe_ will produce the instants or intervals at which user defined temporal phenomena are true or hold. More details can be found in "_M. Pitsikalis, A. Lisitsa and S. Luo. 2021. Representation and Processing of Instantaneous and Durative Temporal Phenomena. Pre-proceedings paper presented at the 31st International Symposium on Logic-Based Program Synthesis and Transformation (LOPSTR 2021) [arXiv:2108.13365](https://arxiv.org/abs/2108.13365)_"

## Features

- A user friendly language that allows the definition of temporal phenomena
- Formal description of the semantics and execution semantics
- An engine that allows the recognition of the instants and intervals at which the user defined phenomena are true/hold

## Installation & Getting Started

Running _Phenesthe_ requires a prolog installation. Specifically it has been tested under [SWI-Prolog](www.swi-prolog.org) 7.6.4 and 8.2.4.

#### Running _Phenesthe_ on the included 'Alice & Bob' sample

The ```samples/alice_and_bob/``` folder includes an example usage of _Phenesthe_. The ```definitions.prolog``` contains definitions of temporal phenomena, the ```narrative.prolog``` contains the input phenomena and 'run.prolog' loads _Phenesthe_ and preprocesses the phenomena definitions.


**Step 0 (optional):** Have a look at the phenomena definitions in ```definitions.prolog``` and the narrative at ```narrative.prolog``` in the ```samples/alice_and_bob``` folder.

**Step 1 (loading):** Load ```run.prolog``` in SWI-Prolog. Calling ```query(5)``` loads the input phenomena that arrived until '5' and performs a recognition query given that time is '5'.
```sh
cd samples/alice_and_bob
swipl -l run.prolog
?- query(5). % performs recognition of phenomena at t=5
```
**Step 2 (events output):** ```event_instants(X,T)``` outputs user defined events (X) and the instants (T) at which they are true. For example the event ```unchanged(bob)``` is true at the instant(s) included in list ```T=[3]```.
```prolog
?- event_instants(X,T). % outputs the recognised events
X = unchanged(bob),
T = [3] ;
 ...
```
**Step 3 (states output):** ```state_intervals(X,I)``` outputs user defined states (X) and the intervals (I) at which they hold. For example the state ```possess(alice,wallet)``` holds for the interval(s) included in list ```I = [[4, inf]] ```.
```prolog
?- state_intervals(X,I). % outputs the recognised states
X = possess(alice, wallet),
I = [[4, inf]] ;
...
```
**Step 4 (dynamic phenomonena output):** ```dynamic_phenomena_intervals(X,I)``` outputs the user defined dynamic temporal phenomena (X) and the intervals (I) at which they hold. For example the state ```drops_objects_when_hungry(bob)``` holds for the interval(s) included in list ```I = [[1, inf]] ```.
```prolog
?- dynamic_phenomenon_intervals(X,I). % outputs the recognised dynamic phenomena
X = drops_objects_when_hungry(bob),
I = [[1, inf]].
```
### Use Phenesthe on a file stream

Phenesthe has a built-in parser for performing Complex Event Processing on a file stream. After the initiliasition of Phenesthe, Complex Event Processing can be performed on a input file stream with the use of the ```queries_on_fstream/7``` predicate.
```prolog
 queries_on_fstream(+InputFile, +LogFile, +ResultsFile, +Start, +End, +Step, +Window).
```
Where the arguments are:
- InputFile: Name of the input file.
- LogFile: Name of the log file (stattistics for each temporal query).
- ResultsFile: Printed instants and intervals at which user defined phenomena are true hold.
- Start: Timestamp to start processing.
- End: Timestampt to end processing.
- Step: Window sliding step.
- Window: Window size.

The input file must be appropriately formatted.
An input event ```eventName(arg1,arg2,...,argN)``` occuring at instant 't1' should appear as follows:
```
input_event_instant(eventName(arg1,arg2,...,argN),t1)
```
an input state ```stateName(arg1,arg2,...,argN)``` holding for the intervals ```[t1s,t1e],...,[tNs,tNe]``` should appear as follows:
```
input_state_interval(eventName(arg1,arg2,...,argN),[t1s,t1e])
...
input_state_interval(eventName(arg1,arg2,...,argN),[tNs,tNe])
```
finally, a dynamic temporal phenomenon holding for the intervals ```[t1s,t1e],...,[tNs,tNe]``` should appear as follows:
```
input_dynamic_phenomenon_interval(eventName(arg1,arg2,...,argN),[t1s,t1e])
...
input_dynamic_phenomenon_interval(eventName(arg1,arg2,...,argN),[tNs,tNe])
```
**Note that the input is expected to be ordered (for now).**

### Using the Phenesthe on the maritime sample
This part of the tutorial shows how to run Phenesthe on the maritime use case of the included samples. Furthermore, it demonstrates the usage of the file stream parser.

**Step 0** Navigate to the ```samples/maritime``` folder and extract the dataset ```BREST_phenesthe_input.tar.gz```
```bash
tar -xvf BREST_phenesthe_input.tar.gz
```

**Step 1** Open SWI-Prolog and load the initiliasition file.
```prolog
?- ['init.prolog'].
```

**Step 3** Perform Complex Event Processing using one week of the available input data.
```prolog
?- queries_on_fstream('BREST_phenesthe.input','logs/log7200.csv','results/results7200.out',1443650401,1443650401,7200,7200).
```

### Writing definitions

Writing phenomena definitions in _Phenesthe_  is a very simple task! In a file called for example ```definitions.prolog```, the user should first declare the input phenomena. As in the included sample this can be done using ```input_phenomenon/2``` as shown below:
```prolog
input_phenomenon(drop(_Person,_Object),event).
input_phenomenon(pickup(_Person,_Object),event).
input_phenomenon(ate(_Person,_Food),event).
input_phenomenon(hunger(_Person),event).
```
The first argument of ```input_phenomenon``` is the phenomenon predicate (e.g., ```drop(_Person,_Object)```), while the second argument denotes the type of the phenomenon (```event/state/dynamic_phenomenon```). As soon as, the required information for the input phenomena is provided the user can write definitions of phenomena as follows.

**Events** are true on instants and can be defined in terms of the ```and/or/tnot``` temporal connectives between _instant formulae_ or the start/end of _disjoint interval formulae_. For example:
```prolog
event_phenomenon gain(Person) :=
    pickup(Person, _) and
    tnot drop(Person, _).
```
Here, ```gain(Person)``` is an event phenomenon that is true on the instants a ```Person``` picks up an object and at the same time he doesn't drop something.

**States** are durative phenomena that hold true on disjoint intervals. States can be defined using _disjoint interval formulae_ i.e.,
 - with the use of the maximal range operator ```~>``` between two _instant formulae_, 
 - or they can be defined using the ```union/intersection/complement``` temporal operators between _disjoint interval formulae_.

For example consider the state definition below:
```prolog
state_phenomenon happy_with_money(Person) :=
    (gain(Person) ~> loss(Person)) %happy
     intersection possess(Person,wallet).
```
The ```happy_with_money(Person)```  phenomenon holds true  for the periods a ```Person``` is happy and has money. In detail, a ```Person``` starts being happy when he gains something, and continues to be happy until he loses something.

**Dynamic temporal phenomena** are durative phenomena that may hold on non-disjoint intervals. Dynamic temporal phenomena are defined in terms of _non disjoint interval formulae_. For example: 
```prolog
dynamic_phenomenon drops_objects_when_hungry(Person) :=
    hungry(Person) contains drop(Person,_Object).
```
``` drops_objects_when_hungry(Person)``` is a dynamic temporal phenomenon that holds true for the intervals a person is hungry and during that intervals he dropped at least one object.

More details about writing definitions can be provided by the EBNF grammar included below.
```ebnf
definitions = eventDefinition | stateDefinition | dynamicDefinition;

eventDefinition = "event_phenomenon" event ":=" instantExpression ".";
stateDefinition = "state_phenomenon" state ":=" intervalOperation ".";
dynamicDefinition = "dynamic_phenomenon" dynamic ":=" intervalRelation ".";

event = eventName(...);
state = stateName(...);
dynamic = dynamicPhenomenonName(...);

temporalExpression = instantExpression | intervalExpression;

instantExpression = "("instantExpresstion")"| "tnot" instantExpression
                    | instantExpression ("and"|"or") instantExpression
                    | startEndOp | event;

intervalExpression = intervalOperation | intervalRelation;

intervalOperation = intervalOperation ("union"|"intersection"|"complement") intervalOperation 
                    | instantExpression "~>" instantExpression 
                    | "("intervalOperation")"| state ;

intervalRelation = temporalExpression "before" temporalExpression
                   | intervalExpression "overlaps" intervalExpression
                   | intervalExpression "meets" intervalExpression
                   | temporalExpression "finishes" intervalExpression
                   | temporalExpression "starts" intervalExpression
                   | intervalExpression "contains" temporalExpression 
                   | intervalExpression "equals" intervalExpression
                   |"("intervalRelation")" | dynamic;
                   
startEndOp ::= ("start"|"end")"("intervalOperation")";
```


## Disclaimer

While _Phenesthe_ has undergone through testing, it's still under development.  Therefore some bugs may exist :) . 

## License

This project is licensed under the terms of the [GNU General Public License version 3.0](https://www.gnu.org/licenses/gpl-3.0.html)
