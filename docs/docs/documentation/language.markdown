---
layout: default
title: Language
parent: Documentation
nav_order: 2 
---

## Language
The language of Phenesthe allows the definition of temporal phenomena. A temporal phenomenon may be an event, a state or a dynamic temporal phenomenon. Events are true in instants of time, states hold in disjoint intervals and dynamic temporal phenomena may hold in possibly non disjoint intervals.
Moreover, the language of phenesthe allows the declaration of the phenomena of the input stream.

### Grammar
The grammar of the language is present below in EBNF:

{% highlight ebnf %}
definitions = eventDefinition | stateDefinition | dynamicDefinition;

eventDefinition = "event_phenomenon" event ":=" instantExpression ".";
stateDefinition = "state_phenomenon" state ":=" intervalOperation ".";
dynamicDefinition = "dynamic_phenomenon" dynamic ":=" intervalRelation ".";

event = eventName(...);
state = stateName(...);
dynamic = dynamicPhenomenonName(...);

temporalExpression = instantExpression | intervalExpression ;

instantExpression = "("instantExpresstion")"| "tnot" instantExpression
                    | instantExpression ("and"|"or") instantExpression
                    | instantExpression "in" intervalOperation
                    | instantExpression ("@<"|"@>="|"@=") PosInteger
                    | instantExpression "aand" atemporalPrologExpression
                    | startEndOp | event; 

intervalExpression = intervalOperation | intervalRelation;

intervalOperation = intervalOperation ("union"|"intersection"|"complement") intervalOperation 
                    | instantExpression "~>" instantExpression 
                    | intervalOperation "aand" atemporalPrologExpression
                    | "filter("intervalOperation"," filter_option ")"
                    | "("intervalOperation")"| state; 

intervalRelation = temporalExpression "before" temporalExpression
                   | intervalExpression "overlaps" intervalExpression
                   | intervalExpression "meets" intervalExpression
                   | temporalExpression "finishes" intervalExpression
                   | temporalExpression "starts" intervalExpression
                   | intervalExpression "contains" temporalExpression 
                   | intervalExpression "equals" intervalExpression
                   | intervalRelation "aand" atemporalPrologExpression
                   |"("intervalRelation")" | dynamic;

filter_option = ("less" | "greater" | equal")"("PosInteger")"
                   
startEndOp = ("start"|"end")"("intervalOperation")";
{% endhighlight %}


### Semantics
In short, the formulae of the language of Phenesthe, are divided into three categories instant formulae, disjoint interval formulae, and non-disjoint interval formulae. Each of the aforementioned formulae categories is used for defining events, states and dynamic temporal phenomena.  Below the semantics of each formulae category are presented with examples.

#### Instant formulae
Instant formulae describe happenings that are true in instants of time.

| Name | Formula | Description  |
|---|---|---|
| Conjunction | `fa and fb`  | True at instants where both `fa` and `fb` occur. |
| Disjunction | `fa or fb`  |  True at instants where either `fa` or `fb` occurs. |
| Negation    | `tnot fa` | True at instants where `fa` does not occur.|
| Start |   `start(fa)` | True at the instant `fa` starts holding. |
| End |   `end(fa)` | True at the instant `fa` stops holding. |       
| Inclusion | `fa in fb` | True if `fa` occurs while `fb` holds. |

##### Instant formulae examples
1. A formula that is true when a person either drops or pickups an object. 
```
drop(Person, ObjectA) or pickup(Person, ObjectB).
```

2. A formula that describes that in order for a person (P) to shoot with gun (G), the gun must be loaded.
```
shoot(G,P) in loaded(G).
```

#### Disjoint interval formulae
Disjoint interval formulae describe durative states that hold in disjoint intervals.

| Name | Formula | Description  |
|---|---|---|
| Maximal range operator | `fa ~> fb`  | Holds when `fa` occurs, and continues to hold unless `fb` occurs. |
| Temporal union | `fa union fb`  |  Holds when either `fa` or `fb` holds. |
| Temporal intersection | `fa intersection fb` | Holds when both `fa` and `fb` hold.|
| Temporal complement |   `fa complement fb` | Holds when `fa` holds but `fb` does not hold. |
| Constrained iteration |   `fa [<,=,>=]@ N` | Holds when `fa' occurs at times with contiguous temporal distance (t1-t0) [<,=,>=] N. |       
| Filtering | `filter(fb,f(...))` | Filter intervals at which `fb` holds by applying function f on their individual size. |

##### Disjoint interval formulae examples
1. A formula the describes the time intervals where a person has a gain but not loss in the meantime.
```
gain(Person) ~> loss(Person)
```

2. A formula that holds when a vessel is both at a port and stopped.
```
stopped(V) intersection in_port(V,P)
```

3. A formula that holds when vegetables are stirred at most every 30 seconds.
```
stir(vegetables) <@ 30
```


#### Non-disjoint interval formulae
Non-disjoint interval formulae describe the temporal arrangement of temporal phenomena.

| Name | Formula | Description  |
|---|---|---|
| Before | `fa before fb`  | `fa` occurs (holds), before `fb` occurs (holds) and they are contiguous. |
| Overlaps | `fa overlaps fb`  |  `fa` starts holding, then `fb` starts holding, next `fa` stops holding, finally `fb` stops holding. |
| Meets | `fa meets fb` | Holds when `fb` starts holding when `fa` stops holding.|
| Finishes |   `fa finishes fb` | Holds when `fb` holds, and `fa` occurs when `fb` stops holding or `fa` starts holding after `fb` starts and finishes at the same time as `fb`. |
| Starts |   `fa starts fb` | Holds when `fb` holds, and `fa` occurs when `fb` starts holding or `fb` starts holding when `fa` starts but stops holding earlier than `fb`.|       
| Contains | `fa contains fb` | Holds when `fa` holds and `fb` occurs (holds) during `fa`.| 
| Equals | `fa equals fb`| Holds when both `fa` and `fb` hold at the same time.|

##### Non-disjoint interval formulae examples
1.  A formula that describes a vessel trip (end of being moored, followed by being underway, the finally being moored again).
```
end(moored(V,PA)) before
     (underway(V) before start(moored(V,PB))).
```
2. The cooking process of fried then oven baked steaks.
```
filter((filter((cooking(P,stove,steaks)),greater(120))),less(180)) before
	 filter((filter((cooking(P,oven,steaks)),greater(480))),less(600))
```

---

