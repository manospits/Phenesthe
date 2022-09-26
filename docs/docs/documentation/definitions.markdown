---
layout: default
title: Input & User Phenomena
parent: Documentation
nav_order: 4
---
## Phenomena

Phenomena in _Phenesthe_ can be either user defined or input. For each application a definition file must created that contains the **user definitions** and the **input phenomena declarations**.

  
### User defined phenomena
Definitions of phenomena in Phenesthe can be of three types:
* Event definitions
* State definitions
* Dynamic temporal phenomena  definitions


#### Events
Events  are defined using instant formulae of the language and via the use of the `event_phenomenon` keyword. An example definition from the Alice and Bob sample is the following:
```
%gain when picking up and not dropping
event_phenomenon gain(Person) :=
    pickup(Person, _Object) and
    tnot drop(Person,_).
```

#### States
States are defined using disjoint interval formulae of the language and the `state_phenomenon` keyword. An example from the maritime domain is the following:
```
dynamic_phenomenon fishing_trip(V,PA,FA,PB):=
    (end(moored(V,PA)) aand vessel_type(V,fishing)) before
        ((underway(V) contains in_fishing_area(V,FA))
            before start(moored(V,PB))).
```
#### Dynamic temporal phenomena
Dynamic temporal phenomena are defined using non-disjoint interval formulae of the language and the `dynamic_phenomenon` keyword. An example from the meal preparation scenario is the following:
```
dynamic_phenomenon mealPrep(P) := 
	stirfryPrep(P) finishes steaksPrep(P).
```


### Input phenomena
Input phenomena declarations should have the following form:
1. Events
```
input_phenomenon(event_name(_,...,_),event).
```
2. States
```
input_phenomenon(state_name(_,...,_),state).
```
3. Dynamic temporal phenomena
```
input_phenomenon(dyn_temp_phe_name(_,...,_),dynamic).
```

---
