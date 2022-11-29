---
layout: default
title: Maritime monitoring
parent: Applications
nav_order: 1 
---

## Introduction
The task of maritime monitoring involves the detection of maritime activities of vessels, using maritime information. Maritime activities involve usual vessel behaviour (e.g., vessel underway, moored etc) or abnormal vessel behaviour such as illegal activities etc. In this example we illustrate the use of Phenesthe for the task of maritime monitoring.

### Input phenomena
We assume that the input is in the appropriate format [Input stream format](documentation/stream) and included the input phenomena of the following table.

| Input Phenomenon | Description  |
|---|---|
| `ais(MMSI,Speed, Course, Heading)` | An AIS message for vessel with `MMSI`, `Speed`, `Course`, and `Heading` at a specific time. |
| `entersPort(MMSI,P)`  | A vessel with `MMSI` enters a port `P`.  |
| `leavesPort(MMSI,P)`  | A vessel with `MMSI` leaves a port `P`. |
| `entersFishingArea(MMSI,P)`  | A vessel with `MMSI` enters a fishing area `FA`.  |
| `leavesFishingArea(MMSI,P)`  |  A vessel with `MMSI` leaves a fishing area `FA`. |

The first step in creating a definition set for a specific application is to create a definitions file, which will include the input declarations as well as the definitions of the phenomena. Therefore the definition file in this case should start as follows:

```
input_phenomenon(ais(_MMSI,_Speed,_CoG,_TrHeading),event).
input_phenomenon(entersPort(_MMSI,_Port),event).
input_phenomenon(leavesPort(_MMSI,_Port),event).
input_phenomenon(entersFishingArea(_MMSI,_FArea),event).
input_phenomenon(leavesFishingArea(_MMSI,_FArea),event).

```

The next step involves writing the definitions of the user's temporal phenomena.

### User defined temporal phenomena
Here we  present some example definitions for the maritime phenomena included in the table below.

|Type | Input Phenomenon | Description  |
|---|---|---|
| event | `stop_start(V)` | A vessel `V` starts a stop. |
| event | `stop_end(V)` | A vessel `V` ends a stop. |
| state | `in_range(V)` | A vessel `V` is in range of the receiver. |
| state | `no_major_speed_changes(V)` | A vessel `V` has the same speed for some time. |
| state | `in_port(V,P)` | A vessel `V` is in port `P`. |
| state | `in_fishing_area(V,F)` | A vessel `V` is in fishing area `F`. |
| state | `stopped(V)` | A vessel `V` is stopped. |
| state | `underway(V)` | A vessel `V` is underway. |
| state | `moored(V,P)` | A vessel `V` is moored at port `P`. |
| dynamic t. phe. | `trip(V,PA,PB)` | A vessel trip start from port `PA` to port `PB`. |
| dynamic t. phe.| `fishing_trip(V,PA,FA,PB)` | A fishing trip starts from port `PA` passes through fishing area `FA` and ends at port `PB` |


The definitions for the above temporal phenomena are included below. 

1. **Stop start and end**
```
event_phenomenon stop_start(V) := ais(V,S,_,_) aand S =< 0.5.
event_phenomenon stop_end(V) := ais(V,S,_,_) aand S > 0.5.
```
2. **In range**
```
state_phenomenon in_range(V) :=
    ais(V,_,_,_) <@ 600.
```
3. **No major speed changes**
```
state_phenomenon no_major_speed_changes(V) :=
    ais(V,S,_,_) <@ collector(600,[S],speed_diff_check).

speed_diff_check([PrevSpeed],[CurSpeed]):- 
    D is abs(CurSpeed-PrevSpeed), D < 6.
```
4. **In port/fishing area**
```
state_phenomenon in_port(V,P) := entersPort(V,P) ~> leavesPort(V,P).
state_phenomenon in_fishing_area(V,F) := entersFishingArea(V,F) ~> leavesFishingArea(V,F).
```
5. **Stopped and underway vessels**
```
state_phenomenon stopped(V) := stop_start(V) ~> stop_end(V).
state_phenomenon underway(V) := 
    ( 
        (ais(V,S1,_,_) aand S1 >= 2.7) and gtnot end(in_range(V))
    ) ~> 
    (
        (ais(V,S2,_,_) aand S2<2.7) or end(in_range(V))
    ).
```
6. **Moored vessels**
```
state_phenomenon moored(V,P) := stopped(V) intersection in_port(V,P).
```
7. **(Fishing) Trips**
```
dynamic_phenomenon trip(V,PA,PB):=
    end(moored(V,PA)) before
     (underway(V) before start(moored(V,PB))).

dynamic_phenomenon fishing_trip(V,PA,FA,PB):=
    (end(moored(V,PA)) aand vessel_type(V,fishing)) before
        ((underway(V) contains in_fishing_area(V,FA))
            before start(moored(V,PB))).
```

### References
For a maritime monitoring application that utilises Phenesthe have a look in the publication below.
*  M. Pitsikalis, A. Lisitsa, P. Totzke and S. Lee, "Making Sense of Heterogeneous Maritime Data," 2022 23rd IEEE International Conference on Mobile Data Management (MDM), 2022, pp. 401-406, doi: [10.1109/MDM55031.2022.00089](https://ieeexplore.ieee.org/document/9861145).

The set of maritime patterns that inspired some of the presented maritime temporal phenomena definitions of this page is included below.
* M. Pitsikalis, A. Artikis, R. Dreo, C. Ray, E. Camossi, and A-L. Jousselme. 2019. Composite Event Recognition for Maritime Monitoring. In Proceedings of the 13th ACM International Conference on Distributed and Event-based Systems (DEBS '19). doi: [10.1145/3328905.3329762](https://doi.org/10.1145/3328905.3329762)

---

