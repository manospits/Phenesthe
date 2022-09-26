---
layout: default
title: Architecture
parent: Documentation
nav_order: 1
---

## Architecture
An overview of the architecture of phenesthe is presented in the figure below.

![Phenesthe architecture](/assets/img/phenesthe_architecture.png "Phenesthe architecture")

### Input information
The input is of two kinds; static and dynamic information.
* __Static input__ refers to a set of phenomena definitions written in the language of this paper along with a declaration of the expected input phenomena. Static information may also include atemporal information such as predicates storing information regarding the elements of a specific use-case.
* __Dynamic input__ refers to the input stream which contains input phenomena associated with some temporal information. The phenomena definitions and declarations pass through a transformation step, during which they are transformed into a standard Prolog language representation. Additionally, the dependencies between phenomena are computed and a valid evaluation order is produced (see "Dependency Graph Computation" in the above Figure).

### Temporal querying
When the transformation and the computation of the evaluation order of the user defined phenomena is complete, processing of the input stream is able to commence. Processing of the input happens in the form of temporal queries, on temporal windows of the input stream, at equally distanced times specified by the value of step. During a temporal query the instants and the intervals at which user defined phenomena are true or hold are computed and printed in the output stream. Additionally, during each temporal query redundant information is discarded and non-redundant information is retained until classified otherwise via the redundancy handling mechanism. Therefore, for stream processing the user is responsible for selecting the following values:

* **Step**: The distance between temporal queries. At the moment, variable step size (i.e., performing temporal queries on unequally distanced times) is not supported.
* **Window size**: The size of the window to be used in each temporal query. Each window is a subset of the input stream.


### Dependency aware parallelisation
User defined phenomena can be processed in a sequential manner by following the evaluation order produced by the topological sort of the directed acyclic graph they form. An example dependency graph is shown below for the YSP example.

![YSP dependency graph](/assets/img/ysp_dep.png "YSP dependency graph")


However, if possible they can also be executed in parallel. In Phenesthe we implement dependency-aware parallelisation whereby phenomena definitions that have no pending dependencies are processed in parallel via a Master-Worker paradigm. Here, the master checks for phenomena definitions that do not have any unmet dependencies and inserts them in the processing queue. Workers remove phenomena definitions from the processing queue, process them, and notify the master as soon as they complete. This process goes on until all the user defined phenomena are processed. 

By default dependency aware parallelisation is enabled. If required, dependency aware parallelisation can be turned off by setting the following:

```prolog
phe_setval(multithreading,0).
```


---
