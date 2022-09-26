---
layout: default
title: Stream processing
parent: Documentation
nav_order: 3
---

## Stream processing
_Phenesthe_ is a tool that allows the detection of temporal phenomena on streams. Currently it can handle two types of streams: file streams and UNIX pipe streams. In this section the input format as well as the methodology for processing both types is described.

### Input stream format
When using _Phenesthe_ for stream processing, the input stream should contain entries as follows:

* Input events
```
input_event_instant(event_name(arg_1,...,arg_n),T)
```
`arg_1,...,arg_n` are atemporal values, while T is the occurrence time.

* Input states
```
input_state_interval(state_name(arg_1,...,arg_n),[Ts,Te])
```
`arg_1,...,arg_n` are atemporal values, while [Ts,Te] is a strict interval (i.e., Ts < Te) denoting the time period where state_name/n holds.
* Input dynamic temporal phenomena
```
input_dynamic_phenomenon_interval(dyn_temp_phe_name(arg_1,...,arg_n),[Ts,Te])
```
`arg_1,...,arg_n` are atemporal values, while [Ts,Te] is a strict interval (i.e., Ts < Te) denoting the time period where arg_1,...,arg_n/n holds.

The input should be ordered via the '<' for instants and then for intervals first on the starting times and then on the ending times.

### File stream processing

Processing a file stream (a text file) is allowed via the following predicate:

```prolog
queries_on_fstream(InputFile,LogFile,ResultsFile,Start,End,Step,Window)
```
Where the arguments denote the following:
1. `InputFile`: Name of the input file.
2.   `LogFile`: Name of the log file (statistics for each temporal query).
3.  `ResultsFile`: Printed instants and intervals at which user defined phenomena are true hold.
4.   `Start`: Timestamp to start processing.
5.   `End`: Timestamp to end processing.
6.   `Step`: Window sliding step.
7.   `Window`: Window size.
Phenesthe will start processing phenomena based on the start timestamp and it will stop based on the end timestamp or the EOF.

### Stream (general) processing
Currently stream processing in _Phenesthe_ is allowed via the use of named UNIX pipe streams. Pipes can be created via the use of the `mkfifo`  command. The predicate used for processing these pipe streams is the following:

```prolog
queries_on_stream(PipeName,LogFile,ResultsFile,Step,Window)
```
Where the arguments denote the following:
1. `PipeName`: Name of the pipe.
2.   `LogFile`: Name of the log file (statistics for each temporal query).
3.  `ResultsFile`: Printed instants and intervals at which user defined phenomena are true hold.
5.   `Step`: Window sliding step.
6.   `Window`: Window size.

Stream processing here will stop when the pipe is closed (by force) or an EOF is read.

### Complete example on a file stream
Therefore to use _Phenesthe_ for stream processing the following steps must be taken.

1. **Initial setup:** load _Phenesthe_, definitions of phenomena and any static information.
```prolog
% Maritime example
:-['../../phenesthe.prolog'].
% load the maritime definitions
:-['./definitions.prolog'].
% load vessel types
:-['./vessel_types.prolog'].
```
2. **Definitions preprocessing:** Here the definitions are transformed into prolog code and their dependencies and evaluation order is computed.
```prolog
% Maritime example
% preprocess phenomena definitions (transform them, find evaluation order, etc.)
:-preprocess_phenomena_definitions.
```
3. **Temporal querying:** Call the appropriate stream processing predicate as follows:
```prolog
% Maritime example
queries_on_fstream('BREST_phenesthe.input','logs/log_1_3600.csv','results/results_1_3600.out',1443650401,1444255201,3600,3600).
```

### Pipe stream

1. **Initialisation & Preprocessing:** Steps 1 and 2 from above remain the same.
2. **Pipe:** Creation of the stream (in shell)
```bash
mkfifo input_stream
cat BREST_phenesthe.input >> input_stream &
```
3. **Temporal querying:** Call the appropriate stream processing predicate as follows:
```prolog
% Maritime example
queries_on_stream('input_stream','logs/log_1_3600.csv','results/results_1_3600.out',3600,3600).
`
