AI Dialysis Symptomatology
========

A basic C4.5 SWI-Prolog implementation for learning symptomatology classification rules from dialysis data.

--------

## Info

This is a university project for the Artificial Intelligence class (2013-2014).

### Dialysis Data

Dialysis data are retrieved from the "Gepadial" database, a software for managing dialysis patients and sessions by [La Traccia](www.latraccia.it/en).

Read [scripts/README.md](scripts/README.md) for more information about importing the data.

### Prolog

The main program implements a basic version of the C4.5, learning a decision tree and asserting a batch of derived Prolog rules.

The Prolog code needs [SWI-Prolog](http://www.swi-prolog.org/) and a working [MySQL](http://www.mysql.com/) database instance to work.

### Weka

Read [weka/README.md](weka/README.md) for more information about using the data with [Weka](http://www.cs.waikato.ac.nz/ml/weka/).

### BayesDB

Read [bayesdb/README.md](bayesdb/README.md) to learn how to import and use the data with [BayesDB](http://probcomp.csail.mit.edu/bayesdb/).

## License

Released under Apache 2.0.

```
Copyright 2014 Francesco Pontillo

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```