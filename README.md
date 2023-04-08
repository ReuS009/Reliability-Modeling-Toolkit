# Reliability-Modeling-Toolkit, aka Reliabilitytk

**Reliabilitytk** is an R library for reliability engineering computations.

### Documentation

* Documentation is included as standard R help files

### Getting Started

Installation instructions from source for Reliabilitytk:

```
install.packages("devtools")
library(devtools)
```
* *If Rtools is installed, build from source:*
```
devtools::install_github("ReuS009/reliabilitytk")
```
* *Otherwise, use the following:*
```
devtools::install_github("ReuS009/reliabilitytk", build = FALSE)
```
Running Unit tests for Reliabilitytk:
```
install.packages("testthat")
setwd("[File location of reliabilitytk library]")
devtools::test()
```
### Source Repository

Reliabilitytk's source code repository is hosted here on GitHub.

### Licensing

Reliabilitytk is licensed under GPLv3.
