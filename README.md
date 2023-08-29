# Reliability-Modeling-Toolkit, aka Reliabilitytk

**Reliabilitytk** is an R library for reliability engineering computations.

### Documentation

* Documentation is included as standard R help files

### Key Applications and Functions

* Probabilitsic physics-of-failure analysis
* Stress-Life, Strain-Life, Variable Amplitude Loading
* Fracture mechanics, wear, creep, and corrosion analysis
* Reliability Modeling: Probability plotting, least squares estimation of probability parameters, maximum likelihood estimation, Bayesian parameter updating
* Accelerated Life Testing
* Step-Stress Accelerated Life Testing
* Accelerated Degradation Testing
* Test Driven Development

### Getting Started

Installation instructions from source for Reliabilitytk:

```
install.packages("devtools")
library(devtools)
```
* *If Rtools is installed, build from source:*
```
devtools::install_github("ReuS009/reliabilitytk", INSTALL_opts = "--install-tests")
```
* *Otherwise, use the following:*
```
devtools::install_github("ReuS009/reliabilitytk", build = FALSE, INSTALL_opts = "--install-tests")
```
Running Unit tests for Reliabilitytk:
```
install.packages("testthat")
library(testthat)
library(reliabilitytk)
test_package("reliabilitytk")
```
### Source Repository

Reliabilitytk's source code repository is hosted here on GitHub.

### Licensing

Reliabilitytk is licensed under GPLv3.
