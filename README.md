[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rte-antares-rpackage/antaresHDF5?branch=master&svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/antaresHDF5)
[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresHDF5.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresHDF5)
[![Coverage Status](https://img.shields.io/codecov/c/github/rte-antares-rpackage/antaresHDF5/master.svg)](https://codecov.io/github/rte-antares-rpackage/antaresHDF5?branch=master)

# Convert an antares study to a h5 file and read him with R package 'antaresHdf5'

## rhdf5 installation

This package depends on **rhdf5** package. This is a **bioconductor** package. Morevover, the current version need the last **R** version (>= 3.4) and a developpement version of **rhdf5** : 

```r

# rhdf5 installation
source('https://bioconductor.org/biocLite.R')
BiocInstaller::useDevel() # for latest version
biocLite('rhdf5')

# update R (one way...)
# installing/loading the package:
if(!require(installr)) {
install.packages("installr"); require(installr)} #load / install+load installr
 
# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
```

## Convert data from a simulation to h5 file

Load the package

```r
library(antaresHdf5)
```

To convert a simulation you must use function ``writeAntaresH5``

Convert a single simulation, path can be specify to give name of output file, default it take same name than input simulation folder

```r
setSimulationPath("C:/Users/TTT/Mystudy", 1)
path <- "Mystudy.h5"
writeAntaresH5(path)
```

Convert all simulations from a study, parallel process, default 4 cores

```r
setSimulationPath("C:/Users/TTT/Mystudy")
writeAntaresH5(writeAllSimulations = TRUE)
```

Choose timestep to write
```r
writeAntaresH5(timeSteps = "hourly")
```

Write with additionnal information
```r
 writeAntaresH5(path, timeSteps = "hourly",
    misc = FALSE, thermalAvailabilities = FALSE,
    hydroStorage = FALSE, hydroStorageMaxPower = FALSE, reserve = FALSE,
    linkCapacity = FALSE, mustRun = FALSE, thermalModulation = FALSE)
```

## Read data from h5 file

Function ``h5ReadAntares``

This function Works the same way to ``antaresRead::readAntares``.

You must specify ``path``, path of h5 file to read.

