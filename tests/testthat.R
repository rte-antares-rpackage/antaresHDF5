#Copyright © 2016 RTE Réseau de transport d’électricité

library(testthat)
library(data.table)
library(antaresRead)
library(rhdf5)
library(pipeR)
library(stringr)
library(antaresHdf5)
library(antaresProcessing)

testthat::test_check("antaresHdf5")
