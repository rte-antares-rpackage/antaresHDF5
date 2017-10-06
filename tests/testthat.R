#Copyright © 2016 RTE Réseau de transport d’électricité

library(testthat)
library(data.table)
library(antaresRead)
library(pipeR)
library(stringr)
library(antaresHdf5)
library(antaresProcessing)
library(rhdf5)

testthat::test_check("antaresHdf5")
