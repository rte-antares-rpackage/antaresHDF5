.onLoad <- function(libname, pkgname){
  if(!require(rhdf5)){
    stop("This package require 'rhdf5' (>= 2.21.2) package.
    This is a bioconductor package. You can install it with :
    source('https://bioconductor.org/biocLite.R')
    BiocInstaller::useDevel() # for latest version
    biocLite('rhdf5')")
  } else if(packageVersion("rhdf5") < '2.21.2'){
    stop("This package require 'rhdf5' (>= 2.21.2) package.
    The installed version is older than expected.
    This is a bioconductor package. You can install it with :
    source('https://bioconductor.org/biocLite.R')
    BiocInstaller::useDevel() # for latest version
    biocLite('rhdf5')")
  }
}

.addClassAndAttributes <- antaresRead:::.addClassAndAttributes

pkgEnvAntareasH5 <- new.env()


pkgEnvAntareasH5$varAreas <- c("OV. COST", "OP. COST", "MRG. PRICE", "CO2 EMIS.", "BALANCE",
                               "ROW BAL.", "PSP", "MISC. NDG",  "LOAD", "H. ROR", "WIND", "SOLAR",
                               "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR",
                               "UNSP. ENRG", "SPIL. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG", "NP COST", "NODU")
pkgEnvAntareasH5$varAreas <- as.vector(sapply(pkgEnvAntareasH5$varAreas, function(X){paste0(X, c("", "_min", "_max", "_std"))}))
pkgEnvAntareasH5$varDistricts <- pkgEnvAntareasH5$varAreas

pkgEnvAntareasH5$varLinks <- c("FLOW LIN.", "UCAP LIN.", "FLOW QUAD.",
                               "CONG. FEE (ALG.)", "CONG. FEE (ABS.)",
                               "MARG. COST", "CONG. PROB +", "CONG. PROB -", "HURDLE COST")
pkgEnvAntareasH5$varLinks <- as.vector(sapply(pkgEnvAntareasH5$varLinks, function(X){paste0(X, c("", "_min", "_max", "_std"))}))



pkgEnvAntareasH5$varClusters <- c("production", "NP Cost", "NODU")





pkgEnvAntareasH5$varAliasCraeted <- list()


#misc
pkgEnvAntareasH5$varAliasCraeted$misc$areas <- c("CHP",
                                                 "Bio_mass",
                                                 "Bio_gas",
                                                 "mustRunWasteTotal",
                                                 "GeoThermal",
                                                 "Other",
                                                 "PSP_input",
                                                 "ROW_Balance")
pkgEnvAntareasH5$varAliasCraeted$misc$districts <- c("CHP",
                                                     "Bio_mass",
                                                     "Bio_gas",
                                                     "mustRunWasteTotal",
                                                     "GeoThermal",
                                                     "Other",
                                                     "PSP_input",
                                                     "ROW_Balance")
#thermalAvailabilities
pkgEnvAntareasH5$varAliasCraeted$thermalAvailabilities$clusters <- c("thermalAvailability",
                                                                     "availableUnits")


#hydroStorage
pkgEnvAntareasH5$varAliasCraeted$hydroStorage$areas <- c("hydroStorage")
pkgEnvAntareasH5$varAliasCraeted$hydroStorage$districts <- c("hydroStorage")



#hydroStorageMaxPower
pkgEnvAntareasH5$varAliasCraeted$hydroStorageMaxPower$areas <- c("hstorPMaxLow",
                                                                 "hstorPMaxAvg",
                                                                 "hstorPMaxHigh")

pkgEnvAntareasH5$varAliasCraeted$hydroStorageMaxPower$districts <- c("hstorPMaxLow",
                                                                     "hstorPMaxAvg",
                                                                     "hstorPMaxHigh")

#reserve
pkgEnvAntareasH5$varAliasCraeted$reserve$areas <- c("primaryRes",
                                                    "strategicRes",
                                                    "DSM",
                                                    "dayAhead")

pkgEnvAntareasH5$varAliasCraeted$reserve$districts <- c("primaryRes",
                                                        "strategicRes",
                                                        "DSM",
                                                        "dayAhead")

#linkCapacity
pkgEnvAntareasH5$varAliasCraeted$linkCapacity$links <- c("transCapacityDirect",
                                                         "transCapacityIndirect",
                                                         "impedances",
                                                         "hurdlesCostDirect",
                                                         "hurdlesCostIndirect")

#mustRun
pkgEnvAntareasH5$varAliasCraeted$mustRun$areas <- c("thermalPmin",
                                                    "mustRun",
                                                    "mustRunPartial",
                                                    "mustRunTotal")

pkgEnvAntareasH5$varAliasCraeted$mustRun$districts <- c("thermalPmin",
                                                        "mustRun",
                                                        "mustRunPartial",
                                                        "mustRunTotal")

pkgEnvAntareasH5$varAliasCraeted$mustRun$clusters <- c("thermalPmin",
                                                       "mustRun",
                                                       "mustRunPartial",
                                                       "mustRunTotal")

pkgEnvAntareasH5$varAliasCraeted$thermalModulation$clusters <- c("marginalCostModulation",
                                                                 "marketBidModulation",
                                                                 "capacityModulation",
                                                                 "minGenModulation")


###Process

pkgEnvAntareasH5$process$addDownwardMargin$areas <- c("isolatedDownwardMargin",
                                                      "interconnectedDownwardMargin")

pkgEnvAntareasH5$process$addUpwardMargin$areas <- c("isolatedUpwardMargin",
                                                    "interconnectedUpwardMargin")

pkgEnvAntareasH5$process$addExportAndImport$areas <- c("import",
                                                       "export")

pkgEnvAntareasH5$process$addLoadFactorLink$links <- c("loadFactor",
                                                      "congestion")

pkgEnvAntareasH5$process$externalDependency$areas <- c("netLoad",
                                                       "exportsLevel",
                                                       "importsLevel",
                                                       "exportsFrequency",
                                                       "importsFrequency")

pkgEnvAntareasH5$process$loadFactor$clusters <- c("loadFactor", "propHoursMinGen", "propHoursMaxGen")

pkgEnvAntareasH5$process$modulation$clusters <- c("upwardModulation", "downwardModulation",
                                                  "absoluteModulation")

pkgEnvAntareasH5$process$netLoadRamp$areas <- c("netLoadRamp", "balanceRamp", "areaRamp")

pkgEnvAntareasH5$process$surplus$areas <- c("consumerSurplus", "producerSurplus", "rowBalanceSurplus",
                                            "storageSurplus", "congestionFees", "globalSurplus")

pkgEnvAntareasH5$process$surplusClusters$clusters <- c("variableCost", "fixedCost", "startupCost",
                                                       "surplusPerUnit", "totalSurplus", "economicGradient")


sapply(names(pkgEnvAntareasH5$process), function(X){
  tpAlias <- pkgEnvAntareasH5$process[[X]]
  X <- paste0("Out_", X)
  sapply(names(tpAlias), function(Y){
    varAlias <- tpAlias[[Y]]
      setAlias(X, X, c(Y, varAlias))
  })
})


#pkgEnvAntareasH5$process$surplusSectors$areas <- c("surplus", "cost")

pkgEnvAntareasH5$processDispo <- data.frame(
  trtName = c("downwardMargin",
              "upwardMargin",
              "exportsImports",
              "loadFactorLink",
              "externalDependency",
              "loadFactor",
              "modulation",
              "netLoadRamp",
              "surplus",
              "surplusClusters"
              )
  , fctname = c( "addDownwardMargin" ,
                                    "addUpwardMargin" ,
                                    "addExportAndImport" ,
                                    "addLoadFactorLink" ,
                                    "externalDependency" ,
                                    "loadFactor" ,
                                    "modulation" ,
                                    "netLoadRamp" ,
                                    "surplus" ,
                                    "surplusClusters"
                                     ))

pkgEnvH5 <- antaresRead:::pkgEnv

