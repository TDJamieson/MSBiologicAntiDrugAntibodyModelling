#==============================================================================#
#==============================================================================#
#                                                                              #
#---    Aggregate split populations run through microsimulation process     ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#

#  This process takes all of the split microsimulation runs, checks that the   #
#  whole population desired has been simulated, and aggregates them.  This     #
#  will aggregate for each combination of seed group and parameter set ID.     #
#                                                                              #
# ============================================================================ #
                                                                              

#===============================================================================
#                              Load libraries                                  #
#===============================================================================

    # Load libraries

      library(Rcpp)         # --
      library(tidyverse)    # -- General helper functions      
      library(plyr)         # --

      library(openxlsx)     # Reading in from excel
      library(data.table)   # Providing big data type containers for population
                            # --
      library(stringr)      # -- String processing functions
      library(stringi)      # --

      options(scipen=999)

#==============================================================================#



#===============================================================================
#       Set overall project, R project directory and define arguments          #
#===============================================================================


  # Tests to see if being run locally or scripted and defines locations and 
  # arguments accordingly using to separate arguments files

    if (interactive() == TRUE){
      
     
      rootDirectory <- "D:/QMULOneDrive/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting_optimised/")
      
      source(paste0(RProjectDirectory, 'aggregationMasters/', 'lifeCourseAggregationMasterArgs_local.R'))
      
      
    } else {
     
         
      rootDirectory <- "/data/home/wpw004/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting_optimised/")
      
      source(paste0(RProjectDirectory, 'aggregationMasters/', 'lifeCourseAggregationMasterArgs_cluster.R'))
      

      }

#===============================================================================



#==============================================================================#
#               Define directories to source, and output                       #
#===============================================================================
#                                                                              #
#  Wherever the model runs directory is held, the folder for the run needs     #     
#  to be specified inside this, and the folder in which the split outputs      #
#  are held need to be identified.  An aggregated outputs directory then needs #
#  to be created.  For checking of completion of all runs, the intended        #
#  population needs to be generated from the run controls spreadsheet.  This   #
#  requires a number of further directories and proscesses to be defined.      #
#                                                                              #
# ============================================================================ #                                                
 

  # Define all run non-specific locations
  
    source(paste0(RProjectDirectory, 'universalCode/', 'defineDirectoryLocations.R'))

  # ---- #


  # Specific run directory (needs a 'run directory description' to be defined) --

    runDirectory <- paste0(runsDirectory, runDirectoryDescription, '/') 

  # ---- # 
  
    
  # Outputs folder inside run directory  --
  
    outputsDirectory <- paste0(runDirectory, 'outputs_all/')

  # ---- #   
  
    
  # Create aggregated outputs directory --
  
    suppressWarnings(       
        
        dir.create(paste0(runDirectory, 'outputs_aggregated'))
    
        )
    
    aggregateOutputsDirectory <- paste0(runDirectory, 'outputs_aggregated/')
    
  # ---- #
  
# ==============================================================================
    
    
    
#===============================================================================
#            Define some generic functions, load run controls                  #
#===============================================================================

    source(paste0(codeDirectory, "defineUtilityFunctions.R"))
    
     # Run-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_runControls.R"))

# ==============================================================================
    
 
    
#==============================================================================#
#                       Identify Population to aggregate                       #
#===============================================================================

  # Load aggregation inputs 
    
    aggregationPopulations <- readRDS(paste0(runDirectory, 
                                             'aggregationPopulations'))
    
  
  # Load list of all runs that should have completed
    
    inputPopulations <- readRDS(paste0(runDirectory, 
                                      'inputPopulations'))
    
    inputPopulations[, runDescriptions := 
                       paste0(runDescription, '__SG_', seedGroup, '_PSAID_',
                              parameterSetID, '_personID_', startID, '_to_', 
                              endID, '_costedIvsCOutcomes.rds')]
    
    inputPopulations <- inputPopulations[, runDescriptions]
    
    
    
  # Identify completed runs and ensure all specified have completed
  
    completedFiles <- list.files(outputsDirectory)
  
    iVcOutcomeFiles <- completedFiles[grepl('IvsCOutcomes', completedFiles, 
                                           perl = TRUE)]  
    
    iAndcOutcomeFiles <- completedFiles[grepl('IandCOutcomes', completedFiles, 
                                           perl = TRUE)]  
    
    check <- setdiff(inputPopulations, completedFiles)

    if(length(check) >0) {
      
      print(check)
      stop('Missing run(s)')
      
    }
    
    
  # Read in runs
    
    IvCOutcomes <- lapply(iVcOutcomeFiles, function(file) {
      
      readRDS(paste0(outputsDirectory, file))
    
    })
    
    IvCOutcomes <- rbindlist(IvCOutcomes)
    
    
    IandCOutcomes <- lapply(iAndcOutcomeFiles, function(file) {
      
      readRDS(paste0(outputsDirectory, file))
    
    })
    
    IandCOutcomes <- rbindlist(IandCOutcomes)
    
    
    
  # Define fields for which mean values are wanted
    
    meanFields <- colnames(IvCOutcomes)[grepl('mean', colnames(IvCOutcomes))]
    
    
  
  # Generate outcomes
    
    if(variations_Switch == 1){
      
      
    # I vs C
      
      meanByVariation <- copy(IvCOutcomes)
      
      meanByVariation[, totalPopSize := sum(popSize), by = runDescription]
      meanByVariation[, weight := popSize/totalPopSize]
      
      
      meanByVariation[, c(meanFields) := lapply(.SD, function(field) {field * weight}),
                      .SDcols = meanFields, 
                      by = .(runDescription)]
      
       meanByVariation[, c(meanFields) := lapply(.SD, sum),
                      .SDcols = meanFields, 
                      by = .(runDescription)]
      
      meanByVariation <- meanByVariation[, .SD[1], by = runDescription]
      
      saveRDS(meanByVariation, file = paste0(aggregateOutputsDirectory, runDescription, 
                                             '_IvsCoutcomeByVariation_', varyingVar))
      
      
       
    # I and C
      
      meanByVariation <- copy(IandCOutcomes)
      
      meanByVariation[, totalPopSize := sum(popSize), by = .(runDescription, interventionCohort)]
      meanByVariation[, weight := popSize/totalPopSize]
      
      
      meanByVariation[, c(meanFields) := lapply(.SD, function(field) {field * weight}),
                      .SDcols = meanFields, 
                      by = .(runDescription, interventionCohort)]
      
       meanByVariation[, c(meanFields) := lapply(.SD, sum),
                      .SDcols = meanFields, 
                      by = .(runDescription, interventionCohort)]
      
      meanByVariation <- meanByVariation[, .SD[1], by = .(runDescription, interventionCohort)]
      
      saveRDS(meanByVariation, file = paste0(aggregateOutputsDirectory, runDescription, 
                                             '_IandCoutcomeByVariation_', varyingVar))
      
      
      
    } else if(variations_Switch == 0){
 
       # I vs C - overall
      
      meanByPSA <- copy(IvCOutcomes)
      
      meanByPSA[, totalPopSize := sum(popSize)]
      meanByPSA[, weight := popSize/totalPopSize]
      
      
      meanByPSA[, c(meanFields) := lapply(.SD, function(field) {field * weight}),
                      .SDcols = meanFields]
      
      meanByPSA[, c(meanFields) := lapply(.SD, sum),
                      .SDcols = meanFields]
      
      meanByPSA <- meanByPSA[, .SD[1]]
      
      saveRDS(meanByPSA, file = paste0(aggregateOutputsDirectory, runDescription,'_AllPopPSAIvsCoutcome'))
      
      
      
      # I vs C - PSAID only
      
      meanByPSA <- copy(IvCOutcomes)
      
      meanByPSA[, totalPopSize := sum(popSize), by = parameterSetID]
      meanByPSA[, weight := popSize/totalPopSize]
      
      
      meanByPSA[, c(meanFields) := lapply(.SD, function(field) {field * weight}),
                      .SDcols = meanFields, 
                      by = .(parameterSetID)]
      
      meanByPSA[, c(meanFields) := lapply(.SD, sum),
                      .SDcols = meanFields, 
                      by = .(parameterSetID)]
      
      meanByPSA <- meanByPSA[, .SD[1], by = parameterSetID]
      
      saveRDS(meanByPSA, file = paste0(aggregateOutputsDirectory, runDescription,'_IvsCoutcomeByPSA'))
      
      
       
    # I and C - PSAID only
      
      meanByPSA <- copy(IandCOutcomes)
      
      meanByPSA[, totalPopSize := sum(popSize), by = .(parameterSetID, interventionCohort)]
      meanByPSA[, weight := popSize/totalPopSize]
      
      
      meanByPSA[, c(meanFields) := lapply(.SD, function(field) {field * weight}),
                      .SDcols = meanFields, 
                      by = .(parameterSetID, interventionCohort)]
      
       meanByPSA[, c(meanFields) := lapply(.SD, sum),
                      .SDcols = meanFields, 
                      by = .(parameterSetID, interventionCohort)]
      
      meanByPSA <- meanByPSA[, .SD[1], by = .(parameterSetID, interventionCohort)]
      
      saveRDS(meanByPSA, file = paste0(aggregateOutputsDirectory, runDescription, 
                                       '_IandCoutcomeByPSA'))
      
      
      
     # I vs C - PSA and seedGroup
      
      meanByPSA <- copy(IvCOutcomes)
      
      meanByPSA[, totalPopSize := sum(popSize), by = .(seedGroup, parameterSetID)]
      meanByPSA[, weight := popSize/totalPopSize]
      
      
      meanByPSA[, c(meanFields) := lapply(.SD, function(field) {field * weight}),
                      .SDcols = meanFields, 
                      by = .(seedGroup, parameterSetID)]
      
       meanByPSA[, c(meanFields) := lapply(.SD, sum),
                      .SDcols = meanFields, 
                      by = .(seedGroup, parameterSetID)]
      
      meanByPSA <- meanByPSA[, .SD[1], by = .(seedGroup, parameterSetID)]
      
      saveRDS(meanByPSA, file = paste0(aggregateOutputsDirectory, runDescription,
                                       '_IvsCoutcomeByPSA_and_seedGroup'))
      
      
       
    # I and C - PSA and seedGroup
      
      meanByPSA <- copy(IandCOutcomes)
      
      meanByPSA[, totalPopSize := sum(popSize), by = .(seedGroup, parameterSetID, interventionCohort)]
      meanByPSA[, weight := popSize/totalPopSize]
      
      
      meanByPSA[, c(meanFields) := lapply(.SD, function(field) {field * weight}),
                      .SDcols = meanFields, 
                      by =  .(seedGroup, parameterSetID, interventionCohort)]
      
       meanByPSA[, c(meanFields) := lapply(.SD, sum),
                      .SDcols = meanFields, 
                      by =  .(seedGroup, parameterSetID, interventionCohort)]
      
      meanByPSA <- meanByPSA[, .SD[1], by =  .(seedGroup, parameterSetID, interventionCohort)]
      
      saveRDS(meanByPSA, file = paste0(aggregateOutputsDirectory, runDescription,
                                       '_IvsCoutcomeByPSA_and_seedGroup'))
      
    }
    