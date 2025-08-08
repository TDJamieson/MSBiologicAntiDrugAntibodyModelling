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
      
      runEnv <- 'local'
      
      print(runEnv)

      
    } else {
     
         
      rootDirectory <- "/data/home/wpw004/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting_optimised/")
      
      source(paste0(RProjectDirectory, 'aggregationMasters/', 'lifeCourseAggregationMasterArgs_cluster.R'))
      
      runEnv <- 'scripted'
      
      print(runEnv)
     
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
  
    outputsDirectory <- paste0(runDirectory, 'outputs_aggregated_population/')

  # ---- #   
  
    
  # Create aggregated outputs directory --
  
    suppressWarnings(       
        
        dir.create(paste0(runDirectory, 'outputs_aggregated_CEOutcomes/'))
    
        )
    
    aggregateOutputsDirectory <- paste0(runDirectory, 'outputs_aggregated_CEOutcomes/')
    
  # ---- #
  
# ==============================================================================
    
    
    
#===============================================================================
#                     Define some generic functions                            #
#===============================================================================

    source(paste0(codeDirectory, "defineUtilityFunctions.R"))
    
# ==============================================================================
    


#==============================================================================#
#                       Identify Population to aggregate                       #
#===============================================================================

  # Load aggregation inputs
    files <- list.files(outputsDirectory)
    CEFiles <- files[grepl('aggregatedIvCOutcome', files)]

    aggregated <- lapply(CEFiles, function(file) {

                    readRDS(paste0(outputsDirectory, file))

                })

    aggregated <- rbindlist(aggregated)
    setDT(aggregated)


  # Check that all that should be present are

    # Read in pre-specified populations which should have been run
    samplePopulation <- readRDS(paste0(runDirectory, 'inputPopulations'))
    samplePopulation <- rbindlist(samplePopulation)
    setDT(samplePopulation)
    
    # identify population size for each
    samplePopulation[, minID := min(startID), 
                     by = .(seedGroup, parameterSetID, runDescription)]
    
    samplePopulation[, maxID := max(endID), 
                     by = .(seedGroup, parameterSetID, runDescription)]
    
    samplePopulation[, popSize := maxID - minID + 1]
    
    samplePopulation <- samplePopulation[, .SD[1], 
                     by = .(seedGroup, parameterSetID, runDescription)]
    
    samplePopulation <- samplePopulation[, .(seedGroup, parameterSetID, runDescription, popSize)]

 
 
    # Compare with individuals present in simulated population
    checkPop <- aggregated[, .SD[1], by = .(seedGroup, parameterSetID, runDescription, popSize)][,
      .(seedGroup, parameterSetID, runDescription, popSize)]
    
    missings <- setdiff(samplePopulation, checkPop)

    if(nrow(missings) > 0){

      saveRDS(missings, paste0('aggregateOutputsDirectory', '_missing'))
      stop('At least one individual in sample is not present')

    }

    
    # Add intervention cohort identifier, copy aggregate DT for 
    # different processing and tidy 
    
    aggregated[, interventionCohort := sub(".*_", "", interventionID)]
    
    delta <- copy(aggregated)

    remove(samplePopulation)
   
 
    # Generate means for IVC across whole population and save
    
    meanFields <- colnames(aggregated)[grepl('mean', colnames(aggregated))]
    
    aggregated[, c(meanFields) := lapply(.SD, mean), .SDcols = meanFields,
          by = .(runDescription, parameterSetID, interventionCohort)]
    
    aggregated <- aggregated[, 
                             c(..meanFields, 'runDescription', 
                               'parameterSetID', 'interventionCohort')]
   
    aggregated <- aggregated[, .SD[1], 
                             by = c( 'runDescription', 'parameterSetID', 
                                     'interventionCohort')]
    
    
    saveRDS(aggregated, paste0(aggregateOutputsDirectory, '/',
                         runDirectoryDescription,
                          '_', 'popIvCMeans'))


    # Generate deltas for all 'mean' fields

    meanFields <- colnames(delta)[grepl('mean', colnames(delta))]
    deltaFields <- str_replace(meanFields, 'mean', 'delta')

    delta[, combinedID := str_replace(interventionID,"_.*", "")]

    setorder(delta, runDescription, parameterSetID, combinedID)
    delta[, c(deltaFields) := lapply(.SD, shift), .SDcols = meanFields,
          by = .(runDescription, parameterSetID, combinedID)]

    # Rename
    
    for(fieldName in deltaFields) {

      meanName <- str_replace(fieldName, 'delta', 'mean')
      delta[, c(fieldName) := get(meanName) - get(fieldName)]

    }
 
    delta <- delta[interventionCohort == 1]

  # Save deltas

    saveRDS(delta, paste0(aggregateOutputsDirectory, '/',
                          runDirectoryDescription,
                          '_', 'allDeltas'))
 
    
 
  # Now produce mean value across run description and/or parameter set ID

    meanFields <- str_replace(meanFields, 'mean_', 'meandiff_')
    delta[, c(meanFields) := lapply(.SD, mean), .SDcols = deltaFields,
          by = .(runDescription, parameterSetID)]
    delta <- delta[, .SD[1], by = .(runDescription, parameterSetID)]
    delta <- delta[, c(..meanFields, 'runDescription', 'parameterSetID')]
 
    
    saveRDS(delta, paste0(aggregateOutputsDirectory, '/',
                          runDirectoryDescription,
                          '_', 'popMeanDeltas'))
    
# ==============================================================================
