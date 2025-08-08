#==============================================================================#
#==============================================================================#
#                                                                              #
#---    Aggregate split populations run through microsimulation process     ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#


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

      library(qs)           # Fast file reading and writing

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
    
    source(paste0(RProjectDirectory, "aggregationMasters/aggregationFunctions.R"))


# ==============================================================================
 
    
    
#===============================================================================
#                         Define output directories                            #
#===============================================================================
    

      outputsDirectory <- paste0(runDirectory, 'outputs_all/')
    
      unaggregatedOutputDirectory <- paste0(outputsDirectory, 'unaggregatedOutputs/')
    
      seedGroupaggregatedOutputDirectory <- paste0(outputsDirectory, 'seedGroupaggregatedOutputs/')
    
      seedGroupPSAIDaggregatedOutputDirectory <- paste0(outputsDirectory, 'seedGroupPSAIDaggregatedOutputs/')
    
      personIDaggregatedOutputDirectory <- paste0(outputsDirectory, 'personIDaggregatedOutputs/')
      
      fullyAggregatedOutputDirectory <- paste0(outputsDirectory, 'fullyAggregatedOutputs/')

      chunkAggregatedOutputDirectory <- paste0(outputsDirectory, 'chunkAggregatedOutputs/')
      
      variationsAggregatedOutputDirectory <- paste0(outputsDirectory, 'variationsOutputs/')
      
      singleAggregatedOutputDirectory <- paste0(outputsDirectory, 'singleAggregatedOutput/')  


    
#===============================================================================
 
    
  # Collapse by variation ------------------------------------------------------
   
    if(variations_Switch == 1){
     
      
     # Turn off all other aggregations
     
     lapply(ls(pattern='ggregat.*_Switch'), function(switch){
       assign(switch, 0, envir = globalenv()) 
     })
     
    
      combineFolder(folder = variationsAggregatedOutputDirectory, 
                    byGrouping = c('runDescription'), 
                    outputDirectory = outputsDirectory, 
                    aggregationDescription = 'Variation')
      
    }
    
  # ----------------------------------------------------------------------------
      
    
  # Collapse across whole population (without variation) ----------------------- 
      
      
    if(aggregateAllExcludingVariation_Switch == 1){
     
      combineFolder(folder = singleAggregatedOutputDirectory, 
                    byGrouping = c('runDescription'), 
                    outputDirectory = outputsDirectory, 
                    aggregationDescription = 'fullyCollapsed')
      
    }

  
  # ----------------------------------------------------------------------------
      
      
      
  # Collapse across seed Group ------------------------------------------------- 
      
      
    if(aggregateSeedGroup_Switch == 1){
     
      combineFolder(folder = seedGroupaggregatedOutputDirectory, 
                    byGrouping = c('seedGroup'), 
                    outputDirectory = outputsDirectory, 
                    aggregationDescription = 'bySeedGroup')
      
    }

  
  # ----------------------------------------------------------------------------

      
      
  # Collapse across seed Group and parameter set ------------------------------- 
      
      
    if(aggregateSeedGroupPSAID_Switch == 1){
     
      combineFolder(folder = seedGroupPSAIDaggregatedOutputDirectory, 
                    byGrouping = c('seedGroup', 'parameterSetID'), 
                    outputDirectory = outputsDirectory, 
                    aggregationDescription = 'bySeedGroupandPSASet')
      
    }

  
  # ----------------------------------------------------------------------------
      
      
  # Collapse across parameter set ---------------------------------------------- 
      
      
    if(aggregatePSAID_Switch == 1){
     
      combineFolder(folder = seedGroupPSAIDaggregatedOutputDirectory, 
                    byGrouping = c('parameterSetID'), 
                    outputDirectory = outputsDirectory, 
                    aggregationDescription = 'byPSASet')
      
    }

  
  # ----------------------------------------------------------------------------


      
      
   # Collapse across personID -------------------------------------------------- 
      
    if(aggregatePersonID_Switch == 1){
     
      combineFolder(folder = personIDaggregatedOutputDirectory, 
                    byGrouping = c('personID'), 
                    outputDirectory = outputsDirectory, 
                    aggregationDescription = 'byPersonID')
      
    }

  
  # ----------------------------------------------------------------------------

      
      
      
      
 # Combine outputs aggregated by chunk size for tracing ------------------------
        
    if (chunkSizeAggregation_Switch == 1){
      
      
      # Combine separate runs
        
        iterationsCombined <- combineFolder(chunkAggregatedOutputDirectory)
        IVC <- iterationsCombined$combinedIVC
        IandC <- iterationsCombined$combinedIandC
      
      
      # Identify fields to take averages for
        
        meanCEFields <- colnames(IVC)[grepl('lifetime', colnames(IVC))]
  
        clinicalOutcomeFields <- c('totalRelapses', 'timeToSPMS', 'timeToDeath', 'timeOnAnyDMT', 
                                    'timeOnAlemtuzumab', 'alemtuzumabDoses', 'ageAtDeath')
    
        meanClinicalOutcomeFields <- paste0('mean_', clinicalOutcomeFields)
    
      
      # Aggregate - I vs C
      
        aggregateAcrossAllSeedGroupsIVC <- 
          
          lapply(1:length(unique(IVC[, seedGroup])),
               function(num) {
                 
                 DT <- IVC[, .SD[num], by = seedGroup]
                 
                 DT[, c(meanCEFields) := lapply(.SD, mean), 
                              .SDcols = meanCEFields]
            
                 DT[, c(meanClinicalOutcomeFields) := lapply(.SD, mean), 
                                    .SDcols = meanClinicalOutcomeFields]
                 
                 DT[, aggregateNum := num]
                 
                 DT <- DT[1]
                 
               })
        
        
        aggregateAcrossAllSeedGroupsIVC <- rbindlist(aggregateAcrossAllSeedGroupsIVC)
        
          
        saveFile(filename = paste0('aggregated_', runDescription, '_IVCOutcomesByChunk'),
                aggregateAcrossAllSeedGroupsIVC, 
                chunkAggregatedOutputDirectory)
        
        remove(aggregateAcrossAllSeedGroupsIVC)
      
      
      #  Aggregate - I and C
        
        aggregateAcrossAllSeedGroupsIandC <- 
          
          lapply(1:length(unique(IandC[, seedGroup])),
               function(num) {
                 
                 DT <- IandC[, .SD[num], by = seedGroup]
                 
                 DT[, c(meanCEFields) := lapply(.SD, mean), 
                              .SDcols = meanCEFields]
            
                 DT[, c(meanClinicalOutcomeFields) := lapply(.SD, mean), 
                                    .SDcols = meanClinicalOutcomeFields]
                 
                 DT[, aggregateNum := num]
                 
                 DT <- DT[1]
                 
               })
        
        
        aggregateAcrossAllSeedGroupsIandC <- rbindlist(aggregateAcrossAllSeedGroupsIandC)
        
          
        saveFile(filename = paste0('aggregated_', runDescription, '_IandCOutcomesByChunk'),
                aggregateAcrossAllSeedGroupsIandC, 
                chunkAggregatedOutputDirectory)
        
        remove(aggregateAcrossAllSeedGroupsIandC)
      
      
    }
  
 # ----------------------------------------------------------------------------