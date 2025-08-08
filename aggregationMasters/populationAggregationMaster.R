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
      
      library(parallel)
      
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
  
    outputsDirectory <- paste0(runDirectory, 'outputs_aggregated_individual/')

  # ---- #   
  
    
  # Create aggregated outputs directory --
  
    suppressWarnings(       
        
        dir.create(paste0(runDirectory, 'outputs_aggregated_population/'))
    
        )
    
    aggregateOutputsDirectory <- paste0(runDirectory, 'outputs_aggregated_population/')
    
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
    
    aggregationPopulations <- readRDS(paste0(runDirectory, 
                                             'populationLevelAggregations'))
    
    
  # Identify no of profiles to aggregate
    
    noProfiles <- length(aggregationPopulations)
    
  
  # If running scripted then set noProfiles to 1 to run only once
   
    if(grepl('scripted', runEnv) == TRUE){

            noProfiles <- 1
  
     }
  
  # Run over all profiles from 1 to number of profiles
 
    for (groupProfile in 1:noProfiles) {
    
  
  # If running scripted then set profile to passed profile
  
    if(grepl('scripted', runEnv) == TRUE){
        
      groupProfile <- passedProfile
    
    } 
 
    
  # Select population to aggregate and identify PSA ID and variation
  # through run description
      
    popToAggregate <- as.data.table(aggregationPopulations[[groupProfile]])

    parameterSetID <- unique(popToAggregate[, parameterSetID])

    runDescription <- unique(popToAggregate[, runDescription])

    filename <- paste0(runDescription, '__',
                      'PSAID_', parameterSetID)

    writeLines(paste0('Aggregating profile ',
                 'PSAID_', parameterSetID, '_', runDescription))
    
      
#==============================================================================#
#     Identify output files and compare against population specified           #
#===============================================================================

 
  # Identify output files
  
      files <- list.files(path = outputsDirectory)
      
      print(paste0('Aggregating files in ', outputsDirectory))
      
  
      # Read in and aggregate different types of output
        outputTypesWanted <- readRDS(paste0(runDirectory, 'populationOutputsWanted'))
 
        lapply(outputTypesWanted, function (output) {
           
          print(paste0('Aggregating ', output))
                       
           # Construct filenames
          popToAggregate[,filename := paste0(output, '_SG_', seedGroup, 
                                             '_PSAID_', parameterSetID, '_', runDescription)]
          
          specificFiles <- files[files %in% popToAggregate[, filename]]
          
          if(runEnv == 'local'){
          
            aggregated <- lapply(specificFiles, function(file) {
            
              readRDS(paste0(outputsDirectory, file))
              
            })
            
          }
           
          if(runEnv == 'scripted'){
          
          print('Parallel loading')  
            
          # Set up clusters
          
            cl <- makeCluster(8)
      
            clusterExport(cl, c('specificFiles', 'outputsDirectory'), envir = environment())
      
            aggregated <- 
              parLapply(cl, specificFiles, function(file) {
              
                readRDS(paste0(outputsDirectory, file))
                
              })
        
             stopCluster(cl)
      
          }
          
          aggregated <- rbindlist(aggregated, fill = TRUE)
          
            
          saveFile(objectToSave = aggregated,
                   filename = paste0(output,'_',
                                    '_PSAID_', parameterSetID,
                                    '_', runDescription),
                   savePath = paste0(aggregateOutputsDirectory,
                                output,'_',
                                '_PSAID_', parameterSetID,
                                '_', runDescription))
           
           print(paste0('Saved: ', paste0(aggregateOutputsDirectory,
                            output,'_',
                            '_PSAID_', parameterSetID,
                            '_', runDescription)))
          
        })
      
    }

    
    
    
