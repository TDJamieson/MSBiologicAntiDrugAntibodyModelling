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
  
    outputsDirectory <- paste0(runDirectory, 'outputs_all/')

  # ---- #   
  
    
  # Create aggregated outputs directory --
  
    suppressWarnings(       
        
        dir.create(paste0(runDirectory, 'outputs_aggregated_individual/'))
    
        )
    
    aggregateOutputsDirectory <- paste0(runDirectory, 'outputs_aggregated_individual/')
    
  # ---- #
  
# ==============================================================================
    
    
    
#===============================================================================
#                     Define some generic functions                            #
#===============================================================================

    source(paste0(codeDirectory, "defineUtilityFunctions.R"))
    source(paste0(costUtilityCodeDirectory, "applyCostEffectivenessToAggregatedIndividuals.R"))
    source(paste0(costUtilityCodeDirectory, "alemtuzumabADACostEffectiveness.R"))


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
    inputPopulations <- do.call(rbind, inputPopulations)
    setDT(inputPopulations)
    
    
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
 
  # Select population to aggregate from aggregation populations and identify profile
 
    popToAggregate <- as.data.table(aggregationPopulations[[groupProfile]])

    seedGroup <- unique(popToAggregate[, seedGroup])

    parameterSetID <- unique(popToAggregate[, parameterSetID])

    runDescription <- unique(popToAggregate[, runDescription])

    filename <- paste0(runDescription, '__',
                      'SG_', seedGroup, '_',
                      'PSAID_', parameterSetID)

    writeLines(paste0('Aggregating profile ', groupProfile, ' SG_',seedGroup,
                 '_PSAID_', parameterSetID, '_', runDescription))

#===============================================================================

    
    
#==============================================================================#
#     Identify output files and compare against population specified           #
#===============================================================================

       
  # Identify output files
  
      files <- list.files(path = outputsDirectory)
      files <- files[grepl('to', files)]
      
      print(paste0('Aggregating files in ', outputsDirectory))
      
 
      # Read in and aggregate different types of output
        outputTypesWanted <- readRDS(paste0(runDirectory, 'individualOutputsWanted'))
 
        lapply(outputTypesWanted, function (output) {
 
          missingCheck(fileSuffix = output, 
                       inputPopulations = inputPopulations,
                       populationToAggregate = popToAggregate)
          
          specificFiles <- files[grepl(paste0(output, '$'), files)]
          specificFiles <- specificFiles[grepl(filename, specificFiles)]
          
          aggregated <- lapply(specificFiles, function(file) {
              readRDS(paste0(outputsDirectory, file))
            })
 
          aggregated <- rbindlist(aggregated, fill = TRUE)
          
          
          # Check expected number of individuals exist
          
            # Identify min and max personIDs expected in files
            minPersonID <- str_match(specificFiles,"(?<=personID_)(.+)(?=\\_to)")
            maxPersonID <- str_match(specificFiles, "(?<=to_)(.+)(?=\\_)")
            
            minPersonID <- min(as.integer(minPersonID))
            maxPersonID <- max(as.integer(maxPersonID))
            
            
            # Identify personID from interventionID
            aggregated[, personID := str_extract(interventionID, "(?<=->)(.+)(?=\\_)")]
            
            
            # Identify any expected personIDs that are not in the DT, if any 
            missing <- 
              setdiff(c(minPersonID:maxPersonID), aggregated[, as.integer(personID)])
            
            if(!is_empty(missing)){
            
              saveRDS(missing)
              
              stop('Missing personID in file being aggregated')
              
            }
            
            
           saveFile(objectToSave = aggregated,
                   filename = paste0(output,'_',
                                    'SG_', seedGroup,
                                    '_PSAID_', parameterSetID,
                                    '_', runDescription),
                   savePath = paste0(aggregateOutputsDirectory,
                                output,'_',
                                'SG_', seedGroup,
                                '_PSAID_', parameterSetID,
                                '_', runDescription))
           
           
          
        })
      
        
 
#==============================================================================#
#     Generate a single cost-effectiveness outcome for 'fullIvCOutcomes'       #
#===============================================================================
    
    # Read in aggregated IvC outcome
         
      IvCOutcome <- readRDS(paste0(aggregateOutputsDirectory,
                                      'fullIvCOutcomes','_',
                                      'SG_', seedGroup,
                                      '_PSAID_', parameterSetID,
                                      '_', runDescription))
        
    # Identify individuals with unusual outcomes so traces can be generated
    # if desired
        
        unusualOutcomes <- IvCOutcome[iVsCCost > 1000| iVsCCost < -1000 |
                                      iVsCUtility < -0.01]
        
        saveRDS(unusualOutcomes[, .(interventionID, parameterSetID, runDescription,
                                    seedGroup, personID, iVsCCost, iVsCUtility)], 
                paste0(aggregateOutputsDirectory,
                        'unusualTraces','_',
                        'SG_', seedGroup,
                        '_PSAID_', parameterSetID,
                        '_', runDescription),
                compress = FALSE)
        
      # Apply cost effectiveness aggregation
        
        aggregatedIvCOutcome <- generateMeanCEOutcomes(IvCOutcome)
        
        saveRDS(aggregatedIvCOutcome,
                paste0(aggregateOutputsDirectory,
                      'aggregatedIvCOutcome','_',
                      'SG_', seedGroup,
                      '_PSAID_', parameterSetID,
                      '_', runDescription),
                compress = FALSE)

        
    }

    
    
# ==============================================================================
    
    
    

