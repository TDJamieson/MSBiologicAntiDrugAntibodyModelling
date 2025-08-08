#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
#                  Produce outputs from completed runs                         #
#                                                                              #
#==============================================================================#  
#==============================================================================#


#===============================================================================
#                              Load libraries                                  #
#===============================================================================

    # Load libraries

      library(tidyverse)    # -- General helper functions      
      library(plyr)         # --
      library(parallel)     # --

      library(openxlsx)     # Reading in from excel
      library(data.table)   # Providing big data type containers for population

      library(stringr)      # -- String processing functions
      library(stringi)      # --
      
      library(patchwork)

      options(scipen=999)


#==============================================================================#


      
#===============================================================================
#       Set overall project, R project directory and define arguments          #
#===============================================================================


  # Tests to see if being run locally or scripted and defines locations and 
  # arguments accordingly using to separate arguments files

    if (interactive() == TRUE){
      
       rootDirectory <- "D:/QMULOneDrive/NIHR245601MSADA/"
    
       RProjectDirectory <- 
         paste0(rootDirectory,"MSMicrosimulationModel_withADATesting_optimised/")
        
       inputs_outputGenerationDirectory <- 
         paste0(rootDirectory,"inputs_outputGeneration/")  
      
      
    PSADummy <- readline('Enter PSA for PSA, otherwise enter, or any value')
      
    if(grepl('PSA', PSADummy, ignore.case = TRUE)) PSADummy = 1 else PSADummy = 0

    
    if(PSADummy == 0){
      
      testCost <- readline('Enter test cost (entering "default" will use £25 : ')
    
      baseCaseValue <- readline('Enter base case value for varying variable: ')
      
      baseCaseValue <- as.numeric(baseCaseValue)
      
      varyingVarTitle <- readline('Enter label for graph x axis if varying variable present (or na for none)')
      
      breakForPlot <- readline('Would you like to break after plotting to allow customising? (yes/no)')
      
      source(paste0(RProjectDirectory, 'lifeCourseMasters/', 'generateLifeCoursesMasterArgs_local.R'))

      
    } else if (PSADummy == 1){
      
      
      testCost <- readline('Enter test cost (entering "default" will use £25 : ')
      
      breakForPlot <- readline('Would you like to break after plotting to allow customising? (yes/no)')
      

    }  
    
    

    if(grepl('default', testCost, ignore.case = TRUE) == TRUE){
        
        testCost <- 25
        
      }
      
        testCost <- as.numeric(testCost)

      
      } else {
      
      
      rootDirectory <- "/data/home/wpw004/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting_optimised/")
      
      inputs_outputGenerationDirectory <- paste0(rootDirectory,"inputs_outputGeneration/")
      
      source(paste0(RProjectDirectory, 'lifeCourseMasters/', 'generateLifeCoursesMasterArgs_cluster.R'))
      
    }
    

#==============================================================================#      

    
      

#===============================================================================
#               Define directories to source, gather, and output               #
#===============================================================================
 

  # Define all run non-specific locations

    source(paste0(RProjectDirectory, 'universalCode/', 'defineDirectoryLocations.R'))

  # ---- #

      
  # Specific run directory (needs a 'run directory description' to be defined) --

  try(
    runDirectory <- paste0(runsDirectory, runDirectoryDescription, '/')
  )
      
  # ---- #


#==============================================================================#



#===============================================================================
#                     Define some generic functions                            #
#===============================================================================

    source(paste0(codeDirectory, "defineUtilityFunctions.R"))

    source(paste0(codeDirectory,
                  "defineStatisticalFunctions.R"))

    source(paste0(codeDirectory, 'postMicrosimulationProcesses/',
                  "meltMicrosimulationOutcome.R"))

#==============================================================================#


    
#===============================================================================
#                      Load inputs for output generation                       #
#===============================================================================
#                                                                              #
# This loads any output generation supporting files, including some            #
# function containing files.                                                   #
#                                                                              #
#------------------------------------------------------------------------------#

  # Label DMTs code
    source(paste0(postProcessingDirectory, 'labelDMTs.R'))
    
  # Disease course plotting
    source(paste0(outputProcessingDirectory, 'graphical/interventionComparatorTraces.R'))

        
  # Output suporting info from excel file
    
    outputSupportingWorkbook <- loadWorkbook(paste0(inputs_outputGenerationDirectory,
                                                    'outputSupportingInformation.xlsx'))
    
    namedRegions <- getNamedRegions(outputSupportingWorkbook)
    
    tables <- namedRegions[grepl('outputsTable', namedRegions)]
    
    outputTables <- lapply(tables, function(tab) {
                            DT <- read.xlsx(outputSupportingWorkbook, namedRegion = tab)
                            setDT(DT)
                    })
    
    names(outputTables) <- tables
    
    list2env(outputTables, envir = globalenv())
    


# ===============================================================================

   
    
if (PSADummy == 0){
    
#===============================================================================
#                          Read in outcome files                               #
#===============================================================================

  # Read in all files then unlist to global environment
  
    filenames <- list.files(paste0(runDirectory, 'outputs_aggregated_CEOutcomes'))

    outcomeFiles <- lapply(filenames, function(file)
                            {readRDS(paste0(runDirectory, 
                                            'outputs_aggregated_CEOutcomes/', file))})

    names(outcomeFiles) <- filenames

    list2env(outcomeFiles, envir = globalenv())

    
# ===============================================================================

    
    
#===============================================================================
#                     Read in different outcome outputs                        #
#===============================================================================

    
  # Identify popMeanDeltas in environment

    popMeanDeltas <- copy(get(paste0(runDescription, '_popMeanDeltas')))
    
    
  # Identify IvCMeanDeltas in environment

    IvCMeanDeltas <- copy(get(paste0(runDescription, '_popIvCMeans')))
  
     
  # Identify all deltas in environment

    allDeltas <- copy(get(paste0(runDescription, '_allDeltas')))
            
 
#===============================================================================

    
    
#===============================================================================
#                   Add test costs and costs/QALY for all                      #
#===============================================================================
    
  # IvC ------------------------------------------------------------------------
    
  # Add I V C labels

    IvCMeanDeltas[, interventionCohort :=
                    ifelse(interventionCohort == 0, 'Comparator', 'Intervention')]

 
  # Test cost and cost/QALY
   
    IvCMeanDeltas[, `:=` (mean_lifetime_TestCosts = testCost * mean_lifetime_TestCosts,
                          mean_lifetime_TestCosts_Discounted = testCost * mean_lifetime_TestCosts_Discounted)]
    
    IvCMeanDeltas[, `:=`(mean_lifetime_cycleCosts = mean_lifetime_cycleCosts + mean_lifetime_TestCosts,
                         mean_lifetime_cycleCosts_discounted = 
                         mean_lifetime_cycleCosts_discounted + mean_lifetime_TestCosts_Discounted)]
    
    IvCMeanDeltas[, `:=` (mean_cost_qaly = 
                            mean_lifetime_cycleCosts/mean_lifetime_utility,
                          mean_cost_qaly_discounted = 
                            mean_lifetime_cycleCosts_discounted/mean_lifetime_utility_discounted)]
    
    
  # ---- #
    
    
  # All deltas -----------------------------------------------------------------
     
  

    allDeltas[, `:=` (mean_lifetime_TestCosts = testCost * mean_lifetime_TestCosts,
                      mean_lifetime_TestCosts_Discounted = testCost * mean_lifetime_TestCosts_Discounted,
                      delta_lifetime_TestCosts = testCost * delta_lifetime_TestCosts,
                      delta_lifetime_TestCosts_Discounted = testCost * delta_lifetime_TestCosts_Discounted)]
    
    allDeltas[, `:=` (mean_lifetime_cycleCosts = mean_lifetime_cycleCosts + mean_lifetime_TestCosts,
                      mean_lifetime_cycleCosts_discounted = mean_lifetime_cycleCosts_discounted + mean_lifetime_TestCosts_Discounted,
                      delta_lifetime_cycleCosts = delta_lifetime_cycleCosts + delta_lifetime_TestCosts,
                      delta_lifetime_cycleCosts_discounted = delta_lifetime_cycleCosts_discounted + delta_lifetime_TestCosts_Discounted)]
                               
    allDeltas[, `:=` (meandiff_cost_qaly = 
                            delta_lifetime_cycleCosts/delta_lifetime_utility,
                          meandiff_cost_qaly_discounted = 
                            delta_lifetime_cycleCosts_discounted/delta_lifetime_utility_discounted)]
    
  # ---- #
    
    
    
  # Population mean deltas -----------------------------------------------------
    
    popMeanDeltas[, `:=` (meandiff_lifetime_TestCosts = testCost * meandiff_lifetime_TestCosts,
                          meandiff_lifetime_TestCosts_Discounted = testCost * meandiff_lifetime_TestCosts_Discounted)]  
      
    popMeanDeltas[, `:=`(meandiff_lifetime_cycleCosts = meandiff_lifetime_cycleCosts + meandiff_lifetime_TestCosts,
                         meandiff_lifetime_cycleCosts_discounted = 
                         meandiff_lifetime_cycleCosts_discounted  + meandiff_lifetime_TestCosts_Discounted)]
    
    popMeanDeltas[, `:=` (meandiff_cost_qaly = 
                            meandiff_lifetime_cycleCosts/meandiff_lifetime_utility,
                          meandiff_cost_qaly_discounted = 
                            meandiff_lifetime_cycleCosts_discounted/meandiff_lifetime_utility_discounted)]
    
  # ---- #
  
#===============================================================================
 
    
     
#===============================================================================
# Split processing depending on whether a varying variable has been specified  #
#===============================================================================
    
    if(sum(grepl('vary', popMeanDeltas[, runDescription])) > 0){

      source(paste0(outputProcessingDirectory, 'varyingVariableOutcomeProcessing.R'))

    } else {
      
      source(paste0(outputProcessingDirectory, 'noVaryingVariableOutcomeProcessing.R'))
      
    }

#===============================================================================    

} else if(PSADummy == 1){
  
    source(paste0(outputProcessingDirectory, 'PSAOutcomeProcessing.R')) 
  
}
    
    
    
# # For plotting
# 
#     trace <- readRDS(paste0(runDirectory, 'outputs_all/', 'RLHAlemtuzumabPop_BC_noDMTEffects__SG_1408371_PSAID_1_personID_1_to_100_lifeCourseOutcomes'))
# 
#     outcome <- readRDS(paste0(runDirectory, 'outputs_all/', 'RLHAlemtuzumabPop_BC_noDMTEffects__SG_1408371_PSAID_1_personID_1_to_100_fullIvCOutcomes'))
# 
# 
#     trace[, combinedID := paste0(seedGroup, '->', personID)]
#     trace[grepl('_1', interventionID), interventionCohort := 1]
#     trace[grepl('_0', interventionID), interventionCohort := 0]
# 
# 
#     outcome[, combinedID := paste0(seedGroup, '->', personID)]
#     outcome[grepl('_1', interventionID), interventionCohort := 1]
#     outcome[grepl('_0', interventionID), interventionCohort := 0]
# 
#     setorder(outcome, interventionID)
#     outcome[, shiftCosts := shift(lifetime_cycleCosts), by = combinedID]
#     outcome[, shiftUtility := shift(lifetime_utility), by = combinedID]
# 
#     differingIDs <- outcome[!is.na(shiftCosts) & shiftCosts != lifetime_cycleCosts & timeToDeath <10, combinedID]
#     poorOutcomeIDs <- outcome[shiftCosts < lifetime_cycleCosts, combinedID]
#     goodOutcomeIDs <- outcome[lifetime_cycleCosts < shiftCosts, combinedID]
# 
#     poorUtilityOutcomeIDs <- outcome[shiftUtility > lifetime_utility, combinedID]
#     goodUtilityOutcomeIDs <- outcome[shiftUtility < lifetime_utility, combinedID]
# 
#      ID <- sample(goodOutcomeIDs, 1)
# 
#       traceComparisonDiseaseCourse(trace, outcome, ID)
# 
# 
# 
# 
#     dir.create(paste0(runDirectory, 'plots_worseCostOutcomes'))
# 
#     lapply(1:5, function(plot) {
# 
#       ID <- sample(poorOutcomeIDs[1:5], 1)
# 
#       traceComparisonDiseaseCourse(trace, outcome, ID)
# 
#       ggsave(filename = paste0(runDirectory, 'plots_worseCostOutcomes/', 'ID_', gsub('->', '_', ID), '.png'), width = 12, height = 10)
# 
#     })
# 
#     dir.create(paste0(runDirectory, 'plots_betterCostOutcomes'))
# 
#     lapply(1:5, function(plot) {
# 
#       ID <- sample(goodOutcomeIDs, 1)
# 
#       traceComparisonDiseaseCourse(trace, outcome, ID)
# 
#       ggsave(filename = paste0(runDirectory, 'plots_betterCostOutcomes/', 'ID_', gsub('->', '_', ID), '.png'), width = 12, height = 10)
# 
#     })
# 
# 
#     dir.create(paste0(runDirectory, 'plots_worseUtilityOutcomes'))
# 
#     lapply(1:5, function(plot) {
# 
#       ID <- sample(poorUtilityOutcomeIDs, 1)
# 
#       traceComparisonDiseaseCourse(trace, outcome, ID)
# 
#       ggsave(filename = paste0(runDirectory, 'plots_worseUtilityOutcomes/', 'ID_', gsub('->', '_', ID), '.png'), width = 12, height = 10)
# 
#     })
# 
#     dir.create(paste0(runDirectory, 'plots_betterUtilityOutcomes'))
# 
#     lapply(1:5, function(plot) {
# 
#       ID <- sample(goodUtilityOutcomeIDs, 1)
# 
#       traceComparisonDiseaseCourse(trace, outcome, ID)
# 
#       ggsave(filename = paste0(runDirectory, 'plots_betterUtilityOutcomes/', 'ID_', gsub('->', '_', ID), '.png'), width = 12, height = 10)
# 
#     })
