#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
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
      library(doParallel)   # Providing capability for parallelising processes
      
                            # --
      library(stringr)      # -- String processing functions
      library(stringi)      # --
      
      library(dqrng)        # Random numbers - 64-bit general purpose
      library(dirmult)      # Random numbers - dirichlet distributions

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
      
      source(paste0(RProjectDirectory, 'lifeCourseMasters/', 'generateLifeCoursesMasterArgs_local.R'))
      
    
      } else {
      
      
      rootDirectory <- "/data/home/wpw004/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting_optimised/")
      
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

    runDirectory <- paste0(runsDirectory, runDirectoryDescription, '/') 

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
    
    source(paste0(codeDirectory, 'postMicrosimulationProcesses/', 'labelDMTs.R'))
    
    source(paste0(costUtilityCodeDirectory, "alemtuzumabADACostEffectiveness.R"))

    source(paste0(RProjectDirectory, "aggregationMasters/aggregationFunctions.R"))

    
#==============================================================================#   
    
    

#===============================================================================
#                    Define microsimulation process                            #
#===============================================================================

  source(paste0(microsimulationProcessDirectory, "microsimulationProcess.R"))

#==============================================================================#   
    
    
    
#===============================================================================
#                              Load inputs                                     #
#===============================================================================
#                                                                              #
# Using openxlsx this takes named regions from an excel model spreadsheet and  #
# creates data tables/matrices.                                                #
#                                                                              #
# For transition matrices the from and to states should be in the first column #
# and row of the named range respectively so that the colNames and rowNames    #
# arguments in read.xlsx can pick them up.                                     #
#                                                                              #
# This uses the 'inputValues.xlsx' file that should be held in the model run   #
# run directory specified.                                                     #
#                                                                              #
#------------------------------------------------------------------------------#
 
  # Run-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_runControls.R"))
    
  # Universal components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_universal.R"))
    
  # Project-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_ADA.R"))
  
  # Undertake some generic processing of inputs - e.g. PSA parameterisation
    source(paste0(microsimulationInputProcessesDirectory, "inputs_prepare.R"))
    
  # Load cost-utility inputs
    source(paste0(costUtilityCodeDirectory, 'loadCostUtilityInputs.R'))    

#===============================================================================
    
    
    
#===============================================================================
#                         Define output directories                            #
#===============================================================================
    
  
   suppressWarnings(
      dir.create(paste0(runDirectory, 'outputs_all/'))
    )
    outputsDirectory <- paste0(runDirectory, 'outputs_all/')
    
    
    if(saveUnaggregated_Switch == 1){
      
       suppressWarnings(
      dir.create(paste0(outputsDirectory, 'unaggregatedOutputs/'))
    )
    
      unaggregatedOutputDirectory <- paste0(outputsDirectory, 'unaggregatedOutputs/')
      
    }
    
    
    if(aggregateSeedGroup_Switch == 1){
      
       suppressWarnings(
      dir.create(paste0(outputsDirectory, 'seedGroupaggregatedOutputs/'))
    )
    
      seedGroupaggregatedOutputDirectory <- paste0(outputsDirectory, 'seedGroupaggregatedOutputs/')
      
    }
    
    
    if(aggregateSeedGroupPSAID_Switch == 1){
      
       suppressWarnings(
      dir.create(paste0(outputsDirectory, 'seedGroupPSAIDaggregatedOutputs/'))
    )
    
      seedGroupPSAIDaggregatedOutputDirectory <- paste0(outputsDirectory, 'seedGroupPSAIDaggregatedOutputs/')
      
    }
    
    
    if(aggregatePersonID_Switch == 1){
      
       suppressWarnings(
      dir.create(paste0(outputsDirectory, 'personIDaggregatedOutputs/'))
    )
    
      personIDaggregatedOutputDirectory <- paste0(outputsDirectory, 'personIDaggregatedOutputs/')
      
    }
    
    
    if(aggregateAllExcludingVariation_Switch == 1){
      
       suppressWarnings(
      dir.create(paste0(outputsDirectory, 'fullyAggregatedOutputs/'))
    )
    
      fullyAggregatedOutputDirectory <- paste0(outputsDirectory, 'fullyAggregatedOutputs/')
      
    }
    
    
    if(chunkSizeAggregation_Switch == 1){
      
      suppressWarnings(
      dir.create(paste0(outputsDirectory, 'chunkAggregatedOutputs/'))
    )
    
      chunkAggregatedOutputDirectory <- paste0(outputsDirectory, 'chunkAggregatedOutputs/')
      
    }
    
    
    if(aggregatePersonID_Switch == 1){
          
      
      suppressWarnings(
      dir.create(paste0(outputsDirectory, 'personIDAggregatedOutputs/'))
    )
    
      personIDaggregatedOutputDirectory <- paste0(outputsDirectory, 'personIDAggregatedOutputs/')  
        
    }
    
    
    if(aggregateAllExcludingVariation_Switch == 1){
          
      
      suppressWarnings(
      
        dir.create(paste0(outputsDirectory, 'singleAggregatedOutput/'))
        
      )
    
      singleAggregatedOutputDirectory <- paste0(outputsDirectory, 'singleAggregatedOutput/')  
        
    }
    
    
    if(variations_Switch == 1){
          
      suppressWarnings(
        dir.create(paste0(outputsDirectory, 'variationsOutputs/'))
      )
    
      variationsAggregatedOutputDirectory <- paste0(outputsDirectory, 'variationsOutputs/')
      
    }

#===============================================================================
    
    
    
#==============================================================================#
#           Load pre-specified populations to run through model                #
#===============================================================================

    MSSampleSplits <- readRDS(paste0(runDirectory, 'inputPopulations'))

#===============================================================================
    


#==============================================================================#
#                 Take population specified by iteration number                #
#===============================================================================

# The inputPopulations RDS above returns a list of data tables with 
# populations that have been split into appropriate sizes given the maxPopSize 
# that can be run, specified in the controls spreadsheet


    # If running on cluster then take a single MSSample split  to run through
    # microsimulation process

    if(interactive() == FALSE){

      MSSampleSplits <- MSSampleSplits[passedProfile]

    }


    for (iteration in 1:nrow(MSSampleSplits)) {

      MSSample <- MSSampleSplits[iteration]
      setDT(MSSample)
 
#-------------------------------------------------------------------------------


      
#==============================================================================#
#                 If varying variable exists then extract                      #
#===============================================================================
       
      if(variations_Switch == 1) {
        
        varyingVarVal <- unique(MSSample[, get(varyingVar)])
        
      }
      

#==============================================================================#
#                           Initialise population                              #
#===============================================================================

    source(paste0(microsimulationSetupDirectory, 'createPopulation/',
                  'initialisePopulation.R'))

#===============================================================================


 
#==============================================================================#
#                   Create outcome random no databases                         #
#===============================================================================

    source(paste0(microsimulationSetupDirectory, 'generateOutcomeRandomNos.R'))


#===============================================================================


    
#==============================================================================#
#                         Parameterise Population                              #
#===============================================================================

    source(paste0(microsimulationSetupDirectory, 'parameterisePopulation/',
                  "parameterisePopulation.R"))

#===============================================================================

    
 
#==============================================================================#
#               Reparameterise varying variable if desired                     #
#===============================================================================

  if(variations_Switch == 1){
 
    MSSample[, c(varyingVar) := varyingVarVal]
    
  }
    
#===============================================================================
    
    


#==============================================================================#
#                          Initialise switching                                #
#===============================================================================

  source(paste0(microsimulationSetupDirectory, "initialiseSwitching.R"))

#===============================================================================



#==============================================================================#
#                      Run microsimulation process                             #
#===============================================================================

  # Run model and assign outcome

  microsimulationOutcome <-
    microsimulationProcess(workingSample = MSSample,
                           outcomeRandNos = outcomeRandNos,
                           DMTRRMSEDSSEffects = DMTRRMSEDSSEffects,
                           DMTARREffects = DMTARREffects,
                           DMTSPMSTransitionEffects = DMTSPMSTransitionEffects,
                           RRMSEDSSTransitionMatrix_Rates = transitionMatrix_EDSS_RRMS_Rates,
                           SPMSEDSSTransitionMatrix = transitionMatrix_EDSS_SPMS,
                           RRMStoSPMSTransitionMatrix = transitionMatrix_RRMStoSPMS)

 
  remove(MSSample)

  
  # Address censoring - for those that didn't enter SPMS, they are recorded as 
  # having reached SPMS either at death or age 100
    
    microsimulationOutcome[is.na(timeToSPMS), 
                           timeToSPMS := ifelse(is.na(Reached_100), timeToDeath, timeToReached100)]
  
#===============================================================================



#==============================================================================#
#       Identify important parameters to include in run description            #
#===============================================================================

    inputParameters <- data.table(

      startingTreatmentLine = startingTreatmentLine,

      startingDMT = ifelse(singleSpecifiedStartingDMT_Switch == 1,
                           includedDMTList[dmtID == singleSpecifiedStartingDMTID,
                                           str_to_title(Name)],
                           'More than one starting DMT'
      ),
      
      continueThirdLineDMT_Switch = continueThirdLineDMT_Switch,
      
      alemtuzumabADATesting = alemtuzumabADATesting_Switch,
      
      otherDMTADATesting = otherDMTADATesting_Switch,
      
      universalADARiskSwitch = universalADARisk_Switch,

      alemtuzumabADAProportion = alemtuzumabADAProportion[, Mean],

      alemtuzumabEffectiveness = alemtuzumabEffectiveness,

      alemtuzumabTestingFNRate = alemtuzumabTestingFNRate,

      alemtuzumabTestingFPRate = alemtuzumabTestingFPRate,


      DMTEffectsActive = ifelse(DMTEffect_Switch == 1,
                                'Yes',
                                'No'),

      DMTSwitchingActive = ifelse(DMTSwitching_Switch,
                                  'Yes',
                                  'No'),

      DMTSwitchingLevel = ifelse(individualLevelSwitchSequencing_Switch == 1,
                                 'Individual',
                                 ifelse(groupLevelSwitchSequencing_Switch == 1,
                                        'Group',
                                        ifelse(parameterSetLevelSwitchSequencing_Switch == 1,
                                               'Parameter Set', 'Unknown'))),

      relapseRateModified = ifelse(ARRModifier_Switch == 1, 
                                   'Modified relapse rate',
                                   'Unmodified relapse rate'),
      
      relapseRateModifier = ifelse(ARRModifier_Switch == 1, 
                                   ARRMultiplier_Value,
                                   NA),
      
      relapseDistribution = relapseDistribution,

      mortalitySet = mortalitySet,

      PSA = ifelse(PSA_Switch == 1, 'PSA on', 'PSA off'),
      
      varationsOn = variations_Switch,
      
      variationArray = ifelse(variations_Switch == 1,
                              paste0(min(variationArray), '-', max(variationArray)),
                              'NA')
      
    )
    
    
#==============================================================================#
    
    
    
#==============================================================================#
#                         Process and save outcomes                            #
#===============================================================================


  # Cycle independent outcomes ----

    # Select cycle independent fields

      cycleIndependentFields <- colnames(microsimulationOutcome)[!grepl('Cycle', colnames(microsimulationOutcome))]
      cycleIndependentOutcomes <- microsimulationOutcome[, c(..cycleIndependentFields)]


  # Long format life courses (split into person and parameter set) ----

 
    # Produce long format
 
      lifeCourseTrace <-
        wideToLongMicrosimulationOutcome(microsimulationDT = microsimulationOutcome)


    # Apply CE values

      source(paste0(RProjectDirectory, 'lifeCourseMasters/applyCEToLifeCourseMaster.R'))
      
      
    # Save outcomes at desired aggregation levels
      
      source(paste0(RProjectDirectory, 'lifeCourseMasters/saveOutcomes.R'))


}



