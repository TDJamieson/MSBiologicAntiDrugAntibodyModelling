#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Relapsing-remitting Multiple Sclerosis Microsimulation Model       ---#
#              Specification of population runs and processing                 #
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

      options(scipen=999)


#==============================================================================#



#===============================================================================
#       Set overall project, R project directory and define arguments          #
#===============================================================================


  # Project directories --
      
      rootDirectory <- "D:/QMULOneDrive/NIHR245601MSADA/"
      
      RProjectDirectory <- paste0(rootDirectory,"MSMicrosimulationModel_withADATesting_optimised/")
      
      
  # Specific run directory location and description --

      runDirectoryDescription <- readline('Please enter run directory description')
      runDescription <- readline('Please enter run description (probably same as above)')
      runsDirectory <- paste0(rootDirectory, 'modelRuns/')
    
  # ---- # 
    
 
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

#==============================================================================#

 
    
#===============================================================================
#                              Load inputs                                     #
#===============================================================================
#                                                                              #
# This uses the same process as has been coded into the microsimulation to     #
# initialise a population, so although in principle we are only interested in  #
# generating the cohorts to be run, the whole population initialisation        #
# process needs to be run to do that, which requires access to the run         #
# non-specific inputs as well as the run-specific components; these are all    #
# loaded here.                                                                 #
#                                                                              #
#------------------------------------------------------------------------------#

  # Run-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_runControls.R"))

  # Universal components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_universal.R"))

  # Project-specific components
    source(paste0(microsimulationInputProcessesDirectory, "inputs_ADA.R"))

  # Sense check inputs
    source(paste0(microsimulationInputProcessesDirectory, "inputs_check.R"))

  # Undertake some generic processing of inputs - e.g. PSA parameterisation
   # source(paste0(microsimulationInputProcessesDirectory, "inputs_prepare.R"))
    
  # Load cost-utility inputs
    source(paste0(costUtilityCodeDirectory, 'loadCostUtilityInputs.R'))    


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
                              'NA'),
      
      
      dmtPriceDiscountingOn = dmtDiscounting_Switch,
    
      dmtSpecificDiscountSwitch = dmtSpecificDiscount_Switch,
      
      universalDMTDiscountSwitch = universalDMTDiscount_Switch,
      
      costsDiscountRate = costDiscountRateTo30Years,
        
      effectsDiscountRate = effectDiscountRateTo30Years
      
    )
 
    dir.create(paste0(runDirectory, 'inputParameters'))
    saveRDS(inputParameters, file = paste0(runDirectory, 'inputParameters/','inputParameters'))


#===============================================================================
    
    
    
#==============================================================================#
#       Identify Population splits and groups that need aggregating            #
#===============================================================================

    source(paste0(microsimulationSetupDirectory, 'createPopulation/',
                  "specifyPopulations.R"))

    inputPopulations <- MSSampleSplits
 
    if(variations_Switch == 1){
    
      aggregatePopulations <- 
        inputPopulations[, .SD[1],
                       by = .(seedGroup, parameterSetID, get(varyingVar))]
    
    } else {
        
      aggregatePopulations <- 
        inputPopulations[, .SD[1],
                       by = .(seedGroup, parameterSetID)]
      
    }
    


#===============================================================================

    

#==============================================================================#
#               Save input and aggregation populations                         #
#===============================================================================

    lifeCourseIterations <- nrow(inputPopulations)
    saveRDS(inputPopulations, paste0(runDirectory, 'inputPopulations'))
    saveRDS(NULL, paste0(runDirectory, lifeCourseIterations,
                         'inputPopulations'))


    individualsToAggregate <- nrow(aggregatePopulations)
    saveRDS(aggregatePopulations, paste0(runDirectory, 'aggregationPopulations'))
    saveRDS(NULL, paste0(runDirectory, individualsToAggregate,
                         'aggregationPopulations'))

#===============================================================================

    

# =============================================================================#
#                     Create Apocrita Instruction Files                        #
#===============================================================================

    source(paste0(RProjectDirectory,
                  'initialSetup/',
                  "apocritaInstructionGeneration.R"))

#===============================================================================
