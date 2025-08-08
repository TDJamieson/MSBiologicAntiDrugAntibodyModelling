#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Apply cost-effectiveness values to completed life courses          ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#
#                                                                              #
#  This process takes all of the split microsimulation runs, checks that the   #
#  whole population desired has been simulated, and then applies               #
#  cost-effectiveness values to each.                                          #
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
                            
      library(stringr)      # -- String processing functions
      library(stringi)      # --

      library(dqrng)        # Random numbers - 64-bit general purpose

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
#                Define population that should have been run                   #
#===============================================================================
#                                                                              #
# Using openxlsx this loads the control spreadsheet inputs and then uses the   #
# population generation process used to specify the simulation runs to         #
# identify which outputs ought to have been generated.                         #
#                                                                              #
# ============================================================================ #                                                                              

  # Load run-specific component inputs
    source(paste0(microsimulationSetupDirectory, 
                  'loadInputs/', "inputs_runControls.R"))

  
  # Define some functions of incidental necessity
    source(paste0(codeDirectory, "defineUtilityFunctions.R"))
    
    source(paste0(codeDirectory, 
                  "defineStatisticalFunctions.R")) 
    
    source(paste0(codeDirectory, 'postMicrosimulationProcesses/', 'labelDMTs.R'))
  
    
  # Load universal components which are also of incidental necessity
    source(paste0(microsimulationSetupDirectory, 'loadInputs/', 
                  "inputs_universal.R"))

    
  # Load cost-utility inputs
    source(paste0(costUtilityCodeDirectory, 'loadCostUtilityInputs.R'))    

        
  # Generate population 
    source(paste0(microsimulationSetupDirectory, 'createPopulation/', 
                  "specifyPopulations.R"))

# ============================================================================ #
    
    
    
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
  


    for (iteration in 1:length(MSSampleSplits)) {

       population_s <- MSSampleSplits[[iteration]]
       population_s <- split(population_s, 1:nrow(population_s))
      
     # Select and read in population

      lapply(population_s, function(population){
        
      setDT(population)

      seedGroup <- population[, seedGroup]

      parameterSetID <- population[, parameterSetID]

      runDescription <- population[, runDescription]

      startID <- population[, startID]

      endID <- population[, endID]

      writeLines(paste0('Cost-effectivenessing_', runDescription, '_SG_', seedGroup,
                   '_PSAID_', parameterSetID, '_personID_', startID, '_to_', endID))


  # Read in file --

    filename <- paste0(runDescription, '__',
                        'SG_', seedGroup, '_',
                        'PSAID_', parameterSetID, '_',
                        'personID_', startID, '_to_',
                        endID, '_', 'traces')

    print(filename)

  # Check file exists, otherwise save something to indicate it doesn't
 
    files <- list.files(outputsDirectory)

    if(sum(grepl(filename, files)) != 1){


      stop(paste0('Missing ', filename, '; process aborted'))

      saveRDS(population,
                paste0(outputsDirectory, 'missing_', filename))

    }


  # Read in

    population <- readRDS(paste0(outputsDirectory, filename))

  # ---- #


#===============================================================================


    
#==============================================================================#
#                       Parameterise costs and utilities                       #
#===============================================================================

 # Costs
    source(paste0(costUtilityCodeDirectory, 'parameterise_Costs.R'), local = TRUE)


 # Utilities
   source(paste0(costUtilityCodeDirectory, 'parameterise_Utility.R'), local = TRUE)

 
 # Add a discount multiplier given cycle

    population[Cycle <=30, costDiscountMultiplier := 1/((1 + costDiscountRateTo30Years)^Cycle)]
    population[Cycle >30, costDiscountMultiplier := 1/((1 + costDiscountRateBeyond30Years)^Cycle)]

    population[Cycle <=30, effectDiscountMultiplier := 1/((1 + effectDiscountRateTo30Years)^Cycle)]
    population[Cycle >30, effectDiscountMultiplier := 1/((1 + effectDiscountRateBeyond30Years)^Cycle)]


# ==============================================================================



#==============================================================================#
#                   Alemtuzumab testing specific variables                     #
#===============================================================================

    population[timeToSPMS == 1, alemtuzumabTestOutcome := 'NA - SPMS transition during dose 1']

    population[timeToDeath == 1, alemtuzumabTestOutcome := 'NA - death prior to dose 2']

    population[interventionID %in% population[Cycle == 1 & EDSS >= 7, interventionID],
               alemtuzumabTestOutcome := 'NA - EDSS 7 prior to dose 2']

# ==============================================================================

    

#==============================================================================#
#                                 Costs                                        #
#===============================================================================

  # Add time on DMT field (need to mark variations of alemtuzumab as
  # alemuzumab and cladribine respectively)

    population[, timeOnDMT := NA_integer_]

    alemtuzumabs <- mget(ls()[grepl('alemtuzumab(.*)ID', ls(), ignore.case = TRUE)])

    population[, timeOnDMTID := NA_integer_]
    population[DMT %in% alemtuzumabs, timeOnDMTID := alemtuzumabID]
    population[DMT %in% cladribineCompleteID, timeOnDMTID := cladribineCompleteID]


    population[, timeOnDMT := 1:.N, by = .(interventionID, parameterSetID, timeOnDMTID)]
    population[DMT == withdrawnID, timeOnDMT := NA_integer_]


  # DMT
    population[costing_DMT, costing_DMT := i.Cost,
               on = .(DMT = dmtID)]


  # DMT - where year-specific costs exist - select those with dmtIDs
  # that have year-specific costs and use conditional equi-join
    population[DMT %in% unique(costing_DMT_yearSpecific[, dmtID]),
               c("costing_DMT") :=
                 costing_DMT_yearSpecific[population[DMT %in% unique(costing_DMT_yearSpecific[, dmtID])], Cost,
                                          on = .(dmtID = DMT,
                                                 Year <= timeOnDMT),
                                          mult = "last"]
    ]


  # EDSS-related
    population[costing_EDSSDT, costing_EDSS := i.Cost,
               on = .(parameterSetID, EDSS)]

  # Relapse-related
    population[!is.na(Relapses), costing_Relapses :=
                 costing_relapsesNumbersDT[population[!is.na(Relapses)], .(Cost),
                                           on = .(parameterSetID,
                                                  Number <= Relapses),
                                           mult = 'last']
    ]

    population[is.na(Relapses), costing_Relapses := 0]

 
  # Adverse events
 
  # Add time since ATE since costing differs
    
    if(nrow(population[!is.na(ATECycle)]) > 0){
      
      
      population[!is.na(ATECycle) & Cycle >= ATECycle,
                 timeSinceATE := 1:.N, by = .(interventionID, parameterSetID)]

      population[!is.na(ATECycle), costing_AE_ATE :=
                   costing_autoimmuneThyroidDiseaseDT[population[!is.na(ATECycle)],
                                                      .(Cost),
                                                      on = .(parameterSetID,
                                                             Year <= timeSinceATE),
                                                      mult = 'last']
                ]
      
      population[is.na(timeSinceATE), costing_AE_ATE := 0L]
      
      
    } else {

      population[, costing_AE_ATE := 0L]

    }

  # Identify cost fields

      costFields <- colnames(population)[grepl("costing_", colnames(population))]

      discountedCostFields <- paste0(costFields, '_discounted')


  # Add discounted costs fields

      population[, c(discountedCostFields) :=
                   lapply(.SD, function(costField){
                                costField * costDiscountMultiplier
                        }),
                .SDcols = costFields]



  # Sum all costs fields

      population[, cycleCosts := rowSums(.SD), .SDcols = costFields]
      population[, cycleCosts_discounted := rowSums(.SD), .SDcols = discountedCostFields]


  # Sum all cycle costs for each person

      allCostFields <- c(costFields, discountedCostFields, 'cycleCosts',
                         'cycleCosts_discounted')

      lifetime_cycleCostsFields <- paste0('lifetime_', allCostFields)

      population[, c(lifetime_cycleCostsFields) := lapply(.SD, sum), .SDcols = allCostFields,
                 by = .(interventionID, parameterSetID, runDescription)]


  # Assign zero to all cost fields for those that died in cycle 0

      population[timeToDeath == 0,
                         c(allCostFields, lifetime_cycleCostsFields) := 0]


#===============================================================================



#==============================================================================#
#                               Utilities                                      #
#===============================================================================

      # Merge in non-relapse-related utility

      population[, timeSinceOnset := currentAge - onsetAge]

      population[utilities_RegressionFull,
                 utility_nonRelapse := i.utility,
                 on = .(femaleGender, MSType, EDSS, currentAge, timeSinceOnset, parameterSetID)]


      # Relapse disutility (assign 0 disutility for 0 relapse)

      population[!is.na(Relapses), relapse_disutility :=
                   utilities_relapseDisutilityDT[population[!is.na(Relapses)], .(disutility),
                                                 on = .(parameterSetID,
                                                        number <= Relapses),
                                                 mult = 'last']]

     population[Relapses == 0, relapse_disutility := 0]


 # Assign zero to deaths and relapses in cycle 0 ------------------------------

       population[timeToDeath == 0, c('relapse_disutility', 'utility_nonRelapse') := 0]

       population[timeToDeath == 0, Relapses := 0]

# ----------------------------------------------------------------------------


      # Sum utility and relapse disutility fields

      population[, cycleUtility := utility_nonRelapse
                 - relapse_disutility]


      # Discounted utility

      population[, cycleUtility_discounted := cycleUtility * effectDiscountMultiplier]


      # Sum all cycle utilities for each person

      population[, lifetime_utility := sum(cycleUtility),
                 by = .(interventionID, parameterSetID, runDescription)]

      population[, lifetime_utility_discounted := sum(cycleUtility_discounted),
                 by = .(interventionID, parameterSetID, runDescription)]

      # Identify Utility fields
      allUtilityFields <- colnames(population)[
        grepl('utility', colnames(population), ignore.case = TRUE)]


#===============================================================================



#==============================================================================#
#                               Relapses                                       #
#===============================================================================

      
      population[, totalRelapses := sum(Relapses), 
                           by = .(runDescription, parameterSetID, interventionID)]

      
#===============================================================================    
      

      
#==============================================================================#
#                                Outcomes                                      #
#===============================================================================
 

  # Create individual outcomes DT

      individualOutcomes <- copy(population)

      individualOutcomes <-
        individualOutcomes[,.SD[1],
                           by = .(runDescription, parameterSetID, interventionID)]


    # Costs

      # Create I vs C person identifier
        individualOutcomes[, combinedID := paste0(seedGroup,'->', personID)]

      # Shift cost after ordering
        setorder(individualOutcomes, runDescription, parameterSetID, interventionID)
        individualOutcomes[, shiftedCost := shift(lifetime_cycleCosts, type = 'lag'),
                           by = .(runDescription, combinedID, parameterSetID)]
        individualOutcomes[, shiftedDiscountedCost :=
                             shift(lifetime_cycleCosts_discounted, type = 'lag'), by = .(combinedID, parameterSetID, runDescription)]


        # Find difference
        individualOutcomes[, iVsCCost := lifetime_cycleCosts - shiftedCost]
        individualOutcomes[, iVsCCost_Discounted := lifetime_cycleCosts_discounted - shiftedDiscountedCost]


      # Utilities

        setorder(individualOutcomes, runDescription, parameterSetID, interventionID)
        individualOutcomes[, shiftedUtility := shift(lifetime_utility, type = 'lag'),
                           by = .(combinedID, parameterSetID, runDescription)]

        individualOutcomes[, shiftedDiscountedUtility :=
                             shift(lifetime_utility_discounted, type = 'lag'),
                           by = .(combinedID, parameterSetID, runDescription)]


      # Find difference
        individualOutcomes[, iVsCUtility := lifetime_utility - shiftedUtility]
        individualOutcomes[, iVsCUtility_Discounted := lifetime_utility_discounted - shiftedDiscountedUtility]



      # Add relapse outcome
       
        setorder(individualOutcomes, runDescription, parameterSetID, interventionID)
        individualOutcomes[, shiftedRelapses := shift(totalRelapses, type = 'lag'),
                           by = .(combinedID, parameterSetID, runDescription)]


      # Find difference
        individualOutcomes[, iVsCRelapses := totalRelapses - shiftedRelapses]



#===============================================================================

  
        
#==============================================================================#
#                                  Testing                                     #
#===============================================================================

  # Add single discount rate
        
    discount <- 1+costDiscountRateTo30Years
        
        
  # Check there any other ADAs tested
  if (max(lengths(strsplit(individualOutcomes[, otherADATestingCycles], ",")))  > 0){
    
    # Split the concatenated field which contains the record of all testing cycles
    
      splits <- max(lengths(strsplit(individualOutcomes[, otherADATestingCycles], ",")))
      splitFields <- c(paste0('testSplit_', 1:splits))
      splitCount <- paste0(splitFields, '_present')
      
      individualOutcomes[, c(splitFields) := 
                                tstrsplit(otherADATestingCycles, ',')]
      
    
    # Produce a count of tests by looking for non-empty fields
    
      individualOutcomes[, c(splitCount) := lapply(.SD, function(col){
        
                            ifelse(is.na(col)|col == '',0,1)
        
                          }), .SDcols = splitFields]
      
      individualOutcomes[, testCount := rowSums(.SD), .SDcols = splitCount]
      
      individualOutcomes[, c(splitCount) := NULL]
      
      
      
    # Create discounted value dependent on testing cycle
    
 
      individualOutcomes[, c(splitFields) := lapply(.SD, function (col){
     
                          1/(discount^as.numeric(col))
                          
                          }), .SDcols = splitFields]  
      
      individualOutcomes[, testCount_Discounted := rowSums(.SD, na.rm = TRUE), 
                         .SDcols = splitFields]
      
      individualOutcomes[, lifetime_TestCosts :=
                           ifelse(!is.na(ADATestingCycle), 
                                  testCount + 1, 
                                  testCount)]
      
    
    # Add the Alemtuzumab test to this (not all will have ended up being tested)
      
      individualOutcomes[, ADATestingCycle_Discounted := 
                           1/(discount^as.numeric(ADATestingCycle))]
      
      
      individualOutcomes[, lifetime_TestCosts_Discounted :=
                           ifelse(!is.na(ADATestingCycle), 
                                  testCount_Discounted + ADATestingCycle_Discounted, 
                                  testCount_Discounted)]

  } else {
    
    individualOutcomes[, ADATestingCycle_Discounted := 
                           1/(discount^as.numeric(ADATestingCycle))]
    
    individualOutcomes[, `:=` (lifetime_TestCosts = 
                                 ifelse(!is.na(ADATestingCycle), 
                                        1, 
                                        0),
             
                                lifetime_TestCosts_Discounted = 
                                 ifelse(!is.na(ADATestingCycle), 
                                        ADATestingCycle_Discounted, 
                                        0)
                               )]
    
    
  }
            
#===============================================================================          




#==============================================================================#
#                               Save files                                     #
#===============================================================================


      population <- split(population, by = c('seedGroup', 'parameterSetID', 
                                             'runDescription'))

    # Run over list and save - whole trace - inly if full outputs wanted
        
      outputTypesWanted <- readRDS(paste0(runDirectory, 'individualOutputsWanted'))
      keepTraces <- readRDS(paste0(runDirectory, 'keepTraces'))

      if(sum(grepl('lifeCourseOutcomes', outputTypesWanted)) > 0){
        
        lapply(population, function(group) {

          seedGroup <- unique(group[, seedGroup])
          parameterSetID <- unique(group[, parameterSetID])
          runDescription <- unique(group[, runDescription])
          minID <- min(group[, personID])
          maxID <- max(group[, personID])


          saveRDS(group,
                  paste0(outputsDirectory,
                         runDescription, '__',
                           'SG_', seedGroup, '_',
                           'PSAID_', parameterSetID, '_',
                           'personID_', startID, '_to_',
                           endID, '_', 'lifeCourseOutcomes'),
                compress = FALSE)
        })



        individualOutcomes <- individualOutcomes[, c('interventionID', 'onsetAge', 'onsetEDSS',
                                                     'femaleGender', 'startingAge', 'seedGroup',
                                                     'personID', 'parameterSetID', 'runDescription',
                                                     'iVsCCost', 'iVsCCost_Discounted', 'iVsCRelapses',
                                                     ..lifetime_cycleCostsFields, 'lifetime_utility',
                                                     'lifetime_utility_discounted', 'totalRelapses',
                                                     'iVsCUtility', 'iVsCUtility_Discounted', 
                                                     'alemtuzumabTestOutcome',
                                                     'ADATestingCycle', 'ADAsTested', 
                                                     'lifetime_TestCosts', 
                                                     'lifetime_TestCosts_Discounted')]
        
        
      }

            
    # Delete traces source file if specified in array construction process
      
      if(keepTraces == 0){

           print(filename)
           unlink(paste0(outputsDirectory, filename))
           
        }

    # Merge in cycle independent

       filename <- paste0(runDescription, '__',
                      'SG_', seedGroup, '_',
                      'PSAID_', parameterSetID, '_',
                      'personID_', startID, '_to_',
                      endID, '_', 'cycleIndependent')

       cycleIndependentOutcomes <- readRDS(paste0(outputsDirectory, filename))

       individualOutcomes[cycleIndependentOutcomes,
                          `:=` (timeToSPMS = i.timeToSPMS,
                                Reached_100 = i.Reached_100,
                                ageAtDeath = i.ageAtDeath,
                                timeToDeath = i.timeToDeath,
                                timeOnAnyDMT = i.timeOnAnyDMT,
                                timeOnAlemtuzumab = i.timeOnAlemtuzumab,
                                alemtuzumabDoses = i.alemtuzumabDoses,
                                DMTWithdrawalReason = i.DMTWithdrawalReason),
                          on = .(runDescription, parameterSetID, interventionID)]

       individualOutcomes <- split(individualOutcomes, by = c('seedGroup', 'parameterSetID', 'runDescription'))



    # Run over list and save - only individual outcomes

        lapply(individualOutcomes, function(group) {

          seedGroup <- unique(group[, seedGroup])
          parameterSetID <- unique(group[, parameterSetID])
          runDescription <- unique(group[, runDescription])
          minID <- min(group[, personID])
          maxID <- max(group[, personID])

          saveRDS(group,
                  paste0(outputsDirectory,
                         runDescription, '__',
                         'SG_', seedGroup, '_',
                         'PSAID_', parameterSetID, '_',
                         'personID_', startID, '_to_',
                         endID, '_', 'fullIvCOutcomes'),
                compress = FALSE)
        })

    
  }) # Close apply

}

    
