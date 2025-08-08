#==============================================================================#
#==============================================================================#
#                                                                              #
#---     Apply cost-effectiveness values to completed life courses          ---#
#                                                                              #
#==============================================================================#  
#==============================================================================#

 
    
  population <- lifeCourseTrace
    
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
  # alemtuzumab and cladribine respectively)

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
      
      individualOutcomes <- individualOutcomes[, c('interventionID', 'interventionCohort', 'onsetAge', 'onsetEDSS',
                                                     'femaleGender', 'startingAge', 'seedGroup',
                                                     'personID', 'parameterSetID', 'runDescription',
                                                     ..lifetime_cycleCostsFields, 'lifetime_utility', 
                                                     'lifetime_utility_discounted', 'totalRelapses',
                                                     'timeToSPMS', 'timeToDeath', 'alemtuzumabTestOutcome',
                                                     'ADATestingCycle', 'ADAsTested', 'otherADATestingCycles')]
      
      
      # Merge time on and alemtuzumab doses outcome

      
       individualOutcomes[cycleIndependentOutcomes,
                          `:=` (timeOnAnyDMT = i.timeOnAnyDMT,
                                timeOnAlemtuzumab = i.timeOnAlemtuzumab,
                                alemtuzumabDoses = i.alemtuzumabDoses),
                          on = .(runDescription, parameterSetID, interventionID)]
       
       
      # Create I vs C person identifier
        individualOutcomes[, combinedID := paste0(seedGroup,'->', personID)]
 
        
      # Create I vs C field names
        fieldNames <- c(lifetime_cycleCostsFields, 'lifetime_utility', 'lifetime_utility_discounted',
                        'timeToSPMS', 'timeToDeath', 'totalRelapses', 'timeOnAnyDMT', 
                        'timeOnAlemtuzumab', 'alemtuzumabDoses')
        
        iVcFieldNames <- paste0('iVc_', fieldNames)
        iVcShiftFieldNames <- paste0('shift_', fieldNames)
        
        
      # Order
        setorder(individualOutcomes, runDescription, parameterSetID, interventionID)
        
      
      # Calculate ivc outcomes for each type of outcome
        
        individualOutcomes[, c(iVcShiftFieldNames) := lapply(.SD, shift),
                           .SDcols = fieldNames, 
                           by = .(combinedID, parameterSetID, runDescription)]
        
        individualOutcomes[ , c(iVcFieldNames) := 
                                lapply(fieldNames, function(name) {
                                  get(name) - get(paste0('shift_', name))
                            })]
      
        individualOutcomes[, c(iVcShiftFieldNames) := NULL]


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
#                           Generate outcomes                                  #
#===============================================================================


      individualOutcomes <- individualOutcomes[, c('interventionID', 'interventionCohort', 'onsetAge', 'onsetEDSS',
                                                     'femaleGender', 'startingAge', 'seedGroup',
                                                     'personID', 'parameterSetID', 'runDescription',
                                                     ..fieldNames, ..iVcFieldNames, 'lifetime_utility',
                                                     'lifetime_utility_discounted', 'totalRelapses',
                                                     'alemtuzumabTestOutcome',
                                                     'ADATestingCycle', 'ADAsTested', 
                                                     'lifetime_TestCosts', 
                                                     'lifetime_TestCosts_Discounted')]
  
    # Merge in cycle independent

       individualOutcomes[cycleIndependentOutcomes,
                          `:=` (Reached_100 = i.Reached_100,
                                ageAtDeath = i.ageAtDeath,
                                DMTWithdrawalReason = i.DMTWithdrawalReason),
                          on = .(runDescription, parameterSetID, interventionID)]
       
       
#==============================================================================#
#        If reached 100, set time to death to time to reached 100              #
#===============================================================================
    
    individualOutcomes[is.na(timeToDeath), `:=`(timeToDeath = 100 - startingAge,
                                ageAtDeath = 100)]
    
# ============================================================================== 
   
  
#==============================================================================#
#                       Add ADA testing specifics                              #
#===============================================================================
          
  # Identify intervention cohort
  
    individualOutcomes[, interventionCohort := sub(".*_", "", interventionID)]

    
    individualOutcomes <- generateMeanADACEOutcomes(individualOutcomes)
    
    
# ============================================================================== 
  
    
    
    
#==============================================================================#
#               Generate fieldnames for mean, sd and var                       #
#===============================================================================

  outcomeFieldNames <- c(fieldNames, iVcFieldNames)
  meanFieldNames <- paste0('mean_', outcomeFieldNames)
  sdFieldNames <- paste0('sd_', outcomeFieldNames)
  varFieldNames <- paste0('var_', outcomeFieldNames)
  
#===============================================================================         
  
 
  