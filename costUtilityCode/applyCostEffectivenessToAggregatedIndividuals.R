generateMeanCEOutcomes <- function(DT){
  
  
   # Identify intervention cohort
  
    DT[, interventionCohort := sub(".*_", "", interventionID)]

    

#==============================================================================#
#        If reached 100, set time to death to time to reached 100              #
#===============================================================================
    
    DT[is.na(timeToDeath), `:=`(timeToDeath = 100 - startingAge,
                                ageAtDeath = 100)]
    
# ============================================================================== 
   
  
  
#==============================================================================#
#                       Add ADA testing specifics                              #
#===============================================================================
    
    DT <- generateMeanADACEOutcomes(DT)
    
# ============================================================================== 
    
    
    
#==============================================================================#
#                           Generate outcomes                                  #
#===============================================================================

 
  # Define fields for which mean values are wanted
    
    lifetimeCostUtilityFields <- colnames(DT)[grepl('lifetime',
                                                          colnames(DT))]
    meanCEFields <- paste0('mean_', lifetimeCostUtilityFields)
    
    
    clinicalOutcomeFields <- c('totalRelapses', 'timeToSPMS', 'timeToDeath', 'timeOnAnyDMT', 
                             'timeOnAlemtuzumab', 'alemtuzumabDoses', 'ageAtDeath')
  
    meanClinicalOutcomeFields <- paste0('mean_', clinicalOutcomeFields)
    
    
  
  # Aggregate by intervention cohortand parameter set
    
    DT[, c(meanCEFields) := lapply(.SD, mean), 
             .SDcols = lifetimeCostUtilityFields,
             by = .(interventionCohort, parameterSetID)]
    
    DT[, c(meanClinicalOutcomeFields) := lapply(.SD, mean), 
             .SDcols = clinicalOutcomeFields,
             by = .(interventionCohort, parameterSetID)]
    
 
    DT[, popSize := .N, by = .(parameterSetID, interventionCohort)]
    
    DT <- DT[, c('runDescription', 'interventionCohort', 'interventionID', 'onsetAge', 
                 'onsetEDSS', 'femaleGender', 'startingAge', 'seedGroup',
                 'personID', 'parameterSetID', ..meanCEFields, ..meanClinicalOutcomeFields,
                 'popSize', 'meanADATestingCycle', 'meanADAsTested', 'numberADAsTested')]
    
    DT <- DT[, .SD[1], by = interventionCohort]
  
}