

testADAs <- function(ADAPopulation){


  # Assume that ADAs only appear beyond year 2, so split ADAPopulation
  # (if there are any beyond year 1)
 
    popSize <- nrow(ADAPopulation)
    
    
    if(nrow(ADAPopulation[timeOnCurrentDMT > 1]) == 0){
      
      return(ADAPopulation)
      
    }
    
    postYear1ADAPopulation <- ADAPopulation[timeOnCurrentDMT > 1]
    firstYearADAPopulation <- ADAPopulation[timeOnCurrentDMT <= 1]
    
  
  # Select only those on DMTs that are to be tested
  
    testingADAPopulation <- 
      postYear1ADAPopulation[currentCycleDMT %in% ADARiskTable[, dmtID]]
    
    nonTestingADAPopulation <- 
      postYear1ADAPopulation[!(currentCycleDMT %in% ADARiskTable[, dmtID])]
    
    
    if(nrow(testingADAPopulation) == 0) {
      
      return(rbind(firstYearADAPopulation, postYear1ADAPopulation))
      
    }
  
  
 # Merge in ADA risk
 
    testingADAPopulation[ADARiskTable, 
                          ADARisk := i.ADARisk, 
                          on = .(currentCycleDMT = dmtID)]

  
 # Merge in ADA random outcome number 
 
    testingADAPopulation[dmtADAoutcomeRandNos, 
                          ADAOutcomeRandNo := i.ADAOutcomeRand, 
                          on = .(combinedID, currentCycleDMT = dmtID)]
  
 
 # Assign ADAs using random number 
 
    testingADAPopulation[ADAOutcomeRandNo - ADARisk < 1e-10, 
                         currentDMTADAs := 1L]
    
    
 # Assign intolerance on this basis but only in intervention cohort
  
    testingADAPopulation[interventionCohort == 1 & currentDMTADAs == 1L,
                         DMTIntolerance := 1L]
    
    
 # Identify testing as having taken place, and if ADAs have arisen
 
    testingADAPopulation[interventionCohort == 1, otherADATestingCycles := 
                           paste0(otherADATestingCycles, ',', cycleNumber)]
    
    testingADAPopulation[DMTIntolerance == 1L, 
                         otherADAsArisen := paste0(otherADAsArisen, ',', cycleNumber)]
     
    
 # Assign reduced effectiveness to all for DMT where ADAs are present
 #
 # This takes a value from the ADA spreasheet which determines proportional 
 # effectiveness.  If it is 1 then full effect results.  If less than 1, 
 # the reduction in IRR/RR is multiplied by that proportion and taken from 1, 
 # so that e.g. a 0.5 efficacy of DMT if it had a RR of 0.6 would give:
 #
 #     1-((1-0.6)*0.5) = 0.8, a reduction of 0.2 instead of 0.4
    
 
  # Find all effect fields
   
    ARREffectFields <- 
      colnames(DMTARREffectsDT)[grepl('Effect', colnames(DMTARREffectsDT), 
                                      perl = TRUE)]
    
    EDSSEffectFields <- 
      colnames(DMTRRMSEDSSProgressionEffectsDT)[grepl('Effect',
                                                      colnames(DMTRRMSEDSSProgressionEffectsDT), 
                                                      perl = TRUE)]

    
  # Merge in testing population with positive ADAs and assign a field a dummy
     
    DMTARREffectsDT[testingADAPopulation[currentDMTADAs == 1], 
                    `:=` (ADAs = 1,
                          DMTEffectiveness = i.DMTEffectiveness),
                    on = .(fullID, dmtID = currentCycleDMT)]
    
    DMTRRMSEDSSProgressionEffectsDT[testingADAPopulation[currentDMTADAs == 1], 
                                   `:=` (ADAs = 1,
                          DMTEffectiveness = i.DMTEffectiveness),
                                    on = .(fullID, dmtID = currentCycleDMT)]
    
    
  # Assign a reduced effectiveness - ARR
    
     DMTARREffectsDT[ADAs == 1 & is.na(modifiedEffect), 
                    c(ARREffectFields) := lapply(.SD, function(colname) {
                           1-((1-colname) * DMTEffectiveness)}), 
                    .SDcols = ARREffectFields]
    
     DMTARREffectsDT[ADAs == 1, modifiedEffect := 1]
     DMTARREffectsDT[, ADAs := NULL]
    
    
  # Assign a reduced effectiveness - EDSS
   
    DMTRRMSEDSSProgressionEffectsDT[ADAs == 1 & is.na(modifiedEffect), 
                                    c(EDSSEffectFields) := lapply(.SD, function(colname) {
                                           1-((1-colname) * DMTEffectiveness)}), 
                                    .SDcols = EDSSEffectFields]
    
    DMTRRMSEDSSProgressionEffectsDT[ADAs == 1, modifiedEffect := 1]
    DMTRRMSEDSSProgressionEffectsDT[, ADAs := NULL]
    
   
     
  # Recreate population from split populations
  
    testingADAPopulation[, c('ADAOutcomeRandNo', 'ADARisk', 'currentDMTADAs') := NULL]
    
    reboundPopulation <- rbind(testingADAPopulation, nonTestingADAPopulation,
                               firstYearADAPopulation)
    
    popSizeCheck <- nrow(reboundPopulation)
  
    if(popSizeCheck != popSize){
      
      stop('!!! Incoming and outgoing ADA testing population size mismatch. Aborted. !!!')
      
    }
    
    return(reboundPopulation)
    
}
