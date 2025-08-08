

testAlemtuzumabADAs <- function(population){
 
  
 # Assign alemtuzumab ADAs and mark that testing has taken place
 
    population[AlemtuzumabADAsOutcomeRandNo - alemtuzumabADARisk < 1e-10, 
               alemtuzumabADAs := 1L]
  
    population[interventionCohort == 1, ADATestingCycle := cycleNumber]
    population[interventionCohort == 1, ADAsTested := 1L]
     

  # Determine false negatives and true positives in those with ADAs
  
    population[alemtuzumabADAs == 1 & 
                 (alemtuzumabFNOutcomeRandNo - alemtuzumabTestingFNRate  < 1e-10),
               alemtuzumabTestOutcome := 'FN']
    
    population[alemtuzumabADAs == 1 & 
                 (alemtuzumabFNOutcomeRandNo - alemtuzumabTestingFNRate  > 1e-10),
               alemtuzumabTestOutcome := 'TP']
    
    
  # Determine false positives and true negatives in those without ADAs
  
    population[is.na(alemtuzumabADAs) & 
                 (alemtuzumabFPOutcomeRandNo - alemtuzumabTestingFPRate  < 1e-10),
               alemtuzumabTestOutcome := 'FP']
    
    population[is.na(alemtuzumabADAs) & 
                 (alemtuzumabFPOutcomeRandNo - alemtuzumabTestingFPRate  > 1e-10),
               alemtuzumabTestOutcome := 'TN']
    
 
  # In those with ADAs, assign alemtuzumab IRR and RR, using alemtuzumab
  # effectiveness value in alemtuzumab spreadsheet
  
    # Find all effect fields
    ARREffectFields <- colnames(DMTARREffectsDT)[grepl('Effect', 
                                                       colnames(DMTARREffectsDT), 
                                                       perl = TRUE)]
    ARREffectFields <- ARREffectFields[!grepl('modified', ARREffectFields, 
                                              perl = TRUE)]

    EDSSEffectFields <- colnames(DMTRRMSEDSSProgressionEffectsDT)[grepl('Effect', 
                                                                          colnames(DMTRRMSEDSSProgressionEffectsDT), 
                                                                        perl = TRUE)]
    EDSSEffectFields <- EDSSEffectFields[!grepl('modified', EDSSEffectFields, 
                                                perl = TRUE)]

    
    # Merge in testing population with positive ADAs and assign a field a dummy
 
    DMTARREffectsDT[fullID %in% population[alemtuzumabADAs == 1, fullID] & 
                    (dmtID == alemtuzumabID | 
                    dmtID == alemtuzumabCompleteID |
                    dmtID == alemtuzumabCourse1CompleteID | 
                    dmtID == alemtuzumabDose2ID|
                    dmtID == alemtuzumabDose3ID), 
                    `:=` (ADAs = 1, 
                          alemtuzumabEffectiveness = unique(population[, alemtuzumabEffectiveness]))]
      
    DMTRRMSEDSSProgressionEffectsDT[fullID %in% population[alemtuzumabADAs == 1, fullID] & 
                                    (dmtID == alemtuzumabID | 
                                    dmtID == alemtuzumabCompleteID |
                                    dmtID == alemtuzumabCourse1CompleteID | 
                                    dmtID == alemtuzumabDose2ID|
                                    dmtID == alemtuzumabDose3ID), 
                                    `:=` (ADAs = 1, 
                                          alemtuzumabEffectiveness = 
                                            unique(population[, alemtuzumabEffectiveness]))]
   
    
    # Assign alemtuzumab impact - this takes a value from the alemtuzumab
    # spreadsheet which determines proportional effectiveness.  If it is 1
    # then full effect results.  If less than 1, the reduction in IRR/RR
    # is multiplied by that proportion and taken from 1, so that e.g. a 
    # 0.5 efficacy of alemtuzumab if it had a RR of 0.6 would give:
    # 1-((1-0.6)*0.5) = 0.8, a reduction of 0.2 instead of 0.4
    
       # Assign a reduced effectiveness - ARR
         
        DMTARREffectsDT[ADAs == 1 & is.na(modifiedEffect), 
                        c(ARREffectFields) := lapply(.SD, function(colname) {
                               1-((1-colname) * alemtuzumabEffectiveness)}), 
                        .SDcols = ARREffectFields]
        
        DMTARREffectsDT[ADAs == 1, modifiedEffect := 1]
        DMTARREffectsDT[, ADAs := NULL]
        
        
      # Assign a reduced effectiveness - EDSS
       
        DMTRRMSEDSSProgressionEffectsDT[ADAs == 1 & is.na(modifiedEffect), 
                                        c(EDSSEffectFields) := lapply(.SD, function(colname) {
                                               1-((1-colname) * alemtuzumabEffectiveness)}), 
                                        .SDcols = EDSSEffectFields]
        
        DMTRRMSEDSSProgressionEffectsDT[ADAs == 1, modifiedEffect := 1]
        DMTRRMSEDSSProgressionEffectsDT[, ADAs := NULL]   
      
      

      if (alemtuzumabADATesting_Switch == 0){
    browser()
          population[interventionCohort == 1, 
                     `:=` (alemtuzumabADAs = 0,
                           alemtuzumabTestOutcome = 'TN')]
          
      }
        
      return(population)
    
      
}

