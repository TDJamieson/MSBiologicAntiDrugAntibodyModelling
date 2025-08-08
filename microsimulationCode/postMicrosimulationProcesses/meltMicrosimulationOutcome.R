#==============================================================================#
#                                                                              #
#---              Produce long format microsimulation outcome               ---#
#                                                                              #
#==============================================================================# 

#  In order to assign costs and utilities by cycle a long format dataset is    #
#  needed for merging in appropriate values.  This function takes all the      #
#  relevant types of outcomes, melts them and joins them together.             #
#
# ============================================================================ #


wideToLongMicrosimulationOutcome <- function(microsimulationDT = microsimulationOutcome){
  
  # Define function to take a type of outcome defined by a pattern in a 
  # variable name, and melt to give a row with outcome for each person for 
  # each cycle
  
      meltSpecific <- function(DT, fieldPattern, newVarName){
         
        fields <- colnames(DT)[grepl(colnames(DT),
                                pattern = fieldPattern, 
                                perl = TRUE)]
        
        microsimulationTraces <-
          DT[, c('interventionID', 'parameterSetID', ..fields)]
        
        setnames(microsimulationTraces,
                 old = c(fields),
                 new = gsub(fieldPattern, "", fields))
        
        
        microsimulationEDSSTraces <- melt(microsimulationTraces,
                                          id.vars = c('interventionID', 'parameterSetID'),
                                          variable.name = "Cycle",
                                          value.name = newVarName)
        
      }
      
  
  # Melt all desired fields in turn
      
    EDSS <- meltSpecific(microsimulationDT, "^EDSS_Cycle", 'EDSS')
    
    MSType <- meltSpecific(microsimulationDT, "^MSType_Cycle", 'MSType')
    
    RRMSType <- meltSpecific(microsimulationDT, "^RRMSType_Cycle", 'RRMSType')
    
    DMT <- meltSpecific(microsimulationDT, "^DMT_Cycle", 'DMT')
    
    relapses <- meltSpecific(microsimulationDT, "^Relapses_Cycle", 'Relapses')
    
    
  # Trim off any cycles where an individual is dead from EDSS melt and use this 
  # smaller DT to merge onto 
    
    EDSS <- EDSS[!is.na(EDSS) | Cycle  == 1]
    
    meltedTrace <- Reduce(merge, list(EDSS, MSType, RRMSType, DMT, relapses))
    
    meltedTrace[, Cycle := as.integer(Cycle)]
    
 
  # Add age in each cycle, ATE since it's needed for costing, onset age, female 
  # gender, onset EDSS and and seedGroup and person ID for convenience
    
    meltedTrace[microsimulationDT[, .(seedGroup, interventionID, interventionCohort, 
                                      personID, parameterSetID, startingAge, 
                                      onsetAge, onsetEDSS, ATECycle, femaleGender,
                                      runDescription, timeToDeath, ageAtDeath, timeToSPMS, alemtuzumabTestOutcome,
                                      sensoryOnset, PML, ADATestingCycle, ADAsTested, DMTIntoleranceCycles,
                                      otherADATestingCycles, otherADAsArisen, alemtuzumabDoses, timeOnAlemtuzumab, timeOnAnyDMT)],
                
                                  `:=` (startingAge = i.startingAge,
                                        ATECycle = i.ATECycle,
                                        seedGroup = i.seedGroup, 
                                        personID = i.personID,
                                        interventionCohort = i.interventionCohort,
                                        onsetAge = i.onsetAge,
                                        onsetEDSS = i.onsetEDSS, 
                                        sensoryOnset = i.sensoryOnset,
                                        femaleGender = i.femaleGender,
                                        timeToDeath = i.timeToDeath,
                                        ageAtDeath = i.ageAtDeath,
                                        timeToSPMS = i.timeToSPMS,
                                        runDescription = i.runDescription, 
                                        alemtuzumabTestOutcome = i.alemtuzumabTestOutcome,
                                        ADATestingCycle = i.ADATestingCycle,
                                        ADAsTested = i.ADAsTested,
                                        PML = i.PML,
                                        DMTIntoleranceCycles = i.DMTIntoleranceCycles,
                                        otherADATestingCycles = i.otherADATestingCycles, 
                                        otherADAsArisen = i.otherADAsArisen),
                                        
                  on = .(interventionID, parameterSetID)]
    
    meltedTrace[, currentAge := startingAge + Cycle - 1]
     
  
    return(meltedTrace)

  
}

