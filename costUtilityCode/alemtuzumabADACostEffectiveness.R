generateMeanADACEOutcomes <- function(DT){
  
  DT[, 'meanADATestingCycle' := mean(ADATestingCycle, na.rm = TRUE)]
  
  DT[is.na(ADAsTested), ADAsTested := 0, by = interventionCohort]
  DT[, 'meanADAsTested' := mean(ADAsTested), by = interventionCohort] 
  
  DT[, 'numberADAsTested' := sum(ADAsTested), by = interventionCohort]
  
}
  