determineIntolerance <- function(population){
 
  
  # Merge in intolerance risk for DMT an individual is currently on, at       --
  # appropriate level - parameter set or individual                           --
  
    if(PSA_Switch == 1 & individualLevelPSA_probabilisticSwitch == 1){
  
      population[DMTIntoleranceRisksDT,
                    on = .(interventionID,
                           parameterSetID,
                           currentCycleDMT = dmtID),
                    DMTIntoleranceRisk := i.DMTIntoleranceRisk]
            
    } else {
      
      population[DMTIntoleranceRisksDT,
                    on = .(parameterSetID,
                           currentCycleDMT = dmtID),
                    DMTIntoleranceRisk := i.DMTIntoleranceRisk]
    
    }
  
  
  # Test if DMT intolerance runif is less than intolerance risk merged in     --
  
    population[DMTIntoleranceOutcomeRandNo - DMTIntoleranceRisk < 1e-10, 
               DMTIntolerance := 1L]
  
    population[,  `:=` (DMTIntoleranceRisk = NULL)]
  
  
  # Additionally assign ADAs, and mark as intolerant in intervention cohort

    if(otherDMTADATesting_Switch == 1){
      
      intolerancePopulation <- population[DMTIntolerance == 1]
      noIntolerancePopulation <- population[is.na(DMTIntolerance)]

      noIntolerancePopulation <- testADAs(ADAPopulation = noIntolerancePopulation)
      
      population <- rbind(intolerancePopulation, noIntolerancePopulation)
      
    }
    

  
  return(population)
  
}
