#==============================================================================#
#                                                                              #
#---                        Parameterise Utility                            ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# As per Briggs, Sculpher and Klaxton, Decision Modelling for Health Economic  #
# Evaluation, utilities for relapse are converted to disutility in order to    # 
# support PSA using a gamma distribution.                                      #
#                                                                              #
# For non-relapse related disutility, regression results from unpublished      #
# work by Annie Hawton with others are used.                                   #
# =============================================================================#
 
  # Expand for all parameter sets

    utilities_Regression <- 
        as.data.table(
                      expand_grid(utilities_Regression_Master,
                                  parameterSetID = unique(population[, parameterSetID]))
        )

    
  # If PSA is desired, regression coefficients need to be randomly drawn
  # prior to the process below - with the usual dqrunif waste prior
  # to the draw to provide fresh randoms

  
  if(healthStateUtilities_probabilisticSwitch == 1){

    if(parameterSetLevelPSA_probabilisticSwitch == 1){
      
       utilities_Regression[, nRow := 1:.N, by = .(parameterSetID)]
      
        utilities_Regression[!grepl('centre', Predictor, perl = TRUE), 
                             Coefficient := {
                             
                            dqset.seed(parameterSetID)  
                            dqrunif(seedRunN + nRow)
                            dqrnorm(1, mean = Coefficient, sd = SEM)},
                           
                          by = .(Predictor, parameterSetID)]
      
      seedRunN <<- seedRunN + nrow(utilities_Regression)

    } else if (individualLevelPSA_probabilisticSwitch == 1){
     
      stop('No capability for individual level parameterisation of utilities') 
      
    }
    
  }
 

# ------------------------------------------------------------------------------
# Non-relapse-related utility
# ------------------------

  # Taking utility regression table, expand so that each gender, EDSS level, 
  # time from onset and current age have a unique row - time since onset
  # and current age are centred; assuming this is at mean, the mean value
  # for each of these is taken from the value in the row and multiplied
  # by the coefficient.  Each row's set of coefficients is then summed, 
  # including the constant to give the utility for that combination of 
  # input values
  
 
    # Create row for each EDSS
    
        utilities_RegressionFull <- utilities_Regression[Variable %in% c(0:9), 
                                                         .(EDSS = as.integer(Variable), 
                                                           EDSSCoefficient = Coefficient,
                                                           parameterSetID = parameterSetID)]
    
    # Create row for each gender - assign coefficient to femaleGender
      
        utilities_RegressionFull <- as.data.table(expand_grid(utilities_RegressionFull,
                                                femaleGender = c(0,1)))
        
        utilities_RegressionFull[utilities_Regression[Variable == 'femaleGender'], 
                                femaleGenderCoefficient := i.Coefficient,
                                on = .(parameterSetID)]
       
    # Assign 0 as coefficient for male gender
  
        utilities_RegressionFull[femaleGender == 0, 
                                 femaleGenderCoefficient := 0]
  
        
    # Add row for SPMS vs RRMS and assign coefficients
    
        utilities_RegressionFull <- as.data.table(expand_grid(
          utilities_RegressionFull, MSType = c('RRMS', 'SPMS')
        ))
        
        utilities_RegressionFull[utilities_Regression[Variable %in% c('RRMS', 'SPMS')],
                                 MSTypeCoefficient := i.Coefficient,
                                 on = .(MSType = Variable, parameterSetID)]
        
    
    # Merge in time since onset coefficient
        
        utilities_RegressionFull[utilities_Regression[Variable == 'timeSinceOnset'], 
                                 timeSinceOnsetCoefficient := i.Coefficient, 
                                 on = .(parameterSetID)]
        
      
    # Add row for each time since onset     
        
        utilities_RegressionFull <- as.data.table(expand_grid(utilities_RegressionFull, 
                                                             timeSinceOnset = c(0:100)))
  
              
    # Take mean value for time since onset from time from onset in this row since 
    # this is centred, and multiply by time since onset coefficient
  
        utilities_RegressionFull[, timeSinceOnsetCentred := timeSinceOnset - 
                                      unique(utilities_Regression[Variable == 'timeSinceOnset_Centre', Coefficient])]
        
        utilities_RegressionFull[, timeSinceOnsetCoefficient := 
                                   timeSinceOnsetCentred * 
                                    timeSinceOnsetCoefficient]
        
        
    
    # Merge in current age coefficient
        
        utilities_RegressionFull[utilities_Regression[Variable == 'currentAge'], 
                                 currentAgeCoefficient := i.Coefficient, 
                                 on = .(parameterSetID)]
        
    # Add row for current age 
    
        utilities_RegressionFull <- as.data.table(expand_grid(utilities_RegressionFull, 
                                                  currentAge = c(18:100))
                                    )
    
        
    # Take mean value for current age from mean age since this is centred, 
    # and multiply by current age coefficient
            
        utilities_RegressionFull[, currentAgeCentred := currentAge - unique(utilities_Regression[Variable == 'age_Centre', Coefficient])]
        utilities_RegressionFull[, currentAgeCoefficient := currentAgeCentred * currentAgeCoefficient]
    
        
    # Merge in constant coefficient
    
       utilities_RegressionFull[utilities_Regression[Variable == 'Constant'], 
                                 constantCoefficient := i.Coefficient, 
                                 on = .(parameterSetID)]
    
        
    # Sum coefficients for each row
    
        regressionFullCoefficients <- colnames(utilities_RegressionFull)[
          grepl('Coefficient', colnames(utilities_RegressionFull), 
                perl = TRUE)]
        
        utilities_RegressionFull[, utility := rowSums(.SD), 
                                 .SDcols = regressionFullCoefficients]

    # Keep relevant fields
        
        utilities_RegressionFull <- 
          utilities_RegressionFull[, .(EDSS, femaleGender, MSType, timeSinceOnset,
                                       currentAge, utility, parameterSetID)]

# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Relapse-related disutility
# ------------------------
        
    # Expand grid so that each parameter set has a set of utilities
         
       utilities_relapseDisutilityDT <- 
          as.data.table(
                        expand_grid(
                          unique(population[, .(parameterSetID)]),
                          utilities_relapseDisutility
                        )
                )
          
          
         
            
    # If PSA is set, parameterise at appropriate level
    
    if (healthStateUtilities_probabilisticSwitch == 1) {    
             

    # Individual level seeds --> runif --> inverse gamma cdf
    
      if(individualLevelPSA_probabilisticSwitch  == 1) {
         
        utilities_relapseDisutilityDT <- 
          as.data.table(expand_grid(
                            population[, .(interventionID, personID, seedGroup)],
                            utilities_relapseDisutilityDT)
          )
            
                utilities_relapseDisutilityDT[, rowID := .I, by = .(seedGroup, personID)]
            
                utilities_relapseDisutilityDT[, disutility := {dqset.seed(seedGroup, personID)
                  
                                  dqrunif(seedRunN + rowID[[1]])
                                  
                                  rand <- dqrunif(1)
                                  
                                  qgamma(rand, shape = gammaShape,
                                               scale = gammaScale)},
                   
                    by = .(seedGroup, personID, rowID)]
                
                seedRunN <<- seedRunN + nrow(utilities_relapseDisutilityDT)

         
      }
    
        

    # Parameter level seeds --> runif --> inverse gamma cdf
      if (parameterSetLevelPSA_probabilisticSwitch == 1) {
          
                utilities_relapseDisutilityDT[, seedRunGroup := .I]
                
                utilities_relapseDisutilityDT[, disutility := {
                  
                                    dqset.seed(c(parameterSetID))
                                  
                                    dqrunif(seedRunN + seedRunGroup[[1]])
                                    
                                    rand <- dqrunif(1)
                                    
                                    qgamma(rand, shape = gammaShape,
                                                   scale = gammaScale)}, 
    
                   by = .(parameterSetID, seedRunGroup)]
                
                seedRunN <<- seedRunN + nrow(utilities_relapseDisutilityDT)

                utilities_relapseDisutilityDT[, seedRunGroup := NULL]
                
                utilities_relapseDisutilityDT[is.na(utilities_relapseDisutilityDT)] <- 0
             
                
      }
      
      
    }
         
    
    seedRunN <<- seedRun_Utilities
         
    
# ==============================================================================         
