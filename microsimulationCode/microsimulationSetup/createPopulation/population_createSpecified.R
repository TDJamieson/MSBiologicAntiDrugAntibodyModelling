#==============================================================================#
#==============================================================================#
#                                                                              #
#---             Create the pre-specified microsimulation cohort            ---#
#                                                                              #
#==============================================================================#
#==============================================================================#  
  
#==============================================================================#
#                                                                              #
# For a deterministic population from one up to several populations can be     #
# pre-specified in the spreadsheet, where ranges for chronological age at      #
# entry into the model, age of onset of MS, and EDSS at onset can be           #
# specified.  In addition gender can be specified, including both genders,     #
# and a starting DMT can be determined.  NB there is a dummy to allow for the  #
# chronological age range not to be incorporated, so that individuals are      #
# simply entering the model at onset of their MS.                              #
#                                                                              #
# The number of replicated individuals is determined in one of two ways.       #
# There is either an equal group size dummy so that all of the prespecified    #
# subpopulations have the same number of replicants, or if this dummy is       #
# set to 0, then there is a field to specify for each subpopulation the        #
# number of replicated individuals that are desired.                           #
# =============================================================================#  
 

  # ----------------------------------------------------------------------------
  # Create deterministic population 
  # ----
        
        # If equal group sample sizes are set then update the data table      --
        # with the no of replicants otherwise these are carried though from   --
        # the entries in each row in the population grid                      --
      
            if(equalGroupSize_Switch == 1) {
              
                  preSpecifiedPopulation[, No.Of.Replicants := noReplicants]
              
            }
        
  
        # Where 'both' genders have been entered for a group, these rows need --
        # to be incorporated twice, once for female, once for male            --
          
          # Split into identified gender and into both 
          bothGenders <- preSpecifiedPopulation[Gender == "Both"]
          noBothGenders <- preSpecifiedPopulation[Gender != "Both"]
          
          # Duplicate 'both' genders 
          bothGenders1 <- copy(bothGenders)
          bothGenders2 <- copy(bothGenders)
  
          # Specify genders for split
          bothGenders1[, Gender := 'Male']
          bothGenders2[, Gender := 'Female']
          
          # Join now specified genders
          bothGenders <- rbind(bothGenders1, bothGenders2)
          
          # Merge newly specified genders and previous already specified genders
          preSpecifiedPopulation <- rbind(bothGenders, noBothGenders)
          
          # Dichotomise genders
          preSpecifiedPopulation[Gender == "Female", femaleGender := 1L]
          preSpecifiedPopulation[Gender == "Male", femaleGender := 0L]
          preSpecifiedPopulation[, Gender := NULL]
          
          # Tidy
          remove(list=ls(pattern = 'both'))
          
          
          
      # Split data table into rows so that each group can be expanded       --
    
          preSpecifiedPopulationList <- split(preSpecifiedPopulation, 
                                             seq(nrow(preSpecifiedPopulation)))
        
          
    
      # Taking max and min age from the age range specified in the          --
      # spreadsheet pre-specified population group grid, max and min        --
      # onset EDSS and dichotomous gender, create a grid containing a       --
      # representative individual for each set of characteristics and       --
      # expand these for the number of replicants set.  If DMTs are         --
      # pre-specified they are straightforwardly included.  If people are   --
      # intended to enter the model at MS onset and so chronological age    --
      # is to be ignored, a separate expand grid process is needed          --
          
          
          
      if(chronologicalAgeIncorporated_Switch == 1) {
          
        MSSample <- rbindlist(
            
             lapply(preSpecifiedPopulationList, function(x) {
           
               expand_grid(
                             onsetAge = c(x$MinOnsetAge:x$MaxOnsetAge),
                              
                             currentAge = c(x$MinAge:x$MaxAge),
                             
                             femaleGender = x$femaleGender,
                             
                             onsetEDSS =  c(x$MinEDSS:x$MaxEDSS),
                             
                             noReplicants = x$No.Of.Replicants,
                             
                             startingDMT = x$Starting.DMT,
                             
                             RRMSType = x$RRMS.Type
           
                          )
                 })
         
              )
        
          setDT(MSSample)
          
      
      } else if(chronologicalAgeIncorporated_Switch == 0) {  
            
          
          MSSample <- rbindlist(
            
             lapply(preSpecifiedPopulationList, function(x) {
           
               expand_grid(
                             onsetAge = c(x$MinOnsetAge:x$MaxOnsetAge),
                              
                             femaleGender = x$femaleGender,
                             
                             onsetEDSS =  c(x$MinEDSS:x$MaxEDSS),
                             
                             noReplicants = x$No.Of.Replicants,
                             
                             startingDMT = x$Starting.DMT,
                              
                             RRMSType = x$RRMS.Type
           
                          )
                 })
         
              )
          
          setDT(MSSample)
          
          MSSample[, currentAge := onsetAge]
      }
          
  # ----------------------------------------------------------------------------
  
              
          
  # ----------------------------------------------------------------------------      
  # Add starting age as current chronological age
  # ----------                         
    
    MSSample[, startingAge := currentAge]
                      
  # ----------------------------------------------------------------------------
  
      
          
  # ----------------------------------------------------------------------------
  # Add indicator for under vs over 28 as the transition matrices          
  # differ on this basis, and onset age bands which are used in the 
  # modelling of annualised relapse rate
  # -------
      
      MSSample[onsetAge <= 28, under28Onset := 1L]
      MSSample[onsetAge > 28, under28Onset := 0L]
      
      MSSample[onsetAgeBands, 
               on = .(onsetAge >= Min, onsetAge <= Max),
               onsetAgeBand := i.Group]
      
        
  # ----------------------------------------------------------------------------
  
        
        
  # ----------------------------------------------------------------------------      
  # Add the treatment line on which the population are starting.  I.e. RRMS
  # first-line clinical failure to start them after one set of switching.
  # -----------
  
        MSSample[, treatmentLine := startingTreatmentLine]
        
        
  # ----------------------------------------------------------------------------
   
      
      
  # ----------------------------------------------------------------------------      
  # Expand for all parameter sets
  # -----------
      
          MSSample <- as.data.table(expand_grid(MSSample,
                                                  parameterSetID = PSASeedRange)) 
      
  # ----------------------------------------------------------------------------
      
      
      
  # ----------------------------------------------------------------------------    
  # If random DMTs are required, then randomly assign them using the list   
  # of DMTs in the appropriate treatment options table based on the treatment 
  # line     
  # ---------
      
        
      # Merge in seed group ID to use for assigning DMTs
      
        MSSample[seedDT, seedGroup := i.seedGroup,
                  on = .(onsetAge, currentAge, femaleGender, onsetEDSS,
                     RRMSType)]
      
          
        if (randomStartingDMT_Switch == 1){
        
          
          # Check options have been filled correctly 
          if(groupLevelDMTRandomness_Switch == parameterSetLevelDMTRandomness_Switch){
            stop('Random DMTs requested, but either no level has been
                 requested, or both have.')
          }
          
          # Select appropriate DMT options table based on line table
          
            if(startingTreatmentLine == 1){
              
              DMTTable <- switchingTables$RRMSFirstLine
              
            } else if(startingTreatmentLine == 2) {
              
              DMTTable <- switchingTables$RRMSFirstLineClinicalFailure
              
            } else if(startingTreatmentLine == 3) {
              
              DMTTable <- switchingTables$RRMSSecondLineClinicalFailure
              
            }
            
              
          
      # Randomly sample from table identified - sampling level either at 
      # group or parameter set level by using seedgroup ID or parameter set ID
      
            
          if(groupLevelDMTRandomness_Switch) {
                
                MSSample[, startingDMT := {dqset.seed(c(seedGroup, NULL))
                                            
                                            dqrunif(seedRunN)
                                            
                                            rep(dqsample(size = 1, 
                                                         x = DMTTable[, dmtID],
                                                         replace = TRUE), 
                                                         .N)
    
                                            }, 
                         by = seedGroup]
    
                
              } else if(parameterSetLevelDMTRandomness_Switch == 1){
                
                 MSSample[, startingDMT := {dqset.seed(c(parameterSetID, NULL))
                              
                                            dqrunif(seedRunN)
                                            
                                            rep(dqsample(size = 1, 
                                                          x = DMTTable[, dmtID],
                                                          replace = TRUE), 
                                                         .N)
    
                                            }, 
                          
                          by = parameterSetID]
    
                
              }
              
          
          # If single specified starting DMT then assign here
              
            } else if(singleSpecifiedStartingDMT_Switch == 1) {
    
                      
              MSSample[, startingDMT := singleSpecifiedStartingDMTID]
            
              
            }
              
          
          # Increment seedRunN 
          
            seedRunN <<- seedRun_DMT
            
            
  # ----------------------------------------------------------------------------
  
      
        
  # ----------------------------------------------------------------------------      
  # Add number of current DMT doses as 1 necessary for use with  DMTs where a 
  # limited number of doses are given
  # -----------
  
        MSSample[, currentDMTDoses := 1L]
        
        
  # ----------------------------------------------------------------------------
 
#==============================================================================#  