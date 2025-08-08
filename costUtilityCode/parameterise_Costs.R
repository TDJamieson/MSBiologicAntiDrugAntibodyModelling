#==============================================================================#
#                                                                              #
#---                        Parameterise Costs                              ---#
#                                                                              #
#==============================================================================#  
#                                                                              #
# Costing is broken down into an EDSS-related cost, specific DMT cost,         #
# and cost arising from relapses. NB for some DMTs costing differs by year.    #
#                                                                              #
# Costing for DMTs is fixed, though a discount may be applied.                 #
#                                                                              #
# PSA uses gamma distributions, and can be at individual or parameter set      #
# level.                                                                       #
# ============================================================================ #


      # Identify health state costing tables
             
        healthCostTables <- list(costing_EDSS, costing_relapsesNumbers)
        namesHealthCostTables <- c('costing_EDSSDT', 'costing_relapsesNumbersDT')
    
        
      # Expand with parameter IDs 

        healthCostTables <- 
      
          lapply(healthCostTables, function(DT) {
        
            # Expand so that there is an entry for all individuals
            
            DT <- 
              as.data.table(expand_grid(
                            unique(population[, .(parameterSetID)]),
                            DT)
                          )
            
          })
        

    # Tidy and send tables to environment
    
        names(healthCostTables) <- namesHealthCostTables
        
        list2env(healthCostTables, envir = globalenv())
    
        remove(healthCostTables, namesHealthCostTables)
  
        
         
      
      # Test if PSA on health state costing is desired, if it is, 
      # then parameterise distributions and draw at appropriate PSA
      # level
  
      if(healthStateCosts_probabilisticSwitch == 1){
              
      
      # If individual level PSA then draw from distribution for each
      # seedgroup and personID 
        
        if(individualLevelPSA_probabilisticSwitch  == 1) {
      
          
          # EDSS costing 
          
            costing_EDSSDT[, seedRunGroup := .GRP, by = EDSS]
            costing_EDSSDT[, Cost := {dqset.seed(c(seedGroup[[1]], 
                                                 personID[[1]]))
              
                                    dqrunif(seedRunN + seedRunGroup[[1]])
                                    
                                    dqrunif(1)
          
                                    }, 
                        
                          by = .(EDSS, seedGroup, personID)]
          
          
            costing_EDSSDT[, Cost := qgamma(rand, shape = gammaShape,
                                                scale = gammaScale)]
            
            seedRunN <<- seedRun_EDSSCosts
            
            costing_EDSSDT[, seedRungroup := NULL]
            
          
          # Relapse  costing
         
            costing_relapsesNumbersDT[, seedRunGroup := .GRP, by = Number]
            costing_relapsesNumbersDT[, Cost := {dqset.seed(c(seedGroup[[1]], 
                                                           personID[[1]]))
              
                                                dqrunif(seedRunN + seedRunGroup[[1]])

                                                rand <- dqrunif(1)
                      
                                                qgamma(rand, shape = gammaShape,
                                                             scale = gammaScale)
                                                },
                                    
                                  by = .(Number, combinedID)]
            
              
          
            seedRunN <<- seedRun_RelapseCosts

          
            costing_relapsesNumbersDT[, seedRungroup := NULL]
    
        } else if (parameterSetLevelPSA_probabilisticSwitch  == 1){
        
     # If parameter level PSA then draw from distribution for each
     # parameterSetID         
          
      
          # EDSS costing 
             
            costing_EDSSDT[, seedRunGroup := .GRP, by = EDSS]

            costing_EDSSDT[, Cost := {dqset.seed(c(parameterSetID[[1]], 
                                                  NULL))
              
                                     dqrunif(seedRunN + seedRunGroup[[1]])
                                    
                                     rand <- dqrunif(1)
            
                                     qgamma(rand, shape = gammaShape,
                                                  scale = gammaScale)
                                      },
                        
                            by = .(EDSS, parameterSetID)]
        
        
             seedRunN <<- seedRun_EDSSCosts
            
             costing_EDSSDT[, seedRunGroup := NULL]
            
            
            
          # Relapse costing 
            
            costing_relapsesNumbersDT[, seedRunGroup := .GRP, by = Number]

            costing_relapsesNumbersDT[, Cost := {dqset.seed(c(parameterSetID[[1]], 
                                                            NULL))
              
                                                dqrunif(seedRunN + seedRunGroup[[1]])
                                    
                                                rand <- dqrunif(1)
                      
                                                qgamma(rand, shape = gammaShape,
                                                            scale = gammaScale)
                                                }, 
                        
                            by = .(Number, parameterSetID)]
            
            seedRunN <<- seedRun_RelapseCosts
          
            costing_relapsesNumbersDT[, seedRunGroup := NULL]
    
            
        }
      
        
      } else if (healthStateCosts_probabilisticSwitch == 0){
      
      # If not probabilistic, apply mean costs

        costing_EDSSDT[, Cost := Mean]
        costing_relapsesNumbersDT[, Cost := Mean]
        
      }
  
            
      # Assign zero cost for zero relapses
      
        costing_relapsesNumbersDT[Number == 0, Cost := 0]
        
        
  # ----------------------------------------------------------------------------
  
         
        
  # ----------------------------------------------------------------------------
  # Parameterise autoimmune thyroid disease costs for Alemtuzumab (fixed costs)
  # ---
  
      costing_autoimmuneThyroidDiseaseDT <- 
          as.data.table(
              expand_grid(parameterSetID = unique(population[, .(parameterSetID)]),
                          costing_autoimmuneThyroidDisease)
          )
        
        costing_autoimmuneThyroidDiseaseDT <- 
          costing_autoimmuneThyroidDiseaseDT[, .(parameterSetID,
                                               Cost = Mean, Year)]
        
        
         
  # ----------------------------------------------------------------------------

        
# ==============================================================================
   
