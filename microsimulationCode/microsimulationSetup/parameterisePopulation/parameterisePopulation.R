#==============================================================================#
#                             Assign parameters                                #
#===============================================================================
#                                                                              #
# This assigns all the parameters that are taken into the microsimulation      # 
# cycle.  Probabilistic vs mean values are determined inside each file         #
# dependent on the controls set in the input spreadsheet.                      #
#                                                                              #
#==============================================================================# 
  
 
  # DMT annualised relapse rate and EDSS progression effects

    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameterise_DMTEffects.R'))


   # DMT intolerance

    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameterise_DMTIntolerance.R'))
    


  # Mortality risk
  
    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameterise_mortalityRisk.R'))
    


  # RRMS to SPMS transitions
  
    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameterise_RRMSSPMSTransitions.R'))
    

 
  # RRMS EDSS transitions
  
    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameterise_RRMSEDSSTransitions.R'))
    


  # SPMS EDSS transitions
  
    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameterise_SPMSEDSSTransitions.R'))
    


  # Annual relapse rates
  
    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameterise_ARRCoefficients.R'))
    
    source(paste0(microsimulationParameterisationProcessesDirectory, 
                 'parameterise_ARRCoefficients_ARRModifier.R'))
    


  # Alemtuzumab thyroid disease risk
  
    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameterise_alemtuzumabAutoimmuneThyroidRisk.R'))
    


  # ADAs
  
    source(paste0(microsimulationParameterisationProcessesDirectory, 
                  'parameteriseADARiskAndImpact.R'))
    


# ==============================================================================
