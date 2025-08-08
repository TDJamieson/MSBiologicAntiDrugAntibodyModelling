#==============================================================================#
#               Annual Relapse Risk Coefficients - Modifier                    #
#===============================================================================
#                                                                              #
#   In addition to the ARR coefficients parameters, a modifier is added here   #
#   to allow flexibility to proprtionally increase or decrease relapse         #
#   numbers. It is added at the individual level, though at present is only    # 
#   implemented as a single value.                                             #
#                                                                              #
# =============================================================================#


  if(ARRModifier_Switch == 1){
  
    MSSample[, ARRMultiplier := ARRMultiplier_Value]     
    
  } else if(ARRModifier_Switch == 0){
    
    MSSample[, ARRMultiplier := 1]
    
  }
  