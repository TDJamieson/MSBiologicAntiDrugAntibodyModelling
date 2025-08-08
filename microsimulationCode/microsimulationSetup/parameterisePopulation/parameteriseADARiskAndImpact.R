#==============================================================================#
#                   Parameterise anti-drug antibody testing                    #
#===============================================================================
#                                                                              #
#  This takes the parameters for determining ADA levels and applies them to    #
#  the whole sample.                                                           #
#                                                                              #
# =============================================================================#


  # ----------------------------------------------------------------------------
  # Base case - risk 
  # ----------
      
        MSSample[, alemtuzumabADARisk := alemtuzumabADAProportion[, Mean]]

  # ----------------------------------------------------------------------------
  

  
  # ----------------------------------------------------------------------------
  # Base - case impact
  # ----------
        
     MSSample[, alemtuzumabEffectiveness := alemtuzumabEffectiveness]


     MSSample[, DMTEffectiveness := DMTEffectiveness]
  
  # ----------------------------------------------------------------------------
