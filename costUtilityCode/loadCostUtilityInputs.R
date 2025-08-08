#==============================================================================#
#                               Load inputs                                    #
#===============================================================================
#                                                                              #
# Using openxlsx this takes named ranges from two spreadsheets, one            #  
# containing controls for choosing how the microsimulation runs, and one       #
# containing inputs for transition matrices and all other values used in       #
# other components of modelling.  The spreadsheets need to be named            # 
# 'modelControls.xlsx' and 'inputValues.xlsx' respectively.                    #
#                                                                              #
# For transition matrices the from and to states should be in the first        #
# column and first row of the named range respectively so that the colNames    #
# and rowNames arguments in read.xlsx can pick them up.                        #
#                                                                              #
# In the controls spreadsheet anything that can be switched on/off,            #
# e.g. DMT effects needs to be given the suffix _Switch and all are then       #
# automatically picked up.  Switches related to PSA need the suffix            #
# _probabilisticSwitch.                                                        #
#                                                                              #
#------------------------------------------------------------------------------#
  
  
  # ----------------------------------------------------------------------------
  # Load excel workbooks for openxlsx to extract inputs from 
  # ----------
      
    # Load spreadsheets
    
        inputValues <- loadWorkbook(paste0(universalInputDirectory, 
                                          "costUtilityInputs.xlsx"))
        
    # Define named regions in each spreadsheet 
        
        inputValuesNamedRegions <- openxlsx::getNamedRegions(inputValues)
        
  # ----------------------------------------------------------------------------
 
  
        
  # Load all general switches, must be identified by '_Switch' suffix     --
    
    switchFields <- inputValuesNamedRegions[grepl("_Switch", 
                                                    inputValuesNamedRegions, perl = TRUE)]
    
    switches <- lapply(switchFields, 
                       function(switch) {
                         as.integer(          
                           
                           read.xlsx(inputValues, namedRegion = switch, colNames = FALSE)
                           
                         )
                         
                       })
    
    names(switches) <- switchFields
    
    list2env(switches, globalenv())
    
    
  # ---- #
    

  # ----------------------------------------------------------------------------
  # Costs and utilities
  # ----
 
    # Read in all costing and utility value tables (identified by prefix)     --
    
        utilityCostingTableNames <- 
          c(inputValuesNamedRegions[grepl("costing_", inputValuesNamedRegions, 
                                          perl = TRUE)],
            inputValuesNamedRegions[grepl("utilities_", inputValuesNamedRegions, 
                                          perl = TRUE)])
        
        
        utilityCostingTables <- 
          lapply(utilityCostingTableNames, function(name) {
            print(name)
            as.data.table(
               read.xlsx(inputValues, namedRegion = name,
               rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
              )  
        
            })
        
        names(utilityCostingTables) <- utilityCostingTableNames
        
        list2env(utilityCostingTables, globalenv())
        remove(utilityCostingTables, utilityCostingTableNames)
    
    # ----
        
        
    # Rename utilities_Regression to save as master between iteration         --
        
      utilities_Regression_Master <- utilities_Regression
      
      
    # Add row for withdrawn with zero cost                                    --
    
      costing_DMT <- rbind(costing_DMT, 
                           list(dmtID = withdrawnID, 
                                Name = 'withdrawn', 
                                Cost = 0L, 
                                PercentDiscount = 0L))
    
    # ----
    
        
    # Add row for completed Alemtuzumab with zero cost                        --

      costing_DMT <- rbind(costing_DMT, 
                           list(dmtID = alemtuzumabCompleteID, 
                                Name = 'Alemtuzumab complete', 
                                Cost = 0L, 
                                PercentDiscount = 0L))
      
    # ----
    
      
      
    # Add row for Alemtuzumab dose 2                                          --

      costing_DMT <- rbind(costing_DMT, 
                           list(dmtID = alemtuzumabDose2ID, 
                                Name = 'Alemtuzumab dose 2', 
                                Cost = costing_DMT[Name == 'Alemtuzumab_Year2', Cost], 
                                PercentDiscount = 0L))
      
    # ----
    
      
      
     # Add row for Alemtuzumab dose 3                                         --

      costing_DMT <- rbind(costing_DMT, 
                           list(dmtID = alemtuzumabDose3ID, 
                                Name = 'Alemtuzumab dose 3', 
                                Cost = costing_DMT[Name == 'Alemtuzumab_Year3', Cost], 
                                PercentDiscount = 0L))
      
    # ----
    
    
     
   
    # Add row for completed Alemtuzumab course 1 with zero cost               --

      costing_DMT <- rbind(costing_DMT, 
                           list(dmtID = alemtuzumabCourse1CompleteID, 
                                Name = 'Alemtuzumab course 1 complete', 
                                Cost = 0L, 
                                PercentDiscount = 0L))
      
    # ----
      
    
      
    # Add row for completed Cladribine with zero cost                         --

      costing_DMT <- rbind(costing_DMT, 
                           list(dmtID = cladribineCompleteID, 
                                Name = 'Cladribine complete', 
                                Cost = 0L, 
                                PercentDiscount = 0L))
      
    # ----
      
     
    # Remove alemtuzumab year_* as these are now specified                    --
      
      costing_DMT <- costing_DMT[!Name %chin% c('Alemtuzumab_Year2', 
                                                'Alemtuzumab_Year3')]
      
      costing_DMT[Name == 'Alemtuzumab_Year1', Name := 'Alemtuzumab']
       
    # Split into those with year-specific and uniform costs but remove        --
    # alemtuzumab since this is dealt with above                              --
    
      costing_DMT_yearSpecific <- 
        costing_DMT[grepl("_Year", Name, perl = TRUE)]
      
      costing_DMT_yearSpecific[, Year := as.integer(
                                          gsub(".*_Year", "", Name)
                                          )]
      
      costing_DMT_yearSpecific <- 
        costing_DMT_yearSpecific[!grepl('alemtuzumab', Name, ignore.case = TRUE, 
                                        perl = TRUE)]
        

    # ----
    
      
    # Autoimmune thyroid disease costing                                      --
      
      costing_autoimmuneThyroidDisease <- 
        costing_autoimmuneThyroidDisease[, .(Year, Aggregated.Total)]
      setnames(costing_autoimmuneThyroidDisease, c('Aggregated.Total'), 
               c('Mean'))
      
      costing_autoimmuneThyroidDisease[, SD := 0]
      
    # ----
    
      
    # Read in universal discount percentage for DMTs                          --
    
      try(
          universalDMTDiscountPercentage <- 
            as.integer(
                 read.xlsx(inputValues, 
                           namedRegion = 'costing_UniversalDMTDiscountPercentage',
                 rowNames = FALSE, colNames = FALSE)
                ), silent = TRUE
          )
    
    # ----
     
     
    # Tidy health state costing tables - need to take inflated values         --
    
      costing_EDSS <- costing_EDSS[, c('EDSS', 'Inflated.to.2021', 'SEM', 'Var')]  
      setnames(costing_EDSS, 'Inflated.to.2021', 'Mean')
      
      costing_relapsesNumbers[, c('Mean.Difference', 'SD.(Inflated)',
                                  'N', 'S^2/N') := NULL]
      setnames(costing_relapsesNumbers, 'Inflated.to.2021', 'Mean')
      
    
  # ----------------------------------------------------------------------------
  
      
  
  # ----------------------------------------------------------------------------
  # Discount rates
  # ---- 
      
    discountingFields <- inputValuesNamedRegions[grepl("DiscountRate", 
                                                           inputValuesNamedRegions, 
                                                       perl = TRUE)]
      
    discountingValues <- lapply(discountingFields, 
                                  function(discount) {
                                    as.numeric(  
                                      read.xlsx(inputValues, namedRegion = discount, colNames = FALSE)
                                    )
                                    
                                  })
      
      names(discountingValues) <- discountingFields          
      
      list2env(discountingValues, globalenv())
      
    
  # ----------------------------------------------------------------------------
  
          
      
  # ----------------------------------------------------------------------------
  # Apply price discount to DMT costs
  # ----
  
      
  # DMT costs are fixed, but a fixed discount may be applied.  This is 
  # a universal value applied to all DMTs, or DMT-specific values taken 
  # from inputValues
  
    # If universal discount, apply to all DMTS
    
      
      if(dmtDiscounting_Switch == 1){
        
      if(universalDMTDiscount_Switch == 1){
      
        costing_DMT[, PercentDiscount := universalDMTDiscountPercentage]
        
        costing_DMT_yearSpecific[, PercentDiscount := universalDMTDiscountPercentage]
    
      } 
        
        
      # Multiply cost by percent discount converted to 1 - proportion
    
         costing_DMT[, Cost := Cost * (1-(PercentDiscount/100))]

         costing_DMT_yearSpecific[, Cost := Cost * (1-(PercentDiscount/100))]
      
      } 
  
  
  # ----------------------------------------------------------------------------
  
      
      
  # ----------------------------------------------------------------------------
  # Add gamma parameters to EDSS and relapse-related costs
  # ----
               
        costing_EDSS[, gammaShape := deriveGammaParameters(Mean, Var)$shape]
        costing_EDSS[, gammaScale := deriveGammaParameters(Mean, Var)$scale]
        costing_EDSS[, Cost := Mean]
        
        
        costing_relapsesNumbers[, gammaShape := deriveGammaParameters(Mean, Var)$shape]
        costing_relapsesNumbers[, gammaScale := deriveGammaParameters(Mean, Var)$scale]
        costing_relapsesNumbers[, Cost := Mean]
        
  # ----------------------------------------------------------------------------
  
  
        
  # ----------------------------------------------------------------------------
  # Create *disutility* rather than utility tables since this is needed
  # for PSA then add gamma parameters ready for PSA
  # ----
        
        utilities_EDSSDisutility <- utilities_EDSS[, .(EDSS, disutility, 
                                                       variance = var_Pop)]
        
        utilities_relapseDisutility <- utilities_relapse[, .(number, 
                                                             disutility = mean.difference, 
                                                             variance = var_Pop)]

        utilities_EDSSDisutility[, gammaShape := 
                                   deriveGammaParameters(disutility, variance)$shape]
            
        utilities_EDSSDisutility[, gammaScale := 
                                   deriveGammaParameters(disutility, variance)$scale] 
        
        utilities_relapseDisutility[, gammaShape := 
                                   deriveGammaParameters(disutility, variance)$shape]
            
        utilities_relapseDisutility[, gammaScale := 
                                   deriveGammaParameters(disutility, variance)$scale]  
        
  # ----------------------------------------------------------------------------
  


  
            
     