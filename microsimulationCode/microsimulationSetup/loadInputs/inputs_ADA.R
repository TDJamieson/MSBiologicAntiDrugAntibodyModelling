#==============================================================================#
#                     Load project-specific inputs                             #
#===============================================================================
#                                                                              #
#  Using openxlsx this takes named ranges from excel and creates               #
#  data tables/matrices/values as desired, in the same way as the              #  
#  larger universal input loading process but from a project-specific          #
#  spreadsheet.                                                                #
# ============================================================================ #


# ------------------------------------------------------------------------------
# Load project-specific components
# ----------
 
    excelModel <- loadWorkbook(paste0(universalInputDirectory, 
                                      "ADAInputs.xlsx"))
    
    namedRegions <- openxlsx::getNamedRegions(excelModel)


    
  # Alemtuzumab 
  
    alemtuzumabADAProportion <- as.data.table(
      read.xlsx(excelModel, namedRegion = "alemtuzumabADAProportion",
                rowNames = FALSE, colNames = TRUE), stringsAsFactors = FALSE
    )
    
    alemtuzumabTestingFPRate <- 
      as.numeric(
        read.xlsx(excelModel, namedRegion = "alemtuzumabTestingFPRate",
                  rowNames = FALSE, colNames = FALSE), stringsAsFactors = FALSE
      )
    
    alemtuzumabTestingFNRate <- 
      as.numeric(
        read.xlsx(excelModel, namedRegion = "alemtuzumabTestingFNRate",
                  rowNames = FALSE, colNames = FALSE), stringsAsFactors = FALSE
      )
    
    
    alemtuzumabEffectiveness <- as.numeric(
      read.xlsx(excelModel, namedRegion = "alemtuzumabEffectiveness",
                rowNames = FALSE, colNames = FALSE), stringsAsFactors = FALSE
    )

    
  # Other DMTs
  
    # Read in table with DMT specific ADA risk; if universal risk 
    # switch is et to 1, then assign universal risk value for all ADA 
    # values
    
    ADARiskTable <- as.data.table(
                       read.xlsx(excelModel, namedRegion = "ADARiskTable")
                    )
    
    universalADARiskValue <- as.numeric(
      read.xlsx(excelModel, namedRegion = "universalADARiskValue",
                rowNames = FALSE, colNames = FALSE), stringsAsFactors = FALSE
    )
    
    if(universalADARisk_Switch == 1){
      ADARiskTable[, ADARisk := universalADARiskValue]
    }
    
    
    # Keep only those specified to be tested, and keep only those in the 
    # included DMT list
    
    ADARiskTable <- ADARiskTable[TestADAs == 1, .(dmtID, Name, ADARisk)]
    
    ADARiskTable <- ADARiskTable[dmtID %in% includedDMTList[, dmtID]]
    
    
    # Read in effectiveness in presence of DMTs
    
    DMTEffectiveness <- as.numeric(
      read.xlsx(excelModel, namedRegion = "DMTEffectiveness",
                rowNames = FALSE, colNames = FALSE), stringsAsFactors = FALSE
    )
    
 
# ------------------------------------------------------------------------------