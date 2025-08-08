# ------------------------------------------------------------------------------
# Add labels to DMTs
# -------------

labelTreatments <- function(microsimulationOutcomeDT){
 
  # Label DMTs (add 'complete' IDs for alemtuzumab and cladribine and dose 2 
  # for alemtuzumab and 'withdrawn' ID)
    
       includedDMTList <- 
        rbind(includedDMTList,
              list(dmtID = alemtuzumabCompleteID, 
                   Name = 'Alemtuzumab complete'),
              list(dmtID = alemtuzumabDose2ID, 
                   Name = 'Alemtuzumab dose 2'),
              list(dmtID = alemtuzumabDose3ID, 
                   Name = 'Alemtuzumab dose 3'),
              list(dmtID = cladribineCompleteID, 
                      Name = 'Cladribine complete'),
              list(dmtID = withdrawnID, 
                      Name = 'Withdrawn'))
    
       DMTFields <- c('DMT')
        
      microsimulationOutcomeDT[, c(DMTFields) := lapply(.SD,  
        function(field) {
          factor(field, levels = includedDMTList[, dmtID], 
                        labels = includedDMTList[, str_to_title(Name)])
        }),
        .SDcols = DMTFields]  
      
      
      return(microsimulationOutcomeDT)
      
}


# ------------------------------------------------------------------------------