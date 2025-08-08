#==============================================================================#
#                                                                              #
#---    Plot disease course for an individual for both intervention and     ---#
#                        comparator instances                                  #           
#                                                                              #
#==============================================================================# 

# NB this requires the patchwork package to have been loaded

 # Disease course plotting function -------------------------------------------
  
      traceDiseaseCourseForOne <- function(traceDT, outcomeDT, ID)  {
 
        print(paste0('interventionID: ', ID))
        
        if(length(ID) > 1){
          stop('Meant for plotting one individual only')
        }
 
         
        traceDT <- copy(traceDT)
        traceDT <- traceDT[interventionID %in% ID]
        
        outcomeDT <- copy(outcomeDT)
        outcomeDT <- outcomeDT[interventionID %in% ID]

        traceDT[, Cycle := as.numeric(Cycle)]
        traceDT[, EDSS := as.numeric(EDSS)]
        
      
        # Prepare labels
        traceDT <- labelTreatments(traceDT)
        
        totalUtilityNum <- outcomeDT[, lifetime_utility]
        totalUtility <- paste0('Total Utility: ', round(totalUtilityNum, 2), ' QALYs')
        
        totalCostNum <- outcomeDT[, lifetime_cycleCosts]
        totalCost <- paste0('Total Cost: \U00a3', round(totalCostNum, 2))
        
        costPerQALY <- totalCostNum/totalUtilityNum
        costPerQALY <- paste0('Total Cost per QALY: ', round(costPerQALY, 2))
        
        lifeYears <- outcomeDT[, timeToDeath]
        lifeYears <- paste0('Life Years: ', lifeYears)
        
        timeToSPMS <- outcomeDT[, timeToSPMS]
        timeToSPMS <- paste0('Time to SPMS: ', timeToSPMS, ' years')
        
        gender <- ifelse(unique(traceDT[, femaleGender]) == 1L, 'Female', 'Male')
        gender <- paste0('Gender: ', gender)
      
        onsetAge <- unique(traceDT[, onsetAge])
        onsetAge <- paste0('Age at onset of MS: ', onsetAge)
        
        ageAtDeath <- outcomeDT[, ageAtDeath]
        ageAtDeath <- paste0('Age at death: ', ageAtDeath)
        
        alemtuzumabTestResult <- unique(traceDT[, alemtuzumabTestOutcome])
        alemtuzumabTestResult <- paste0('Alemtuzumab Test Outcome: ', 
                                        alemtuzumabTestResult)
        
        traceDT[, shiftedCycleCosts := shift(cycleCosts, type = 'lead')]
        

        # Adverse events combined label
        traceDT[, AEs := NA_character_]
        traceDT[!is.na(ATECycle), AEs := 'Adverse Thyroid event']
        traceDT[!is.na(AEs) & PML == 1, AEs := paste0(AEs, ' and PML')]
        traceDT[is.na(AEs) & PML == 1, AEs := 'PML']
        traceDT[is.na(AEs), AEs := 'None']
        AELabel <- paste0('Adverse Events: ', unique(traceDT[, AEs]))
        
        
        # Fill columns with times that relevant events need to be plotted
        traceDT[Relapses > 0, RelapsePlotPoint := EDSS]
        traceDT[Relapses == 0, RelapsePlotPoint := NA_integer_]
    
        traceDT[timeToDeath != Cycle, timeToDeathPlotPoint := NA_integer_]
        traceDT[timeToDeath == Cycle, timeToDeathPlotPoint := EDSS]

        traceDT[timeToSPMS != Cycle, timeToSPMSPlotPoint := NA_integer_]
        traceDT[timeToSPMS == Cycle, timeToSPMSPlotPoint := EDSS]
    
        traceDT[!is.na(timeToDeathPlotPoint), timeToDeathPlotLabel := startingAge + timeToDeath]
 
        traceDT[, nextDMT := shift(DMT,1)]
        traceDT[is.na(nextDMT) & !is.na(DMT) | 
                        DMT != nextDMT, DMTSwitch := EDSS]
        traceDT[, nextDMT := NULL]
        traceDT[!is.na(DMTSwitch), DMTSwitched := DMT]
        
        traceDT[!is.na(DMTSwitch), DMTSwitch := shift(EDSS, type = 'lead')]
    
   
        
        # 'Other DMTs' ADAs
        ADADT <- traceDT[1, .(interventionID, otherADAsArisen)]
        ADAs <- ADADT[, str_split_fixed(otherADAsArisen,',', 90)]

        ADAs<- melt(ADAs)
        setDT(ADAs)
        setnames(ADAs, 'value', 'Cycle')
        
        ADAs <- ADAs[Cycle != '']
        ADAs[, Cycle := as.integer(Cycle)]
        
        traceDT[ADAs, ADA := 'A', on = .(Cycle)]
        
    
        # Intolerance
        
        IntoleranceDT <- traceDT[1, .(interventionID, DMTIntoleranceCycles)]
        Intolerances <- IntoleranceDT[, str_split_fixed(DMTIntoleranceCycles,',', 90)]

        Intolerances<- melt(Intolerances)
        setDT(Intolerances)
        setnames(Intolerances, 'value', 'Cycle')
        
        Intolerances <- Intolerances[Cycle != '']
        Intolerances[, Cycle := as.integer(Cycle)]
        
        traceDT[Intolerances, intolerance := 'I', on = .(Cycle)]
        
        traceDT[!is.na(intolerance), intolerance := 'I']
        
        
        
        labelXVal <- unique(traceDT[, timeToDeath]+5) 
 
                      ggplot(traceDT, 
                             aes(x = Cycle, 
                                 y = EDSS,
                                 linetype = MSType,
                                 fill = interventionID)) +

                              
                              geom_line()+

                              geom_point(alpha = 0.5, size = 0.7)+

                              geom_point(aes(y = RelapsePlotPoint,
                                             size = factor(Relapses)),
                                          alpha = 0.3)+


                              geom_point(aes(y=timeToDeathPlotPoint),
                                         shape = 4, size = 2, stroke = 2)+


                              geom_point(aes(y=timeToSPMSPlotPoint),
                                         shape = 108,
                                         size = 3,
                                         stroke = 2)+


                

                              # geom_text(aes(label = timeToDeathPlotLabel),
                              #           hjust=-0.5, vjust = -0.5)+

                              geom_text(aes(label = DMTSwitched, y = EDSS+0.25), angle = 90, size = rel(2),
                                        hjust = "bottom", colour = "black")+
                        
                              geom_text(aes(label = ADA, y = 12), angle = 0, size = rel(4),
                                        hjust = "bottom", colour = 'red')+
                        
                              geom_text(aes(label = intolerance, y = 12), angle = 0, size = rel(4),
                                        hjust = "bottom", colour = 'red')+
                        
                              geom_text(aes(label = format(cycleCosts, digits = 2), y = -3), angle = 90, size = rel(3),
                                        hjust = "bottom", colour = "black")+

                              annotate("text", y = 1, x = labelXVal,
                                       label = AELabel,
                                       hjust = "left")+
                              annotate("text", y = 2, x = labelXVal,
                                       label = costPerQALY,
                                       hjust = "left")+
                              annotate("text", y = 3, x = labelXVal,
                                       label = totalCost,
                                       hjust = "left")+
                              annotate("text", y = 4, x = labelXVal,
                                       label = totalUtility,
                                       hjust = "left")+
                              annotate("text", y = 5, x = labelXVal,
                                       label = lifeYears,
                                       hjust = "left")+
                              annotate("text", y = 6, x = labelXVal,
                                       label = ageAtDeath,
                                       hjust = "left")+
                              annotate("text", y = 7, x = labelXVal,
                                       label = timeToSPMS,
                                       hjust = "left")+
                              annotate("text", y = 8, x = labelXVal,
                                       label = gender,
                                       hjust = "left")+
                               annotate("text", y = 9, x = labelXVal,
                                       label = alemtuzumabTestResult,
                                       hjust = "left")+

                              scale_y_continuous(name = 'EDSS',
                                               breaks = seq(0,10,1),
                                               expand = c(0,0.25))+
                              scale_x_continuous(name = 'Annual Cycle',
                                               breaks = seq(0,100,5),
                                               expand = c(0,0))+

                              scale_linetype(na.translate = F)+
                              scale_color_discrete(guide = 'none')+
                              scale_fill_discrete(guide = 'none')+
                              coord_cartesian(xlim = c(0,labelXVal + 30),
                                              ylim = c(-3,15))+

                              labs(colour = 'Group',
                                   linetype = 'Disease State',
                                   size = "No of Relapses")+

                              ggtitle('Disease course')+
                              theme_classic()+
                              theme(plot.margin = unit(c(1,0,0,0), "cm"))
      }     
      
# ==============================================================================
      
      
      
# ==============================================================================     
# Intervention vs comparator disease course plotting function 
# ==============================================================================
   
      traceComparisonDiseaseCourse <- function(traceDT, outcomeDT, combinedID){
 
        traceDT[, combinedID := paste0(seedGroup, '->', personID)]
        comparatorIndividual <- paste0(combinedID, '_', '0')
        interventionIndividual <- paste0(combinedID, '_', '1')
        
        interventionTrace <- traceDiseaseCourseForOne(traceDT, outcomeDT, ID = interventionIndividual) + 
          ggtitle('Disease Course - Intervention individual')
        
        comparatorTrace <- traceDiseaseCourseForOne(traceDT, outcomeDT, ID = comparatorIndividual) + 
          ggtitle('Disease Course - Comparator individual')
       
        p <- interventionTrace + comparatorTrace + plot_layout(ncol = 1)
      
        return(p)
      
      }
    
# ==============================================================================