catDF<-select(dataF,"admin2pcode", contains("W.1. "), contains("W.1.1. "), contains("W.3. "),
                     contains("W.4.1. "),contains("W.5.1. "), contains("W.5.2. "),contains("W.9.1. "),
                     contains("W.10. "),contains("W.11. "),contains("W.11.1. "), contains("S.1.1. "), 
                     contains("S.1.2. "),contains("S.1.3. "), contains("S.2. "), contains("S.2.2. "),
                     contains("S.3. "),  contains("S.4. "), contains("S.5. "),
                     contains("S.6. "), contains("S.6.1. "), contains("S.7. "),  contains("S.9. "))
catDM<-select(dataM,"admin2pcode", contains("W.1. "), contains("W.1.1. "), contains("W.3. "),
              contains("W.4.1. "),contains("W.5.1. "), contains("W.5.2. "),contains("W.9.1. "),
              contains("W.10. "),contains("W.11. "),contains("W.11.1. "), contains("S.1.1. "), 
              contains("S.1.2. "),contains("S.1.3. "), contains("S.2. "), contains("S.2.2. "),
              contains("S.3. "),  contains("S.4. "), contains("S.5. "),
              contains("S.6. "), contains("S.6.1. "), contains("S.7. "),  contains("S.9. "))

catDI<-select(dataI,"admin2pcode", contains("W.1. "), contains("W.1.1. "), contains("W.3. "),
              contains("W.4.1. "),contains("W.5.1. "), contains("W.5.2. "),contains("W.9.1. "),
              contains("W.10. "),contains("W.11. "),contains("W.11.1. "), contains("S.1.1. "), 
              contains("S.1.2. "),contains("S.1.3. "), contains("S.2. "), contains("S.2.2. "),
              contains("S.3. "),  contains("S.4. "), contains("S.5. "),
              contains("S.6. "), contains("S.6.1. "), contains("S.7. "),  contains("S.9. "))
catDH<-select(dataHC,"admin2pcode", contains("W.1. "), contains("W.1.1. "), contains("W.3. "),
              contains("W.4.1. "),contains("W.5.1. "), contains("W.5.2. "),contains("W.9.1. "),
              contains("W.10. "),contains("W.11. "),contains("W.11.1. "), contains("S.1.1. "), 
              contains("S.1.2. "),contains("S.1.3. "), contains("S.2. "), contains("S.2.2. "),
              contains("S.3. "),  contains("S.4. "), contains("S.5. "),
              contains("S.6. "), contains("S.6.1. "), contains("S.7. "),  contains("S.9. "))

#Subsetting categorical question type from main dataset (pvalues calculation)
categoricalDd<-select(dataT,"Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"), contains("G.4. D"),
                      contains("D.5."),contains("W.1.1. What"), contains("W.3. "),contains("W.4. "),
                      contains("W.5.Do"),contains("W.9. "),contains("W.10."), contains("W.11. "), 
                      contains("S.1.W"),contains("S.5."), contains("S.6. "), contains("S.6.1."),
                      contains("S.8. Did you"),  contains("S.2. Do "), contains("S.2.1. Is"),
                      contains("S.2.2."), contains("S.3."), contains("S.4. What is"))
#Subsetting multiple choice question type from main dataset (pvalues calculation)
multipleDd<-select(dataT,"Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"), contains("G.4. D"),
                   contains("W.1. "),contains("W.4.1. "),contains("W.5.1."),contains("Reason for not treating water"),
                   contains("W.9.1."), contains("W.11.1."), contains("Please name specific activities before"),
                   contains("S.9. "))


#This function is for questions which prompts respondents to select more than one answer
#Categories with less than 5% are removed

DistFunctM<- function(IND, disag, DistSel) {
  
  if (disag=="HC v.s. IDP") {
    
    S_HC <- catDH %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Host Community")%>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    S_IDP <- catDI %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="IDP") %>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    chartD<-merge(S_HC, S_IDP, by="variable", all=TRUE)
    chartD<-filter_all(chartD, any_vars(.>5))
    chartD$variable<-gsub(paste0(IND ,collapse = "|"),"", chartD$variable, fixed=TRUE)
    chartD
    
  } else if (disag=="Female v.s. Male") {
    
    SC_F <- catDF %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Female") %>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    
    SC_M <- catDM %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Male")%>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    
    chartD<-merge(SC_F, SC_M, by="variable", all=TRUE)
    chartD<-filter_all(chartD, any_vars(.>5))
    chartD$variable<-gsub(paste0(IND ,collapse = "|"),"", chartD$variable, fixed=TRUE)
    chartD
    
  }
}

#This function is for questions which prompts respondents to select only one answer
#no categories are removed in this aggregation so results will all add to 100%

DistFunct<- function(IND, disag, DistSel) {
  
  if (disag=="HC v.s. IDP") {
    
    S_HC <- catDH %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Host Community")%>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    S_IDP <- catDI %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="IDP") %>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    chartD<-merge(S_HC, S_IDP, by="variable", all=TRUE)
    chartD$variable<-gsub(paste0(IND ,collapse = "|"),"", chartD$variable, fixed=TRUE)
    chartD
    
  } else if (disag=="Female v.s. Male") {
    
    SC_F <- catDF %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Female") %>%
      mutate_if(is.numeric, funs(round(100*(.),1)))

    
    SC_M <- catDM %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Male")%>%
      mutate_if(is.numeric, funs(round(100*(.),1)))

    chartD<-merge(SC_F, SC_M, by="variable", all=TRUE)
    chartD$variable<-gsub(paste0(IND ,collapse = "|"),"", chartD$variable, fixed=TRUE)
    chartD
  }
}
# trydist<-DistFunct("W.3. ", "HC v.s. IDP", "Abs")
# IND<-"W.3. "
# disag<-"HC v.s. IDP"
# district<- "Abs"
# 
# tryyit<-PvalsD(IND, disag, district)
# tryittt<-melt(merge(trydist,tryyit[,c(-1,-3)],by="variable")[,-1])
# 
# tryittt
# PvalsD<- function(IND, disag, DistSel) {
#   
#   if (disag=="HC v.s. IDP") {
#     
#     IDPHC_Fam <- categoricalDd %>%
#       filter(categoricalDd$`G.4. District`==district) %>%
#       select(status="status", var= contains(IND))%>%
#       na.omit()
#     
#     #Create boolean columns for all response options
#     for (i in (unique(IDPHC_Fam$var))){
#       contai<- ifelse(IDPHC_Fam$var==i,1,0)
#       IDPHC_Fam[i]<-contai }
#     
#     #loop through response columns and calculate pvalue
#     outputIDPHC<-data.frame() #IDP/HC STATUS
#     for (i in (3:length(IDPHC_Fam))){
#       if (length(unique(IDPHC_Fam$status))>1 & length(unique(IDPHC_Fam[[i]]))>1){
#         okay<-(chisq.test(IDPHC_Fam$status, IDPHC_Fam[[i]]))$p.value
#         outputIDPHC<-rbind(outputIDPHC,okay)}
#       else if(length(unique(IDPHC_Fam$status))<2 |length(unique(IDPHC_Fam[[i]]))<2){
#         okay<-NA
#         outputIDPHC<-rbind(outputIDPHC, okay) }
#     }
#     
#     outputIDPHC$var<-c(colnames(IDPHC_Fam[3:length(IDPHC_Fam)]))
#     colnames(outputIDPHC)<-c("pvalue","variable")
#     
#     outputIDPHC$sigg<- ifelse(outputIDPHC$pvalue <= 0.05 & outputIDPHC$pvalue >0.01, "*",
#                               ifelse(outputIDPHC$pvalue <=0.01 & outputIDPHC$pvalue > 0.001, "**",
#                                      ifelse(outputIDPHC$pvalue <=0.001, "***", "")))
#     
#     outputIDPHC$name<-paste0(outputIDPHC$var, outputIDPHC$sigg)
#     outputIDPHC
#     
#   } else if (disag=="Female v.s. Male") {
#     
#     #Create dataframe containing indicator, and HoH gender/IDPHC status
#     #filtering to district and indicator
#     FM_Fam <- categoricalDd %>%
#       filter(categoricalDd$`G.4. District`==district) %>%
#       select( HoHgender= "HoHgender", var= contains(IND))%>%
#       na.omit()
#     
#     #Create boolean columns for all response options
#     for (i in (unique(FM_Fam$var))){
#       contai<- ifelse(FM_Fam$var==i,1,0)
#       FM_Fam[i]<-contai
#     }
#     
#     #loop through response columns and calculate pvalue
#     outputFM<-data.frame() #GENDER
#     for (i in (3:length(FM_Fam))){
#       if (length(unique(FM_Fam$HoHgender))>1 & length(unique(FM_Fam[[i]]))>1){
#         okay<-(chisq.test(FM_Fam$HoHgender, FM_Fam[[i]]))$p.value
#         outputFM<-rbind(outputFM,okay)}
#       else if(length(unique(FM_Fam$HoHgender))<2 |length(unique(FM_Fam[[i]]))<2){
#         okay<-NA
#         outputFM<-rbind(outputFM, okay)
#       }
#     }
#     outputFM$var<-c(colnames(FM_Fam[3:length(FM_Fam)]))
#     colnames(outputFM)<-c("pvalue","variable")
#     
#     outputFM$sigg<- ifelse(outputFM$pvalue <= 0.05 & outputFM$pvalue >0.01, "*",
#                               ifelse(outputFM$pvalue <=0.01 & outputFM$pvalue > 0.001, "**",
#                                      ifelse(outputFM$pvalue <=0.001, "***", "")))
#     
#     outputFM$name<-paste0(outputFM$var, outputFM$sigg)
#     outputFM
#   }
# }
# IND<- "W.1. "
# disag<-"HC v.s. IDP"
# DistSel<-"Abs"
# PvalsMD("W.1. ","HC v.s. IDP","Abs")
# PvalsMD<- function(IND, disag, DistSel) {
#   
#   if (disag=="HC v.s. IDP") {
#     
#     IDPHC_Fam <- multipleDd %>%
#       filter(multipleDd$`G.4. District`==district) %>%
#       select(status="status",contains(IND))%>%
#       na.omit()
#     
#     #Create boolean columns for all response options
#     for (i in (unique(IDPHC_Fam$var))){
#       contai<- ifelse(IDPHC_Fam$var==i,1,0)
#       IDPHC_Fam[i]<-contai }
#     
#     #loop through response columns and calculate pvalue
#     outputIDPHC<-data.frame() #IDP/HC STATUS
#     for (i in (3:length(IDPHC_Fam))){
#       if (length(unique(IDPHC_Fam$status))>1 & length(unique(IDPHC_Fam[[i]]))>1){
#         okay<-(chisq.test(IDPHC_Fam$status, IDPHC_Fam[[i]]))$p.value
#         outputIDPHC<-rbind(outputIDPHC,okay)}
#       else if(length(unique(IDPHC_Fam$status))<2 |length(unique(IDPHC_Fam[[i]]))<2){
#         okay<-NA
#         outputIDPHC<-rbind(outputIDPHC, okay) }
#     }
#     
#     outputIDPHC$var<-c(colnames(IDPHC_Fam[3:length(IDPHC_Fam)]))
#     colnames(outputIDPHC)<-c("pvalue","variable")
#     
#     outputIDPHC$sigg<- ifelse(outputIDPHC$pvalue <= 0.05 & outputIDPHC$pvalue >0.01, "*",
#                               ifelse(outputIDPHC$pvalue <=0.01 & outputIDPHC$pvalue > 0.001, "**",
#                                      ifelse(outputIDPHC$pvalue <=0.001, "***", "")))
#     
#     outputIDPHC$name<-paste0(outputIDPHC$var, outputIDPHC$sigg)
#     outputIDPHC
# 
#     
#   } else if (disag=="Female v.s. Male") {
#     
#     #Create dataframe containing indicator, and HoH gender/IDPHC status
#     #filtering to district and indicator
#     FM_Fam <- multipleDd %>%
#       filter(multipleDd$`G.4. District`==district) %>%
#       select(HoHgender= "HoHgender", contains(IND))%>%
#       na.omit()
#     
#     #loop through response columns and calculate pvalue
#     outputFM<-data.frame()
#     for (i in (3:length(FM_Fam))){
#       
#       if (length(unique(FM_Fam$HoHgender))>1 & length(unique(FM_Fam[[i]]))>1){
#         okay<-(chisq.test(FM_Fam$HoHgender, FM_Fam[[i]]))$p.value
#         outputFM<-rbind(outputFM,okay)}
#       
#       else if(length(unique(FM_Fam$HoHgender))<2  |length(unique(FM_Fam[[i]]))<2){
#         okay<-NA
#         outputFM<-rbind(outputFM, okay)
#       }
#     }
#     outputFM$variable<-c(colnames(FM_Fam[3:length(FM_Fam)]))
#     
#     colnames(outputFM)<-c("pvalue","variable")
#     
#     outputFM$sigg<- ifelse(outputFM$pvalue <= 0.05 & outputFM$pvalue >0.01, "*",
#                            ifelse(outputFM$pvalue <=0.01 & outputFM$pvalue > 0.001, "**",
#                                   ifelse(outputFM$pvalue <=0.001, "***", "")))
#     
#     outputFM$name<-paste0(outputFM$var, outputFM$sigg)
#     outputFM
#   }
# }

#okay$variable <- gsub("W.4.1. ","", okay$variable, fixed=TRUE)

#stacked ggplot [OPTIONS ADD TO 100%]
StackP<-function(try, titleS, color_S){
  ggplot(try, aes(x=Group, y=value, fill=variable)) + 
    geom_bar(stat="identity", width=0.7) + 
    theme_minimal()+
    scale_fill_manual(values = color_S) +
    labs(subtitle=titleS) + 
    theme(text = element_text(family = "Arial Narrow", size=14),
          axis.text.x = element_text(vjust=0.6),
          axis.title.x=element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(lineheight=.1),
          axis.title.y=element_blank()) +  
    coord_flip()+
    scale_y_continuous(labels = function(x) paste0(x, "%"))
}

#Dot plot [MULTIPLE CHOICE, MANY MANY CHOICES]
dotP<-function(try, titleS, color_S){
  ggplot(try, aes(x=variable)) + 
    geom_point(aes(fill=as.factor(Group), y=value, color=as.factor(Group)), alpha = 0.6, size=4) +   # Draw points
    geom_segment(aes(x=variable, 
                     xend=variable, 
                     y=min(value), 
                     yend=max(value)), 
                 linetype="dashed", 
                 size=0.1) +   # Draw dashed lines
    labs(subtitle = titleS) +  
    coord_flip()+
    theme_classic()+
    scale_color_manual(values = color_S) +
    theme(text = element_text(family = "Arial Narrow", size=14),
          legend.key.size = unit(0.3,"cm"),
          legend.title = element_blank(),
          plot.title = element_text(lineheight=.1),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
    scale_y_continuous(labels = function(x) paste0(x, "%"))
}


#Not stacked barplot [MULTIPLE CHOICE, SELECT MORE THAN ONE]

barP<-function(try, titleS,  color_S){
  ggplot(try, aes(x=variable, y=round(try$value,2),fill=Group))+ 
    geom_bar(stat="identity", position = "dodge", width=1, alpha=0.7) + 
    scale_fill_manual(values=color_S)+
    theme_minimal(base_line_size = 0.5) +
    coord_flip()+
    labs(subtitle=titleS) +
    scale_y_continuous(labels = function(x) paste0(x, "%"))+
    # geom_text(data=try, aes(x = variable, y=value+5, label=paste(value, "%")), 
    #           position = position_dodge(width = 0.90),
    #           family="Arial Narrow", size= 3)+
    theme(text = element_text(family = "Arial Narrow", size=14),
          #axis.line=element_blank(),
          #axis.text.y = element_text(margin = margin(r =5)),
          legend.key.size = unit(0.3,"cm"),
          plot.title = element_text(lineheight=.1),
          legend.title = element_blank(),
          #axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
          #axis.text.x=element_blank()) 
  
}

cat1DF<-select(dataF,"admin2pcode", contains("W.4. "), contains("W.5. "),contains("W.8. "),
               contains("W.9. "),contains("S.1. "), contains("S.8. "),contains("S.2.1. "))
cat1DM<-select(dataM,"admin2pcode", contains("W.4. "), contains("W.5. "),contains("W.8. "),
              contains("W.9. "),contains("S.1. "), contains("S.8. "),contains("S.2.1. "))
cat1DI<-select(dataI,"admin2pcode", contains("W.4. "), contains("W.5. "),contains("W.8. "),
              contains("W.9. "),contains("S.1. "), contains("S.8. "),contains("S.2.1. "))
cat1DH<-select(dataHC,"admin2pcode", contains("W.4. "), contains("W.5. "),contains("W.8. "),
              contains("W.9. "),contains("S.1. "), contains("S.8. "),contains("S.2.1. "))


#This function is for questions which only have 1 column, with percentages per row.
newfuncc<- function(IND, disag, DistSel) {
  
  if (disag=="IDP Status") {
    
    S_HC <- cat1DH %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Host Community")%>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    S_IDP <- cat1DI %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="IDP") %>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    chartD<-merge(S_HC, S_IDP, by="variable", all=TRUE)
    chartD$variable<-gsub(paste0(IND ,collapse = "|"),"", chartD$variable, fixed=TRUE)
    chartD
    
  } else if (disag=="Gender") {
    
    SC_F <- cat1DF %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Female") %>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    
    SC_M <- cat1DM %>%
      filter(admin2pcode == DistSel) %>%
      select(contains(IND)) %>%
      melt(value.name="Male")%>%
      mutate_if(is.numeric, funs(round(100*(.),1)))
    
    chartD<-merge(SC_F, SC_M, by="variable", all=TRUE)
    chartD$variable<-gsub(paste0(IND ,collapse = "|"),"", chartD$variable, fixed=TRUE)
    chartD
  }
}
melt(newfuncc("W.4. ", "Gender", "Abs") )
tryme<-DistFunct("W.3. ", "Gender", "Abs") 
