library(openxlsx)
library(dplyr)
library(tidyverse)
library(tidyselect)
library(reshape2)
library(weights)
dataT<-as.data.frame(read_csv("WASHdata.csv"))


##Data calculations
###########################################################################################################################################################
##################################################C A T E G O R I C A L ###################################################################################
###########################################################################################################################################################

#Subsetting categorical question type from dataset
categoricalDd<-select(dataT,"Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"), contains("G.4. D"),
                     contains("D.5."),contains("W.1.1. What"), contains("W.3. "),contains("W.4. "),
                     contains("W.5.Do"),contains("W.8. "),contains("W.9. "),contains("W.10."), contains("W.11. "), 
                     contains("S.1.W"),contains("S.5."), contains("S.6. "), contains("S.6.1."),
                     contains("S.8. Did you"),  contains("S.2. Do "), contains("S.2.1. Is"),
                     contains("S.2.2."), contains("S.3."), contains("S.4. What is"))



#Function to calculate pvalues of input indicator/disaggregation levels
Pvalsd<- function(IND,  district) {
  
  #Create dataframe containing indicator, and HoH gender/IDPHC status
  #filtering to district and indicator
  FM_Fam <- categoricalDd %>%
    filter(categoricalDd$`G.4. District`==district) %>%
    select( HoHgender= "HoHgender", var= contains(IND))%>%
    na.omit()
  IDPHC_Fam <- categoricalDd %>%
    filter(categoricalDd$`G.4. District`==district) %>%
    select(status="status", var= contains(IND))%>%
    na.omit()
  
  #Create boolean columns for all response options
  for (i in (unique(FM_Fam$var))){
    contai<- ifelse(FM_Fam$var==i,1,0)
    FM_Fam[i]<-contai
  }
  for (i in (unique(IDPHC_Fam$var))){
    contai<- ifelse(IDPHC_Fam$var==i,1,0)
    IDPHC_Fam[i]<-contai
  }
  
  #loop through response columns and calculate pvalue
  outputFM<-data.frame() #GENDER
  for (i in (3:length(FM_Fam))){
    if (length(unique(FM_Fam$HoHgender))>1 & length(unique(FM_Fam[[i]]))>1){
    okay<-(chisq.test(FM_Fam$HoHgender, FM_Fam[[i]]))$p.value
    outputFM<-rbind(outputFM,okay)}
    else if(length(unique(FM_Fam$HoHgender))<2 |length(unique(FM_Fam[[i]]))<2){
      okay<-NA
      outputFM<-rbind(outputFM, okay)
    }
  }
  outputFM$var<-c(colnames(FM_Fam[3:length(FM_Fam)]))
  
  outputIDPHC<-data.frame() #IDP/HC STATUS
  for (i in (3:length(IDPHC_Fam))){
    if (length(unique(IDPHC_Fam$status))>1 & length(unique(IDPHC_Fam[[i]]))>1){
    okay<-(chisq.test(IDPHC_Fam$status, IDPHC_Fam[[i]]))$p.value
    outputIDPHC<-rbind(outputIDPHC,okay)}
    else if(length(unique(IDPHC_Fam$status))<2 |length(unique(IDPHC_Fam[[i]]))<2){
      okay<-NA
      outputIDPHC<-rbind(outputIDPHC, okay)
    }
  }
  outputIDPHC$var<-c(colnames(IDPHC_Fam[3:length(IDPHC_Fam)]))
  
  #bind together pvalues for comparison of both IDPHC status and Gender for the specified indicator
  pvals<-merge(outputFM, outputIDPHC, by="var", all=TRUE)
  colnames(pvals)<-c("var", paste("Gender_pvalue",district),paste("IDPHC_pvalue", district))
  pvals
}

#Funtion to calculate the averages for specified district and indicator/ merge with pvalues

catSsheetDIST<- function(IND, district) { #Function to calculate figures for categorical question types
  #########################HOST COMMUNITY AND IDP################################
  S_HCf <- categoricalDd %>%
    filter(status=="Host Community",categoricalDd$`G.4. District`==district) %>%
    select(var= contains(IND)) %>%
    dplyr::count(var) %>%
    mutate("Host Community" = round(100*(n / sum(n)),1))%>%
    na.omit()
  
  S_IDPf <- categoricalDd %>%
    filter( status=="IDP",categoricalDd$`G.4. District`==district) %>%
    select(var= contains(IND)) %>%
    dplyr::count(var) %>%
    mutate(IDP = round(100*(n / sum(n)),1))%>%
    na.omit()
  
  ########################Gender##################################################
  
  S_Ff <- categoricalDd %>%
    filter(HoHgender=="Female",categoricalDd$`G.4. District`==district) %>%
    select(var= contains(IND)) %>%
    dplyr::count(var) %>%
    mutate(Female = round(100*(n / sum(n)),1))%>%
    na.omit()
  
  S_Mf <- categoricalDd %>%
    filter(HoHgender=="Male",categoricalDd$`G.4. District`==district) %>%
    select(var= contains(IND)) %>%
    dplyr::count(var) %>%
    mutate(Male = round(100*(n / sum(n)),1))%>%
    na.omit()
  
  print(IND)
  print(district) #check progress of code
  
  Inddata<-Merge(S_HCf[,-2], S_IDPf[,-2],S_Ff[,-2], S_Mf[,-2], id=~var)
  if (nrow(Inddata)>0){
    Inddata<-Merge(Inddata,Pvalsd(IND, district), id=~var)}
  else if (nrow(Inddata)==0){
    Inddata<-"No responses for this category"
  }
  Inddata
}

DistDATA1<- function(IND, disag, district) {
  AllData<-catSsheetDIST(IND, district)
  if (disag=="HC v.s. IDP") {
    AllData<-AllData[,c(-4,-5,-6)]
    AllData$var<-gsub(paste0(IND ,collapse = "|"),"", AllData$var, fixed=TRUE)
    AllData$sigg<- ifelse(AllData[,4] <= 0.05 & AllData[,4]>0.01, "*",
                          ifelse(AllData[,4] <=0.01 & AllData[,4] > 0.001, "**",
                                 ifelse(AllData[,4]<=0.001, "***", "")))
    AllData[is.na(AllData$sigg),5] <- ""
    AllData$variable<-paste0(AllData$var, AllData$sigg)
    AllData<-AllData[,c(6,2,3)]
    AllData
    
  } else if (disag=="Female v.s. Male") {
    AllData<-AllData[,c(-2,-3,-7)]
    AllData$var<-gsub(paste0(IND ,collapse = "|"),"", AllData$var, fixed=TRUE)
    AllData$sigg<- ifelse(AllData[,4] <= 0.05 & AllData[,4]>0.01, "*",
                          ifelse(AllData[,4] <=0.01 & AllData[,4] > 0.001, "**",
                                 ifelse(AllData[,4]<=0.001, "***", "")))
    AllData[is.na(AllData$sigg),5] <- ""
    AllData$variable<-paste0(AllData$var, AllData$sigg)
    AllData<-AllData[,c(6,2,3)]
    AllData
  }
}
###########################################################################################################################################################
################################################## S E L E C T   M O R E ##################################################################################
##################################################### T H A N  O N E ######################################################################################

#multiple choice %
#tallys several columns to find percentage of respondents for reach answer option


#Subsetting categorical question types where have the option to select more than one
multipleDd<-select(dataT,"Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"), contains("G.4. D"),
                  contains("W.1. "),contains("W.4.1. "),contains("W.5.1."),contains("Reason for not treating water"),
                  contains("W.9.1."), contains("W.11.1."), contains("Please name specific activities before"),
                  contains("S.9. "))

#Function to calculate pvalues of input indicator/disaggregation levels
PvalsM<- function(IND, district2) {
  FM_Fam <- multipleDd %>%
    filter(multipleDd$`G.4. District`==district2) %>%
    select(HoHgender= "HoHgender", contains(IND))%>%
    na.omit()
  IDPHC_Fam <- multipleDd %>%
    filter(multipleDd$`G.4. District`==district2) %>%
    select(status="status",contains(IND))%>%
    na.omit()
  
  #loop through response columns and calculate pvalue
  outputFM<-data.frame()
  for (i in (3:length(FM_Fam))){
    
    if (length(unique(FM_Fam$HoHgender))>1 & length(unique(FM_Fam[[i]]))>1){
      okay<-(chisq.test(FM_Fam$HoHgender, FM_Fam[[i]]))$p.value
      outputFM<-rbind(outputFM,okay)}
    
    else if(length(unique(FM_Fam$HoHgender))<2  |length(unique(FM_Fam[[i]]))<2){
      okay<-NA
      outputFM<-rbind(outputFM, okay)
    }
  }
  outputFM$variable<-c(colnames(FM_Fam[3:length(FM_Fam)]))
  
  outputIDPHC<-data.frame()
  for (i in (3:length(IDPHC_Fam))){
    
    if (length(unique(IDPHC_Fam$status))>1 & length(unique(IDPHC_Fam[[i]]))>1){
      okay<-(chisq.test(IDPHC_Fam$status, IDPHC_Fam[[i]]))$p.value
      outputIDPHC<-rbind(outputIDPHC,okay)}
    
    else if(length(unique(IDPHC_Fam$status))<2  |length(unique(IDPHC_Fam[[i]]))<2){
      okay<-NA
      outputIDPHC<-rbind(outputIDPHC, okay)
    }
  }
  outputIDPHC$variable<-c(colnames(IDPHC_Fam[3:length(IDPHC_Fam)]))
  
  #bind together pvalues for comparison of both IDPHC status and Gender for the specified indicator
  pvals<-Merge(outputFM, outputIDPHC, id=~variable)
  colnames(pvals)<-c("variable",paste("Gender_pvalue"),paste("IDPHC_pvalue"))
  pvals
}

#Funtion to calculate the averages for specified district and indicator/ merge with pvalues
multicsheetDIST<- function(IND, district3) {
  
  #IDP/HC DISAGGREGATION###################################################
  S_HCf <- multipleDd %>%
    filter(multipleDd$`G.4. District`==district3, status=="Host Community") %>%
    select(contains(IND))%>% 
    na.omit() %>%
      dplyr::summarise_all(mean) %>% 
      mutate_all(funs(.*100))%>% 
      melt(value.name="Host Community")
    
    S_IDPf <- multipleDd %>%
      filter(multipleDd$`G.4. District`==district3, status=="IDP") %>%
      select(contains(IND))%>% 
      na.omit() %>%
      mutate_all(funs(.*100))%>% 
      dplyr::summarise_all(mean) %>% 
      melt(value.name="IDP")
    
    
#GENDER DISSAGGREGATION######################################################
    SC_Ff <- multipleDd %>%
      filter(multipleDd$`G.4. District`==district3, HoHgender=="Female") %>%
      select(contains(IND))%>% 
      na.omit() %>%
      dplyr::summarise_all(mean) %>% 
      mutate_all(funs(.*100))%>% 
      melt(value.name="Female")
    
    SC_Mf <- multipleDd %>%
      filter(multipleDd$`G.4. District`==district3, HoHgender=="Male") %>%
      select(contains(IND))%>% 
      na.omit() %>%
      dplyr::summarise_all(mean) %>% 
      mutate_all(funs(.*100))%>% 
      melt(value.name="Male")
    
   
    multDat<-Merge(SC_Ff, SC_Mf,S_HCf, S_IDPf, id=~variable)
    # print(IND)
    # print(district)
    
    #bind together pvalues for comparison of both IDPHC status and Gender for the specified indicator
    if (nrow(multDat)>0){
      multiploutput<-Merge(multDat, PvalsM(IND,district3),id=~variable)
    }
    else if (nrow(multDat)==0){
      multiploutput<-"No responses for this category"
    }
    multiploutput
}


#Function to append astericks to the variable names and delete variables with percentages less than 5%
DistDATAm<- function(IND, disag, district0) {
  AllData<-multicsheetDIST(IND, district0)
if (disag=="HC v.s. IDP") {
  AllData<-AllData[,c(-2,-3,-6)]
  AllData$variable<-gsub(paste0(IND ,collapse = "|"),"", AllData$variable, fixed=TRUE)
  AllData$sigg<- ifelse(AllData[,4] <= 0.05 & AllData[,4]>0.01, "*",
                        ifelse(AllData[,4] <=0.01 & AllData[,4] > 0.001, "**",
                                      ifelse(AllData[,4]<=0.001, "***", "")))
  AllData[is.na(AllData$sigg),5] <- ""
  AllData$variable<-paste0(AllData$variable, AllData$sigg)
  
  AllData<-AllData[,c(1,2,3)]
  AllData<-filter_at(AllData, vars(-variable), any_vars(.>5))
  AllData
} else if (disag=="Female v.s. Male") {
  AllData<-AllData[,c(-4,-5,-7)]
  AllData$variable<-gsub(paste0(IND ,collapse = "|"),"", AllData$variable, fixed=TRUE)
  AllData$sigg<- ifelse(AllData[,4] <= 0.05 & AllData[,4]>0.01, "*",
                        ifelse(AllData[,4] <=0.01 & AllData[,4] > 0.001, "**",
                               ifelse(AllData[,4]<=0.001, "***", "")))
  AllData[is.na(AllData$sigg),5] <- ""
  AllData$variable<-paste0(AllData$variable, AllData$sigg)
  
  AllData<-AllData[,c(1,2,3)]
  AllData<-filter_at(AllData, vars(-variable), any_vars(.>5))
  AllData
}
}

#Function to append astericks and NOT delete variables with percentages less than 5%
DistDATAmNOT<- function(IND, disag, district0) {
  AllData<-multicsheetDIST(IND, district0)
  if (disag=="HC v.s. IDP") {
    AllData<-AllData[,c(-2,-3,-6)]
    AllData$variable<-gsub(paste0(IND ,collapse = "|"),"", AllData$variable, fixed=TRUE)
    AllData$sigg<- ifelse(AllData[,4] <= 0.05 & AllData[,4]>0.01, "*",
                          ifelse(AllData[,4] <=0.01 & AllData[,4] > 0.001, "**",
                                 ifelse(AllData[,4]<=0.001, "***", "")))
    AllData[is.na(AllData$sigg),5] <- ""
    AllData$variable<-paste0(AllData$variable, AllData$sigg)
    
    AllData<-AllData[,c(1,2,3)]
    AllData
  } else if (disag=="Female v.s. Male") {
    AllData<-AllData[,c(-4,-5,-7)]
    AllData$variable<-gsub(paste0(IND ,collapse = "|"),"", AllData$variable, fixed=TRUE)
    AllData$sigg<- ifelse(AllData[,4] <= 0.05 & AllData[,4]>0.01, "*",
                          ifelse(AllData[,4] <=0.01 & AllData[,4] > 0.001, "**",
                                 ifelse(AllData[,4]<=0.001, "***", "")))
    AllData[is.na(AllData$sigg),5] <- ""
    AllData$variable<-paste0(AllData$variable, AllData$sigg)
    
    AllData<-AllData[,c(1,2,3)]
    AllData
  }
}
  
###########################################################################################################################################################
######################################################### N U M E R I C ###################################################################################
###########################################################################################################################################################

#Subsetting indicators that are numeric responses.
# numericDd<-select(dataT,"Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"), contains("G.4. D"),
#                  contains("D.4.1."), contains("D.4.2."), contains("D.4.3."), 
#                  contains("D.4.4. "),contains("D.6.1."),contains("D.6.2."),contains("W.6."),
#                  contains("W.8.1."))
# 
# #Function to calculate pvalues of input indicator/disaggregation levels
# PvalsN<- function(INDn,district) {
#   #fileter dataset
#   HC_IDP <- numericDd %>%
#     filter(numericDd$`G.4. District`==district) %>%
#     select(status="status", numb=contains(INDn))%>%
#     na.omit()
#   
#   F_M <- numericDd %>%
#     filter(numericDd$`G.4. District`==district) %>%
#     select(HoHgender="HoHgender", numb=contains(INDn))%>%
#     na.omit()
#   
#   #calculate pvalues
#   HC_IDPp<-(if (length(unique(HC_IDP$status))>1& length(unique(HC_IDP$numb))<1){
#     (t.test(HC_IDP$numb[HC_IDP$status=="Host Community"], 
#                        HC_IDP$numb[HC_IDP$status=="IDP"]))$p.value}
#     
#     else{NA})
# 
#   F_Mp<-(if (length(unique(F_M$HoHgender))>1 & length(which(F_M$HoHgender=="Male"))>4 & length(which(F_M$HoHgender=="Female"))>4){
#     (t.test(F_M$numb[F_M$HoHgender=="Female"], 
#             F_M$numb[F_M$HoHgender=="Male"]))$p.value
#   }
#   else{NA})
#   
#   #bind together pvalues for comparison of both IDPHC status and Gender for the specified indicator
#   pvals<-cbind(F_Mp,HC_IDPp)
#   colnames(pvals)<-c(paste("Gender_pvalue"),paste("IDPHC_pvalue"))
#   pvals
# }
# 
# #Funtion to calculate the averages for specified district and indicator/ merge with pvalues
# numSheetDIST<-function(INDn, district) {
#     #IDP/HC DISAGGREGATION######################################################################################
# 
#     S_HCf <- numericDd %>%
#       filter(numericDd$`G.4. District`==district, status=="Host Community") %>%
#       select(HostCommunity=contains(INDn))%>%
#       na.omit()%>%
#       dplyr::summarise_all(mean)
#     S_IDPf <- numericDd %>%
#       filter(numericDd$`G.4. District`==district, status=="IDP") %>%
#       select(IDP=contains(INDn))%>%
#       na.omit()%>%
#       dplyr::summarise_all(mean)
#   
# 
# #GENDER DISAGGREGATION #######################################################################################
# 
#     SC_Ff <- numericDd %>%
#       filter(numericDd$`G.4. District`==district, HoHgender=="Female") %>%
#       select(Female=contains(INDn))%>%
#       na.omit()%>%
#       dplyr::summarise_all(mean)
#     
#     SC_Mf <- numericDd %>%
#       filter(numericDd$`G.4. District`==district, HoHgender=="Male") %>%
#       select(Male=contains(INDn))%>%
#       na.omit()%>%
#       dplyr::summarise_all(mean)
#     
#     print(INDn)
#     print(district)
#     multDat<-cbind(SC_Ff, SC_Mf,S_HCf, S_IDPf)
#     
#     multDat<-cbind(multDat,PvalsN(INDn,district))
# 
#     multDat
# }

##############################################################################################################################################################
#S.1 AFFORD- data wrangle###################################################################################################################################
##########################################################################################################################################################

#Subsetting the dataset to just S1
Hitemsd<-select(dataT,  "d_category", contains("HoHgender"), contains("status"),contains("G.4. D"),contains("S.1. "))

#function to calculate pvalues for responses to S1-ACCESS
PvalsS1<- function(district) {
  IDPHCpv<-Hitemsd %>%
    filter(Hitemsd$`G.4. District`==district) %>%
    select(status="status", contains("S.1. "),-contains("afford"))%>%
    na.omit()
  FMpv<-Hitemsd %>%
    filter(Hitemsd$`G.4. District`==district) %>%
    select(HoHgender="HoHgender", contains("S.1. "),-contains("afford"))%>%
    na.omit()

  #loop through response columns and calculate pvalue
  #some responses had too little variation or only one population group responding
  outputFM<-data.frame()
  for (i in (2:length(FMpv))){
    if (length(unique(FMpv$HoHgender))>1 & length(unique(FMpv[[i]]))>1 ){
    okay<-(chisq.test(FMpv$HoHgender, FMpv[[i]]))$p.value
    outputFM<-rbind(outputFM,okay)}
    else if(length(unique(FMpv$HoHgender))<2 |length(unique(FMpv[[i]]))<2){
      okay<-NA
      outputFM<-rbind(outputFM, okay)
    }
  }
  outputFM$variable<-c(colnames(FMpv[2:length(FMpv)]))

  outputIDPHC<-data.frame()
  for (i in (2:length(IDPHCpv))){
    if (length(unique(IDPHCpv$status))>1 & length(unique(IDPHCpv[[i]]))>1 ){
    okay<-(chisq.test(IDPHCpv$status, IDPHCpv[[i]]))$p.value
    outputIDPHC<-rbind(outputIDPHC,okay)}
    else if(length(unique(IDPHCpv$status))<2|length(unique(FMpv[[i]]))<2){
      okay<-NA
      outputIDPHC<-rbind(outputIDPHC, okay)
    }
  }
  outputIDPHC$variable<-c(colnames(IDPHCpv[2:length(IDPHCpv)]))

  #bind together pvalues for comparison of both IDPHC status and Gender for the specified indicator
  pvals<-Merge(outputFM, outputIDPHC, id=~variable)
  colnames(pvals)<-c("variable",paste("Gender_pvalue"),paste("IDPHC_pvalue"))
  pvals
}


#Function to calculate the average for ACCESSING items
S1AccessD<- function(disag, district) {
  #IDP/HC dissaggregation
  if (disag=="HC v.s. IDP") {
    HitemsHC<-Hitemsd %>%
      filter( Hitemsd$`G.4. District`==district,status=="Host Community") %>%
      select(contains("S.1. "),-contains("afford"))%>%
      na.omit()%>%
      summarise_all(mean)%>%#divide columns by total weight
      mutate_all(funs(round(.*100)),2)%>%
      melt(value.name="Host Community")


    HitemsIDP<-Hitemsd %>%
      filter(Hitemsd$`G.4. District`==district, status=="IDP") %>%
      select(contains("S.1. "),-contains("afford"))%>%
      na.omit()%>%
      summarise_all(mean)%>%#divide columns by total weight
      mutate_all(funs(round(.*100)),2)%>%
      melt(value.name="IDP")

    chartD<-merge(HitemsHC, HitemsIDP, by="variable", all=TRUE)

    #Gender dissaggregation
  } else if (disag=="Female v.s. Male") {
    HitemsF<-Hitemsd %>%
      filter(Hitemsd$`G.4. District`==district, HoHgender=="Female") %>%
      select(contains("S.1. "),-contains("afford"))%>%
      na.omit()%>%
      summarise_all(mean)%>%#divide columns by total weight
      mutate_all(funs(round(.*100)),2)%>%
      melt(value.name="Female")

    HitemsM<-Hitemsd %>%
      filter(Hitemsd$`G.4. District`==district, HoHgender=="Male") %>%
      select(contains("S.1. "),-contains("afford"))%>%
      na.omit()%>%
      summarise_all(mean)%>%#divide columns by total weight
      mutate_all(funs(round(.*100)),2)%>%
      melt(value.name="Male")

    chartD<-merge(HitemsF, HitemsM, by="variable", all=TRUE)

  }
}
#Merge results for all disaggregation options
# s1_access<-Merge(S1AccessD("IDP Status", district),S1AccessD("Gender", district),id=~variable)
# melt(variable.name= "Group",S1AccessD("IDP Status", district))
# 
# #Merge averages with corresponding pvalues
# s1sheet<-Merge(s1_access, PvalsS1(district),id=~variable)
# s1sheet


#S.1 AFFORD-data wrangle##########################################################################################################################################
##########################################################################################################################################################

#function to calculate pvalues for responses to S1 AFFORD
PvalsS1aff<- function(district) {
  IDPHCpv<-Hitemsd%>%
    filter(Hitemsd$`G.4. District`==district) %>%
    select(status="status", contains("S.1. "),-contains("access"))%>%
    na.omit()
  FMpv<-Hitemsd %>%
    filter(Hitemsd$`G.4. District`==district) %>%
    select(HoHgender="HoHgender", contains("S.1. "),-contains("access"))%>%
    na.omit()
  

  #loop through response columns and calculate pvalue
  outputFM<-data.frame()
  for (i in (2:length(FMpv))){
    if (length(unique(FMpv$HoHgender))>1 & length(unique(FMpv[[i]]))>1 ){
      okay<-(chisq.test(FMpv$HoHgender, FMpv[[i]]))$p.value
      outputFM<-rbind(outputFM,okay)
    }
    else if(length(unique(FMpv$HoHgender))<2|length(unique(FMpv[[i]]))<2){
      okay<-NA
      outputFM<-rbind(outputFM, okay)
    }}
    outputFM$variable<-c(colnames(FMpv[2:length(FMpv)]))
    
  outputIDPHC<-data.frame()
  
  for (i in (2:length(IDPHCpv))){
    if (length(unique(IDPHCpv$status))>1 & length(unique(IDPHCpv[[i]]))>1 ){
      okay<-(chisq.test(IDPHCpv$status, IDPHCpv[[i]]))$p.value
      outputIDPHC<-rbind(outputIDPHC,okay)
    }
    else if(length(unique(IDPHCpv$status))<2|length(unique(IDPHCpv[[i]]))<2){
      okay<-NA
      outputIDPHC<-rbind(outputIDPHC, okay)
    }
  }
  outputIDPHC$variable<-c(colnames(IDPHCpv[2:length(IDPHCpv)]))
  
  #bind together pvalues for comparison of both IDPHC status and Gender for the specified indicator
  pvals<-Merge(outputFM, outputIDPHC, id=~variable)
  colnames(pvals)<-c("variable","Gender_pvalue","IDPHC_pvalue")
  pvals
} 

#Function to calculate the average for AFFORDING items 
S1AffordD<- function(disag, district) {
  if (disag=="HC v.s. IDP") {
    HitemsHC<-Hitemsd %>%
      filter(Hitemsd$`G.4. District`==district, status=="Host Community") %>%
      select(contains("S.1. "),-contains("access"))%>% 
      na.omit()%>%
      summarise_all(mean)%>%#divide columns by total weight
      mutate_all(funs(round(.*100)),2)%>% 
      melt(value.name="Host Community")
    
    
    HitemsIDP<-Hitems%>%
      filter(Hitemsd$`G.4. District`==district, status=="IDP") %>%
      select(contains("S.1. "),-contains("access"))%>% 
      na.omit()%>%
      summarise_all(mean)%>%#divide columns by total weight
      mutate_all(funs(round(.*100)),2)%>% 
      melt(value.name="IDP")
    
    chartD<-merge(HitemsHC, HitemsIDP, by="variable", all=TRUE)
    
  } else if (disag=="Female v.s. Male") {
    HitemsF<-Hitemsd %>%
      filter(Hitemsd$`G.4. District`==district, HoHgender=="Female") %>%
      select(contains("S.1. "),-contains("access"))%>% 
      na.omit()%>%
      summarise_all(mean)%>%#divide columns by total weight
      mutate_all(funs(round(.*100)),2)%>% 
      melt(value.name="Female")
    
    
    HitemsM<-Hitemsd %>%
      filter(Hitemsd$`G.4. District`==district, HoHgender=="Male") %>%
      select(contains("S.1. "),-contains("access"))%>% 
      na.omit()%>%
      summarise_all(mean)%>%#divide columns by total weight
      mutate_all(funs(round(.*100)),2)%>% 
      melt(value.name="Male")
    
    chartD<-merge(HitemsF, HitemsM, by="variable", all=TRUE)
    
  }
}




