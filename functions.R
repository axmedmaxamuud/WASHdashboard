
# FUNCTIONS TO WRANGLE DATA INTO PROPER FORMAT FOR CHARTS (chart section below)



#########################################################################################################################################
#######################################  C A L C U L A T I O N   F U N C T I O N S  #####################################################
#########################################################################################################################################


###########################################  C A T E G O R i C A L % ####################################################################
#calculates percent of reponses for each answer option


categoricalD<-select(dataT,"Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"), 
                     contains("D.5."),contains("W.1.1. What"), contains("W.3. "),contains("W.4. "),
                     contains("W.5.Do"),contains("W.9. "),contains("W.10."), contains("W.11. "), 
                     contains("S.1.W"),contains("S.5."), contains("S.6. "), contains("S.6.1."),
                     contains("S.8. Did you"),  contains("S.2. Do "), contains("S.2.1. Is"),
                     contains("S.2.2."), contains("S.3."), contains("S.4. What is"))


catS<- function(IND, disag, gcat) {
  
  if (disag=="HC v.s. IDP") {
    
    S_HC <- categoricalD %>%
      filter(d_category == gcat, status=="Host Community") %>%
      select(weight="Weight_group_comparison", var= contains(IND)) %>%
      group_by(var)%>%
      dplyr::summarise(n=sum(weight)) %>%
      mutate(S_HC =round(100*(n / sum(n)),1))%>%
      na.omit() %>%
      filter(S_HC!=0)
    
    S_IDP <- categoricalD %>%
      filter(d_category == gcat, status=="IDP") %>%
      select(weight="Weight_group_comparison", var= contains(IND)) %>%
      group_by(var)%>%
      dplyr::summarise(n=sum(weight)) %>%
      mutate(S_IDP = round(100*(n / sum(n)),1)) %>%
      na.omit() %>%
      filter(S_IDP!=0)
    
    chartD<-merge(S_HC[,-2], S_IDP[,-2], by="var", all=TRUE)
    colnames(chartD)<-c("var","Host Community", "IDP")
    chartD
    
  } else if (disag=="Female v.s. Male") {
    
    SC_F <- categoricalD %>%
      filter(d_category == gcat, HoHgender=="Female") %>%
      select(weight="Weight_group_comparison", var= contains(IND)) %>%
      group_by(var)%>%
      dplyr::summarise(n=sum(weight)) %>%
      filter_all(any_vars(. != 0)) %>%
      mutate(SC_F = round(100*(n / sum(n)),1))%>%
      na.omit() %>%
      filter(SC_F!=0)
    
    SC_M <- categoricalD %>%
      filter(d_category == gcat, HoHgender=="Male") %>%
      select(weight="Weight_group_comparison", var= contains(IND)) %>%
      group_by(var)%>%
      dplyr::summarise(n=sum(weight)) %>%
      mutate(SC_M = round(100*(n / sum(n)),1)) %>%
      na.omit() %>%
      filter(SC_M!=0)
    
    chartD<-merge(SC_F[,-2], SC_M[,-2], all=TRUE)
    colnames(chartD)<-c("var","Female", "Male")
    chartD
  }
}

Pvals<- function(IND, disag,DISS) {
  
  if (disag=="HC v.s. IDP") {
    
    #Create dataframe containing weight, indicator, and IDPHC status
    #iltering to only FAMINE, CHOLERA or BOTH
    IDPHC_Fam <- categoricalD %>%
      filter(d_category == DISS) %>%
      select(d_category= "d_category", status="status",
             weight="Weight_group_comparison", var= contains(IND))%>%
      na.omit()
    
    #Create boolean columns for all response options
    for (i in (unique(IDPHC_Fam$var))){
      contai<- ifelse(IDPHC_Fam$var==i,1,0)
      IDPHC_Fam[i]<-contai
    }
    
    #loop through response columns and calculate pvalue
    outputIDPHC<-data.frame()
    
    for (i in (5:length(IDPHC_Fam))){
      okay<-(wtd.chi.sq(IDPHC_Fam$status, IDPHC_Fam[[i]], weight=IDPHC_Fam$weight))[3]
      outputIDPHC<-rbind(outputIDPHC,okay)
    }
    
    
    outputIDPHC$var<-c(colnames(IDPHC_Fam[5:length(IDPHC_Fam)]))
    colnames(outputIDPHC)<-c("pvalue","var")
    outputIDPHC$sigg<- ifelse(outputIDPHC$pvalue <= 0.05 & outputIDPHC$pvalue >0.01, "*",
                          ifelse(outputIDPHC$pvalue <=0.01 & outputIDPHC$pvalue > 0.001, "**",
                                 ifelse(outputIDPHC$pvalue <=0.001, "***", "")))
    
  outputIDPHC$name<-paste0(outputIDPHC$var, outputIDPHC$sigg)
  outputIDPHC
    
  } else if (disag=="Female v.s. Male") {
    
    #Create dataframe containing weight, indicator, and HoH gender/IDPHC status
    #iltering to only FAMINE, CHOLERA or BOTH
    FM_Fam <- categoricalD %>%
      filter(d_category == DISS) %>%
      select(d_category= "d_category", HoHgender= "HoHgender",
             weight="Weight_group_comparison", var= contains(IND))%>%
      na.omit()
    
    #Create boolean columns for all response options
    for (i in (unique(FM_Fam$var))){
      contai<- ifelse(FM_Fam$var==i,1,0)
      FM_Fam[i]<-contai
    }
    
    #loop through response columns and calculate pvalue
    outputFM<-data.frame()
    for (i in (5:length(FM_Fam))){
      okay<-(wtd.chi.sq(FM_Fam$HoHgender, FM_Fam[[i]], weight=FM_Fam$weight))[3]
      outputFM<-rbind(outputFM,okay)
    }
    outputFM$var<-c(colnames(FM_Fam[5:length(FM_Fam)]))
    colnames(outputFM)<-c("pvalue","var")
    outputFM$sigg<-ifelse(outputFM$pvalue <= 0.05 & outputFM$pvalue >0.01, "*",
                          ifelse(outputFM$pvalue <=0.01 & outputFM$pvalue > 0.001, "**",
                                 ifelse(outputFM$pvalue <=0.001, "***", "")))
    outputFM$name<-paste0(outputFM$var, outputFM$sigg)
    outputFM
  }
}

###########################################  M U L T I P L E  C H O I C E % ####################################################################
#tallys several columns to find percentage of respondents for reach answer option

multipleD<-select(dataT,"Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"), 
                  contains("W.1. "),contains("W.4.1. "),contains("W.5.1."),contains("Reason for not treating water"),
                  contains("W.9.1."), contains("W.11.1."), contains("Please name specific activities before"),
                  contains("S.9. "))

multicS<- function(IND, disag, gcat) {
  if (disag=="HC v.s. IDP") {

    S_HC <- multipleD %>%
      filter(d_category == gcat, status=="Host Community") %>%
      select(weight="Weight_group_comparison", contains(IND))%>%
      mutate_at(vars(-weight), funs(.*weight))%>% #multipy columns by weights
      na.omit() %>%
      summarise_if(is.numeric, funs(sum(.)))%>% #summarize columns
      summarise_at(vars(-weight), funs(round(100*(./weight))))%>% #divide columns by total weight
      melt(value.name="Host Community")

    S_IDP <- multipleD %>%
      filter(d_category == gcat, status=="IDP") %>%
      select(weight="Weight_group_comparison", contains(IND))%>%
      mutate_at(vars(-weight), funs(.*weight))%>% #multipy columns by weights
      na.omit() %>%
      summarise_if(is.numeric, funs(sum(.)))%>% #summarize columns
      summarise_at(vars(-weight), funs(round(100*(./weight))))%>% #divide columns by total weight
      melt(value.name="IDP")

    chartD<-merge(S_HC, S_IDP, by="variable", all=TRUE)

  } else if (disag=="Female v.s. Male") {

    SC_F <- multipleD %>%
      filter(d_category == gcat, HoHgender=="Female") %>%
      select(weight="Weight_group_comparison", contains(IND))%>%
      mutate_at(vars(-weight), funs(.*weight))%>% #multipy columns by weights
      na.omit() %>%
      summarise_if(is.numeric, funs(sum(.)))%>% #summarize columns
      summarise_at(vars(-weight), funs(round(100*(./weight))))%>% #divide columns by total weight
      melt(value.name="Female")

    SC_M <- multipleD %>%
      filter(d_category == gcat, HoHgender=="Male") %>%
      select(weight="Weight_group_comparison", contains(IND))%>%
      mutate_at(vars(-weight), funs(.*weight))%>% #multipy columns by weights
      na.omit() %>%
      summarise_if(is.numeric, funs(sum(.)))%>% #summarize columns
      summarise_at(vars(-weight), funs(round(100*(./weight))))%>% #divide columns by total weight
      melt(value.name="Male")

    chartD<-merge(SC_F, SC_M, by="variable", all=TRUE)
  }
}



###########################################  N U M E R I C   R E S P O N S E ####################################################################

numericD<-select(dataT,"Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"), 
                 contains("D.4.1."), contains("D.4.2."), contains("D.4.3."), 
                 contains("D.4.4. "),contains("D.6.1."),contains("D.6.2."),contains("W.6."),
                 contains("W.8.1."))



numS<-function(INDn, disag, gcat) {
  if (disag=="HC v.s. IDP") {
    
    S_HC <- numericD %>%
      filter(d_category == gcat, status=="Host Community") %>%
      select(weight="Weight_group_comparison", numb=contains(INDn))%>%
      mutate_at(vars(-weight), funs(.*weight))%>% #multipy columns by weights
      na.omit()%>%
      dplyr::summarise("Host Community"=round(sum(numb)/sum(weight),2)) #divide total number by total weight for average
    S_IDP <- numericD %>%
      filter(d_category == gcat, status=="IDP") %>%
      select(weight="Weight_group_comparison", numb=contains(INDn))%>%
      mutate_at(vars(-weight), funs(.*weight))%>% #multipy columns by weights
      na.omit()%>%
      dplyr::summarise("IDP"=round(sum(numb)/sum(weight),2)) #divide total number by total weight for average
    chartD<-merge(S_HC, S_IDP, all=TRUE)
    chartD
  } else if (disag=="Female v.s. Male") {
    SC_F <- numericD %>%
      filter(d_category == gcat, HoHgender=="Female") %>%
      select(weight="Weight_group_comparison", numb=contains(INDn))%>%
      mutate_at(vars(-weight), funs(.*weight))%>% #multipy columns by weights
      na.omit()%>%
      dplyr::summarise("Female"=round(sum(numb)/sum(weight),2)) #divide total number by total weight for average
    SC_M <- numericD %>%
      filter(d_category == gcat, HoHgender=="Male") %>%
      select(weight="Weight_group_comparison", numb=contains(INDn))%>%
      mutate_at(vars(-weight), funs(.*weight))%>% #multipy columns by weights
      na.omit()%>%
      dplyr::summarise("Male"=round(sum(numb)/sum(weight),2)) #divide total number by total weight for average
    chartD<-merge(SC_F, SC_M, all=TRUE)
    chartD
  }
}


PvalsN<- function(INDn, disag,DISS) {
  
  if (disag=="HC v.s. IDP") {
    
    HC_IDP <- numericD %>%
      filter(d_category == DISS) %>%
      select(weight="Weight_group_comparison",status="status", numb=contains(INDn))
    
    HC_IDPp<-(wtd.t.test(HC_IDP$numb[HC_IDP$status=="Host Community"], 
                         HC_IDP$numb[HC_IDP$status=="IDP"], 
                         weight=HC_IDP$weight[HC_IDP$status=="Host Community"], 
                         weighty=HC_IDP$weight[HC_IDP$status=="IDP"],bootse=TRUE))$coefficients[3]
    sigg<- ifelse(HC_IDPp <= 0.05 & HC_IDPp >0.01, "*",
                          ifelse(HC_IDPp <=0.01 & HC_IDPp > 0.001, "**",
                                 ifelse(HC_IDPp <=0.001, "***", "")))
  } else if (disag=="Female v.s. Male") {
    F_M <- numericD %>%
      filter(d_category == DISS) %>%
      select(weight="Weight_group_comparison",HoHgender="HoHgender", numb=contains(INDn))
    
    F_Mp<-(wtd.t.test(F_M$numb[F_M$HoHgender=="Female"], 
                      F_M$numb[F_M$HoHgender=="Male"], 
                      weight=F_M$weight[F_M$HoHgender=="Female"], 
                      weighty=F_M$weight[F_M$HoHgender=="Male"],bootse=TRUE))$coefficients[3]
    sigg<-ifelse(F_Mp <= 0.05 & F_Mp >0.01, "*",
                          ifelse(F_Mp <=0.01 & F_Mp > 0.001, "**",
                                 ifelse(F_Mp <=0.001, "***", "")))
  }
}

###########################################  S.1 DATA CALULCAITON ####################################################################

Hitems<-select(dataT, "Weight_group_comparison",  "d_category", contains("HoHgender"), contains("status"),contains("S.1. "))


#### A C C E S S ######

S1Access<- function(disag, gcat) {
  if (disag=="HC v.s. IDP") {
    HitemsHC<-Hitems%>%
      filter(d_category == gcat, status=="Host Community") %>%
      select(weight="Weight_group_comparison", contains("S.1. "),-contains("afford"))%>% 
      na.omit() %>%
      mutate_at(vars(-weight), funs(.*weight)) %>%
      summarise_if(is.numeric, funs(sum(.))) %>%
      summarise_at(vars(-weight), funs(round(100*(./weight)))) %>%#divide columns by total weight
      melt(value.name="Host Community")
    
    
    HitemsIDP<-Hitems%>%
      filter(d_category == gcat, status=="IDP") %>%
      select(weight="Weight_group_comparison", contains("S.1. "),-contains("afford"))%>% 
      na.omit() %>%
      mutate_at(vars(-weight), funs(.*weight)) %>%
      summarise_if(is.numeric, funs(sum(.))) %>%
      summarise_at(vars(-weight), funs(round(100*(./weight)))) %>%#divide columns by total weight
      melt(value.name="IDP")
    
    chartD<-merge(HitemsHC, HitemsIDP, by="variable", all=TRUE)
    
    
  } else if (disag=="Female v.s. Male") {
    HitemsF<-Hitems%>%
      filter(d_category == gcat, HoHgender=="Female") %>%
      select(weight="Weight_group_comparison", contains("S.1. "),-contains("afford"))%>% 
      na.omit() %>%
      mutate_at(vars(-weight), funs(.*weight)) %>%
      summarise_if(is.numeric, funs(sum(.))) %>%
      summarise_at(vars(-weight), funs(round(100*(./weight)))) %>%#divide columns by total weight
      melt(value.name="Female")
    
    
    HitemsM<-Hitems%>%
      filter(d_category == gcat, HoHgender=="Male") %>%
      select(weight="Weight_group_comparison", contains("S.1. "),-contains("afford"))%>% 
      na.omit() %>%
      mutate_at(vars(-weight), funs(.*weight)) %>%
      summarise_if(is.numeric, funs(sum(.))) %>%
      summarise_at(vars(-weight), funs(round(100*(./weight)))) %>%#divide columns by total weight
      melt(value.name="Male")
    
    chartD<-merge(HitemsF, HitemsM, by="variable", all=TRUE)
    
  }
}



#### A F F O R D  ##########

S1Afford<- function(disag, gcat) {
  if (disag=="HC v.s. IDP") {
    HitemsHC<-Hitems%>%
      filter(d_category == gcat, status=="Host Community") %>%
      select(weight="Weight_group_comparison", contains("S.1. "),-contains("access"))%>% 
      na.omit() %>%
      mutate_at(vars(-weight), funs(.*weight)) %>%
      summarise_if(is.numeric, funs(sum(.))) %>%
      summarise_at(vars(-weight), funs(round(100*(./weight)))) %>%#divide columns by total weight
      melt(value.name="Host Community")
    
    
    HitemsIDP<-Hitems%>%
      filter(d_category == gcat, status=="IDP") %>%
      select(weight="Weight_group_comparison", contains("S.1. "),-contains("access"))%>% 
      na.omit() %>%
      mutate_at(vars(-weight), funs(.*weight)) %>%
      summarise_if(is.numeric, funs(sum(.))) %>%
      summarise_at(vars(-weight), funs(round(100*(./weight)))) %>%#divide columns by total weight
      melt(value.name="IDP")
    
    chartD<-merge(HitemsHC, HitemsIDP, by="variable", all=TRUE)
    
  } else if (disag=="Female v.s. Male") {
    HitemsF<-Hitems%>%
      filter(d_category == gcat, HoHgender=="Female") %>%
      select(weight="Weight_group_comparison", contains("S.1. "),-contains("access"))%>% 
      na.omit() %>%
      mutate_at(vars(-weight), funs(.*weight)) %>%
      summarise_if(is.numeric, funs(sum(.))) %>%
      summarise_at(vars(-weight), funs(round(100*(./weight)))) %>%#divide columns by total weight
      melt(value.name="Female")
    
    
    HitemsM<-Hitems%>%
      filter(d_category == gcat, HoHgender=="Male") %>%
      select(weight="Weight_group_comparison", contains("S.1. "),-contains("access"))%>% 
      na.omit() %>%
      mutate_at(vars(-weight), funs(.*weight)) %>%
      summarise_if(is.numeric, funs(sum(.))) %>%
      summarise_at(vars(-weight), funs(round(100*(./weight)))) %>%#divide columns by total weight
      melt(value.name="Male")
    
    chartD<-merge(HitemsF, HitemsM, by="variable", all=TRUE)
    
  }
}
#########################################################################################################################################
#############################################  C H A R T    F U N C T I O N S  ##########################################################
#########################################################################################################################################

#Set colors for charts for each tab
# THEMECOL!!, NAVY BLUE, TURQOISE, BLACK, LightGREY, Baby BLUE
Dcols <- c("#333333","#006666",  "#009999",  "#000000", "#CCCCCC", #dark grey for DEMOGRAPHICS
           "#8cbfbf")
Wcols <- c("#3399CC","#006666",  "#009999",  "#000000", "#CCCCCC", #Water blue for WATER
           "#8cbfbf")
Scols <- c("#CC6633","#006666",  "#009999",  "#000000", "#CCCCCC",  #orange for SANITATION
           "#8cbfbf")
Hcols <- c("#009933","#006666",  "#009999",  "#000000", "#CCCCCC", #Green for HYGINE
           "#8cbfbf")

###########################################  B A R   C H A R TS (hc)####################################################################

hcBARchart<-function(bchart, colorL, gtitle ) {
  bchart %>%
    hchart('bar', hcaes(x = 'var',y= "value", group="variable" ))%>%
    hc_colors(colorL) %>%
    hc_legend(itemStyle = list(fontSize=13, fontFamily='Arial Narrow', fontWeight="normal"))%>%
    hc_title(text = paste("District Prioritization: ",gtitle),
             style = list(fontFamily='Arial Narrow')) %>%
    hc_yAxis(
      title = list(text = ""),
      gridLineWidth = 0,
      fontFamily = 'Arial',
      labels = list(format = "{value}%",
                    style = list(fontFamily='Arial Narrow'))
    ) %>%
    hc_xAxis(
      lineWidth=0,
      minorTickWidth=0,
      title = list(text = ""),
      labels = list(style = list(fontSize = "11px",
                                 textOverflow='none',
                                 fontFamily='Arial Narrow'))
    ) %>%
    hc_add_theme(hc_theme_smpl())
}

#COLchart (HC)
fntltpC <- JS("function(){
            return Highcharts.numberFormat(this.point.value, 2)+ ' guests';
             }")
# gcat<-"Famine"
# Colchart<-melt(numS("D.6.2.","Female v.s. Male", gcat))
# 
# hcColchart(Colchart, Dcols, gcat)

hcColchart<-function(Colchart, colorL, gtitle ) {
Colchart %>%
  hchart('bar',hcaes(x = 'variable',y= "value" , group="variable"),
         dataLabels = list(enabled = FALSE))%>%
    hc_colors(colorL) %>%
    hc_title(text = paste("District Prioritization: ",gtitle),
             style = list(fontFamily='Arial Narrow')) %>%
    hc_tooltip(formatter = fntltpC) %>%
    hc_legend(enabled= FALSE,
              itemStyle = list(fontSize=13, fontFamily='Arial Narrow', fontWeight="normal"))%>%
    hc_yAxis(
      title = list(text = ""),
      gridLineWidth = 0.2,
      labels = list(format = "{value}",
                    style = list(fontFamily='Arial Narrow'))
    ) %>%
    hc_subtitle(text = "Please download data for levels of significance")%>%
    hc_plotOptions(bar = list(
      pointPadding= 0,
      borderWidth= 0,
      groupPadding= 0,
      stacking = "normal",
      pointWidth=30)
    ) %>%
    hc_xAxis(
      lineWidth=0,
      minorTickWidth=0,
      labels = list(style = list(fontFamily='Arial Narrow')),
      title = list(text = "")
    ) %>%
    hc_add_theme(hc_theme_smpl())
}


###########################################  H E A T M A P (hc) ####################################################################

plotline <- list(
  color = "#006666", value = 19, width = 10, zIndex = 5
)
fntltp1 <- JS("function(){
             return this.series.yAxis.categories[this.point.y] + ':<br>' +
             Highcharts.numberFormat(this.point.value, 2) +'%';
             }")
hcHMchart<-function(hchartd, gtitle ) {
hchart(hchartd, "heatmap", hcaes(x = variable, y = options, value = value)) %>%
  hc_colorAxis(stops=color_stops(n=2, colors=c("#EFEFEF","#009999"))) %>%
  hc_yAxis(reversed = TRUE, offset = -10, tickLength = 0,
           gridLineWidth = 1, minorGridLineWidth = 0,
           labels = list(style = list(fontSize = "11px",
                                      textOverflow='none', 
                                      fontFamily='Arial Narrow')), #enable textwrap 
           title=list(text="")) %>%
  hc_tooltip(formatter = fntltp1) %>%
  hc_xAxis(opposite=TRUE,
           labels = list(style = list(fontFamily='Arial Narrow')),
           plotLines = list(plotline),
           title = list(text = "")) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_title(text = paste("District Prioritization: ",gtitle),
           style = list(fontFamily='Arial Narrow')) %>%
  hc_legend(layout = "vertical", verticalAlign = "middle",
            align = "right", valueDecimals = 0,
            itemStyle = list(fontSize=13, fontFamily='Arial Narrow', fontWeight="normal")) %>%
  hc_size(height = 250)
}

#########################################################################################################################################
###############################################  D I S T R I C T   T A B  ###############################################################
##################################################  F U N C T I O N S  ###################################################################
#####################################################( G G P L O T )###########################################################################


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
#########################################################################################################################################
###############################################  D I S T R I C T   T A B  ###############################################################
##################################################  F U N C T I O N S  ###################################################################
######################################################## ( H c ) ###########################################################################

# #stacked bar chart
hcSTACKEDc<-function(pchart, colorL, gtitle ) {
  pchart %>%
    hchart('bar', hcaes(x = 'Group',y= "value", group="variable" ))%>%
    hc_colors(colorL) %>%
    hc_yAxis(
      title = list(text = ""),
      gridLineWidth = 1,
      max=100,
      labels = list(format = "{value}%",
                    style = list(fontSize=13, 
                                 fontFamily='Arial Narrow', 
                                 fontWeight="normal"))
    ) %>%
    hc_title(text = paste(gtitle),
             style = list(fontSize="18px",
                          fontFamily='Arial Narrow', 
                          fontWeight="normal")) %>%
    hc_xAxis(
      lineWidth=0,
      minorTickWidth=0,
      title = list(text = ""),
      labels = list(style = list(fontSize=13, fontFamily='Arial Narrow', fontWeight="normal"))
    )  %>%
    hc_plotOptions(bar = list(
      dataLabels = list(enabled = FALSE),
      stacking = "normal",
      pointPadding= 0,
      borderWidth= 0,
      groupPadding= 0,
      pointWidth=40)
    ) %>%
    hc_legend(align = "right", verticalAlign = "middle",
              layout = "vertical",
              itemStyle = list(fontSize=13, fontFamily='Arial Narrow', fontWeight="normal")) %>%
    hc_add_theme(hc_theme_smpl())
}


#Dot chart
#need to change NA values to 0

hcDOTc<-function(c_data, colorL, gtitle ) {
  hchart(c_data, "scatter", hcaes(group = 'Group',y= "value", x="variable"))%>%
    hc_colors(colorL)%>%
    hc_legend(itemStyle = list(fontSize=13, 
                               fontFamily='Arial Narrow', 
                               fontWeight="normal")) %>%
    hc_chart(inverted = TRUE) %>%
    hc_xAxis(gridLineWidth=1,
             gridLineColor='#666666',
             title = list(text = ""),
             labels = list(style = list(fontSize=13, 
                                        fontFamily='Arial Narrow', 
                                        fontWeight="normal",
                                        textOverflow='none')),
             tickmarkPlacement="on")%>%
    hc_tooltip(formatter = JS("function(){
                              return this.series.name+ ':<br>' +
                              Highcharts.numberFormat(this.point.value, 2) +'%';
}")) %>%
    hc_title(text = paste(gtitle),
             style = list(fontSize="18px", 
                          fontFamily='Arial Narrow', 
                          fontWeight="normal")) %>%
    hc_yAxis(gridLineWidth= 0,
             pointPadding= 0,
             borderWidth= 0,
             groupPadding= 0,
             labels = list(format = "{value}%",
                           
                           style = list(fontSize=13, 
                                        fontFamily='Arial Narrow', 
                                        fontWeight="normal")),
             title = list(text = ""),
             max=100)%>%
    hc_add_theme(hc_theme_smpl())
  }
#hcDOTc(dataW10, Wcols, titleW10)

hcBARc<-function(c_data, colorL, gtitle ) {
  c_data %>%
    hchart('bar', hcaes(x = 'variable',y= "value", group="Group"))%>%
    hc_colors(colorL) %>%
    hc_title(text = paste(gtitle),
             style = list(fontSize="18px", 
                          fontFamily='Arial Narrow', 
                          fontWeight="normal")) %>%
    hc_yAxis(
      title = list(text = ""),
      gridLineWidth = 0,
      max=100,
      labels = list(format = "{value}%",
                    style = list(fontSize=13, 
                                 fontFamily='Arial Narrow', 
                                 fontWeight="normal"))
    ) %>%
    hc_xAxis(
      lineWidth=0,
      minorTickWidth=0,
      title = list(text = ""),
      labels = list(style = list(fontSize=13, 
                                 fontFamily='Arial Narrow', 
                                 fontWeight="normal",textOverflow='none'))
    ) %>%
    hc_legend(itemStyle = list(fontSize=13, 
                               fontFamily='Arial Narrow', 
                               fontWeight="normal")) %>%
    hc_add_theme(hc_theme_smpl())
}
#hcBARc(dataW10, Wcols, titleW10)
