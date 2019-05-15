source("./functions.R")
source("./functionsDist.R")

function(input, output, session) {
  
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("output", "zip", sep=".")
    },
    
    content <- function(file) {
      file.copy("extdata/WASH_Dashboard_Data.zip", file)
    },
    contentType = "application/zip"
  )

  
  #Demographics - IDPs Tab###########################################################################################
  #..................................Percentage of HHs Hosting IDP...............................................
  output$hcDaF <- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("D.5.", input$DselectD, gcat)
    pvalsD<-Pvals("D.5.", input$DselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Dcols, gcat)
  })
  output$hcDaC <- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("D.5.", input$DselectD, gcat)
    pvalsD<-Pvals("D.5.", input$DselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Dcols, gcat)
  })
  output$hcDaB <- renderHighchart({
    gcat<-"Both"
    
    dvalsD<-catS("D.5.", input$DselectD, gcat)
    pvalsD<-Pvals("D.5.", input$DselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Dcols, gcat)
  })
  #.................................Average Number of Guests........................................
  output$hcDa1F<-renderHighchart({
    gcat<-"Famine"
    Colchart<-melt(numS("D.6.2.", input$DselectD, gcat))

    hcColchart(Colchart, Dcols, gcat)
  })
  output$hcDa1C<-renderHighchart({
    gcat<-"Cholera"
    Colchart<-melt(numS("D.6.2.", input$DselectD, gcat))

    hcColchart(Colchart, Dcols, gcat)
  })
  output$hcDa1B<-renderHighchart({
    gcat<-"Both"
    Colchart<-melt(numS("D.6.2.", input$DselectD, gcat))

    hcColchart(Colchart, Dcols, gcat)
  })
# Demographics - Households Tab######################################################################################
  output$hcDbF<-renderHighchart({
    gcat<-"Famine"
    HHmembers<-melt(numS("D.6.1.", input$DselectD, gcat))
    Children<-melt(numS("D.4.1.", input$DselectD, gcat))
    Disability<-melt(numS("D.4.2.", input$DselectD, gcat))
    Pregnant<-melt(numS("D.4.3.", input$DselectD, gcat))
    Over60<-melt(numS("D.4.4.", input$DselectD, gcat))
    
    bindnum<-cbind(HHmembers, Children[2], Disability[2], Pregnant[2], Over60[2])
    colnames(bindnum)<-c("variable", "Household Members",
                         "Children Under 5","Disabled People",
                         "Pregnant Women","People Over 60 Years of Age")
    bindnum<-melt(bindnum, variable.name = "var")

    bindnum %>%
      hchart('bar', hcaes(x = 'var',y= "value", group="variable" ))%>%
      hc_colors(Dcols) %>%
      hc_yAxis(
        title = list(text = ""),
        gridLineWidth = 0,
        fontFamily = 'Arial',
        labels = list(format = "{value}")
      ) %>%
      hc_subtitle(text = "Please download data for levels of significance")%>%
      hc_title(text = paste("District Prioritization: ",gcat)) %>%
      hc_xAxis(
        lineWidth=0,
        minorTickWidth=0,
        title = list(text = "")
      ) %>%
      hc_add_theme(hc_theme_smpl())
  })
  output$hcDbC<-renderHighchart({
    gcat<-"Cholera"
    HHmembers<-melt(numS("D.6.1.", input$DselectD, gcat))
    Children<-melt(numS("D.4.1.", input$DselectD, gcat))
    Disability<-melt(numS("D.4.2.", input$DselectD, gcat))
    Pregnant<-melt(numS("D.4.3.", input$DselectD, gcat))
    Over60<-melt(numS("D.4.4.", input$DselectD, gcat))
    
    bindnum<-cbind(HHmembers, Children[2], Disability[2], Pregnant[2], Over60[2])
    colnames(bindnum)<-c("variable", "Household Members",
                         "Children Under 5","Disabled People",
                         "Pregnant Women","People Over 60 Years of Age")
    bindnum<-melt(bindnum, variable.name = "var")

    bindnum %>%
      hchart('bar', hcaes(x = 'var',y= "value", group="variable" ))%>%
      hc_colors(Dcols) %>%
      hc_yAxis(
        title = list(text = ""),
        gridLineWidth = 0,
        fontFamily = 'Arial',
        labels = list(format = "{value}")
      ) %>%
      hc_subtitle(text = "Please download data for levels of significance")%>%
      hc_title(text = paste("District Prioritization: ",gcat)) %>%
      hc_xAxis(
        lineWidth=0,
        minorTickWidth=0,
        title = list(text = "")
      ) %>%
      hc_add_theme(hc_theme_smpl())
  })
  output$hcDbB<-renderHighchart({
    gcat<-"Both"
    HHmembers<-melt(numS("D.6.1.", input$DselectD, gcat))
    Children<-melt(numS("D.4.1.", input$DselectD, gcat))
    Disability<-melt(numS("D.4.2.", input$DselectD, gcat))
    Pregnant<-melt(numS("D.4.3.", input$DselectD, gcat))
    Over60<-melt(numS("D.4.4.", input$DselectD, gcat))
    
    bindnum<-cbind(HHmembers, Children[2], Disability[2], Pregnant[2], Over60[2])
    colnames(bindnum)<-c("variable", "Household Members",
                         "Children Under 5","Disabled People",
                         "Pregnant Women","People Over 60 Years of Age")
    bindnum<-melt(bindnum, variable.name = "var")

    bindnum %>%
      hchart('bar', hcaes(x = 'var',y= "value", group="variable" ))%>%
      hc_colors(Dcols) %>%
      hc_title(text = paste("District Prioritization: ",gcat)) %>%
      hc_yAxis(
        title = list(text = ""),
        gridLineWidth = 0,
        fontFamily = 'Arial',
        labels = list(format = "{value}")
      ) %>%
      hc_subtitle(text = "Please download data for levels of significance")%>%
      hc_xAxis(
        lineWidth=0,
        minorTickWidth=0,
        title = list(text = "")
      ) %>%
      hc_add_theme(hc_theme_smpl())
  })
  #Water- Access Tab################################################################################################
  output$hcWa1F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(multicS("W.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.1. ", "(river, damn, lake, pond, stream, canal)")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")

    hcHMchart(hchartd, gcat)
  })
  output$hcWa2F<- renderHighchart ({
    gcat<- "Famine"
    dvalsD<-catS("W.1.1. What", input$WselectD, gcat)
    pvalsD<-Pvals("W.1.1. What", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    

    hcBARchart(pchart, Wcols, gcat)
  })
  output$hcWa3F<- renderHighchart ({
    gcat<- "Famine"
    
    dvalsD<-catS("W.3. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.3. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Wcols, gcat)
  })
  output$hcWa1C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(multicS("W.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.1. ", "(river, damn, lake, pond, stream, canal)")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")

    hcHMchart(hchartd, gcat)
})
  output$hcWa2C<- renderHighchart ({
    gcat<- "Cholera"
    dvalsD<-catS("W.1.1. What", input$WselectD, gcat)
    pvalsD<-Pvals("W.1.1. What", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Wcols, gcat)
  })
  output$hcWa3C<- renderHighchart ({
    gcat<- "Cholera"
    
    dvalsD<-catS("W.3. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.3. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Wcols, gcat)
  })
  output$hcWa1B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(multicS("W.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.1. ", "(river, damn, lake, pond, stream, canal)")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")

    hcHMchart(hchartd, gcat)
})
  output$hcWa2B<- renderHighchart ({
    gcat<- "Both"
    
    dvalsD<-catS("W.1.1. What", input$WselectD, gcat)
    pvalsD<-Pvals("W.1.1. What", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #pchart<-melt(catS("W.1.1. What", input$WselectD, gcat))

    hcBARchart(pchart, Wcols, gcat)
  })
  output$hcWa3B<- renderHighchart ({
    gcat<- "Both"
    
    dvalsD<-catS("W.3. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.3. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Wcols, gcat)
  }) #WATER- Quality tab ###################################################################################################
  output$hcWb1F <- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("W.4. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.4. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWb2F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(multicS("W.4.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.4.1. What was/were the issues?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")

    hcHMchart(hchartd, gcat)
})
  output$hcWb1C <- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("W.4. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.4. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWb2C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(multicS("W.4.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.4.1. What was/were the issues?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")

    hcHMchart(hchartd, gcat)
})
  output$hcWb1B <- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("W.4. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.4. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWb2B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(multicS("W.4.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.4.1. What was/were the issues?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")

    hcHMchart(hchartd, gcat)
}) #WATER- QUANTITY TAB###############################################################################################################
  output$hcWc1F <- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("W.9. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.9. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Wcols, gcat ) 
  })
  
  output$hcWc2F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(multicS("W.9.1.", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.9.1. How did you adjust for the lack of water? (select all that apply but do not prompt answers)/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")

    hcHMchart(hchartd, gcat)
})
  output$hcWc3F<-renderHighchart({
    gcat<-"Famine"
    Containers<-melt(numS("W.6.", input$WselectD, gcat))
    Tanks<-melt(numS("W.8.1.", input$WselectD, gcat))
    
    bindnum<-cbind(Containers, Tanks[2])
    colnames(bindnum)<-c("variable", "Containers", "Tanks")
    bindnum<-melt(bindnum, variable.name = "var")
    
    bindnum %>%
      hchart('bar', hcaes(x = 'var',y= "value", group="variable" ))%>%
      hc_colors(Wcols) %>%
      hc_yAxis(
        title = list(text = ""),
        gridLineWidth = 0,
        fontFamily = 'Arial',
        labels = list(format = "{value}")
      ) %>%
      hc_title(text = gcat) %>%
      hc_xAxis(
        lineWidth=0,
        minorTickWidth=0,
        title = list(text = "")
      ) %>%
      hc_add_theme(hc_theme_smpl())
  })
  output$hcWc1C <- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("W.9. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.9. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    
    hcBARchart(pchart, Wcols, gcat) 
  })
  output$hcWc2C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(multicS("W.9.1.", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.9.1. How did you adjust for the lack of water? (select all that apply but do not prompt answers)/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcWc3C<-renderHighchart({
    gcat<-"Cholera"
    Containers<-melt(numS("W.6.", input$WselectD, gcat))
    Tanks<-melt(numS("W.8.1.", input$WselectD, gcat))
    
    bindnum<-cbind(Containers, Tanks[2])
    colnames(bindnum)<-c("variable", "Containers", "Tanks")
    bindnum<-melt(bindnum, variable.name = "var")
    
    bindnum %>%
      hchart('bar', hcaes(x = 'var',y= "value", group="variable" ))%>%
      hc_colors(Wcols) %>%
      hc_yAxis(
        title = list(text = ""),
        gridLineWidth = 0,
        fontFamily = 'Arial',
        labels = list(format = "{value}")
      ) %>%
      hc_title(text = gcat) %>%
      hc_xAxis(
        lineWidth=0,
        minorTickWidth=0,
        title = list(text = "")
      ) %>%
      hc_add_theme(hc_theme_smpl())
  })
  output$hcWc1B <- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("W.9. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.9. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWc2B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(multicS("W.9.1.", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.9.1. How did you adjust for the lack of water? (select all that apply but do not prompt answers)/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcWc3B<-renderHighchart({
    gcat<-"Both"
    Containers<-melt(numS("W.6.", input$WselectD, gcat))
    Tanks<-melt(numS("W.8.1.", input$WselectD, gcat))
    
    bindnum<-cbind(Containers, Tanks[2])
    colnames(bindnum)<-c("variable", "Containers", "Tanks")
    bindnum<-melt(bindnum, variable.name = "var")
    
    bindnum %>%
      hchart('bar', hcaes(x = 'var',y= "value", group="variable" ))%>%
      hc_colors(Wcols) %>%
      hc_yAxis(
        title = list(text = ""),
        gridLineWidth = 0,
        fontFamily = 'Arial',
        labels = list(format = "{value}")
      ) %>%
      hc_title(text = gcat) %>%
      hc_xAxis(
        lineWidth=0,
        minorTickWidth=0,
        title = list(text = "")
      ) %>%
      hc_add_theme(hc_theme_smpl())
    
  })   # WATER- TREATMENT TAB
  output$hcWd1F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("W.5.Do", input$WselectD, gcat)
    pvalsD<-Pvals("W.5.Do", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.5.Do", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWd2F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(multicS("W.5.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.5.1. How do you treat your drinking water?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcWd3F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(multicS("W.5.2. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.5.2. Reason for not treating water_")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcWd1C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("W.5.Do", input$WselectD, gcat)
    pvalsD<-Pvals("W.5.Do", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.5.Do", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWd2C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(multicS("W.5.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.5.1. How do you treat your drinking water?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcWd3C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(multicS("W.5.2. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.5.2. Reason for not treating water_")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcWd1B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("W.5.Do", input$WselectD, gcat)
    pvalsD<-Pvals("W.5.Do", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.5.Do", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWd2B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(multicS("W.5.1. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.5.1. How do you treat your drinking water?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcWd3B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(multicS("W.5.2. ", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.5.2. Reason for not treating water_")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) #WATER- FETCHING #############################################################################################################################
  output$hcWe1F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("W.10.", input$WselectD, gcat)
    pvalsD<-Pvals("W.10.", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.10.", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWe2F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("W.11. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.11. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.11. ", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWe3F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(multicS("W.11.1.", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.11.1. What are the problems associated with fetching water? (Select all that apply)/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
  })
  output$hcWe1C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("W.11. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.11. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.10.", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWe2C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("W.11. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.11. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.11. ", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWe3C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(multicS("W.11.1.", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.11.1. What are the problems associated with fetching water? (Select all that apply)/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
  })
  output$hcWe1B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("W.10.", input$WselectD, gcat)
    pvalsD<-Pvals("W.10.", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.10.", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWe2B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("W.11. ", input$WselectD, gcat)
    pvalsD<-Pvals("W.11. ", input$WselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("W.11. ", input$WselectD, gcat))
    hcBARchart(pchart, Wcols, gcat ) 
  })
  output$hcWe3B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(multicS("W.11.1.", input$WselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("W.11.1. What are the problems associated with fetching water? (Select all that apply)/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) #HYGIENE- ITEMS ############################################################################################################################3
  output$hcHa1F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.1.W", input$HselectD, gcat)
    pvalsD<-Pvals("S.1.W", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.1.W", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHa2F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(S1Access(input$HselectD,gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("_could not access", "S.1. ")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=FALSE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) 
  output$hcHa3F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(S1Afford(input$HselectD,gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("_could not afford", "S.1. ")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=FALSE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) 
  output$hcHa1C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.1.W", input$HselectD, gcat)
    pvalsD<-Pvals("S.1.W", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    
    #bchart<-melt(catS("S.1.W", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHa2C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(S1Access(input$HselectD,gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("_could not access", "S.1. ")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=FALSE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) 
  output$hcHa3C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(S1Afford(input$HselectD,gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("_could not afford", "S.1. ")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=FALSE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) 
  output$hcHa1B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.1.W", input$HselectD, gcat)
    pvalsD<-Pvals("S.1.W", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.1.W", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHa2B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(S1Access(input$HselectD,gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("_could not access", "S.1. ")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=FALSE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) 
  output$hcHa3B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(S1Afford(input$HselectD,gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("_could not afford", "S.1. ")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=FALSE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) # HYGIENE- ACCESSIBILITY#################################################################################
  output$hcHb1F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.5.", input$HselectD, gcat)
    pvalsD<-Pvals("S.5.", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.5.", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHb2F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.6. ", input$HselectD, gcat)
    pvalsD<-Pvals("S.6. ", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.6. ", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHb3F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.6.1.", input$HselectD, gcat)
    pvalsD<-Pvals("S.6.1.", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.6.1.", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHb1C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.5.", input$HselectD, gcat)
    pvalsD<-Pvals("S.5.", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.5.", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHb2C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.6. ", input$HselectD, gcat)
    pvalsD<-Pvals("S.6. ", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.6. ", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHb3C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.6.1.", input$HselectD, gcat)
    pvalsD<-Pvals("S.6.1.", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.6.1.", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHb1B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.5.", input$HselectD, gcat)
    pvalsD<-Pvals("S.5.", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.5.", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHb2B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.6. ", input$HselectD, gcat)
    pvalsD<-Pvals("S.6. ", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.6. ", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  })
  output$hcHb3B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.6.1.", input$HselectD, gcat)
    pvalsD<-Pvals("S.6.1.", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.6.1.", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  }) #HYGIENE- BEHAVIOR##################################################################################
  output$hcHcF<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(multicS("Please name specific activities before", input$HselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("Please name specific activities before or after which you wash your hands with soap or soap substitute. Specify the focus on activities (example:  before eating), not times of the day (example: in the morning). Select all that apply but DO NOT read out options./")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
  })
  output$hcHcC<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(multicS("Please name specific activities before", input$HselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("Please name specific activities before or after which you wash your hands with soap or soap substitute. Specify the focus on activities (example:  before eating), not times of the day (example: in the morning). Select all that apply but DO NOT read out options./")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcHcB<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(multicS("Please name specific activities before", input$HselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("Please name specific activities before or after which you wash your hands with soap or soap substitute. Specify the focus on activities (example:  before eating), not times of the day (example: in the morning). Select all that apply but DO NOT read out options./")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) #HYGIENE- ASSISTANCE################################################################################3
  output$hcHd1F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.8. Did you", input$HselectD, gcat)
    pvalsD<-Pvals("S.8. Did you", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.8. Did you", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  }) 
  output$hcHd2F<-renderHighchart({
    gcat<- "Famine"
    hchartd<-melt(multicS("S.9. ", input$HselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("S.9. Did you or a member of your household receive any of the following types of WASH assistance in the last six months?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
})
  output$hcHd1C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.8. Did you", input$HselectD, gcat)
    pvalsD<-Pvals("S.8. Did you", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.8. Did you", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  }) 
  output$hcHd2C<-renderHighchart({
    gcat<- "Cholera"
    hchartd<-melt(multicS("S.9. ", input$HselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("S.9. Did you or a member of your household receive any of the following types of WASH assistance in the last six months?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
  })
  output$hcHd1B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.8. Did you", input$HselectD, gcat)
    pvalsD<-Pvals("S.8. Did you", input$HselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.8. Did you", input$HselectD, gcat))
    hcBARchart(pchart, Hcols, gcat ) 
  }) 
  output$hcHd2B<-renderHighchart({
    gcat<- "Both"
    hchartd<-melt(multicS("S.9. ", input$HselectD, gcat))
    colnames(hchartd)<-c("options","variable","value")
    stopwords = c("S.9. Did you or a member of your household receive any of the following types of WASH assistance in the last six months?/")
    hchartd$options <- gsub(paste0(stopwords ,collapse = "|"),"", hchartd$options, fixed=TRUE)
    plotline <- list(
      color = "#006666", value = 19, width = 10, zIndex = 5
    )
    fntltp <- JS("function(){
                 return this.series.yAxis.categories[this.point.y] + ':<br>' +
                 Highcharts.numberFormat(this.point.value, 2) +'%';
  }")
    hcHMchart(hchartd, gcat)
}) #SANITATION- LATRIENE################################################################################
  output$hcSa1F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.2. Do ", input$SselectD, gcat)
    pvalsD<-Pvals("S.2. Do ", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSa2F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.2.1. Is", input$SselectD, gcat)
    pvalsD<-Pvals("S.2.1. Is", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSa3F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.2.2.", input$SselectD, gcat)
    pvalsD<-Pvals("S.2.2.", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSa1C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.2. Do ", input$SselectD, gcat)
    pvalsD<-Pvals("S.2. Do ", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")

    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSa2C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.2.1. Is", input$SselectD, gcat)
    pvalsD<-Pvals("S.2.1. Is", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.2.1. Is", input$SselectD, gcat))
    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSa3C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.2.2.", input$SselectD, gcat)
    pvalsD<-Pvals("S.2.2.", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.2.2.", input$SselectD, gcat))
    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSa1B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.2. Do ", input$SselectD, gcat)
    pvalsD<-Pvals("S.2. Do ", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.2. Do ", input$SselectD, gcat))
    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSa2B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.2.1. Is", input$SselectD, gcat)
    pvalsD<-Pvals("S.2.1. Is", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.2.1. Is", input$SselectD, gcat))
    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSa3B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.2.2.", input$SselectD, gcat)
    pvalsD<-Pvals("S.2.2.", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.2.2.", input$SselectD, gcat))
    hcBARchart(pchart, Scols, gcat ) 
  }) #SANITATION- WASTE DISPOSAL #######################################################################
  output$hcSb1F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.3.", input$SselectD, gcat)
    pvalsD<-Pvals("S.3.", input$SselectD, gcat)
    
    pchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(pchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.3.", input$SselectD, gcat))
    hcBARchart(pchart, Scols, gcat ) 
  })
  output$hcSb2F<- renderHighchart({
    gcat<-"Famine"
    dvalsD<-catS("S.4. What is", input$SselectD, gcat)
    pvalsD<-Pvals("S.4. What is", input$SselectD, gcat)
    
    bchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(bchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.4. What is", input$SselectD, gcat))
    hcBARchart(bchart, Scols, gcat ) 
  })
  output$hcSb1C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.3.", input$SselectD, gcat)
    pvalsD<-Pvals("S.3.", input$SselectD, gcat)
    
    bchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(bchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.3.", input$SselectD, gcat))
    hcBARchart(bchart, Scols, gcat ) 
  })
  output$hcSb2C<- renderHighchart({
    gcat<-"Cholera"
    dvalsD<-catS("S.4. What is", input$SselectD, gcat)
    pvalsD<-Pvals("S.4. What is", input$SselectD, gcat)
    
    bchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(bchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.4. What is", input$SselectD, gcat))
    hcBARchart(bchart, Scols, gcat ) 
  })
  output$hcSb1B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.3.", input$SselectD, gcat)
    pvalsD<-Pvals("S.3.", input$SselectD, gcat)
    
    bchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(bchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.3.", input$SselectD, gcat))
    hcBARchart(bchart, Scols, gcat ) 
  })
  output$hcSb2B<- renderHighchart({
    gcat<-"Both"
    dvalsD<-catS("S.4. What is", input$SselectD, gcat)
    pvalsD<-Pvals("S.4. What is", input$SselectD, gcat)
    
    bchart<-melt(merge(dvalsD,pvalsD[,c(-1,-3)],by="var")[,-1])
    colnames(bchart)<-c("var","variable","value")
    #bchart<-melt(catS("S.4. What is", input$SselectD, gcat))
    hcBARchart(bchart, Scols, gcat ) 
  })
  #DISTRICT LEVEL ANALYSIS#############################################################################################################################
  
  #MAP
  output$Map <- renderLeaflet({
    leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(48.5, 15.75, zoom = 6)%>%
      
      
      addPolygons(data = Gov,    # Governorates
                  # label = oblasts$adm1NameLa,
                  options = pathOptions(clickable = FALSE),
                  
                  
                  color = "#333333",
                  weight= 0.6,
                  fill=FALSE,
                  fillOpacity = 0.8,
                  opacity = 0.9 ) %>%
      
      addPolygons(data= Dist,    # Districts
                  color = "#009999",
                  weight = 1.5,
                  opacity = 1,
                  smoothFactor = 0.5,
                  fill = TRUE,
                  fillColor = "#009999",
                  fillOpacity = 0.1,
                  layerId = ~admin2name,
                  highlightOptions = highlightOptions(color = "#e36159", weight = 3,
                                                      bringToFront = TRUE, sendToBack = FALSE),
                  label = Dist$admin2name,
                  labelOptions= labelOptions(
                    interactive= FALSE,
                    permanent= FALSE,
                    opacity= 1,
                    textOnly = TRUE,
                    style = list(
                      "color"= "#333333",
                      "font-weight" = "normal",
                      "font-size" = "15px",
                      "font-family"= "Calibri")
                  )
      ) 
    
  })
  observeEvent(input$Map_shape_click, { #When shape is clicked add highlight poly and remove any exsisting
    p <- input$Map_shape_click
    if(p$id=="Selected"){
      leafletProxy("Map") %>% removeShape(layerId="Selected")
    } else {
      leafletProxy("Map")  %>% addPolygons(data=(subset(Dist, admin2name==p$id)), 
                                           fillColor="#e36159", fillOpacity=1, opacity=1, 
                                           stroke=FALSE, layerId="Selected",
                                           color = "#e36159",
                                           weight = 3.5)
    }
  })
  observeEvent(input$Map_shape_click, {
    p <- input$Map_shape_click
    if(!is.null(p$id)){
      if(is.null(input$Distsel)) updateSelectInput(session, "Distsel", selected=p$id)
      if(!is.null(input$Distsel) && input$Distsel!=p$id) updateSelectInput(session, "Distsel", selected=p$id)
    }
  })
  observeEvent(input$Distsel, {
    p <- input$Map_shape_click
    p2 <- subset(Dist, admin2name==input$Distsel)
    if(nrow(p2)==0){
      leafletProxy("Map") %>% removeMarker(layerId="Selected")
    } else if(is.null(p$id) || input$Distsel!=p$id){
      leafletProxy("Map")  %>% addPolygons(data=p2, fillColor="#e36159", fillOpacity=1, opacity=1, 
                                           stroke=FALSE, layerId="Selected",
                                           color = "#e36159",
                                           weight = 3.5)
    }
  })
  #WATER TAB################
  output$W1<- renderHighchart({
    titleW1<-"What water source did your household have access to in the last 30 days?‡"
    dataW1<-melt(variable.name= "Group", DistDATAm("W.1. ",input$DistselectD,input$Distsel))
    dataW1[is.na(dataW1)]<-0
    hcDOTc(dataW1,Wcols, titleW1)
  })
  output$W11<- renderHighchart({
    titleW11<-"What water source did your household use the most for drinking in the last 30 days?"
    dataW11<-melt(variable.name= "Group",DistDATA1("W.1.1. What", input$DistselectD, input$Distsel))
    stopwords1 = c(" (river, damn, lake, pond, stream, canal)")
    dataW11$variable <- gsub(paste0(stopwords1 ,collapse = "|"),"", dataW11$variable, fixed=TRUE)
    dataW11[is.na(dataW11)]<-0
    hcDOTc(dataW11,Wcols, titleW11)
  })
  output$W3<- renderHighchart({
    titleW3<-"What water source did your household use the most in the last 30 days for other purposes such as washing and cooking?"
    dataW3<-melt(variable.name= "Group",DistDATA1("W.3. ", input$DistselectD, input$Distsel))
    dataW3[is.na(dataW3)]<-0
    hcDOTc(dataW3,Wcols, titleW3)
  })
  
  output$W4<- renderHighchart({
    titleW4<-"Did you have any issue relating to taste, appearance or smell of your water sources \nin the last 30 days?"
    dataW4<-melt(variable.name= "Group",DistDATA1("W.4. ", input$DistselectD, input$Distsel))
    dataW4<-dataW4[grepl("Yes",dataW4$variable),]
    dataW4[is.na(dataW4)]<-0
    dataW4$value<-round(dataW4$value,2)
    hcBARc(dataW4,Wcols, titleW4)
  })
  output$W5<- renderHighchart({
    titleW5<-"Do you use any methods to treat your drinking water for better quality?"
    dataW5<-melt(variable.name= "Group",DistDATA1("W.5.Do", input$DistselectD, input$Distsel))
    dataW5<-dataW5[grepl("Yes",dataW5$variable),]
    dataW5[is.na(dataW5)]<-0
    dataW5$value<-round(dataW5$value,2)
    hcBARc(dataW5,Wcols, titleW5)
  })
  
  output$W52<- renderHighchart({
    titleW52<-"Why don't you treat your drinking water?‡"
    dataW52<-melt(variable.name= "Group",DistDATAm("Reason for not treating water", input$DistselectD, input$Distsel))
    stopwords1 = c("W.5.2. _")
    dataW52$variable <- gsub(paste0(stopwords1 ,collapse = "|"),"", dataW52$variable, fixed=TRUE)
    dataW52[is.na(dataW52)]<-0
    dataW52$value<-round(dataW52$value,2)
    hcBARc(dataW52,Wcols, titleW52)
  })
  output$W8<- renderHighchart({
    titleW8<-"In this household, do you have water tanks to store water?"
    dataW8<-melt(variable.name= "Group",DistDATA1("W.8. ", input$DistselectD, input$Distsel))
    dataW8<-dataW8[grepl("Yes",dataW8$variable),]
    dataW8[is.na(dataW8)]<-0
    dataW8$value<-round(dataW8$value,2)
    hcBARc(dataW8,Wcols, titleW8)
  })
  output$W9<- renderHighchart({
    titleW9<-"Did you have enough water in the last 30 days to meet your household needs?"
    dataW9<-melt(variable.name= "Group",DistDATA1("W.9. ", input$DistselectD, input$Distsel))
    dataW9<-dataW9[grepl("Yes",dataW9$variable),]
    dataW9[is.na(dataW9)]<-0
    dataW9$value<-round(dataW9$value,2)
    hcBARc(dataW9,Wcols, titleW9)
  })
  output$W41<- renderHighchart({
    titleW41<-"What was/were the issues?‡"
    dataW41<-melt(variable.name= "Group",DistDATAm("W.4.1. ", input$DistselectD, input$Distsel))
    stopwords2 = c("What was/were the issues?/")
    dataW41$variable <- gsub(paste0(stopwords2 ,collapse = "|"),"", dataW41$variable, fixed=TRUE)
    dataW41[is.na(dataW41)]<-0
    dataW41$value<-round(dataW41$value,2)
    hcBARc(dataW41,Wcols, titleW41)
  })
  output$W51<- renderHighchart({
    titleW51<-"How do you treat your drinking water?‡"
    dataW51<-melt(variable.name= "Group",DistDATAm("W.5.1.", input$DistselectD, input$Distsel))
    stopwords = c("How do you treat your drinking water?/")
    dataW51$variable <- gsub(paste0(stopwords ,collapse = "|"),"", dataW51$variable , fixed=TRUE)
    dataW51[is.na(dataW51)]<-0
    dataW51$value<-round(dataW51$value,2)
    hcBARc(dataW51,Wcols, titleW51)
  })
  output$W111<- renderHighchart({
    titleW111<-"What are the problems associated with fetching water?‡"
    dataW111<-melt(variable.name= "Group",DistDATAm("W.11.1.", input$DistselectD, input$Distsel))
    stopwords = c("What are the problems associated with fetching water? (Select all that apply)/")
    dataW111$variable <- gsub(paste0(stopwords ,collapse = "|"),"", dataW111$variable , fixed=TRUE)
    dataW111[is.na(dataW111)]<-0
    dataW111$value<-round(dataW111$value,2)
    hcBARc(dataW111,Wcols, titleW111)
  })
  output$W91<- renderHighchart({
    titleW91<-"How did you adjust for the lack of water?‡"
    dataW91<-melt(variable.name= "Group",DistDATAm("W.9.1.", input$DistselectD, input$Distsel))
    stopwords = c("How did you adjust for the lack of water? (select all that apply but do not prompt answers)/")
    dataW91$variable <- gsub(paste0(stopwords ,collapse = "|"),"", dataW91$variable, fixed=TRUE)
    dataW91[is.na(dataW91)]<-0
    hcDOTc(dataW91,Wcols, titleW91)
  })
  
  output$W10<- renderHighchart({
    titleW10<-"How long does it take to go on foot to your main water point, fetch water, and return (at peak time)?"
    dataW10<-melt(variable.name= "Group",DistDATA1("W.10.", input$DistselectD, input$Distsel))
    dataW10[is.na(dataW10)]<-0
    hcSTACKEDc(dataW10,Wcols, titleW10)
  })
  output$W110<- renderHighchart({
    titleW11a<-"Do you feel the activity of fetching water (distance and queuing time) constitutes a problem for your household?"
    dataW11a<-melt(variable.name= "Group",DistDATA1("W.11. ", input$DistselectD, input$Distsel))
    dataW11a[is.na(dataW11a)]<-0
    hcSTACKEDc(dataW11a,Wcols, titleW11a)
  })
  
  # })# HYGIENE TAB IN DISTRICT TAB########################################################################
  
  output$H1<- renderHighchart({
    titleH1<-"Was there one/multiple hygiene item(s) you needed to procure in the last 30 days but could not find or afford?‡"
    dataH1<-melt(variable.name= "Group",DistDATA1("S.1.W", input$DistselectD, input$Distsel))
    dataH1<-dataH1[grepl("Yes",dataH1$variable),]
    dataH1[is.na(dataH1)]<-0
    dataH1$value<-round(dataH1$value,2)
    hcBARc(dataH1,Hcols, titleH1)
  })
  output$H7<- renderHighchart({
    titleH7<-"Please name specific activities before or after which you wash your hands with soap or soap substitute.‡"
    dataH7<-melt(variable.name= "Group",DistDATAm("Please name specific activities before", input$DistselectD, input$Distsel))
    stopwords1 = c("or after which you wash your hands with soap or soap substitute. Specify the focus on activities (example:  before eating), not times of the day (example: in the morning). Select all that apply but DO NOT read out options./")
    dataH7$variable <- gsub(paste0(stopwords1 ,collapse = "|"),"", dataH7$variable, fixed=TRUE)
    dataH7[is.na(dataH7)]<-0
    dataH7$value<-round(dataH7$value,2)
    hcBARc(dataH7,Hcols, titleH7)
  })
  output$H8<- renderHighchart({
    titleH8<-"Did you or a member of your household receive hygiene promotion messaging or training in the last year?"
    dataH8<-melt(variable.name= "Group",DistDATA1("S.8. Did you", input$DistselectD, input$Distsel))
    dataH8<-dataH8[grepl("Yes",dataH8$variable),]
    dataH8[is.na(dataH8)]<-0
    dataH8$value<-round(dataH8$value,2)
    hcBARc(dataH8,Hcols, titleH8)
  })
  output$H9<- renderHighchart({
    titleH9<-"Did you or a member of your household receive any of the following types of WASH assistance in the last six months?‡"
    dataH9<-melt(variable.name= "Group",DistDATAm("S.9. ", input$DistselectD, input$Distsel))
    stopwords2 = c("Did you or a member of your household receive any of the following types of WASH assistance in the last six months?/")
    dataH9$variable <- gsub(paste0(stopwords2 ,collapse = "|"),"", dataH9$variable, fixed=TRUE)
    dataH9[is.na(dataH9)]<-0
    dataH9$value<-round(dataH9$value,2)
    hcBARc(dataH9,Hcols, titleH9)
  })
  output$H11<- renderHighchart({
    titleH11<-"Unable to Access"
    dataH11<-melt(variable.name= "Group",S1AccessD(input$DistselectD, input$Distsel))
    stopwords1 = c("_could not access", "S.1. ")
    dataH11$variable <- gsub(paste0(stopwords1 ,collapse = "|"),"", dataH11$variable, fixed=FALSE)
    dataH11[is.na(dataH11)]<-0
    hcDOTc(dataH11,Hcols, titleH11)
  })
  output$H12<- renderHighchart({
    titleH12<-"Unable to Afford"
    dataH12<-melt(variable.name= "Group",S1AffordD(input$DistselectD, input$Distsel))
    stopwords2 = c("_could not afford", "S.1. ")
    dataH12$variable <- gsub(paste0(stopwords2 ,collapse = "|"),"", dataH12$variable, fixed=FALSE)
    dataH12[is.na(dataH12)]<-0
    hcDOTc(dataH12,Hcols, titleH12)
  })
  output$H61<- renderHighchart({
    titleH61<-"Why don't you have soap?"
    dataH61<-melt(variable.name= "Group",DistDATA1("S.6.1.", input$DistselectD, input$Distsel))
    dataH61[is.na(dataH61)]<-0
    hcDOTc(dataH61,Hcols, titleH61)
  })
  output$H5<- renderHighchart({
    titleH5<-"Do you have handwashing facilities in your household?"
    dataH5<-melt(variable.name= "Group",DistDATA1("S.5.", input$DistselectD, input$Distsel))
    dataH5[is.na(dataH5)]<-0
    hcSTACKEDc(dataH5,Hcols, titleH5)
  })
  output$H6<- renderHighchart({
    titleH6<-"Do you have soap in your household and use it?"
    dataH6<-melt(variable.name= "Group",DistDATA1("S.6. ", input$DistselectD, input$Distsel))
    dataH6[is.na(dataH6)]<-0
    hcSTACKEDc(dataH6,Hcols, titleH6)
  })

  
  output$S2<- renderHighchart({
    titleS2<-"Do your household members have access to a functioning latrine?"
    dataS2<-melt(variable.name= "Group",DistDATA1("S.2. Do ", input$DistselectD, input$Distsel))
    dataS2[is.na(dataS2)]<-0
    hcSTACKEDc(dataS2,Scols, titleS2)
  })
  output$S3<- renderHighchart({
    titleS3<-"Was there visible wastewater in the vicinity (30 meters or less) of your house in the last 30 days?"
    dataS3<-melt(variable.name= "Group",DistDATA1("S.3.", input$DistselectD, input$Distsel))
    stopwords = c("in the vicinity of my household (1-2 times per month)")
    dataS3$variable <- gsub(paste0(stopwords ,collapse = "|"),"", dataS3$variable , fixed=TRUE)
    dataS3[is.na(dataS3)]<-0
    hcSTACKEDc(dataS3,Scols, titleS3)
  })  
  output$S4<- renderHighchart({
    titleS4<-"What is the most common way your household  disposed of garbage in the last 30 days?"
    dataS4<-melt(variable.name= "Group",DistDATA1("S.4. What is", input$DistselectD, input$Distsel))
    dataS4[is.na(dataS4)]<-0
    hcSTACKEDc(dataS4,Scols, titleS4)
  })
  output$S22<- renderHighchart({
    titleS22<-"What type of latrine do your household members have access to?"
    dataS22<-melt(variable.name= "Group",DistDATA1("S.2.2.", input$DistselectD, input$Distsel))
    dataS22[is.na(dataS22)]<-0
    dataS22$value<-round(dataS22$value,2)
    hcBARc(dataS22,Scols, titleS22)
  })
  output$S21<- renderHighchart({
    titleS21<-"Is your latrine shared with people other than your household members?"
    dataS21<-melt(variable.name= "Group",DistDATA1("S.2.1. Is", input$DistselectD, input$Distsel))
    dataS21<-dataS21[grepl("Yes",dataS21$variable),]
    dataS21[is.na(dataS21)]<-0
    dataS21$value<-round(dataS21$value,2)
    hcBARc(dataS21,Scols, titleS21)
  })
  
  category<-reactive({
    if (input$Distsel=="Abs"|input$Distsel=="Al Abr"|input$Distsel=="Al Hashwah"|input$Distsel=="Al Maqatirah"|input$Distsel=="Al Qabbaytah"|input$Distsel=="Ash Shamayatayn"|
        input$Distsel=="At Ta'iziyah"|input$Distsel=="Ataq"|input$Distsel=="Bani Dhabyan"|input$Distsel=="Bani Sa'd"|
        input$Distsel=="Bani Suraim"|input$Distsel=="Dhi Bin"|input$Distsel=="Dimnat Khadir"|input$Distsel=="Khanfir"|input$Distsel=="Majz"|
        input$Distsel=="Mudhaykhirah"|input$Distsel=="Shara'b As Salam"|input$Distsel=="Tuban"|input$Distsel=="Zingibar") {
      return("famine")
      
    } else if(input$Distsel=="Al Jafariyah"|input$Distsel=="Al Maslub"|input$Distsel=="Al Maton"|input$Distsel=="Al Miftah"|input$Distsel=="Arhab"|
              input$Distsel=="Az Zahir"|input$Distsel=="Iyal Surayh"|input$Distsel=="Kharif"|input$Distsel=="Kushar"|input$Distsel=="Marib"|input$Distsel=="Marib City"|
              input$Distsel=="Qarah"|input$Distsel=="Wald Rabi'"|input$Distsel== "Washhah"){
      return("cholera")
    } else if (input$Distsel=="Aslem"|input$Distsel=="Khamir"|input$Distsel=="Kitaf wa Al Boqe'e"|input$Distsel=="Manakhah"|input$Distsel== "Mustaba" ){
      return("both cholera and famine")
    }
  })
  output$priori <- renderText({ 
    paste(input$Distsel, "is prioritized for<B>", category(), "</B>by the Yemen WASH Cluster")
  })
  #map at the top of page ############################################################################################################
  output$Map2 <- renderLeaflet({
    leaflet() %>%
      leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(46.5, 15.75, zoom = 6)%>%
      #Create map title
      addControl(tags$div(HTML('
		<p style="text-align: center;
    padding-left: 3px; 
    padding-right: 10px;
font-family: Arial Narrow;
color:#333333;
font-size: 15px;
    font-weight: bold;">Prioritization of Assessed Districts</p>')), position = "topleft",
className = "fieldset {
    border: 0;
}")%>%
      
      addPolygons(data = Gov,    # Governorates
                  # label = oblasts$adm1NameLa,
                  options = pathOptions(clickable = FALSE),
                  
                  color = "#333333",
                  weight= 0.6,
                  fill=FALSE,
                  fillOpacity = 0.1,
                  opacity = 0.4 ) %>%
      addPolygons(data= Dist2,    # Districts
                  color = "#333333",
                  weight = 0.5,
                  opacity = 1,
                  smoothFactor = 0.5,
                  fill = TRUE,
                  fillColor = ~factpal(Prio),
                  fillOpacity = 0.9,
                  layerId = ~admin2name,
                  highlightOptions = highlightOptions(color = "#e36159", weight = 3,
                                                      bringToFront = TRUE, sendToBack = FALSE),
                  label = paste0(Dist2$admin2name, ":  ", Dist2$Prio),
                  labelOptions= labelOptions(
                    interactive= FALSE,
                    permanent= FALSE,
                    opacity= 1,
                    textOnly = TRUE,
                    style = list(
                      "color"= "#333333",
                      "font-weight" = "normal",
                      "font-size" = "15px",
                      "font-family"= "Calibri")
                  )) 
  })
}