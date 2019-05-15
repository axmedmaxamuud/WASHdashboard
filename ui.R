
fluidPage(tags$head(includeCSS("www/style.css")), 
          tags$head(includeHTML(("google-analytics.html"))),
          wellPanel(style = "background: #CCEAEA;",
                    titlePanel(title = fluidRow(column(6,
                                                       div(h1("Yemen WASH Cluster Assessment", align = "left", style = "color:#009999;
                        font-size: 100; font-family: 'Arial Narrow'; font-weight:700;
                                              "))),
                                                column(6,div(img(src="WASHtransp.png",height="70px"), align = "right"))
                                                
                    ),
                    windowTitle = "Yemen WASH 2018"),
                    fluidRow(column(7,
                                    fluidRow(column(12, h4("Yemen is facing one of the world’s worst Water, Sanitation and Hygiene (WASH) 
                         crises, as deteriorating WASH infrastructure contributes to a cholera outbreak, 
                         and represents one of the underlying causes of malnutrition in the country. ", tags$sup(1), style = "color:#333333;
                        font-size: 70; font-family: 'Arial Narrow'; font-weight:200;
                         line-height: 1.1;"))),
                                    fluidRow(column(12, h4("On behalf of the Yemen WASH Cluster, REACH coordinated a household-level assessment
                         to provide an understanding of WASH needs, gaps, and priorities in 38 districts 
                         prioritized for famine and/or cholera interventions that also host a high 
                         concentration of Internally Displaced People (IDPs - 8% or more of the total 
                         district population). The prioritization of districts was done using 2017 data for 2018 intervention.",
                                                           tags$sup(2,3), style = "color:#333333;
                        font-size: 70; font-family: 'Arial Narrow'; font-weight:200;
                         line-height: 1.1;"))),
                                    fluidRow(column(6, h5("Findings  are  based  on  data  collection  conducted  from  4  September  to  
                        28  November  2018.  Following  a  two-stage  random  sampling  approach,  
                        representative  samples  of  host  community  and  IDP  populations  were  collected  
                        in  randomly-selected  locations  in  the 38 selected districts.  
                        Findings  are  representative at district level and population group(IDP/Host Community) with a 95%
                        confidence level and a 10% margin of error.", tags$sup("*"),
                                                          style = "color:#333333;
                        font-size: 70; font-family: 'Arial Narrow'; font-weight:70;
                        line-height: 1.1;"),
                                                    h5("Partners:",
                                                       style = "color:#333333;
                                                       font-size: 75; font-family: 'Arial Narrow'; font-weight:100;
                                                       line-height: 1.1;")),
                                             column(6, h5("This dashboard provides an overview of the assessment data by indicator, disaggregated either by IDPs 
                        and host community households, or by head of household gender. District-level analysis facilitates the exploration
                        of assessment results by district.", tags$sup(4), style = "color:#333333;
                        font-size: 70; font-family: 'Arial Narrow'; font-weight:70;
                        line-height: 1.1;"), 
                                                    fluidRow(column(8,downloadButton('downloadData', 'DATA DOWNLOAD',class="btn-block", 
                                                                                     style="color: #009999; border-color: #009999;")),
                                                             
                                                             column(2,actionButton("header_sources", "", icon= icon("info-circle"),class="btn-block", 
                                                                                   style="color: #009999; background-color: transparent; border-color: transparent;"))
                                                    )
                                             )
                                    ),
                                    bsModal("modal_loc", h5("Sources",style = "color:#333333;
                        font-size: 70; font-family: 'Arial Narrow'; font-weight:700;"), "header_sources", size="large",
                                            HTML('
		<p style="text-align:justify">1. According to the Humanitarian Needs Overview (HNO) 2018, half of all malnutrition cases in the country were associated with WASH-related infections.</p>
		
		<p style="text-align:justify">2. International Organization for Migration (IOM) <a href="https://mali.iom.int/reports/displacement-tracking-matrix" target="_blank">Displacement Tracking Matrix (DTM) Report</a>, April/May 2018.</p>
		
		<p style="text-align:justify">3. For the purpose of this assessment, IDP households also include households who have been displaced because of the conflict that started in 2015 but have now returned to their place of habitual residence as of the day of data collection (returnees) </p>
		
		<p style="text-align:justify">4. Terms of Reference (ToR) for the Yemen WASH Cluster Assessment can be found <a href="http://www.reachresourcecentre.info/system/files/resource-documents/ymn2018_wash_assessment_tor.pdf" target="_blank">here</a>. 
Dataset can be found <a href="https://bit.ly/2BsAIDz" target="_blank">here</a>. </p>

		<p style="text-align:justify">* Findings concerning IDPs for Bani Dhabyan, Khamir, Iyal Surayh, Kharif and Khanfir districts are representative at district level with a 90% confidence level and a 10% margin of error  </p>
		
		<p style="text-align:justify">
		To find the static district-level factsheets and other REACH Yemen resources please visit the <a href="http://www.reachresourcecentre.info/countries/yemen" target="_blank">Resource Centre</a>.</p>'
                                            ))
                    ),
                    column(5, leafletOutput("Map2", height=310), 
                           fluidRow(column(3, actionButton("famine_info", "Famine", icon= icon("circle"),class="btn-block", 
                                                           style="color: #FFF; background-color: transparent; border-color: transparent;font-family: 'Arial Narrow'; font-weight:900;")),
                                    
                                    column(3,actionButton("cholera_info", "Cholera", icon= icon("circle"),class="btn-block", 
                                                          style="color: #666666; background-color: transparent; border-color: transparent;font-family: 'Arial Narrow'; font-weight:900;")),
                                    column(6,actionButton("both_info", "Both Cholera and Famine", icon= icon("circle"),class="btn-block", 
                                                          style="color: #009999; background-color: transparent; border-color: transparent;font-family: 'Arial Narrow'; font-weight:900;"))))
                    
                    ),
          
                    fluidRow(
                      column(7, div(img(src="group2.png",height="49px"), align = "left")))
                    
                    
                    
                    ),
          bsModal("modal_loc1", h5("Districts prioritized for famine intervention:",style = "color:#333333;
                        font-size: 70; font-family: 'Arial Narrow'; font-weight:700;"), "famine_info", size="small",
                  HTML('	<p style="text-align:left">
Abs, Al Abr, Al Hashwah, Al Maqatirah, Al Qabbaytah, Ash Shamayatayn, At Taiziyah, Ataq, Bani Dhabyan, Bani Sad, Bani Suraim, Dhi Bin,
 Dimnat Khadir, Khanfir, Majz, Mudhaykhirah, Sharab As Salam, Tuban, Zingibar
</p>'
                  )),
          bsModal("modal_loc2", h5("Districts prioritized for cholera intervention:",style = "color:#333333;
                        font-size: 70; font-family: 'Arial Narrow'; font-weight:700;"), "cholera_info", size="small",
                  HTML('	<p style="text-align:left">
Al Jafariyah, Al Maslub, Al Maton, Al Miftah, Arhab, Az Zahir-Al Jawf, Iyal Surayh, Kharif, Kushar, Marib, Marib City, Qarah, Wald Rabi, Washhah
</p>'
                  )),
          bsModal("modal_loc3", h5("Districts prioritized for both famine and cholera intervention:",style = "color:#333333;
                        font-size: 70; font-family: 'Arial Narrow'; font-weight:700;"), "both_info", size="small",
                  HTML('	<p style="text-align:left">
Aslem, Khamir, Kitaf wa Al Boqee, Manakhah, Mustaba
</p>'
                  )),
          navbarPage(title= tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><ahref=\"http://www.reach-initiative.org/\"><img src=\"REACH OFFICIAL Logo.jpg\" alt=\"alt\" style=\"float:right;height:48px;padding-right:0px;\"> </a>`</div>');
                                     console.log(header)")
          ),
          
          tabPanel(strong(HTML("<span style='font-size:18px;'>Demographics</span>")),
                   icon= icon("chart-bar"), #DEMOGRAPHICS #################################################################################################
                   fluidRow(column(3,
                                   radioButtons("DselectD", label= div(style = "font-size:18px; font-weight:900;color:#666666;
                         line-height: 0.1;","Disaggregate by: "),
                                                choices=list("HC v.s. IDP", "Female v.s. Male"),
                                                selected= "HC v.s. IDP", inline = TRUE))
                   ),
                   tabsetPanel(
                     tabPanel(h5("Hosting", align = "left", style = "color:#383f48;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"), 
                              fluidRow(column(5, h4("Percentage of HHs Hosting IDPs",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4(" Average Number of Guests",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;"))),
                              fluidRow(column(4,highchartOutput("hcDaF", height= 220)), #FAMINE
                                       column(4,highchartOutput("hcDa1F", height= 190))),
                              fluidRow(column(4,highchartOutput("hcDaC", height= 220)), #CHOLERA
                                       column(4,highchartOutput("hcDa1C", height= 190))),
                              fluidRow(column(4,highchartOutput("hcDaB", height= 220)), #BOTH
                                       column(4,highchartOutput("hcDa1B", height=190))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))),
                     
                     tabPanel(h5("Households", align = "left", style = "color:#383f48;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"), 
                              fluidRow( column(12, h4("How many people currently in the following categories live in your household?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                              line-height: 0.5;"))),
                              fluidRow(column(6, highchartOutput("hcDbF", height= 290))),
                              fluidRow(column(6, highchartOutput("hcDbC", height=290))),
                              fluidRow(column(6, highchartOutput("hcDbB", height=290))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ))
          ),
          bsTooltip("DselectD", "HC v.s. IDP: comparing Host Community and Internally Displaced Populations. Female v.s. Male: comparing Female headed households to Male", "top"),
          tabPanel(strong(HTML("<span style='font-size:18px'>Water</span>")),
                   icon= icon("tint"), #WATER ##############################################################################################################
                   fluidRow(column(3,
                                   radioButtons("WselectD", label= div(style = "font-size:18px; font-weight:900;color:#666666;
                         line-height: 0.1;","Disaggregate by: "),
                                                choices=list("HC v.s. IDP", "Female v.s. Male"),
                                                selected= "HC v.s. IDP", inline = TRUE))
                   ),
                   tabsetPanel(
                     tabPanel(h5("Access", align = "left", style = "color:#3399CC;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(4, h4("What water source did your household have access to in the last 30 days? (select all that apply)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("What water source did your household use the most for drinking in the last 30 days? (select one)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("What water source did your household use the most in the last 30 days for other purposes such as washing and cooking? (select one)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;"))),
                              fluidRow(column(4, highchartOutput("hcWa1F", height=375)), #FAMINE
                                       column(4, highchartOutput("hcWa2F",height=375)),
                                       column(4, highchartOutput("hcWa3F", height=375))),
                              fluidRow(column(4, highchartOutput("hcWa1C", height=375)), #CHOLERA
                                       column(4, highchartOutput("hcWa2C",height=375)),
                                       column(4, highchartOutput("hcWa3C", height=375))),
                              fluidRow(column(4, highchartOutput("hcWa1B", height=375)), #BOTH
                                       column(4, highchartOutput("hcWa2B",height=375)),
                                       column(4, highchartOutput("hcWa3B", height=375))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ),
                     tabPanel(h5("Quality", align = "left", style = "color:#3399CC;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"), 
                              fluidRow( column(3, h4("Did you have any issue relating to taste, appearance or smell of your water sources in the last 30 days?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                              line-height: 1.1;")),
                                        column(3, h4("What was/were the issues?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                             line-height: 1.1;"))),
                              fluidRow(column(3, highchartOutput("hcWb1F", height= 250)), #FAMINE
                                       column(5, highchartOutput("hcWb2F", height=260))),
                              fluidRow(column(3, highchartOutput("hcWb1C", height=250)), #CHOLERA
                                       column(5, highchartOutput("hcWb2C", height=260))),
                              fluidRow(column(3, highchartOutput("hcWb1B", height=250)), #BOTH
                                       column(5, highchartOutput("hcWb2B", height=260))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ),
                     tabPanel(h5("Quantity", align = "left", style = "color:#3399CC;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(3, h4("Did you have enough water in the last 30 days to meet your household needs (i.e. for drinking, cooking and washing)?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                              line-height: 1.1;")),
                                       column(5, h4("How did you adjust for the lack of water? (select all that apply)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(3, h4("How many water tanks/containers do you have?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;"))),
                              fluidRow(column(3, highchartOutput("hcWc1F", height=270)),
                                       column(5, highchartOutput("hcWc2F", height=300)),
                                       column(3, highchartOutput("hcWc3F", height=270))),
                              fluidRow(column(3, highchartOutput("hcWc1C", height=270)),
                                       column(5, highchartOutput("hcWc2C", height=300)),
                                       column(3, highchartOutput("hcWc3C", height=270))),
                              fluidRow(column(3, highchartOutput("hcWc1B", height=270)),
                                       column(5, highchartOutput("hcWc2B", height=300)),
                                       column(3, highchartOutput("hcWc3B", height=270))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ),
                     tabPanel(h5("Treatment", align = "left", style = "color:#3399CC;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(3, h4("Do you use any methods to treat your drinking water for better quality?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("How do you treat your drinking water?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("Why don't you treat your drinking water?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;"))),
                              fluidRow(column(3, highchartOutput("hcWd1F", height=270)),
                                       column(4, highchartOutput("hcWd2F", height=300)),
                                       column(4, highchartOutput("hcWd3F", height=300))),
                              fluidRow(column(3, highchartOutput("hcWd1C", height=270)),
                                       column(4, highchartOutput("hcWd2C", height=300)),
                                       column(4, highchartOutput("hcWd3C", height=300))),
                              fluidRow(column(3, highchartOutput("hcWd1B", height=270)),
                                       column(4, highchartOutput("hcWd2B", height=300)),
                                       column(4, highchartOutput("hcWd3B", height=300))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ),
                     tabPanel(h5("Fetching", align = "left", style = "color:#3399CC;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(3, h4("How long does it take to go on foot to your main water point, fetch water, and return (at peak time)? ",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(3, h4("Do you feel the activity of fetching water (distance and queuing time) constitutes a problem for your household?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("What are the problems associated with fetching water? (Select all that apply)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;"))),
                              fluidRow(column(3, highchartOutput("hcWe1F", height=300)),
                                       column(3, highchartOutput("hcWe2F", height=300)),
                                       column(4, highchartOutput("hcWe3F", height=300))),
                              fluidRow(column(3, highchartOutput("hcWe1C", height=300)),
                                       column(3, highchartOutput("hcWe2C", height=300)),
                                       column(4, highchartOutput("hcWe3C", height=300))),
                              fluidRow(column(3, highchartOutput("hcWe1B", height=300)),
                                       column(3, highchartOutput("hcWe2B", height=300)),
                                       column(4, highchartOutput("hcWe3B", height=300))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                        font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                        line-height: 0.1;"))))
                     ))
          ),
          bsTooltip("WselectD", "HC v.s. IDP: comparing Host Community and Internally Displaced Populations. Female v.s. Male: comparing Female headed households to Male", "top"),
          tabPanel(strong(HTML("<span style='font-size:18px'>Hygiene</span>")),
                   icon= icon("shower"),#HYGIENE ###############################################################################################################
                   fluidRow(column(3, radioButtons("HselectD", label= div(style = "font-size:18px; font-weight:900;color:#666666;
                         line-height: 0.1;","Disaggregate by: "),
                                                   choices=list("HC v.s. IDP", "Female v.s. Male"),
                                                   selected= "HC v.s. IDP" , inline = TRUE))
                   ),
                   tabsetPanel(
                     tabPanel(h5("Items", align = "left", style = "color:#009933;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(3, h4("Was there one/multiple hygiene item(s) you needed to procure in the last 30 days but could not find or afford?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("Could not Access Item",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                              line-height: 1.1;")),
                                       column(4, h4("Could not Afford Item",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;"))),
                              fluidRow(column(3, highchartOutput("hcHa1F", height=270)),
                                       column(4, highchartOutput("hcHa2F", height=300)),
                                       column(4, highchartOutput("hcHa3F", height=300))),
                              fluidRow(column(3, highchartOutput("hcHa1C", height=270)),
                                       column(4, highchartOutput("hcHa2C", height=300)),
                                       column(4, highchartOutput("hcHa3C", height=300))),
                              fluidRow(column(3, highchartOutput("hcHa1B", height=270)),
                                       column(4, highchartOutput("hcHa2B", height=300)),
                                       column(4, highchartOutput("hcHa3B", height=300))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ),
                     tabPanel(h5("Accessibility", align = "left", style = "color:#009933;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(4, h4(" Do you have handwashing facilities in your household?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("Do you have soap in your household and use it? (Ask to see soap)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("Why don't you have soap?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                              line-height: 1.1;"))),
                              fluidRow(column(4, highchartOutput("hcHb1F", height=270)),
                                       column(4, highchartOutput("hcHb2F", height=270)),
                                       column(4, highchartOutput("hcHb3F", height=300))),
                              fluidRow(column(4, highchartOutput("hcHb1C", height=270)),
                                       column(4, highchartOutput("hcHb2C", height=270)),
                                       column(4, highchartOutput("hcHb3C", height=300))),
                              fluidRow(column(4, highchartOutput("hcHb1B", height=270)),
                                       column(4, highchartOutput("hcHb2B", height=270)),
                                       column(4, highchartOutput("hcHb3B", height=300))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ),
                     tabPanel(h5("Behavior", align = "left", style = "color:#009933;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(12, h4("Please name specific activities before or after which you wash your hands with soap or soap substitute. Specify the focus on activities (example:  before eating), not times of the day (example: in the morning). Select all that apply but DO NOT read out options.",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                             line-height: 1.1;"))),
                              fluidRow(column(4, highchartOutput("hcHcF", height=320)),
                                       column(4, highchartOutput("hcHcC", height=320)),
                                       column(4, highchartOutput("hcHcB", height=320)))
                     ),
                     tabPanel(h5("Assistance", align = "left", style = "color:#009933;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(4, h4("Did you or a member of your household receive hygiene promotion messaging or training in the last year? ",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4(" Did you or a member of your household receive any of the following types of WASH assistance in the last six months?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                              line-height: 1.1;"))),
                              fluidRow(column(4, highchartOutput("hcHd1F", height=320)),
                                       column(6, highchartOutput("hcHd2F", height=320))),
                              fluidRow(column(4, highchartOutput("hcHd1C", height=320)),
                                       column(6, highchartOutput("hcHd2C", height=320))),
                              fluidRow(column(4, highchartOutput("hcHd1B", height=320)),
                                       column(6, highchartOutput("hcHd2B", height=320))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ))
          ),
          bsTooltip("HselectD", "HC v.s. IDP: comparing Host Community and Internally Displaced Populations. Female v.s. Male: comparing Female headed households to Male", "top"),
          tabPanel(strong(HTML("<span style='font-size:18px'>Sanitation</span>")),
                   icon= icon("trash-alt"),#SANITATION############################################################################################################
                   fluidRow(column(3,radioButtons("SselectD", label=div(style = "font-size:18px; font-weight:900;color:#666666;
                         line-height: 0.1;", "Disaggregate by: "),
                                                  choices=list("HC v.s. IDP", "Female v.s. Male"),
                                                  selected= "HC v.s. IDP", inline = TRUE))
                   ),
                   tabsetPanel(
                     tabPanel(h5("Latriene", align = "left", style = "color:#CC6633;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(4, h4("Do your household members have access to a functioning latrine? (select one)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(3, h4("Is your latrine shared with people other than your household members?",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(4, h4("What type of latrine do your household members have access to? (select one)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                              line-height: 1.1;"))),
                              fluidRow(column(4, highchartOutput("hcSa1F", height=320)),
                                       column(3, highchartOutput("hcSa2F", height=320)),
                                       column(4, highchartOutput("hcSa3F", height=320))),
                              fluidRow(column(4, highchartOutput("hcSa1C", height=320)),
                                       column(3, highchartOutput("hcSa2C", height=320)),
                                       column(4, highchartOutput("hcSa3C", height=320))),
                              fluidRow(column(4, highchartOutput("hcSa1B", height=320)),
                                       column(3, highchartOutput("hcSa2B", height=320)),
                                       column(4, highchartOutput("hcSa3B", height=320))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ),
                     tabPanel(h5("Waste Disposal", align = "left", style = "color:#CC6633;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:800;
                         line-height: 1.1;"),
                              fluidRow(column(6, h4("Was there visible wastewater in the vicinity (30 meters or less) of your house in the last 30 days?(select one)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                            line-height: 1.1;")),
                                       column(6, h4("What is the most common way your household  disposed of garbaage in the last 30 days? (select one)",align = "left", style = "color:#58585A;
                        font-size: 15; font-family: 'Arial Narrow'; font-weight:700;
                                              line-height: 1.1;"))),
                              fluidRow(column(6, highchartOutput("hcSb1F", height=320)),
                                       column(6, highchartOutput("hcSb2F", height=320))),
                              fluidRow(column(6, highchartOutput("hcSb1C", height=320)),
                                       column(6, highchartOutput("hcSb2C", height=320))),
                              fluidRow(column(6, highchartOutput("hcSb1B", height=320)),
                                       column(6, highchartOutput("hcSb2B", height=320))),
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)",
                                                        align="left", style = "color:#666666;
                                                         font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                         line-height: 0.1;"))))
                     ))
          ),
          bsTooltip("SselectD", "HC v.s. IDP: comparing Host Community and Internally Displaced Populations. Female v.s. Male: comparing Female headed households to Male", "top"),
          tabPanel(strong(HTML("<span style='font-size:18px'>District-Level</span>")), #DISTRICT LEVEL############################################################
                   icon= icon("map-marker"),
                   fluidRow(
                     column(8, leafletOutput("Map", height=310)),
                     column(4,h4("Select a district on the map or from the drop-down menu to explore WASH assessment findings", align = "left", 
                                 style =  "font-size: 10; font-family: 'Arial Narrow'; font-weight:100;color:#666666;
                                                line-height: 1.1;"),
                            
                            radioButtons("DistselectD", label= div(style = "font-size:18px; font-weight:900;color:#666666;
                         line-height: 0.1;","Disaggregate by: "),
                                         choices=list("HC v.s. IDP", "Female v.s. Male"),
                                         selected= "HC v.s. IDP", inline = TRUE),
                            div(style = "font-size:16px;font-family: 'Arial Narrow';",
                                selectInput("Distsel", label= div(style = "font-size:18px; font-weight:900;color:#666666;
                         line-height: 0.1;","District: "),
                                            choices=distlist,
                                            selected= "Abs"),
                                htmlOutput("priori"))
                     )
                   ), 
                   tabsetPanel(
                     tabPanel(h5("WATER", align = "left", style = "color:#3399CC;
                        font-size: 18; font-family: 'Arial Narrow'; font-weight:900;
                         line-height: 1.1;"),
                              fluidRow(column(4, highchartOutput("W1", height=310)),
                                       column(4, highchartOutput("W11", height=310)),
                                       column(4, highchartOutput("W3", height=310))),
                              fluidRow(column(4, highchartOutput("W4", height=195)),
                                       column(4, highchartOutput("W5", height=195)),
                                       column(4, highchartOutput("W52", height=195))),
                              fluidRow(column(4, highchartOutput("W41", height=260)),
                                       column(4, highchartOutput("W51", height=260)),
                                       column(4, highchartOutput("W111", height=260))),
                              fluidRow(column(7, highchartOutput("W10", height= 200),
                                              highchartOutput("W110", height=200),
                                              highchartOutput("W9", height=150)),
                                       column(5, highchartOutput("W8", height=150),
                                              highchartOutput("W91", height=350)))
                     ),
                     tabPanel(h5("HYGIENE", align = "left", style = "color:#009933;
                        font-size: 18; font-family: 'Arial Narrow'; font-weight:900;
                         line-height: 1.1;"),
                              fluidRow(column(4, highchartOutput("H1", height=195),
                                              highchartOutput("H61", height=195),
                                              highchartOutput("H8", height=195)),
                                       column(8, fluidRow(column(6,highchartOutput("H11", height=265)),
                                                          column(6,highchartOutput("H12", height = 265))),
                                              highchartOutput("H5", height = 170),
                                              highchartOutput("H6", height= 170))),
                              fluidRow(column(4, highchartOutput("H7", height=350)),
                                       column(8, highchartOutput("H9", height=350)))
                              
                              ),
                     tabPanel(h5("SANITATION", align = "left", style = "color:#CC6633;
                        font-size: 18; font-family: 'Arial Narrow'; font-weight:900;
                         line-height: 1.1;"),
                              fluidRow(column(8, highchartOutput("S2", height=200),
                                              highchartOutput("S3", height=200),
                                              highchartOutput("S4", height=200)),
                                       column(4, highchartOutput("S22", height=270),
                                              highchartOutput("S21", height = 170)))
                              ),
                              fluidRow(column(8, div(h5("‡ Dentoes questions where respondents were prompted to select more than one answer", align = "left", style = "color:#666666;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:100;
                                                line-height: 1.1;")))),
                              fluidRow(column(8, div(h5("Note: the multiple choice questions shown as barcharts only display categories with values over 5%",
                                                        align="left", style = "color:#666666;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:200;
                                                line-height: 0.1;")))),
                              
                              fluidRow(column(8, div(h5("Note for barcharts: (* P ≤ 0.05)(** P ≤ 0.01)(*** P ≤ 0.001)", align = "left", style = "color:#666666;
                        font-size: 10; font-family: 'Arial Narrow'; font-weight:100;
                                                line-height: 1.1;")))))
                     
                     
                   ),
                   bsTooltip("DistselectD", "HC v.s. IDP: comparing Host Community and Internally Displaced Populations. Female v.s. Male: comparing Female headed households to Male", "top"),
                   #footer
                   wellPanel(style = "background: #58585A;",
                             # fluidRow(tags$sup(1),h5(" According to the Humanitarian Needs Overview (HNO) 2018, half of all malnutrition cases in the country were associated with WASH-related infections.", style = "color:#FFF;
                             #             font-size: 5; font-family: 'Arial Narrow'; font-weight:100;
                             #             line-height: 1.1;"), h5("International Organization for Migration (IOM)"), tags$a(href="https://mali.iom.int/reports/displacement-tracking-matrix", "Displacement Tracking Matrix (DTM) Report,"),
                             #          h5("April/May 2018.")),
                             fluidRow(
                               column(4,div(a(img(src="washlogo_grey-300DPI.png",height="77px",
                                                  style = "position: center; top: 7px; right: 0px;"), 
                                              href="https://washcluster.net/"))),
                               column(4,h5("For more information please contact REACH: \nreach.yemen@reach-initiative.org",
                                           align="center", style = "color:#FFF; top: 10px;
                      font-size: 7; font-family: 'Arial Narrow'; font-weight:400;
                      line-height: 1.1;")),
                               column(4, div(a(img(src="REACH OFFICIAL Logo.jpg",height="60px",
                                                   style = "position: absolute; top: 7px; right: 10px;"), 
                                               href="http://www.reach-initiative.org/where-we-work")))))))
          
          
          
          