###### Tool dependencies ######

library(tidyverse)
library(shiny)
library(data.table)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(sf)
library(mapview)
library(scales)
library(readxl)

###### Data Prep - Summarize Raw VE Outputs ######

test <- read_csv("27Sept_mmresults.csv")

# percentile_fn <- function(x){
#   results %>% mutate(
#   test = case_when(
#     x >= quantile(x, na.rm=T, 0.9, names=F) ~ 9, 
#     x < quantile(x, na.rm=T, .9, names=F) & x >= quantile(x, na.rm=T, .8, names=F) ~ 8, 
#     x < quantile(x, na.rm=T, .8, names=F) & x >= quantile(x, na.rm=T, .7, names=F) ~ 7,
#     x < quantile(x, na.rm=T, .7, names=F) & x >= quantile(x, na.rm=T, .6, names=F) ~ 6, 
#     x < quantile(x, na.rm=T, .6, names=F) & x>= quantile(x, na.rm=T, .5, names=F) ~ 5,
#     x < quantile(x, na.rm=T, .5, names=F) & x >= quantile(x, na.rm=T, .4, names=F) ~ 4, 
#     x < quantile(x, na.rm=T, .4, names=F) & x>= quantile(x, na.rm=T, .3, names=F) ~ 3,
#     x < quantile(x, na.rm=T, .3, names=F) & x >= quantile(x, na.rm=T, .2, names=F) ~ 2, 
#     x < quantile(x, na.rm=T, .2, names=F) ~ 1)
#   )
# }
# 
# percentile_fn(TotalCO2e)
#   
# test<-map_dfc(
#   .x=c(TotalCO2e,HouseholdDvmtPerPrsn,TotalDvmtPerPrsn), .f=percentile_fn()
# )


#jon_results <- read_csv("17Aug_Results.csv")

mapping <- read_excel("ODOT_TMIP_MMRESULTS.xlsm", sheet="Sheet3")

summary<-read_excel("ODOT_TMIP_MMRESULTS.xlsm", sheet="Priorities", skip = 4) %>%
  select(c(10,12))


###### Read in Summarized VE Outputs ######

hh1_group<-read_csv("h5group.csv") %>%
  mutate(scenario="hh1",
        workers1=case_when(Workers==0 ~ "No Worker",
            Workers==1 ~ "Single Worker",
           TRUE ~ as.character("Multiple Workers")),
        car_sufficiency=case_when(Vehicles==0 ~ "Zero Car",
                                  Vehicles < Drivers ~ "Less Cars Than Drivers",
                                  Vehicles >= Drivers ~ "More Than or Equal Cars to Drivers")
         )

hh2_group<-read_csv("h1group.csv") %>%
  mutate(scenario="hh2",
        workers1=case_when(Workers==0 ~ "No Worker",
        Workers==1 ~ "Single Worker",
        TRUE ~ as.character("Multiple Workers")),
        car_sufficiency=case_when(Vehicles==0 ~ "Zero Car",
                                  Vehicles < Drivers ~ "Less Cars Than Drivers",
                                  Vehicles >= Drivers ~ "More Than or Equal Cars to Drivers")
         )

hh3_group<-read_csv("h2group.csv") %>%
  mutate(scenario="hh3",
        workers1=case_when(Workers==0 ~ "No Worker",
         Workers==1 ~ "Single Worker",
        TRUE ~ as.character("Multiple Workers")),
        car_sufficiency=case_when(Vehicles==0 ~ "Zero Car",
                                  Vehicles < Drivers ~ "Less Cars Than Drivers",
                                  Vehicles >= Drivers ~ "More Than or Equal Cars to Drivers")
        )

hh4_group<-read_csv("h3group.csv")%>%
  mutate(scenario="h4",
        workers1=case_when(Workers==0 ~ "No Worker",
           Workers==1 ~ "Single Worker",
         TRUE ~ as.character("Multiple Workers")),
        car_sufficiency=case_when(Vehicles==0 ~ "Zero Car",
                                  Vehicles < Drivers ~ "Less Cars Than Drivers",
                                  Vehicles >= Drivers ~ "More Than or Equal Cars to Drivers")
        )

hh5_group<-read_csv("h4group.csv") %>%
  mutate(scenario="hh5",
         workers1=case_when(Workers==0 ~ "No Worker",
          Workers==1 ~ "Single Worker",
         TRUE ~ as.character("Multiple Workers")),
         car_sufficiency=case_when(Vehicles==0 ~ "Zero Car",
                                   Vehicles < Drivers ~ "Less Cars Than Drivers",
                                   Vehicles >= Drivers ~ "More Than or Equal Cars to Drivers")
         )

all<- bind_rows(hh1_group, hh2_group, hh3_group, hh4_group) #%>%
  #filter(!composition %in% c("0 adults, 1-2 children","0 adults, 3+ children"))

counties <- unique(all$Azone)
hh_size <- unique(all$HhSize)
inc <- unique(all$income_cat)

linebreaks <- function(n){HTML(strrep(br(), n))}

############ Shiny Tool Code ##################

ui <- dashboardPage(skin="black", 
                    dashboardHeader(title="Oregon Scenario Explorer",titleWidth = 450),
                    dashboardSidebar(disable = T),
                    dashboardBody(
                      fluidRow(
                        tabBox(width=12,
                               tabPanel("Introduction",
                                        fluidRow(box(width=6,
                                                     h3(tags$b("Purpose")),
                                                     h4("This tool was developed by RSG for the Oregon DOT as part of the Oregon State Transportation Plan. The purpose of the tool is to assist in developing household 'personas' and to better understand the average travel characteristics of those personas under different future year scenarios. This was done by summarizing and visualizing disaggregated household output data from VE-State modeled scenarios."),
                                                     linebreaks(1),
                                                     h3(tags$b("Tool Usage")),
                                                     h4('The summary data tab displays the average of individual household records from the scenarios and allows the user to interactively select different household characteristics (personas) and observe key summary metrics. The summary data points represent the percent change in future scenarios from the base year. The displayed information is the average of all households characterized by the selected drop down input values. Select values from the dropdowns and observe how percentages change over different scenarios. The household characteristics you can select include County, Household Size, Household Composition, # of Workers, Housing Type, Location, # of Vehicles, and # of Drivers.'),
                                                     linebreaks(1),
                                                     h4(tags$p("The full tool code can be found on",tags$a(href="https://github.com/rhaefer/ODOT_HH_Explorer/blob/master/app.R", "Github"))),
                                                     h4("..."),
                                                     linebreaks(2),
                                                     img(src='RSG Logo.jpg',  height = 80, width = 250)
                                        ))),
                               tabPanel("Priorities",
                                        tabBox(width=12,
                                               tabPanel("Overview",
                                        fluidRow(
                                          box(width=12,
                                              column(width=2,
                                              fileInput("upload_file", "Select Results File (.csv)",
                                                        multiple = TRUE,
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values,text/plain",
                                                                   ".csv"))
                                              ),
                                              column(width=2,
                                            numericInput("no_exp","Select Top Number of Experiments", min=1, max=100, value=5)
                                              )
                                            )
                                          ),
                                        fluidRow(
                                          box(
                                            width=12, title="", collapsed = F, collapsible = TRUE,
                                              column(
                                                width=4,
                                                fluidRow(
                                                box(width=12, title="Equity", status = "primary",solidHeader = T,
                                                    column(width=3,
                                                    numericInput("goal_pri_equity","Goal", value=1, min=0, max=100)
                                                    ),
                                                    column(width=4,
                                                           numericInput("obj_pri_eq1","Objective 1 - text text text text", value=1, min=0, max=100)
                                                           ),
                                                    column(width=4,
                                                           numericInput("pol_pri_OwnCostPropHhLess25K","OwnCostPropHhLess25K", value=1, min=0, max=100),
                                                           numericInput("pol_pri_OwnCostProp","OwnCostProp", value=1, min=0, max=100),
                                                           numericInput("pol_pri_OwnCostPropHh25Kto50K","OwnCostPropHh25Kto50K", value=1, min=0, max=100)
                                                    )
                                                    )
                                                ),
                                                fluidRow(
                                                box(width=12, title="Safety", status = "info",solidHeader = T,
                                                    column(width=3,
                                                    numericInput("goal_pri_safety","Goal", value=1, min=0, max=100)
                                                    ),
                                                    column(width=4,
                                                           numericInput("obj_pri_sfty1","Objective 1 - text text text", value=1, min=0, max=100)
                                                    ),
                                                    column(width=4,
                                                           numericInput("pol_pri_AutoFatalCrash_Metro","AutoFatalCrash_Metro", value=1, min=0, max=100),
                                                           numericInput("pol_pri_AutoFatalUrban","AutoFatalUrban", value=1, min=0, max=100),
                                                           numericInput("pol_pri_AutoInjuryUrban","AutoInjuryUrban", value=1, min=0, max=100),
                                                           numericInput("pol_pri_AutoFatalRural","AutoFatalRural", value=1, min=0, max=100)
                                                    )
                                                )
                                            ),
                                          fluidRow(
                                                box(width=12, title="Sustainability", status = "warning",solidHeader = T,
                                                    fluidRow(
                                                    column(width=3,
                                                      numericInput("goal_pri_sust","Goal", value=1, min=0, max=100)
                                                    ),column(width=4,
                                                             numericInput("obj_pri_sust1","Objective 1 - Reduce GHG emissions", value=1, min=0, max=100),
                                                             br(),
                                                             br(),
                                                             br()
                                                             ),
                                                    column(width=5,
                                                           numericInput("pol_pri_TotalCO2e","TotalCO2e", value=1, min=0, max=100),
                                                           numericInput("pol_pri_HouseholdCO2ePerPrsn","HouseholdCO2ePerPrsn", value=1, min=0, max=100),
                                                           numericInput("pol_pri_HouseholdCO2ePerMileRural","HouseholdCO2ePerMileRural", value=1, min=0, max=100)
                                                    )      
                                                    ),
                                                    br(),
                                                    fluidRow(
                                                      column(width=3),
                                                      column(width=4,
                                                             numericInput("obj_pri_sust2","Objective 2 - Cleaner vehicles and energy efficiency", value=1, min=0, max=100)),
                                                    column(width=4,
                                                             numericInput("pol_pri_BusCO2eRate_Metro","BusCO2eRate_Metro", value=1, min=0, max=100),
                                                             numericInput("pol_pri_HvyTrkAveUrbanCO2eRate_Metro","HvyTrkAveUrbanCO2eRate_Metro", value=1, min=0, max=100),
                                                           numericInput("pol_pri_ElecDvmtProp","ElecDvmtProp", value=1, min=0, max=100)
                                                    )
                                                    )
                                                )
                                              )
                                          ),
                                          column(width=4,
                                                 fluidRow(
                                                   box(width=12, title="Stewardship", status = "info",solidHeader = T,
                                                       column(width=3,
                                                              numericInput("goal_pri_stew","Goal", value=1, min=0, max=100)
                                                       ),
                                                       column(width=4,
                                                              numericInput("goal_pri_stew1","Objective (Goal)", value=1, min=0, max=100)
                                                       ),
                                                       column(width=4,
                                                              numericInput("pol_pri_PresAdapt_2022","PresAdapt$22", value=1, min=0, max=100)
                                                       )
                                                   )
                                                 ),
                                                 fluidRow(
                                                   box(width=12, title="Mobility", status = "success",solidHeader = T,
                                                       fluidRow(
                                                       column(width=3,
                                                              numericInput("goal_pri_mob","Goal", value=1, min=0, max=100)
                                                       ),
                                                       column(width=4,
                                                              numericInput("obj_pri_mob1","Objective 1 -  Complete, maintain, and improve multimodal transportation facilities and services that are essential to Oregonians’ prosperity and quality of life.", value=1, min=0, max=100) 
                                                       ),
                                                       column(width=5,
                                                              numericInput("pol_pri_BikePMTPerPrsnUrban","BikePMTPerPrsnUrban", value=1, min=0, max=100),
                                                              numericInput("pol_pri_WalkPMTPerPrsnUrban","WalkPMTPerPrsnUrban", value=1, min=0, max=100),
                                                              numericInput("pol_pri_TransitTripsPerCapita","TransitTripsPerCapita", value=1, min=0, max=100),
                                                              numericInput("pol_pri_TransitPMTPerPrsnUrban","TransitPMTPerPrsnUrban", value=1, min=0, max=100),
                                                              numericInput("pol_pri_BikeTripsPerCapita","BikeTripsPerCapita", value=1, min=0, max=100),
                                                              numericInput("pol_pri_WalkTripsPerCapita","WalkTripsPerCapita", value=1, min=0, max=100),
                                                              ),
                                                       ),
                                                       br(),
                                                       fluidRow(
                                                         column(width=3),
                                                         column(width=4,
                                                              numericInput("obj_pri_mob2","Objective 2 - Reduce passenger vehicle miles traveled (VMT) per capita.", value=1, min=0, max=100)
                                                         ),
                                                         column(width = 5,
                                                                numericInput("pol_pri_HouseholdDvmtPerPrsn","HouseholdDvmtPerPrsn", value=1, min=0, max=100),
                                                                numericInput("pol_pri_TotalDvmtPerPrsn","TotalDvmtPerPrsn", value=1, min=0, max=100),
                                                                )
                                                         ),
                                                       br(),
                                                       fluidRow(
                                                         column(width=3),
                                                         column(width=4,
                                                                numericInput("obj_pri_mob4","Objective 4 - Maintain or improve travel reliability for movement of goods and access to services", value=1, min=0, max=100)),
                                                                column(width=5,
                                                                       numericInput("pol_pri_FwyExtCongTTI_Metro","FwyExtCongTTI_Metro", value=1, min=0, max=100),
                                                                       numericInput("pol_pri_ArtExtCongTTI_Metro","ArtExtCongTTI_Metro", value=1, min=0, max=100),
                                                                       numericInput("pol_pri_ArtDvmtPropExtCong_SalemKeizer","ArtDvmtPropExtCong_SalemKeizer", value=1, min=0, max=100)
                                                                       )
                                                                )
                                                   )
                                                 )
                                                 ),
                                           column(width=3,
                                                      box(width=12, title="Results", status = "danger",solidHeader = T,
                                                          dataTableOutput("sum_results")
                                                      )
                                            )
                                            )#, dataTableOutput("pol_weight_table")
                                        ),
                                        fluidRow(box(width=12, title="Scored Full Experiments", status = "danger",solidHeader = T,
                                          dataTableOutput('un_sum_results'))
                                        )
                                        ),
                                        tabPanel("Documentation",
                                                 fluidRow(box(width=12,
                                                              fluidRow(
                                                        h3(tags$b("Using the Prioritization Tool Feature")),
                                                        h4("Explore the tool by entering in different weights for the different goals, objectives, and measures.  The prioritization tool feature is designed to optimally function with multiple goals, objectives, and measures weighted. For example, if the user adds a '0' weight for each of the goals, the results tab will simply reflect the top 'n' experiments and will not reflect any sort of ranked/scored information.")
                                                              ),
                                                              fluidRow(
                                                   h3(tags$b("Scoring Methodology")),
                                                   h4("By default the tool weights all goals, objectives, equally; it is up to the user to change the numeric inputs from '1' to other numeric weights. The weighting system has several key design elements. Goals (e.g. Equity) are only weighted against other goals. For example, if all of the goals has a '1', then each goal of the five goals receives a 20% weight (1/5). Alternatively, if the user changes one goal weight to 10 and leaves the other goals at 1, the goal with the 9 will receive a 90% weight (9/10) while the others will have a 10% weight (1/10). Beneath goals, objectives are only weighted against other objectives within that goal and policy measures are only weighted against other measures within that objective. Scores are developed by applying these weights to a set of normalized measure values that come directly out of the TMIP-EMAT process. Measures, such as 'TotalCO2e', are normalized using percentiles against all other values of the measure and then added together - once the weights are applied - to create a total score. This total score represents the degree of desireability of given experiment considering the weighted priority that a user has provided and how those weights relate to estimated TMIP-EMAT experiment values. Once all of the experiments are scored, the top 'n' experiments are averaged and summary results of those 'top' experiments are shown in the results pane."),
                                                   h4("Score = (Normalized Measure Value1 * (Goal Weight * Objective Weight * Policy Weight)) + (Normalized Measure Value2 * (Goal Weight * Objective Weight * Policy Weight)) + (Measure Value3 * (Goal Weight * Objective Weight * Policy Weight)) ...")),
                                                 fluidRow(
                                                   linebreaks(2),
                                                   h3(tags$b("Goals & Policies & Measures")),
                                                   img(src='hierarchy.jpg',  height = 380, width = 750)
                                                 )
                                                 )
                                                 )
                                        ), tabPanel("Experiments",
                                                    fluidRow(
                                                      box(width=12, title = "Results",
                                                          dataTableOutput("results")
                                                      )
                                                    )
                                        )
                                        )
                                        ),
                               tabPanel("Household Query",
                        box(title="Select Household Characteristics",
                                   column(1,pickerInput("input_azone","County" ,choices=counties, selected=counties,options = list(`actions-box` = TRUE),multiple = T)), 
                                   column(1,pickerInput("input_region","Economic Region" ,choices=unique(all$region), selected=unique(all$region),options = list(`actions-box` = TRUE),multiple = T)), 
                                   #column(1,pickerInput("input_hh","Household Size", choices=hh_size, selected=hh_size, options = list(`actions-box` = TRUE),multiple = T)), 
                                   column(1,pickerInput("input_lifecycle","Household Composition", choices=sort(unique(all$odot_comp)), selected=unique(all$odot_comp),options = list(`actions-box` = TRUE),multiple = T)),
                                   column(1,pickerInput("input_income","Household Income", choices=inc, selected=inc,options = list(`actions-box` = TRUE),multiple = T)), 
                                   column(1,pickerInput("input_workers","# of Workers", choices=unique(all$workers1), selected=unique(all$workers1),options = list(`actions-box` = TRUE),multiple = T)), 
                                   column(1,pickerInput("input_house_type","House Type", choices=unique(all$HouseType), selected=unique(all$HouseType) ,options = list(`actions-box` = TRUE),multiple = T)),
                                   column(1,pickerInput("input_loc_type","Location Type", choices=unique(all$LocType), selected=unique(all$LocType),options = list(`actions-box` = TRUE),multiple = T)),
                                   column(1,pickerInput("input_car_sufficiency","Car Sufficiency", choices=unique(all$car_sufficiency), selected=unique(all$car_sufficiency), options = list(`actions-box` = TRUE),multiple = T)),
                                  # column(1,pickerInput("input_drivers","# of Drivers", choices=unique(all$Drivers), selected=unique(all$Drivers) ,options = list(`actions-box` = TRUE),multiple = T)),
                                   width=12),
                      # fluidRow(box(title="Common 2015 - Household Summary", status="primary", solidHeader = F,
                      #              column(2,valueBoxOutput("DvmtPerCapita_1", width=12) %>% withSpinner()),
                      #              column(2,valueBoxOutput("veh_trips_1", width=12)),
                      #              column(2,valueBoxOutput("tran_trips_1", width=12)),
                      #              column(2,valueBoxOutput("gge_1", width=12)),
                      #              column(2,valueBoxOutput("cost_1", width=12)),
                      #              column(2,valueBoxOutput("co2_1", width=12)),
                      #              width=12)
                      # ),
                      fluidRow(box(title="Base (Adopted Plan 2010)",status="primary", solidHeader = F,
                                   column(2,valueBoxOutput("DvmtPerCapita_1", width=12)),  
                                   column(2,valueBoxOutput("veh_trips_1", width=12)),
                                   column(2,valueBoxOutput("tran_trips_1", width=12)),
                                   column(2,valueBoxOutput("gge_1", width=12)),
                                   column(2,valueBoxOutput("cost_1", width=12)),
                                   column(2,valueBoxOutput("co2_1", width=12)),
                                   column(2,valueBoxOutput("bike_trips_1", width=12)),
                                   column(2,valueBoxOutput("walk_trips_1", width=12)),
                                   column(2,valueBoxOutput("records111", width=12)),
                                  # column(2,valueBoxOutput("cost_per_income_1", width=12)),
                                   width=12)
                      ),
                      fluidRow(box(title="Reduce Emissions (2050)", status="success", solidHeader = F,
                                   column(2,valueBoxOutput("DvmtPerCapita_2", width=12)),  
                                   column(2,valueBoxOutput("veh_trips_2", width=12)),
                                   column(2,valueBoxOutput("tran_trips_2", width=12)),
                                   column(2,valueBoxOutput("gge_2", width=12)),
                                   column(2,valueBoxOutput("cost_2", width=12)),
                                   column(2,valueBoxOutput("co2_2", width=12)),
                                   column(2,valueBoxOutput("bike_trips_2", width=12)),
                                   column(2,valueBoxOutput("walk_trips_2", width=12)),
                                   column(2,valueBoxOutput("records2", width=12)),
                                   width=12)
                      ),
                      fluidRow(box(title="Good Repair (2050)", status="success",solidHeader = F,
                                   column(2,valueBoxOutput("DvmtPerCapita_3", width=12)),  
                                   column(2,valueBoxOutput("veh_trips_3", width=12)),
                                   column(2,valueBoxOutput("tran_trips_3", width=12)),
                                   column(2,valueBoxOutput("gge_3", width=12)),
                                   column(2,valueBoxOutput("cost_3", width=12)),
                                   column(2,valueBoxOutput("co2_3", width=12)),
                                   column(2,valueBoxOutput("bike_trips_3", width=12)),
                                   column(2,valueBoxOutput("walk_trips_3", width=12)),
                                   column(2,valueBoxOutput("records3", width=12)),
                                   width=12)),
                      fluidRow(box(title="Travel Options (2050)", status="success",solidHeader = F,
                                   column(2,valueBoxOutput("DvmtPerCapita_4", width=12)),  
                                   column(2,valueBoxOutput("veh_trips_4", width=12)),
                                   column(2,valueBoxOutput("tran_trips_4", width=12)),
                                   column(2,valueBoxOutput("gge_4", width=12)),
                                   column(2,valueBoxOutput("cost_4", width=12)),
                                   column(2,valueBoxOutput("co2_4", width=12)),
                                   column(2,valueBoxOutput("bike_trips_4", width=12)),
                                   column(2,valueBoxOutput("walk_trips_4", width=12)),
                                   column(2,valueBoxOutput("records4", width=12)),
                                   width=12)),
                      fluidRow(box(title="Balanced Future (2050)", status="success",solidHeader = F,
                                   column(2,valueBoxOutput("DvmtPerCapita_5", width=12)),
                                   column(2,valueBoxOutput("veh_trips_5", width=12)),
                                   column(2,valueBoxOutput("tran_trips_5", width=12)),
                                   column(2,valueBoxOutput("gge_5", width=12)),
                                   column(2,valueBoxOutput("cost_5", width=12)),
                                   column(2,valueBoxOutput("co2_5", width=12)),
                                   column(2,valueBoxOutput("bike_trips_5", width=12)),
                                   column(2,valueBoxOutput("walk_trips_5", width=12)),
                                   column(2,valueBoxOutput("records5", width=12)),
                                   width=12))
                      )#,
                      # tabPanel("Equity Analysis",
                      #          box(width = 7, status = 'primary', solidHeader = TRUE, title = "ODOT_Equity",
                      #              tags$iframe(style="height:800px; width:100%", src="ODOT_Equity.pdf")
                      #          ))
                      )
                    )
)
)
server <- function(input, output, session) { 
  tool_data <- reactive({
    if(!is.null(input$upload_file)){
      results_new()
    } else{
      read_csv("27Sept_mm_2results.csv")
    }
  })
results <- reactive({
  tool_data() %>%
    mutate(
      `TotalCO2e_norm`=case_when(
        TotalCO2e >= quantile(TotalCO2e, na.rm=T, 0.9333338, names=F) ~ 1, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .99333338, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .8666671, names=F) ~ 2, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .8666671, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .8000004, names=F) ~ 3,
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .8000004, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .7333337, names=F) ~ 4, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .7333337, names=F) & TotalCO2e>= quantile(TotalCO2e, na.rm=T, .666667, names=F) ~ 5,
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .666667, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .6000003, names=F) ~ 6, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .6000003, names=F) & TotalCO2e>= quantile(TotalCO2e, na.rm=T, .5333336, names=F) ~ 7,
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .5333336, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .4666669, names=F) ~ 8, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .4666669, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .4000002, names=F) ~ 9, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .4000002, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .3333335, names=F) ~ 10, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .3333335, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .2666668, names=F) ~ 11, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .2666668, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .2000001, names=F) ~ 12, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .2000001, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .1333334, names=F) ~ 13, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .1333334, names=F) & TotalCO2e >= quantile(TotalCO2e, na.rm=T, .0666667, names=F) ~ 14, 
        TotalCO2e < quantile(TotalCO2e, na.rm=T, .0666667, names=F) ~ 15),
      `HouseholdDvmtPerPrsn_norm`=case_when(
        HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, 0.9333338, names=F) ~ 1, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .99333338, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .8666671, names=F) ~ 2, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .8666671, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .8000004, names=F) ~ 3,
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .8000004, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .7333337, names=F) ~ 4, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .7333337, names=F) & HouseholdDvmtPerPrsn>= quantile(HouseholdDvmtPerPrsn, na.rm=T, .666667, names=F) ~ 5,
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .666667, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .6000003, names=F) ~ 6, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .6000003, names=F) & HouseholdDvmtPerPrsn>= quantile(HouseholdDvmtPerPrsn, na.rm=T, .5333336, names=F) ~ 7,
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .5333336, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .4666669, names=F) ~ 8, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .4666669, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .4000002, names=F) ~ 9, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .4000002, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .3333335, names=F) ~ 10, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .3333335, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .2666668, names=F) ~ 11, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .2666668, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .2000001, names=F) ~ 12, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .2000001, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .1333334, names=F) ~ 13, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .1333334, names=F) & HouseholdDvmtPerPrsn >= quantile(HouseholdDvmtPerPrsn, na.rm=T, .0666667, names=F) ~ 14, 
        HouseholdDvmtPerPrsn < quantile(HouseholdDvmtPerPrsn, na.rm=T, .0666667, names=F) ~ 15),
      `TotalDvmtPerPrsn_norm`=case_when(
        TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, 0.9333338, names=F) ~ 1, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .99333338, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .8666671, names=F) ~ 2, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .8666671, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .8000004, names=F) ~ 3,
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .8000004, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .7333337, names=F) ~ 4, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .7333337, names=F) & TotalDvmtPerPrsn>= quantile(TotalDvmtPerPrsn, na.rm=T, .666667, names=F) ~ 5,
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .666667, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .6000003, names=F) ~ 6, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .6000003, names=F) & TotalDvmtPerPrsn>= quantile(TotalDvmtPerPrsn, na.rm=T, .5333336, names=F) ~ 7,
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .5333336, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .4666669, names=F) ~ 8, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .4666669, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .4000002, names=F) ~ 9, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .4000002, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .3333335, names=F) ~ 10, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .3333335, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .2666668, names=F) ~ 11, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .2666668, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .2000001, names=F) ~ 12, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .2000001, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .1333334, names=F) ~ 13, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .1333334, names=F) & TotalDvmtPerPrsn >= quantile(TotalDvmtPerPrsn, na.rm=T, .0666667, names=F) ~ 14, 
        TotalDvmtPerPrsn < quantile(TotalDvmtPerPrsn, na.rm=T, .0666667, names=F) ~ 15),
      `BusCO2eRate_Metro_norm`=case_when(
        BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, 0.9333338, names=F) ~ 1, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .99333338, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .8666671, names=F) ~ 2, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .8666671, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .8000004, names=F) ~ 3,
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .8000004, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .7333337, names=F) ~ 4, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .7333337, names=F) & BusCO2eRate_Metro>= quantile(BusCO2eRate_Metro, na.rm=T, .666667, names=F) ~ 5,
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .666667, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .6000003, names=F) ~ 6, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .6000003, names=F) & BusCO2eRate_Metro>= quantile(BusCO2eRate_Metro, na.rm=T, .5333336, names=F) ~ 7,
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .5333336, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .4666669, names=F) ~ 8, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .4666669, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .4000002, names=F) ~ 9, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .4000002, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .3333335, names=F) ~ 10, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .3333335, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .2666668, names=F) ~ 11, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .2666668, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .2000001, names=F) ~ 12, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .2000001, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .1333334, names=F) ~ 13, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .1333334, names=F) & BusCO2eRate_Metro >= quantile(BusCO2eRate_Metro, na.rm=T, .0666667, names=F) ~ 14, 
        BusCO2eRate_Metro < quantile(BusCO2eRate_Metro, na.rm=T, .0666667, names=F) ~ 15),
      `HouseholdCO2ePerPrsn_norm`=case_when(
        HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, 0.9333338, names=F) ~ 1, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .99333338, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .8666671, names=F) ~ 2, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .8666671, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .8000004, names=F) ~ 3,
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .8000004, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .7333337, names=F) ~ 4, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .7333337, names=F) & HouseholdCO2ePerPrsn>= quantile(HouseholdCO2ePerPrsn, na.rm=T, .666667, names=F) ~ 5,
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .666667, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .6000003, names=F) ~ 6, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .6000003, names=F) & HouseholdCO2ePerPrsn>= quantile(HouseholdCO2ePerPrsn, na.rm=T, .5333336, names=F) ~ 7,
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .5333336, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .4666669, names=F) ~ 8, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .4666669, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .4000002, names=F) ~ 9, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .4000002, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .3333335, names=F) ~ 10, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .3333335, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .2666668, names=F) ~ 11, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .2666668, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .2000001, names=F) ~ 12, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .2000001, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .1333334, names=F) ~ 13, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .1333334, names=F) & HouseholdCO2ePerPrsn >= quantile(HouseholdCO2ePerPrsn, na.rm=T, .0666667, names=F) ~ 14, 
        HouseholdCO2ePerPrsn < quantile(HouseholdCO2ePerPrsn, na.rm=T, .0666667, names=F) ~ 15),
      `HouseholdCO2ePerMileRural_norm`=case_when(
        HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, 0.9333338, names=F) ~ 1, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .99333338, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .8666671, names=F) ~ 2, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .8666671, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .8000004, names=F) ~ 3,
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .8000004, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .7333337, names=F) ~ 4, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .7333337, names=F) & HouseholdCO2ePerMileRural>= quantile(HouseholdCO2ePerMileRural, na.rm=T, .666667, names=F) ~ 5,
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .666667, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .6000003, names=F) ~ 6, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .6000003, names=F) & HouseholdCO2ePerMileRural>= quantile(HouseholdCO2ePerMileRural, na.rm=T, .5333336, names=F) ~ 7,
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .5333336, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .4666669, names=F) ~ 8, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .4666669, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .4000002, names=F) ~ 9, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .4000002, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .3333335, names=F) ~ 10, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .3333335, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .2666668, names=F) ~ 11, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .2666668, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .2000001, names=F) ~ 12, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .2000001, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .1333334, names=F) ~ 13, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .1333334, names=F) & HouseholdCO2ePerMileRural >= quantile(HouseholdCO2ePerMileRural, na.rm=T, .0666667, names=F) ~ 14, 
        HouseholdCO2ePerMileRural < quantile(HouseholdCO2ePerMileRural, na.rm=T, .0666667, names=F) ~ 15),
      `HvyTrkAveUrbanCO2eRate_Metro_norm`=case_when(
        HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, 0.9333338, names=F) ~ 1, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .99333338, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .8666671, names=F) ~ 2, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .8666671, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .8000004, names=F) ~ 3,
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .8000004, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .7333337, names=F) ~ 4, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .7333337, names=F) & HvyTrkAveUrbanCO2eRate_Metro>= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .666667, names=F) ~ 5,
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .666667, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .6000003, names=F) ~ 6, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .6000003, names=F) & HvyTrkAveUrbanCO2eRate_Metro>= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .5333336, names=F) ~ 7,
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .5333336, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .4666669, names=F) ~ 8, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .4666669, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .4000002, names=F) ~ 9, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .4000002, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .3333335, names=F) ~ 10, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .3333335, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .2666668, names=F) ~ 11, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .2666668, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .2000001, names=F) ~ 12, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .2000001, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .1333334, names=F) ~ 13, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .1333334, names=F) & HvyTrkAveUrbanCO2eRate_Metro >= quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .0666667, names=F) ~ 14, 
        HvyTrkAveUrbanCO2eRate_Metro < quantile(HvyTrkAveUrbanCO2eRate_Metro, na.rm=T, .0666667, names=F) ~ 15),
      `TransitTripsPerCapita_norm`=case_when(
        TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, 0.9333338, names=F) ~ 15, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .99333338, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .8666671, names=F) ~ 14, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .8666671, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .8000004, names=F) ~ 13,
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .8000004, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .7333337, names=F) ~ 12, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .7333337, names=F) & TransitTripsPerCapita>= quantile(TransitTripsPerCapita, na.rm=T, .666667, names=F) ~ 11,
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .666667, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .6000003, names=F) ~ 10, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .6000003, names=F) & TransitTripsPerCapita>= quantile(TransitTripsPerCapita, na.rm=T, .5333336, names=F) ~ 9,
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .5333336, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .4666669, names=F) ~ 8, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .4666669, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .4000002, names=F) ~ 7, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .4000002, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .3333335, names=F) ~ 6, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .3333335, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .2666668, names=F) ~ 5, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .2666668, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .2000001, names=F) ~ 4, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .2000001, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .1333334, names=F) ~ 3, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .1333334, names=F) & TransitTripsPerCapita >= quantile(TransitTripsPerCapita, na.rm=T, .0666667, names=F) ~ 2, 
        TransitTripsPerCapita < quantile(TransitTripsPerCapita, na.rm=T, .0666667, names=F) ~ 1),
      `TransitPMTPerPrsnUrban_norm`=case_when(
        TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, 0.9333338, names=F) ~ 15, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .99333338, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .8666671, names=F) ~ 14, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .8666671, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .8000004, names=F) ~ 13,
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .8000004, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .7333337, names=F) ~ 12, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .7333337, names=F) & TransitPMTPerPrsnUrban>= quantile(TransitPMTPerPrsnUrban, na.rm=T, .666667, names=F) ~ 11,
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .666667, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .6000003, names=F) ~ 10, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .6000003, names=F) & TransitPMTPerPrsnUrban>= quantile(TransitPMTPerPrsnUrban, na.rm=T, .5333336, names=F) ~ 9,
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .5333336, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .4666669, names=F) ~ 8, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .4666669, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .4000002, names=F) ~ 7, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .4000002, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .3333335, names=F) ~ 6, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .3333335, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .2666668, names=F) ~ 5, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .2666668, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .2000001, names=F) ~ 4, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .2000001, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .1333334, names=F) ~ 3, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .1333334, names=F) & TransitPMTPerPrsnUrban >= quantile(TransitPMTPerPrsnUrban, na.rm=T, .0666667, names=F) ~ 2, 
        TransitPMTPerPrsnUrban < quantile(TransitPMTPerPrsnUrban, na.rm=T, .0666667, names=F) ~ 1),
      `BikePMTPerPrsnUrban_norm`=case_when(
        BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, 0.9333338, names=F) ~ 15, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .99333338, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .8666671, names=F) ~ 14, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .8666671, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .8000004, names=F) ~ 13,
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .8000004, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .7333337, names=F) ~ 12, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .7333337, names=F) & BikePMTPerPrsnUrban>= quantile(BikePMTPerPrsnUrban, na.rm=T, .666667, names=F) ~ 11,
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .666667, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .6000003, names=F) ~ 10, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .6000003, names=F) & BikePMTPerPrsnUrban>= quantile(BikePMTPerPrsnUrban, na.rm=T, .5333336, names=F) ~ 9,
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .5333336, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .4666669, names=F) ~ 8, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .4666669, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .4000002, names=F) ~ 7, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .4000002, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .3333335, names=F) ~ 6, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .3333335, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .2666668, names=F) ~ 5, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .2666668, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .2000001, names=F) ~ 4, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .2000001, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .1333334, names=F) ~ 3, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .1333334, names=F) & BikePMTPerPrsnUrban >= quantile(BikePMTPerPrsnUrban, na.rm=T, .0666667, names=F) ~ 2, 
        BikePMTPerPrsnUrban < quantile(BikePMTPerPrsnUrban, na.rm=T, .0666667, names=F) ~ 1),
      `WalkTripsPerCapita_norm`=case_when(
        WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, 0.9333338, names=F) ~ 15, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .99333338, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .8666671, names=F) ~ 14, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .8666671, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .8000004, names=F) ~ 13,
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .8000004, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .7333337, names=F) ~ 12, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .7333337, names=F) & WalkTripsPerCapita>= quantile(WalkTripsPerCapita, na.rm=T, .666667, names=F) ~ 11,
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .666667, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .6000003, names=F) ~ 10, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .6000003, names=F) & WalkTripsPerCapita>= quantile(WalkTripsPerCapita, na.rm=T, .5333336, names=F) ~ 9,
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .5333336, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .4666669, names=F) ~ 8, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .4666669, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .4000002, names=F) ~ 7, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .4000002, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .3333335, names=F) ~ 6, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .3333335, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .2666668, names=F) ~ 5, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .2666668, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .2000001, names=F) ~ 4, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .2000001, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .1333334, names=F) ~ 3, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .1333334, names=F) & WalkTripsPerCapita >= quantile(WalkTripsPerCapita, na.rm=T, .0666667, names=F) ~ 2, 
        WalkTripsPerCapita < quantile(WalkTripsPerCapita, na.rm=T, .0666667, names=F) ~ 1),
      `WalkPMTPerPrsnUrban_norm`=case_when(
        WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, 0.9333338, names=F) ~ 15, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .99333338, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .8666671, names=F) ~ 14, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .8666671, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .8000004, names=F) ~ 13,
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .8000004, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .7333337, names=F) ~ 12, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .7333337, names=F) & WalkPMTPerPrsnUrban>= quantile(WalkPMTPerPrsnUrban, na.rm=T, .666667, names=F) ~ 11,
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .666667, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .6000003, names=F) ~ 10, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .6000003, names=F) & WalkPMTPerPrsnUrban>= quantile(WalkPMTPerPrsnUrban, na.rm=T, .5333336, names=F) ~ 9,
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .5333336, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .4666669, names=F) ~ 8, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .4666669, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .4000002, names=F) ~ 7, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .4000002, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .3333335, names=F) ~ 6, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .3333335, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .2666668, names=F) ~ 5, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .2666668, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .2000001, names=F) ~ 4, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .2000001, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .1333334, names=F) ~ 3, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .1333334, names=F) & WalkPMTPerPrsnUrban >= quantile(WalkPMTPerPrsnUrban, na.rm=T, .0666667, names=F) ~ 2, 
        WalkPMTPerPrsnUrban < quantile(WalkPMTPerPrsnUrban, na.rm=T, .0666667, names=F) ~ 1),
      `ElecDvmtProp_norm`=case_when(
        ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, 0.9333338, names=F) ~ 15, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .99333338, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .8666671, names=F) ~ 14, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .8666671, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .8000004, names=F) ~ 13,
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .8000004, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .7333337, names=F) ~ 12, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .7333337, names=F) & ElecDvmtProp>= quantile(ElecDvmtProp, na.rm=T, .666667, names=F) ~ 11,
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .666667, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .6000003, names=F) ~ 10, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .6000003, names=F) & ElecDvmtProp>= quantile(ElecDvmtProp, na.rm=T, .5333336, names=F) ~ 9,
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .5333336, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .4666669, names=F) ~ 8, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .4666669, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .4000002, names=F) ~ 7, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .4000002, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .3333335, names=F) ~ 6, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .3333335, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .2666668, names=F) ~ 5, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .2666668, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .2000001, names=F) ~ 4, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .2000001, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .1333334, names=F) ~ 3, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .1333334, names=F) & ElecDvmtProp >= quantile(ElecDvmtProp, na.rm=T, .0666667, names=F) ~ 2, 
        ElecDvmtProp < quantile(ElecDvmtProp, na.rm=T, .0666667, names=F) ~ 1),
      `ArtDvmtPropExtCong_SalemKeizer_norm`=case_when(
        ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, 0.9333338, names=F) ~ 1, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .99333338, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .8666671, names=F) ~ 2, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .8666671, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .8000004, names=F) ~ 3,
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .8000004, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .7333337, names=F) ~ 4, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .7333337, names=F) & ArtDvmtPropExtCong_SalemKeizer>= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .666667, names=F) ~ 5,
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .666667, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .6000003, names=F) ~ 6, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .6000003, names=F) & ArtDvmtPropExtCong_SalemKeizer>= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .5333336, names=F) ~ 7,
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .5333336, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .4666669, names=F) ~ 8, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .4666669, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .4000002, names=F) ~ 9, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .4000002, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .3333335, names=F) ~ 10, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .3333335, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .2666668, names=F) ~ 11, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .2666668, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .2000001, names=F) ~ 12, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .2000001, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .1333334, names=F) ~ 13, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .1333334, names=F) & ArtDvmtPropExtCong_SalemKeizer >= quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .0666667, names=F) ~ 14, 
        ArtDvmtPropExtCong_SalemKeizer < quantile(ArtDvmtPropExtCong_SalemKeizer, na.rm=T, .0666667, names=F) ~ 15),
      `FwyExtCongTTI_Metro_norm`=case_when(
        FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, 0.9333338, names=F) ~ 1, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .99333338, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .8666671, names=F) ~ 2, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .8666671, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .8000004, names=F) ~ 3,
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .8000004, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .7333337, names=F) ~ 4, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .7333337, names=F) & FwyExtCongTTI_Metro>= quantile(FwyExtCongTTI_Metro, na.rm=T, .666667, names=F) ~ 5,
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .666667, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .6000003, names=F) ~ 6, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .6000003, names=F) & FwyExtCongTTI_Metro>= quantile(FwyExtCongTTI_Metro, na.rm=T, .5333336, names=F) ~ 7,
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .5333336, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .4666669, names=F) ~ 8, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .4666669, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .4000002, names=F) ~ 9, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .4000002, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .3333335, names=F) ~ 10, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .3333335, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .2666668, names=F) ~ 11, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .2666668, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .2000001, names=F) ~ 12, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .2000001, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .1333334, names=F) ~ 13, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .1333334, names=F) & FwyExtCongTTI_Metro >= quantile(FwyExtCongTTI_Metro, na.rm=T, .0666667, names=F) ~ 14, 
        FwyExtCongTTI_Metro < quantile(FwyExtCongTTI_Metro, na.rm=T, .0666667, names=F) ~ 15),
      `ArtExtCongTTI_Metro_norm`=case_when(
        ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, 0.9333338, names=F) ~ 1, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .99333338, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .8666671, names=F) ~ 2, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .8666671, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .8000004, names=F) ~ 3,
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .8000004, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .7333337, names=F) ~ 4, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .7333337, names=F) & ArtExtCongTTI_Metro>= quantile(ArtExtCongTTI_Metro, na.rm=T, .666667, names=F) ~ 5,
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .666667, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .6000003, names=F) ~ 6, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .6000003, names=F) & ArtExtCongTTI_Metro>= quantile(ArtExtCongTTI_Metro, na.rm=T, .5333336, names=F) ~ 7,
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .5333336, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .4666669, names=F) ~ 8, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .4666669, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .4000002, names=F) ~ 9, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .4000002, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .3333335, names=F) ~ 10, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .3333335, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .2666668, names=F) ~ 11, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .2666668, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .2000001, names=F) ~ 12, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .2000001, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .1333334, names=F) ~ 13, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .1333334, names=F) & ArtExtCongTTI_Metro >= quantile(ArtExtCongTTI_Metro, na.rm=T, .0666667, names=F) ~ 14, 
        ArtExtCongTTI_Metro < quantile(ArtExtCongTTI_Metro, na.rm=T, .0666667, names=F) ~ 15),
      `LdvAveSpeed_Metro_norm`=case_when(
        LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, 0.9333338, names=F) ~ 15, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .99333338, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .8666671, names=F) ~ 14, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .8666671, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .8000004, names=F) ~ 13,
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .8000004, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .7333337, names=F) ~ 12, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .7333337, names=F) & LdvAveSpeed_Metro>= quantile(LdvAveSpeed_Metro, na.rm=T, .666667, names=F) ~ 11,
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .666667, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .6000003, names=F) ~ 10, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .6000003, names=F) & LdvAveSpeed_Metro>= quantile(LdvAveSpeed_Metro, na.rm=T, .5333336, names=F) ~ 9,
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .5333336, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .4666669, names=F) ~ 8, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .4666669, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .4000002, names=F) ~ 7, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .4000002, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .3333335, names=F) ~ 6, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .3333335, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .2666668, names=F) ~ 5, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .2666668, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .2000001, names=F) ~ 4, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .2000001, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .1333334, names=F) ~ 3, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .1333334, names=F) & LdvAveSpeed_Metro >= quantile(LdvAveSpeed_Metro, na.rm=T, .0666667, names=F) ~ 2, 
        LdvAveSpeed_Metro < quantile(LdvAveSpeed_Metro, na.rm=T, .0666667, names=F) ~ 1),
      `AutoFatalCrash_Metro_norm`=case_when(
        AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, 0.9333338, names=F) ~ 1, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .99333338, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .8666671, names=F) ~ 2, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .8666671, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .8000004, names=F) ~ 3,
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .8000004, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .7333337, names=F) ~ 4, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .7333337, names=F) & AutoFatalCrash_Metro>= quantile(AutoFatalCrash_Metro, na.rm=T, .666667, names=F) ~ 5,
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .666667, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .6000003, names=F) ~ 6, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .6000003, names=F) & AutoFatalCrash_Metro>= quantile(AutoFatalCrash_Metro, na.rm=T, .5333336, names=F) ~ 7,
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .5333336, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .4666669, names=F) ~ 8, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .4666669, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .4000002, names=F) ~ 9, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .4000002, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .3333335, names=F) ~ 10, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .3333335, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .2666668, names=F) ~ 11, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .2666668, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .2000001, names=F) ~ 12, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .2000001, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .1333334, names=F) ~ 13, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .1333334, names=F) & AutoFatalCrash_Metro >= quantile(AutoFatalCrash_Metro, na.rm=T, .0666667, names=F) ~ 14, 
        AutoFatalCrash_Metro < quantile(AutoFatalCrash_Metro, na.rm=T, .0666667, names=F) ~ 15),
      `AutoFatalUrban_norm`=case_when(
        AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, 0.9333338, names=F) ~ 1, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .99333338, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .8666671, names=F) ~ 2, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .8666671, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .8000004, names=F) ~ 3,
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .8000004, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .7333337, names=F) ~ 4, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .7333337, names=F) & AutoFatalUrban>= quantile(AutoFatalUrban, na.rm=T, .666667, names=F) ~ 5,
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .666667, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .6000003, names=F) ~ 6, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .6000003, names=F) & AutoFatalUrban>= quantile(AutoFatalUrban, na.rm=T, .5333336, names=F) ~ 7,
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .5333336, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .4666669, names=F) ~ 8, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .4666669, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .4000002, names=F) ~ 9, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .4000002, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .3333335, names=F) ~ 10, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .3333335, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .2666668, names=F) ~ 11, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .2666668, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .2000001, names=F) ~ 12, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .2000001, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .1333334, names=F) ~ 13, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .1333334, names=F) & AutoFatalUrban >= quantile(AutoFatalUrban, na.rm=T, .0666667, names=F) ~ 14, 
        AutoFatalUrban < quantile(AutoFatalUrban, na.rm=T, .0666667, names=F) ~ 15),
      `AutoInjuryUrban_norm`=case_when(
        AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, 0.9333338, names=F) ~ 1, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .99333338, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .8666671, names=F) ~ 2, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .8666671, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .8000004, names=F) ~ 3,
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .8000004, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .7333337, names=F) ~ 4, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .7333337, names=F) & AutoInjuryUrban>= quantile(AutoInjuryUrban, na.rm=T, .666667, names=F) ~ 5,
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .666667, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .6000003, names=F) ~ 6, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .6000003, names=F) & AutoInjuryUrban>= quantile(AutoInjuryUrban, na.rm=T, .5333336, names=F) ~ 7,
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .5333336, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .4666669, names=F) ~ 8, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .4666669, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .4000002, names=F) ~ 9, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .4000002, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .3333335, names=F) ~ 10, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .3333335, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .2666668, names=F) ~ 11, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .2666668, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .2000001, names=F) ~ 12, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .2000001, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .1333334, names=F) ~ 13, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .1333334, names=F) & AutoInjuryUrban >= quantile(AutoInjuryUrban, na.rm=T, .0666667, names=F) ~ 14, 
        AutoInjuryUrban < quantile(AutoInjuryUrban, na.rm=T, .0666667, names=F) ~ 15),
      `AutoFatalRural_norm`=case_when(
        AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, 0.9333338, names=F) ~ 1, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .99333338, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .8666671, names=F) ~ 2, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .8666671, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .8000004, names=F) ~ 3,
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .8000004, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .7333337, names=F) ~ 4, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .7333337, names=F) & AutoFatalRural>= quantile(AutoFatalRural, na.rm=T, .666667, names=F) ~ 5,
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .666667, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .6000003, names=F) ~ 6, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .6000003, names=F) & AutoFatalRural>= quantile(AutoFatalRural, na.rm=T, .5333336, names=F) ~ 7,
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .5333336, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .4666669, names=F) ~ 8, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .4666669, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .4000002, names=F) ~ 9, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .4000002, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .3333335, names=F) ~ 10, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .3333335, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .2666668, names=F) ~ 11, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .2666668, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .2000001, names=F) ~ 12, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .2000001, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .1333334, names=F) ~ 13, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .1333334, names=F) & AutoFatalRural >= quantile(AutoFatalRural, na.rm=T, .0666667, names=F) ~ 14, 
        AutoFatalRural < quantile(AutoFatalRural, na.rm=T, .0666667, names=F) ~ 15),
      `OwnCostProp_norm`=case_when(
        OwnCostProp >= quantile(OwnCostProp, na.rm=T, 0.9333338, names=F) ~ 1, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .99333338, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .8666671, names=F) ~ 2, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .8666671, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .8000004, names=F) ~ 3,
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .8000004, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .7333337, names=F) ~ 4, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .7333337, names=F) & OwnCostProp>= quantile(OwnCostProp, na.rm=T, .666667, names=F) ~ 5,
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .666667, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .6000003, names=F) ~ 6, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .6000003, names=F) & OwnCostProp>= quantile(OwnCostProp, na.rm=T, .5333336, names=F) ~ 7,
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .5333336, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .4666669, names=F) ~ 8, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .4666669, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .4000002, names=F) ~ 9, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .4000002, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .3333335, names=F) ~ 10, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .3333335, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .2666668, names=F) ~ 11, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .2666668, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .2000001, names=F) ~ 12, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .2000001, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .1333334, names=F) ~ 13, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .1333334, names=F) & OwnCostProp >= quantile(OwnCostProp, na.rm=T, .0666667, names=F) ~ 14, 
        OwnCostProp < quantile(OwnCostProp, na.rm=T, .0666667, names=F) ~ 15),
      `OwnCostPropHhLess25K_norm`=case_when(
         OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, 0.9333338, names=F) ~ 1, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .99333338, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .8666671, names=F) ~ 2, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .8666671, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .8000004, names=F) ~ 3,
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .8000004, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .7333337, names=F) ~ 4, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .7333337, names=F) &  OwnCostPropHhLess25K>= quantile( OwnCostPropHhLess25K, na.rm=T, .666667, names=F) ~ 5,
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .666667, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .6000003, names=F) ~ 6, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .6000003, names=F) &  OwnCostPropHhLess25K>= quantile( OwnCostPropHhLess25K, na.rm=T, .5333336, names=F) ~ 7,
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .5333336, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .4666669, names=F) ~ 8, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .4666669, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .4000002, names=F) ~ 9, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .4000002, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .3333335, names=F) ~ 10, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .3333335, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .2666668, names=F) ~ 11, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .2666668, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .2000001, names=F) ~ 12, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .2000001, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .1333334, names=F) ~ 13, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .1333334, names=F) &  OwnCostPropHhLess25K >= quantile( OwnCostPropHhLess25K, na.rm=T, .0666667, names=F) ~ 14, 
         OwnCostPropHhLess25K < quantile( OwnCostPropHhLess25K, na.rm=T, .0666667, names=F) ~ 15),
      `OwnCostPropHh25Kto50K_norm`=case_when(
       OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, 0.9333338, names=F) ~ 1, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .99333338, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .8666671, names=F) ~ 2, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .8666671, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .8000004, names=F) ~ 3,
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .8000004, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .7333337, names=F) ~ 4, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .7333337, names=F) & OwnCostPropHh25Kto50K>= quantile(OwnCostPropHh25Kto50K, na.rm=T, .666667, names=F) ~ 5,
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .666667, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .6000003, names=F) ~ 6, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .6000003, names=F) & OwnCostPropHh25Kto50K>= quantile(OwnCostPropHh25Kto50K, na.rm=T, .5333336, names=F) ~ 7,
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .5333336, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .4666669, names=F) ~ 8, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .4666669, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .4000002, names=F) ~ 9, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .4000002, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .3333335, names=F) ~ 10, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .3333335, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .2666668, names=F) ~ 11, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .2666668, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .2000001, names=F) ~ 12, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .2000001, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .1333334, names=F) ~ 13, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .1333334, names=F) & OwnCostPropHh25Kto50K >= quantile(OwnCostPropHh25Kto50K, na.rm=T, .0666667, names=F) ~ 14, 
       OwnCostPropHh25Kto50K < quantile(OwnCostPropHh25Kto50K, na.rm=T, .0666667, names=F) ~ 15),
      `BikeTripsPerCapita_norm`=case_when(
        BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, 0.9333338, names=F) ~ 15, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .99333338, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .8666671, names=F) ~ 14, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .8666671, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .8000004, names=F) ~ 13,
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .8000004, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .7333337, names=F) ~ 12, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .7333337, names=F) & BikeTripsPerCapita>= quantile(BikeTripsPerCapita, na.rm=T, .666667, names=F) ~ 11,
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .666667, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .6000003, names=F) ~ 10, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .6000003, names=F) & BikeTripsPerCapita>= quantile(BikeTripsPerCapita, na.rm=T, .5333336, names=F) ~ 9,
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .5333336, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .4666669, names=F) ~ 8, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .4666669, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .4000002, names=F) ~ 7, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .4000002, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .3333335, names=F) ~ 6, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .3333335, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .2666668, names=F) ~ 5, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .2666668, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .2000001, names=F) ~ 4, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .2000001, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .1333334, names=F) ~ 3, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .1333334, names=F) & BikeTripsPerCapita >= quantile(BikeTripsPerCapita, na.rm=T, .0666667, names=F) ~ 2, 
        BikeTripsPerCapita < quantile(BikeTripsPerCapita, na.rm=T, .0666667, names=F) ~ 1),
      `PresAdaptBudget_norm`=case_when(
        `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, 0.9333338, names=F) ~ 15, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .99333338, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .8666671, names=F) ~ 14, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .8666671, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .8000004, names=F) ~ 13,
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .8000004, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .7333337, names=F) ~ 12, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .7333337, names=F) & `PresAdaptBudget`>= quantile(`PresAdaptBudget`, na.rm=T, .666667, names=F) ~ 11,
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .666667, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .6000003, names=F) ~ 10, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .6000003, names=F) & `PresAdaptBudget`>= quantile(`PresAdaptBudget`, na.rm=T, .5333336, names=F) ~ 9,
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .5333336, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .4666669, names=F) ~ 8, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .4666669, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .4000002, names=F) ~ 7, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .4000002, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .3333335, names=F) ~ 6, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .3333335, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .2666668, names=F) ~ 5, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .2666668, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .2000001, names=F) ~ 4, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .2000001, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .1333334, names=F) ~ 3, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .1333334, names=F) & `PresAdaptBudget` >= quantile(`PresAdaptBudget`, na.rm=T, .0666667, names=F) ~ 2, 
        `PresAdaptBudget` < quantile(`PresAdaptBudget`, na.rm=T, .0666667, names=F) ~ 1)
    )
})
  results_new <- reactive({
    req(input$upload_file)
    df <- read_csv(input$upload_file$datapath) 
  })
goal_weighting <- reactive({
  data.frame(weighting=c(input$goal_pri_sust,input$goal_pri_mob,input$goal_pri_equity,input$goal_pri_stew,input$goal_pri_safety),
             goal=c("sust","mob","equity","stew","safety")
  ) %>% mutate(total=sum(weighting), norm=weighting/total)
  })
pres_obj_weighting <- reactive({
  data.frame(weighting=c(input$goal_pri_stew1),
             obj=c("goal")
  ) %>% mutate(total=sum(weighting), norm=weighting/total)
})
sfty_obj_weighting <- reactive({
  data.frame(weighting=c(input$obj_pri_sfty1),
             obj=c("sfty1")
  ) %>% mutate(total=sum(weighting), norm=weighting/total)
})
eq_obj_weighting <- reactive({
  data.frame(weighting=c(input$obj_pri_eq1),
             obj=c("eq1")
  ) %>% mutate(total=sum(weighting), norm=weighting/total)
})
sus_obj_weighting <- reactive({
  data.frame(weighting=c(input$obj_pri_sust1,input$obj_pri_sust2),
             obj=c("sus1","sus2")
  ) %>% mutate(total=sum(weighting), norm=weighting/total)
})
output$sus_obj_weighting<- renderDataTable({
  datatable(sus_obj_weighting())
})
mob_obj_weighting <- reactive({
  data.frame(weighting=c(input$obj_pri_mob1,input$obj_pri_mob2,input$obj_pri_mob4),
             obj=c("mob1","mob2","mob4")
  ) %>% mutate(total=sum(weighting), norm=weighting/total)
})
pres_pol_weighting <- reactive({
  data.frame(weighting=c(
                         input$pol_pri_PresAdapt_2022),
             policy=c("Goal")) %>% mutate(total=sum(weighting), norm=weighting/total)
})
sfty_pol_weighting <- reactive({
  data.frame(weighting=c(
                         input$pol_pri_AutoFatalCrash_Metro,
                         input$pol_pri_AutoFatalUrban,
                         input$pol_pri_AutoInjuryUrban,
                         input$pol_pri_AutoFatalRural),
             policy=c("Sfty11","Sfty12","Sfty13","Sfty14")) %>% mutate(total=sum(weighting), norm=weighting/total)
})
eq_pol_weighting <- reactive({
  data.frame(weighting=c(input$pol_pri_OwnCostPropHhLess25K,
                         input$pol_pri_OwnCostProp,
                         input$pol_pri_OwnCostPropHh25Kto50K
                        ),
             policy=c("Eq11","Eq12","Eq13")) %>% mutate(total=sum(weighting), norm=weighting/total)
})
sus1_pol_weighting <- reactive({
  data.frame(weighting=c(
                         input$pol_pri_TotalCO2e,
                         input$pol_pri_HouseholdCO2ePerPrsn,
                         input$pol_pri_HouseholdCO2ePerMileRural),
             policy=c("Sus11","Sus12","Sus13")) %>% mutate(total=sum(weighting), norm=weighting/total)
})
output$sus1_pol_weighting <-renderDataTable({
  datatable(sus1_pol_weighting())
})
sus2_pol_weighting <- reactive({
  data.frame(weighting=c(
                         input$pol_pri_BusCO2eRate_Metro,
                         input$pol_pri_HvyTrkAveUrbanCO2eRate_Metro,
                         input$pol_pri_ElecDvmtProp),
             policy=c("Sus21","Sus22","Sus23")) %>% mutate(total=sum(weighting), norm=weighting/total)
})
output$sus2_pol_weighting <-renderDataTable({
  datatable(sus2_pol_weighting())
})
mob1_pol_weighting <- reactive({
  data.frame(weighting=c(
                         input$pol_pri_BikePMTPerPrsnUrban,
                         input$pol_pri_WalkPMTPerPrsnUrban,
                         input$pol_pri_TransitTripsPerCapita,
                         input$pol_pri_TransitPMTPerPrsnUrban,
                         input$pol_pri_BikeTripsPerCapita,
                         input$pol_pri_WalkTripsPerCapita
                       ),
             policy=c("Mob11","Mob12","Mob13","Mob14","Mob15","Mob17")) %>% mutate(total=sum(weighting), norm=weighting/total)
})
mob2_pol_weighting <- reactive({
  data.frame(weighting=c(
                         input$pol_pri_HouseholdDvmtPerPrsn,
                         input$pol_pri_TotalDvmtPerPrsn),
             policy=c("Mob21","Mob22")) %>% mutate(total=sum(weighting), norm=weighting/total)
})
mob3_pol_weighting <- reactive({
  data.frame(weighting=c(
                         input$pol_pri_ArtDvmtPropExtCong_SalemKeizer,
                         input$pol_pri_FwyExtCongTTI_Metro,
                         input$pol_pri_ArtExtCongTTI_Metro),
             policy=c("Mob41","Mob42","Mob43")) %>% mutate(total=sum(weighting), norm=weighting/total)
})
output$pol_weight_table<-renderDataTable({
  datatable(pol_weighting())
})
sum_results <- reactive({
  if(input$goal_pri_sust == 0 & input$goal_pri_mob == 0 & input$goal_pri_equity==0 & input$goal_pri_safety==0 & input$goal_pri_stew ==1){
    results()  %>% mutate(
      CARSVCAVAILSCEN=case_when(
        CARSVCAVAILSCEN =="low" ~ 1,
        CARSVCAVAILSCEN =="mid" ~ 2,
        CARSVCAVAILSCEN =="high" ~ 3
      ),
      POWERTRAINSCEN=case_when(
        POWERTRAINSCEN =="LOWEV" ~ 1,
        POWERTRAINSCEN =="MEDHH" ~ 2
      ),
      score= rowSums(
        cbind(
          (OwnCostPropHhLess25K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                          eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                          eq_pol_weighting() %>% filter(policy=="Eq11") %>% pull(norm))),
          (OwnCostProp_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                 eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                 eq_pol_weighting() %>% filter(policy=="Eq12") %>% pull(norm))),
          (OwnCostPropHh25Kto50K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                           eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                           eq_pol_weighting() %>% filter(policy=="Eq13") %>% pull(norm))),
          (BikePMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                         mob1_pol_weighting() %>% filter(policy=="Mob11") %>% pull(norm))),
          (WalkPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                         mob1_pol_weighting() %>% filter(policy=="Mob12") %>% pull(norm))),
          (TransitTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                           mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                           mob1_pol_weighting() %>% filter(policy=="Mob13") %>% pull(norm))),
          (TransitPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                            mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                            mob1_pol_weighting() %>% filter(policy=="Mob14") %>% pull(norm))),
          (BikeTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                        mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                        mob1_pol_weighting() %>% filter(policy=="Mob15") %>% pull(norm))),
          (WalkTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                        mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                        mob1_pol_weighting() %>% filter(policy=="Mob17") %>% pull(norm))),
          (HouseholdDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                          mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
                                          mob2_pol_weighting() %>% filter(policy=="Mob21") %>% pull(norm))),
          (TotalDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                      mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
                                      mob2_pol_weighting() %>% filter(policy=="Mob22") %>% pull(norm))) + 
            (ArtDvmtPropExtCong_SalemKeizer_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                                      mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                                      mob3_pol_weighting() %>% filter(policy=="Mob41") %>% pull(norm))),
          (FwyExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                         mob3_pol_weighting() %>% filter(policy=="Mob42") %>% pull(norm))), 
          (ArtExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                         mob3_pol_weighting() %>% filter(policy=="Mob43") %>% pull(norm))) ,
          (AutoFatalCrash_Metro_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                          sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                          sfty_pol_weighting() %>% filter(policy=="Sfty11") %>% pull(norm))) , 
          (AutoFatalUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                    sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                    sfty_pol_weighting() %>% filter(policy=="Sfty12") %>% pull(norm))) ,
          (AutoInjuryUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                     sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                     sfty_pol_weighting() %>% filter(policy=="Sfty13") %>% pull(norm))) ,
          (AutoFatalRural_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                    sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                    sfty_pol_weighting() %>% filter(policy=="Sfty14") %>% pull(norm))) , 
          (TotalCO2e_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                               sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                               sus1_pol_weighting() %>% filter(policy=="Sus11") %>% pull(norm))) ,
          (HouseholdCO2ePerPrsn_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                          sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                          sus1_pol_weighting() %>% filter(policy=="Sus12") %>% pull(norm))) , 
          (HouseholdCO2ePerMileRural_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                               sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                               sus1_pol_weighting() %>% filter(policy=="Sus13") %>% pull(norm))),
          (BusCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                       sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                       sus2_pol_weighting() %>% filter(policy=="Sus21") %>% pull(norm))) ,
          (HvyTrkAveUrbanCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                                  sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                                  sus2_pol_weighting() %>% filter(policy=="Sus22") %>% pull(norm))),
          (`PresAdaptBudget_norm` * (goal_weighting() %>% filter(goal=="stew") %>% pull(norm) * 
                                       pres_obj_weighting() %>% filter(obj=="goal") %>% pull(norm) * 
                                       pres_pol_weighting() %>% filter(policy=="Goal") %>% pull(norm))),
          (ElecDvmtProp_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                                  sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                                  sus2_pol_weighting() %>% filter(policy=="Sus23") %>% pull(norm)))
        ))
    ) %>%
      arrange(desc(score), desc(PresAdaptBudget)) %>% 
      slice(1:input$no_exp) %>%
      summarise(CARCHARGEAVAILSCEN=mean(CARCHARGEAVAILSCEN,na.rm=T),
                INTDENSITYSCEN=mean(INTDENSITYSCEN,na.rm=T),
                LANEMILESCEN=mean(LANEMILESCEN,na.rm=T),
                CARSVCAVAILSCEN=mean(CARSVCAVAILSCEN,na.rm=T),
                POWERTRAINSCEN=mean(POWERTRAINSCEN, na.rm=T),
                TRANSITSCEN=mean(TRANSITSCEN,na.rm=T),
                LUDENSITYMIX=mean(LUDENSITYMIX,na.rm=T),
                OPSDEPLOYSCEN=mean(OPSDEPLOYSCEN,na.rm=T),
                SHDCARSVCOCCUPRATE=mean(SHDCARSVCOCCUPRATE,na.rm=T),
                SOVDIVIVERTSCEN=mean(SOVDIVIVERTSCEN,na.rm=T),
                VehOwnTax=mean(VehOwnTax,na.rm=T),
                TAXSCEN=mean(TAXSCEN,na.rm=T),
                TotalFuelTax=mean(TotalFuelTax,na.rm=T),
                PerMileCharge  =mean(PerMileCharge ,na.rm=T),
                TDMINVESTMENTSCEN=mean(TDMINVESTMENTSCEN,na.rm=T),
                TRANSITSERVICESCEN=mean(TRANSITSERVICESCEN,na.rm=T),
                TotalBudget=mean(TotalBudget,na.rm=T),
                PresAdaptBudget=mean(PresAdaptBudget,na.rm=T),
                FwyExtCongTTI_Metro=mean(FwyExtCongTTI_Metro,na.rm=T),
                ArtExtCongTTI_Metro=mean(ArtExtCongTTI_Metro,na.rm=T),
                LdvAveSpeed_Metro=mean(LdvAveSpeed_Metro,na.rm=T),
                OwnCostPropHhLess25K=mean(OwnCostPropHhLess25K,na.rm=T),
                AutoFatalUrban=mean(AutoFatalUrban,na.rm=T)
      ) %>%
      pivot_longer(cols=everything()) %>%
      mutate(value=comma(round(value,3)))   
  }else{
  results()  %>% mutate(
    CARSVCAVAILSCEN=case_when(
      CARSVCAVAILSCEN =="low" ~ 1,
      CARSVCAVAILSCEN =="mid" ~ 2,
      CARSVCAVAILSCEN =="high" ~ 3
    ),
    POWERTRAINSCEN=case_when(
      POWERTRAINSCEN =="LOWEV" ~ 1,
      POWERTRAINSCEN =="MEDHH" ~ 2
    ),
    score= rowSums(
      cbind(
      (OwnCostPropHhLess25K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                      eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                      eq_pol_weighting() %>% filter(policy=="Eq11") %>% pull(norm))),
      (OwnCostProp_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                             eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                             eq_pol_weighting() %>% filter(policy=="Eq12") %>% pull(norm))),
      (OwnCostPropHh25Kto50K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                       eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                               eq_pol_weighting() %>% filter(policy=="Eq13") %>% pull(norm))),
      (BikePMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                       mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                     mob1_pol_weighting() %>% filter(policy=="Mob11") %>% pull(norm))),
      (WalkPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                     mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                     mob1_pol_weighting() %>% filter(policy=="Mob12") %>% pull(norm))),
      (TransitTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                       mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                       mob1_pol_weighting() %>% filter(policy=="Mob13") %>% pull(norm))),
      (TransitPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                        mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                        mob1_pol_weighting() %>% filter(policy=="Mob14") %>% pull(norm))),
      (BikeTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                    mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                    mob1_pol_weighting() %>% filter(policy=="Mob15") %>% pull(norm))),
      (WalkTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                    mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                    mob1_pol_weighting() %>% filter(policy=="Mob17") %>% pull(norm))),
      (HouseholdDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                      mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
                                      mob2_pol_weighting() %>% filter(policy=="Mob21") %>% pull(norm))),
      (TotalDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                  mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
                                  mob2_pol_weighting() %>% filter(policy=="Mob22") %>% pull(norm))) + 
      (ArtDvmtPropExtCong_SalemKeizer_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                                mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                                mob3_pol_weighting() %>% filter(policy=="Mob41") %>% pull(norm))),
      (FwyExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                     mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                     mob3_pol_weighting() %>% filter(policy=="Mob42") %>% pull(norm))), 
      (ArtExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                     mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                     mob3_pol_weighting() %>% filter(policy=="Mob43") %>% pull(norm))) ,
      (AutoFatalCrash_Metro_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                      sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                      sfty_pol_weighting() %>% filter(policy=="Sfty11") %>% pull(norm))) , 
      (AutoFatalUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                sfty_pol_weighting() %>% filter(policy=="Sfty12") %>% pull(norm))) ,
      (AutoInjuryUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                 sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                 sfty_pol_weighting() %>% filter(policy=="Sfty13") %>% pull(norm))) ,
      (AutoFatalRural_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                sfty_pol_weighting() %>% filter(policy=="Sfty14") %>% pull(norm))) , 
       (TotalCO2e_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                            sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                            sus1_pol_weighting() %>% filter(policy=="Sus11") %>% pull(norm))) ,
      (HouseholdCO2ePerPrsn_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                      sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                      sus1_pol_weighting() %>% filter(policy=="Sus12") %>% pull(norm))) , 
      (HouseholdCO2ePerMileRural_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                           sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                           sus1_pol_weighting() %>% filter(policy=="Sus13") %>% pull(norm))),
      (BusCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                   sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                   sus2_pol_weighting() %>% filter(policy=="Sus21") %>% pull(norm))) ,
      (HvyTrkAveUrbanCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                              sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                              sus2_pol_weighting() %>% filter(policy=="Sus22") %>% pull(norm))),
      (`PresAdaptBudget_norm` * (goal_weighting() %>% filter(goal=="stew") %>% pull(norm) * 
                                  pres_obj_weighting() %>% filter(obj=="goal") %>% pull(norm) * 
                                  pres_pol_weighting() %>% filter(policy=="Goal") %>% pull(norm))),
      (ElecDvmtProp_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                              sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                              sus2_pol_weighting() %>% filter(policy=="Sus23") %>% pull(norm)))
      ))
     ) %>%
    arrange(desc(score)) %>% 
    slice(1:input$no_exp) %>%
      summarise(CARCHARGEAVAILSCEN=mean(CARCHARGEAVAILSCEN,na.rm=T),
                INTDENSITYSCEN=mean(INTDENSITYSCEN,na.rm=T),
                LANEMILESCEN=mean(LANEMILESCEN,na.rm=T),
                CARSVCAVAILSCEN=mean(CARSVCAVAILSCEN,na.rm=T),
                POWERTRAINSCEN=mean(POWERTRAINSCEN, na.rm=T),
                TRANSITSCEN=mean(TRANSITSCEN,na.rm=T),
                LUDENSITYMIX=mean(LUDENSITYMIX,na.rm=T),
                OPSDEPLOYSCEN=mean(OPSDEPLOYSCEN,na.rm=T),
                SHDCARSVCOCCUPRATE=mean(SHDCARSVCOCCUPRATE,na.rm=T),
                SOVDIVIVERTSCEN=mean(SOVDIVIVERTSCEN,na.rm=T),
                VehOwnTax=mean(VehOwnTax,na.rm=T),
                TAXSCEN=mean(TAXSCEN,na.rm=T),
                TotalFuelTax=mean(TotalFuelTax,na.rm=T),
                PerMileCharge  =mean(PerMileCharge ,na.rm=T),
                TDMINVESTMENTSCEN=mean(TDMINVESTMENTSCEN,na.rm=T),
                TRANSITSERVICESCEN=mean(TRANSITSERVICESCEN,na.rm=T),
                TotalBudget=mean(TotalBudget,na.rm=T),
                PresAdaptBudget=mean(PresAdaptBudget,na.rm=T),
                FwyExtCongTTI_Metro=mean(FwyExtCongTTI_Metro,na.rm=T),
                ArtExtCongTTI_Metro=mean(ArtExtCongTTI_Metro,na.rm=T),
                LdvAveSpeed_Metro=mean(LdvAveSpeed_Metro,na.rm=T),
                OwnCostPropHhLess25K=mean(OwnCostPropHhLess25K,na.rm=T),
                AutoFatalUrban=mean(AutoFatalUrban,na.rm=T)
              ) %>%
    pivot_longer(cols=everything()) %>%
    mutate(value=comma(round(value,3)))
}
})
output$goal_weighting <- renderDataTable({
  datatable(goal_weighting())
})
un_sum_results <- reactive({
  if(input$goal_pri_sust == 0 & input$goal_pri_mob == 0 & input$goal_pri_equity==0 & input$goal_pri_safety==0 & input$goal_pri_stew ==1){
    results()  %>% mutate(
      score= rowSums(
        cbind(
          (OwnCostPropHhLess25K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                          eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                          eq_pol_weighting() %>% filter(policy=="Eq11") %>% pull(norm))),
          (OwnCostProp_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                 eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                 eq_pol_weighting() %>% filter(policy=="Eq12") %>% pull(norm))),
          (OwnCostPropHh25Kto50K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                           eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                           eq_pol_weighting() %>% filter(policy=="Eq13") %>% pull(norm))),
          (BikePMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                         mob1_pol_weighting() %>% filter(policy=="Mob11") %>% pull(norm))),
          (WalkPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                         mob1_pol_weighting() %>% filter(policy=="Mob12") %>% pull(norm))),
          (TransitTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                           mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                           mob1_pol_weighting() %>% filter(policy=="Mob13") %>% pull(norm))),
          (TransitPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                            mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                            mob1_pol_weighting() %>% filter(policy=="Mob14") %>% pull(norm))),
          (BikeTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                        mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                        mob1_pol_weighting() %>% filter(policy=="Mob15") %>% pull(norm))),
          (WalkTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                        mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                        mob1_pol_weighting() %>% filter(policy=="Mob17") %>% pull(norm))),
          (HouseholdDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                          mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
                                          mob2_pol_weighting() %>% filter(policy=="Mob21") %>% pull(norm))),
          (TotalDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                      mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
                                      mob2_pol_weighting() %>% filter(policy=="Mob22") %>% pull(norm))) + 
            (ArtDvmtPropExtCong_SalemKeizer_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                                      mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                                      mob3_pol_weighting() %>% filter(policy=="Mob41") %>% pull(norm))),
          (FwyExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                         mob3_pol_weighting() %>% filter(policy=="Mob42") %>% pull(norm))), 
          (ArtExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                         mob3_pol_weighting() %>% filter(policy=="Mob43") %>% pull(norm))) ,
          (AutoFatalCrash_Metro_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                          sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                          sfty_pol_weighting() %>% filter(policy=="Sfty11") %>% pull(norm))) , 
          (AutoFatalUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                    sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                    sfty_pol_weighting() %>% filter(policy=="Sfty12") %>% pull(norm))) ,
          (AutoInjuryUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                     sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                     sfty_pol_weighting() %>% filter(policy=="Sfty13") %>% pull(norm))) ,
          (AutoFatalRural_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                    sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                    sfty_pol_weighting() %>% filter(policy=="Sfty14") %>% pull(norm))) , 
          (TotalCO2e_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                               sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                               sus1_pol_weighting() %>% filter(policy=="Sus11") %>% pull(norm))) ,
          (HouseholdCO2ePerPrsn_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                          sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                          sus1_pol_weighting() %>% filter(policy=="Sus12") %>% pull(norm))) , 
          (HouseholdCO2ePerMileRural_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                               sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                               sus1_pol_weighting() %>% filter(policy=="Sus13") %>% pull(norm))),
          (BusCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                       sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                       sus2_pol_weighting() %>% filter(policy=="Sus21") %>% pull(norm))) ,
          (HvyTrkAveUrbanCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                                  sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                                  sus2_pol_weighting() %>% filter(policy=="Sus22") %>% pull(norm))),
          (`PresAdaptBudget_norm` * (goal_weighting() %>% filter(goal=="stew") %>% pull(norm) * 
                                       pres_obj_weighting() %>% filter(obj=="goal") %>% pull(norm) * 
                                       pres_pol_weighting() %>% filter(policy=="Goal") %>% pull(norm))),
          (ElecDvmtProp_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                  sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                  sus2_pol_weighting() %>% filter(policy=="Sus23") %>% pull(norm)))
        ))
    ) %>%
      arrange(desc(score), desc(PresAdaptBudget)) %>%
      mutate(score=round(score,2))
  }else{
    results()  %>% mutate(
      score= rowSums(
        cbind(
          (OwnCostPropHhLess25K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                          eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                          eq_pol_weighting() %>% filter(policy=="Eq11") %>% pull(norm))),
          (OwnCostProp_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                 eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                 eq_pol_weighting() %>% filter(policy=="Eq12") %>% pull(norm))),
          (OwnCostPropHh25Kto50K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
                                           eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
                                           eq_pol_weighting() %>% filter(policy=="Eq13") %>% pull(norm))),
          (BikePMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                         mob1_pol_weighting() %>% filter(policy=="Mob11") %>% pull(norm))),
          (WalkPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                         mob1_pol_weighting() %>% filter(policy=="Mob12") %>% pull(norm))),
          (TransitTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                           mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                           mob1_pol_weighting() %>% filter(policy=="Mob13") %>% pull(norm))),
          (TransitPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                            mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                            mob1_pol_weighting() %>% filter(policy=="Mob14") %>% pull(norm))),
          (BikeTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                        mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                        mob1_pol_weighting() %>% filter(policy=="Mob15") %>% pull(norm))),
          (WalkTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                        mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
                                        mob1_pol_weighting() %>% filter(policy=="Mob17") %>% pull(norm))),
          (HouseholdDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                          mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
                                          mob2_pol_weighting() %>% filter(policy=="Mob21") %>% pull(norm))),
          (TotalDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                      mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
                                      mob2_pol_weighting() %>% filter(policy=="Mob22") %>% pull(norm))) + 
            (ArtDvmtPropExtCong_SalemKeizer_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                                      mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                                      mob3_pol_weighting() %>% filter(policy=="Mob41") %>% pull(norm))),
          (FwyExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                         mob3_pol_weighting() %>% filter(policy=="Mob42") %>% pull(norm))), 
          (ArtExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
                                         mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
                                         mob3_pol_weighting() %>% filter(policy=="Mob43") %>% pull(norm))) ,
          (AutoFatalCrash_Metro_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                          sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                          sfty_pol_weighting() %>% filter(policy=="Sfty11") %>% pull(norm))) , 
          (AutoFatalUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                    sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                    sfty_pol_weighting() %>% filter(policy=="Sfty12") %>% pull(norm))) ,
          (AutoInjuryUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                     sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                     sfty_pol_weighting() %>% filter(policy=="Sfty13") %>% pull(norm))) ,
          (AutoFatalRural_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
                                    sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
                                    sfty_pol_weighting() %>% filter(policy=="Sfty14") %>% pull(norm))) , 
          (TotalCO2e_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                               sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                               sus1_pol_weighting() %>% filter(policy=="Sus11") %>% pull(norm))) ,
          (HouseholdCO2ePerPrsn_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                          sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                          sus1_pol_weighting() %>% filter(policy=="Sus12") %>% pull(norm))) , 
          (HouseholdCO2ePerMileRural_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                               sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                               sus1_pol_weighting() %>% filter(policy=="Sus13") %>% pull(norm))),
          (BusCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                       sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                       sus2_pol_weighting() %>% filter(policy=="Sus21") %>% pull(norm))) ,
          (HvyTrkAveUrbanCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                                  sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                                  sus2_pol_weighting() %>% filter(policy=="Sus22") %>% pull(norm))),
          (`PresAdaptBudget_norm` * (goal_weighting() %>% filter(goal=="stew") %>% pull(norm) * 
                                       pres_obj_weighting() %>% filter(obj=="goal") %>% pull(norm) * 
                                       pres_pol_weighting() %>% filter(policy=="Goal") %>% pull(norm))),
          (ElecDvmtProp_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                  sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
                                  sus2_pol_weighting() %>% filter(policy=="Sus23") %>% pull(norm)))
        ))
    ) %>%
      arrange(desc(score)) %>%
      mutate(score=round(score,2))
  }
})
test_results <- reactive({
  results() %>% mutate(
    score= 
      (TotalCO2e_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                           sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                           sus1_pol_weighting() %>% filter(policy=="Sus11") %>% pull(norm))) + 
      (HouseholdCO2ePerPrsn_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
                                      sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
                                      sus1_pol_weighting() %>% filter(policy=="Sus12") %>% pull(norm))) + 
      (HouseholdCO2ePerMileRural_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) ))
  ) %>% pull(score)
})
output$test<-renderText({
  test_results()
})
output$un_sum_results<-renderDataTable({
  datatable(un_sum_results(), rownames = F,options = list(pageLength = 10,scrollX = TRUE))
}) 
output$sum_results<-renderDataTable({
  datatable(sum_results(), rownames = F,options = list(pageLength = 20, dom="t"))
})  
output$results  <-renderDataTable({
    datatable(results(), rownames = F,options = list(pageLength = 30,scrollX = TRUE))
  })
output$summary  <-renderDataTable({
  datatable(summary, rownames = F,options = list(pageLength = 10))
})
  base_values <- reactive({
    hh1_group %>% filter(Azone %in%input$input_azone & 
                           region %in%input$input_region & 
                          # HhSize %in%input$input_hh & 
                           odot_comp %in%input$input_lifecycle & 
                           income_cat %in%input$input_income & 
                           workers1 %in%input$input_workers & 
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           car_sufficiency %in%input$input_car_sufficiency) %>% summarise(DvmtPerCapita=round(mean(DvmtPerCapita, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTripsPerCapita=round(mean(VehicleTripsPerCapita, na.rm=T),2),
                                                                          TransitTripsPerCapita=round(mean(TransitTripsPerCapita, na.rm=T),2),
                                                                          BikeTripsPerCapita=round(mean(BikeTripsPerCapita, na.rm=T),2),
                                                                          WalkTripsPerCapita=round(mean(WalkTripsPerCapita, na.rm=T),2),
                                                                          cost_per_income=mean(cost_per_income, na.rm=T))
  })
  output$records111 <-renderValueBox({
    valueBox(comma(hh1_group %>% filter(Azone %in%input$input_azone & 
                                          region %in%input$input_region & 
                                          # HhSize %in%input$input_hh & 
                                          odot_comp %in%input$input_lifecycle & 
                                          income_cat %in%input$input_income & 
                                          workers1 %in%input$input_workers & 
                                          HouseType %in%input$input_house_type &
                                          LocType %in%input$input_loc_type &
                                          car_sufficiency %in%input$input_car_sufficiency) %>%
                     summarise(households=sum(households,na.rm=T)) %>% pull()), "Number of HH Selected", width=12, color="navy")
  })
  # scenario 1
  output$DvmtPerCapita_1 <-renderValueBox({
    valueBox(base_values() %>% pull(DvmtPerCapita), "Daily VMT Per Capita", width=12, color="navy")
  })
  output$gge_1 <-renderValueBox({
    valueBox(base_values() %>% pull(DailyGGE), "Daily GHG", width=12, color="navy")
  })
  output$cost_1 <-renderValueBox({
    valueBox(base_values() %>% pull(AveVehCostPM), "Vehicle Cost", width=12, color="navy")
  })
  output$co2_1 <-renderValueBox({
    valueBox(base_values() %>% pull(DailyCO2e), "Daily CO2", width=12, color="navy")
  })
  output$veh_trips_1 <-renderValueBox({
    valueBox(base_values() %>% pull(VehicleTripsPerCapita), "Vehicle Trips Per Capita", width=12, color="navy")
  })
  output$tran_trips_1 <-renderValueBox({
    valueBox(base_values() %>% pull(TransitTripsPerCapita), "Transit Trips Per Capita", width=12, color="navy")
  })
  output$walk_trips_1 <-renderValueBox({
    valueBox(base_values() %>% pull(WalkTripsPerCapita), "Walk Trips Per Capita", width=12, color="navy")
  })
  output$bike_trips_1 <-renderValueBox({
    valueBox(base_values() %>% pull(BikeTripsPerCapita), "Bike Trips Per Capita", width=12, color="navy")
  })
  output$cost_per_income_1 <-renderValueBox({
    valueBox(base_values() %>% pull(cost_per_income), "Travel Cost / Income", width=12, color="navy")
  })
  # scenario 2
  values2<- reactive({
    hh2_group %>% filter(Azone %in%input$input_azone & 
                           region %in%input$input_region & 
                           #HhSize %in%input$input_hh & 
                           odot_comp %in%input$input_lifecycle & 
                           income_cat %in%input$input_income & 
                           workers1 %in%input$input_workers & 
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           car_sufficiency %in%input$input_car_sufficiency) %>% summarise(DvmtPerCapita=round(mean(DvmtPerCapita, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTripsPerCapita=round(mean(VehicleTripsPerCapita, na.rm=T),2),
                                                                          TransitTripsPerCapita=round(mean(TransitTripsPerCapita, na.rm=T),2),
                                                                          BikeTripsPerCapita=round(mean(BikeTripsPerCapita, na.rm=T),2),
                                                                          WalkTripsPerCapita=round(mean(WalkTripsPerCapita, na.rm=T),2),
                                                                          cost_per_income=mean(cost_per_income, na.rm=T))
  })
 DvmtPerCapita_2<- reactive({
    (values2() %>% pull(DvmtPerCapita)-base_values() %>% pull(DvmtPerCapita))/base_values() %>% pull(DvmtPerCapita)
  })
 DvmtPerCapita_2_color <- reactive({
   if( DvmtPerCapita_2() < 0){
     'red'
   } else if(DvmtPerCapita_2() > 0){
     'navy'
   }
 })
  output$DvmtPerCapita_2 <-renderValueBox({
    valueBox(percent(DvmtPerCapita_2()), "Daily VMT Per Capita", width=12, color="navy")
  })
  gge_2<- reactive({
    (values2() %>% pull(DailyGGE)-base_values() %>% pull(DailyGGE))/base_values() %>% pull(DailyGGE)
  })
  gg3_2_color <- reactive({
    if( gge_2() < 0){
      'red'
    } else if(gge_2() > 0){
      'navy'
    }
  })
  output$gge_2 <-renderValueBox({
    valueBox(percent(gge_2()), "Daily GHG", width=12, color="navy")
  })
  output$cost_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(AveVehCostPM)-base_values() %>% pull(AveVehCostPM))/base_values() %>% pull(AveVehCostPM)), "Vehicle Cost", width=12, color="navy")
  })
  output$co2_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(DailyCO2e)-base_values() %>% pull(DailyCO2e))/base_values() %>% pull(DailyCO2e)), "Daily CO2", width=12, color="navy")
  })
  output$veh_trips_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(VehicleTripsPerCapita)-base_values() %>% pull(VehicleTripsPerCapita))/base_values() %>% pull(VehicleTripsPerCapita), accuracy = 0.01), "Vehicle Trips Per Capita", width=12, color="navy")
  })
  output$tran_trips_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(TransitTripsPerCapita)-base_values() %>% pull(TransitTripsPerCapita))/base_values() %>% pull(TransitTripsPerCapita)), "Transit Trips Per Capita", width=12, color="navy")
  })
  output$walk_trips_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(WalkTripsPerCapita)-base_values() %>% pull(WalkTripsPerCapita))/base_values() %>% pull(WalkTripsPerCapita)), "Walk Trips Per Capita", width=12, color="navy")
  })
  output$bike_trips_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(BikeTripsPerCapita)-base_values() %>% pull(BikeTripsPerCapita))/base_values() %>% pull(BikeTripsPerCapita)), "Bike Trips Per Capita", width=12, color="navy")
  })
  output$records2 <-renderValueBox({
    valueBox(comma(hh2_group %>% filter(Azone %in%input$input_azone & 
                                          region %in%input$input_region & 
                            #HhSize %in%input$input_hh & 
                            odot_comp %in%input$input_lifecycle & 
                            income_cat %in%input$input_income & 
                            workers1 %in%input$input_workers & 
                            HouseType %in%input$input_house_type &
                            LocType %in%input$input_loc_type &
                            car_sufficiency %in%input$input_car_sufficiency) %>%
                     summarise(households=sum(households,na.rm=T)) %>% pull()), "Number of HH Selected", width=12, color="navy")
  })
  # scenario 3
  values3<- reactive({
    hh3_group %>% filter(Azone %in%input$input_azone & 
                           region %in%input$input_region & 
                           #HhSize %in%input$input_hh & 
                           odot_comp %in%input$input_lifecycle & 
                           income_cat %in%input$input_income & 
                           workers1 %in%input$input_workers & 
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           car_sufficiency %in%input$input_car_sufficiency) %>% summarise(DvmtPerCapita=round(mean(DvmtPerCapita, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTripsPerCapita=round(mean(VehicleTripsPerCapita, na.rm=T),2),
                                                                          TransitTripsPerCapita=round(mean(TransitTripsPerCapita, na.rm=T),2),
                                                                          BikeTripsPerCapita=round(mean(BikeTripsPerCapita, na.rm=T),2),
                                                                          WalkTripsPerCapita=round(mean(WalkTripsPerCapita, na.rm=T),2),
                                                                          cost_per_income=mean(cost_per_income, na.rm=T))
  })
  output$DvmtPerCapita_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(DvmtPerCapita)-base_values() %>% pull(DvmtPerCapita))/base_values() %>% pull(DvmtPerCapita)), "Daily VMT Per Capita", width=12, color="navy")
  })
  output$gge_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(DailyGGE)-base_values() %>% pull(DailyGGE))/base_values() %>% pull(DailyGGE)), "Daily GHG", width=12, color="navy")
  })
  output$cost_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(AveVehCostPM)-base_values() %>% pull(AveVehCostPM))/base_values() %>% pull(AveVehCostPM)), "Vehicle Cost", width=12, color="navy")
  })
  output$co2_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(DailyCO2e)-base_values() %>% pull(DailyCO2e))/base_values() %>% pull(DailyCO2e)), "Daily CO2", width=12, color="navy")
  })
  output$veh_trips_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(VehicleTripsPerCapita)-base_values() %>% pull(VehicleTripsPerCapita))/base_values() %>% pull(VehicleTripsPerCapita), accuracy = 0.01), "Vehicle Trips Per Capita", width=12, color="navy")
  })
  output$tran_trips_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(TransitTripsPerCapita)-base_values() %>% pull(TransitTripsPerCapita))/base_values() %>% pull(TransitTripsPerCapita)), "Transit Trips Per Capita", width=12, color="navy")
  })
  output$walk_trips_3<-renderValueBox({
    valueBox(percent((values3() %>% pull(WalkTripsPerCapita)-base_values() %>% pull(WalkTripsPerCapita))/base_values() %>% pull(WalkTripsPerCapita)), "Walk Trips Per Capita", width=12, color="navy")
  })
  output$bike_trips_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(BikeTripsPerCapita)-base_values() %>% pull(BikeTripsPerCapita))/base_values() %>% pull(BikeTripsPerCapita)), "Bike Trips Per Capita", width=12, color="navy")
  })
  output$records3 <-renderValueBox({
    valueBox(comma(hh3_group %>% filter(Azone %in%input$input_azone & 
                                          region %in%input$input_region & 
                                         # HhSize %in%input$input_hh & 
                                          odot_comp %in%input$input_lifecycle & 
                                          income_cat %in%input$input_income & 
                                          workers1 %in%input$input_workers & 
                                          HouseType %in%input$input_house_type &
                                          LocType %in%input$input_loc_type &
                                          car_sufficiency %in%input$input_car_sufficiency) %>%
                     summarise(households=sum(households,na.rm=T)) %>% pull()
             ), "Number of HH Selected", width=12, color="navy")
  })
  # scenario 4
  values4<- reactive({
    hh4_group %>% filter(Azone %in%input$input_azone & 
                           region %in%input$input_region & 
                          # HhSize %in%input$input_hh & 
                           odot_comp %in%input$input_lifecycle & 
                           income_cat %in%input$input_income & 
                           workers1 %in%input$input_workers & 
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           car_sufficiency %in%input$input_car_sufficiency) %>% summarise(DvmtPerCapita=round(mean(DvmtPerCapita, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTripsPerCapita=round(mean(VehicleTripsPerCapita, na.rm=T),2),
                                                                          TransitTripsPerCapita=round(mean(TransitTripsPerCapita, na.rm=T),2),
                                                                          BikeTripsPerCapita=round(mean(BikeTripsPerCapita, na.rm=T),2),
                                                                          WalkTripsPerCapita=round(mean(WalkTripsPerCapita, na.rm=T),2),
                                                                          cost_per_income=mean(cost_per_income, na.rm=T))
  })
  output$DvmtPerCapita_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(DvmtPerCapita)-base_values() %>% pull(DvmtPerCapita))/base_values() %>% pull(DvmtPerCapita)), "Daily VMT Per Capita", width=12, color="navy")
  })
  output$gge_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(DailyGGE)-base_values() %>% pull(DailyGGE))/base_values() %>% pull(DailyGGE)), "Daily GHG", width=12, color="navy")
  })
  output$cost_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(AveVehCostPM)-base_values() %>% pull(AveVehCostPM))/base_values() %>% pull(AveVehCostPM)), "Vehicle Cost", width=12, color="navy")
  })
  output$co2_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(DailyCO2e)-base_values() %>% pull(DailyCO2e))/base_values() %>% pull(DailyCO2e)), "Daily CO2", width=12, color="navy")
  })
  output$veh_trips_4<-renderValueBox({
    valueBox(percent((values4() %>% pull(VehicleTripsPerCapita)-base_values() %>% pull(VehicleTripsPerCapita))/base_values() %>% pull(VehicleTripsPerCapita), accuracy = 0.01), "Vehicle Trips Per Capita", width=12, color="navy")
  })
  output$tran_trips_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(TransitTripsPerCapita)-base_values() %>% pull(TransitTripsPerCapita))/base_values() %>% pull(TransitTripsPerCapita)), "Transit Trips Per Capita", width=12, color="navy")
  })
  output$walk_trips_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(WalkTripsPerCapita)-base_values() %>% pull(WalkTripsPerCapita))/base_values() %>% pull(WalkTripsPerCapita)), "Walk Trips Per Capita", width=12, color="navy")
  })
  output$bike_trips_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(BikeTripsPerCapita)-base_values() %>% pull(BikeTripsPerCapita))/base_values() %>% pull(BikeTripsPerCapita)), "Bike Trips Per Capita", width=12, color="navy")
  })
  output$records4 <-renderValueBox({
    valueBox(comma(hh4_group %>% filter(Azone %in%input$input_azone & 
                                          region %in%input$input_region & 
                                              # HhSize %in%input$input_hh & 
                                               odot_comp %in%input$input_lifecycle & 
                                               income_cat %in%input$input_income & 
                                               workers1 %in%input$input_workers & 
                                               HouseType %in%input$input_house_type &
                                               LocType %in%input$input_loc_type &
                                               car_sufficiency %in%input$input_car_sufficiency) %>%
                          summarise(households=sum(households,na.rm=T)) %>% pull()), "Number of HH Selected", width=12, color="navy")
  })
 # scenario 5
  values5<- reactive({
    hh5_group %>% filter(Azone %in%input$input_azone &
                           region %in%input$input_region & 
                          # HhSize %in%input$input_hh &
                           odot_comp %in%input$input_lifecycle &
                           income_cat %in%input$input_income &
                           workers1 %in%input$input_workers &
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           car_sufficiency %in%input$input_car_sufficiency) %>% summarise(DvmtPerCapita=round(mean(DvmtPerCapita, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTripsPerCapita=round(mean(VehicleTripsPerCapita, na.rm=T),2),
                                                                          TransitTripsPerCapita=round(mean(TransitTripsPerCapita, na.rm=T),2),
                                                                          BikeTripsPerCapita=round(mean(BikeTripsPerCapita, na.rm=T),2),
                                                                          WalkTripsPerCapita=round(mean(WalkTripsPerCapita, na.rm=T),2),
                                                                          cost_per_income=mean(cost_per_income, na.rm=T))
  })
  output$DvmtPerCapita_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(DvmtPerCapita)-base_values() %>% pull(DvmtPerCapita))/base_values() %>% pull(DvmtPerCapita)), "Daily VMT Per Capita", width=12, color="navy")
  })
  output$gge_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(DailyGGE)-base_values() %>% pull(DailyGGE))/base_values() %>% pull(DailyGGE)), "Daily GHG", width=12, color="navy")
  })
  output$cost_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(AveVehCostPM)-base_values() %>% pull(AveVehCostPM))/base_values() %>% pull(AveVehCostPM)), "Vehicle Cost", width=12, color="navy")
  })
  output$co2_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(DailyCO2e)-base_values() %>% pull(DailyCO2e))/base_values() %>% pull(DailyCO2e)), "Daily CO2", width=12, color="navy")
  })
  output$veh_trips_5<-renderValueBox({
    valueBox(percent((values5() %>% pull(VehicleTripsPerCapita)-base_values() %>% pull(VehicleTripsPerCapita))/base_values() %>% pull(VehicleTripsPerCapita), accuracy = 0.01), "Vehicle Trips Per Capita", width=12, color="navy")
  })
  output$tran_trips_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(TransitTripsPerCapita)-base_values() %>% pull(TransitTripsPerCapita))/base_values() %>% pull(TransitTripsPerCapita)), "Transit Trips Per Capita", width=12, color="navy")
  })
  output$walk_trips_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(WalkTripsPerCapita)-base_values() %>% pull(WalkTripsPerCapita))/base_values() %>% pull(WalkTripsPerCapita)), "Walk Trips Per Capita", width=12, color="navy")
  })
  output$bike_trips_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(BikeTripsPerCapita)-base_values() %>% pull(BikeTripsPerCapita))/base_values() %>% pull(BikeTripsPerCapita)), "Bike Trips Per Capita", width=12, color="navy")
  })
  output$records5 <-renderValueBox({
    valueBox(comma(
      hh5_group %>% filter(Azone %in%input$input_azone & 
                             region %in%input$input_region & 
                                              # HhSize %in%input$input_hh & 
                                               odot_comp %in%input$input_lifecycle & 
                                               income_cat %in%input$input_income & 
                                               workers1 %in%input$input_workers & 
                                               HouseType %in%input$input_house_type &
                                               LocType %in%input$input_loc_type &
                                               car_sufficiency %in%input$input_car_sufficiency) %>%
        summarise(households=sum(households,na.rm=T)) %>% pull()
      ), "Number of HH Selected", width=12, color="navy")
  })
}
shinyApp(ui, server)

# sf::sf_use_s2(FALSE)
# regions<-st_read("Oregon Regions","OR_regions") %>% st_transform(crs=4326)
# equity<-st_read(".","equity_final")  %>% st_transform(crs=4326) %>%
#   select(id,ar_typ_,county_1)
# test<-st_join(st_buffer(equity, 0), st_buffer(regions,0), largest=T)
# 
# write.csv(data.frame(test) %>% select(-geometry) , "regions.csv")
# 

# # https://nhts.ornl.gov/tables09/CodebookPage.aspx?id=960
# # 
# # lc <- data.frame(code=c(1,2,3,4,9,10),
# #                  meaning=c("one adult, no children","2+ adults, no children","one adult, youngest child 0-5","2+ adults, youngest child 0-5","one adult, retired, no children","2+ adults, retired, no children"))
# 
# # inc_cat <- data.frame(values=round(unname(quantile(hh1$Income, probs = seq(0, 1, 1/5))),0),
# #                       income_cat=c("","low","low-med","med","med-high","high"))
# 
# 
# # hh1<-fread("Household_2050_Exp1_Reduce_Emissions.csv") %>% # filter(Azone=="Baker") %>%
# #   mutate(scenario="one") %>% rowwise() %>%
# #   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
# #                               between(Income, 23794,50289) ~ "$24k-$50k",
# #                               between(Income, 50289,86286) ~ "$50k-$86k",
# #                               between(Income, 86286,148656) ~ "$86k-$148k",
# #                               Income > 148656 ~ "More than $148k"),
# #          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
# #          adult_group=case_when(adults_no==0 ~ "0 adults",
# #                                adults_no==1 ~ "1 adult",
# #                                adults_no==2 ~ "2 adults",
# #                                adults_no>=3 ~ "3+ adults"),
# #          children_no=sum(c(Age15to19,Age0to14)),
# #          children_group=case_when(children_no==0 ~ "0 children",
# #                                   children_no %in% c(1,2) ~ "1-2 children",
# #                                   children_no>= 3 ~ "3+ children"),
# #          composition=paste(adult_group, children_group, sep=", "))
# 
# # hh1_group<-hh1 %>%
# #   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat) %>%
# #   summarise(DvmtPerCapita=mean(DvmtPerCapita,na.rm=T),
# #             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
# #             DailyCO2e=mean(DailyCO2e,na.rm=T),
# #             DailyGGE=mean(DailyGGE,na.rm=T),
# #             VehicleTrips=mean(VehicleTrips,na.rm=T),
# #             TransitTripsPerCapita=mean(TransitTripsPerCapita,na.rm=T)) %>% ungroup()
# 
# # write.csv(hh1_group,"h1group.csv", row.names=F)
# 
# # hh2<-fread("Household_2050_Exp2_Good_Repair.csv") %>% #filter(Azone=="Baker") %>%
# #   mutate(scenario="two") %>% 
# #   rowwise() %>%
# #   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
# #                               between(Income, 23794,50289) ~ "$24k-$50k",
# #                               between(Income, 50289,86286) ~ "$50k-$86k",
# #                               between(Income, 86286,148656) ~ "$86k-$148k",
# #                               Income > 148656 ~ "More than $148k"),
# #          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
# #          adult_group=case_when(adults_no==0 ~ "0 adults",
# #                                adults_no==1 ~ "1 adult",
# #                                adults_no==2 ~ "2 adults",
# #                                adults_no>=3 ~ "3+ adults"),
# #          children_no=sum(c(Age15to19,Age0to14)),
# #          children_group=case_when(children_no==0 ~ "0 children",
# #                                   children_no %in% c(1,2) ~ "1-2 children",
# #                                   children_no>= 3 ~ "3+ children"),
# #          composition=paste(adult_group, children_group, sep=", "))
# 
# # hh2_group<-hh2 %>%
# #   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat)%>%
# #   summarise(DvmtPerCapita=mean(DvmtPerCapita,na.rm=T),
# #             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
# #             DailyCO2e=mean(DailyCO2e,na.rm=T),
# #             DailyGGE=mean(DailyGGE,na.rm=T),
# #             VehicleTrips=mean(VehicleTrips,na.rm=T),
# #             TransitTripsPerCapita=mean(TransitTripsPerCapita,na.rm=T))%>% ungroup()
# 
# # write.csv(hh2_group,"h2group.csv", row.names=F)
# 
# # hh3<-fread("Household_2050_Exp3_Travel_Options.csv") %>% 
# #   # filter(Azone=="Baker")  %>%
# #   mutate(scenario="three", BikePMT=as.numeric(BikePMT))  %>%
# #   rowwise() %>%
# #   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
# #                               between(Income, 23794,50289) ~ "$24k-$50k",
# #                               between(Income, 50289,86286) ~ "$50k-$86k",
# #                               between(Income, 86286,148656) ~ "$86k-$148k",
# #                               Income > 148656 ~ "More than $148k"),
# #          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
# #          adult_group=case_when(adults_no==0 ~ "0 adults",
# #                                adults_no==1 ~ "1 adult",
# #                                adults_no==2 ~ "2 adults",
# #                                adults_no>=3 ~ "3+ adults"),
# #          children_no=sum(c(Age15to19,Age0to14)),
# #          children_group=case_when(children_no==0 ~ "0 children",
# #                                   children_no %in% c(1,2) ~ "1-2 children",
# #                                   children_no>= 3 ~ "3+ children"),
# #          composition=paste(adult_group, children_group, sep=", "))
# 
# # hh3_group<-hh3 %>%
# #   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat) %>%
# #   summarise(DvmtPerCapita=mean(DvmtPerCapita,na.rm=T),
# #             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
# #             DailyCO2e=mean(DailyCO2e,na.rm=T),
# #             DailyGGE=mean(DailyGGE,na.rm=T),
# #             VehicleTrips=mean(VehicleTrips,na.rm=T),
# #             TransitTripsPerCapita=mean(TransitTripsPerCapita,na.rm=T))%>% ungroup()
# 
# # write.csv(hh3_group,"h3group.csv", row.names=F)
# 
# # hh4<-fread("Household_2050_Exp4_Balanced_Future.csv") %>% # filter(Azone=="Baker") 
# #   mutate(scenario="four") %>%
# #   rowwise() %>%
# #   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
# #                               between(Income, 23794,50289) ~ "$24k-$50k",
# #                               between(Income, 50289,86286) ~ "$50k-$86k",
# #                               between(Income, 86286,148656) ~ "$86k-$148k",
# #                               Income > 148656 ~ "More than $148k"),
# #          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
# #          adult_group=case_when(adults_no==0 ~ "0 adults",
# #                                adults_no==1 ~ "1 adult",
# #                                adults_no==2 ~ "2 adults",
# #                                adults_no>=3 ~ "3+ adults"),
# #          children_no=sum(c(Age15to19,Age0to14)),
# #          children_group=case_when(children_no==0 ~ "0 children",
# #                                   children_no %in% c(1,2) ~ "1-2 children",
# #                                   children_no>= 3 ~ "3+ children"),
# #          composition=paste(adult_group, children_group, sep=", "))
# 
# # hh4_group<-hh4 %>%
# #   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat)%>%
# #   summarise(DvmtPerCapita=mean(DvmtPerCapita,na.rm=T),
# #             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
# #             DailyCO2e=mean(DailyCO2e,na.rm=T),
# #             DailyGGE=mean(DailyGGE,na.rm=T),
# #             VehicleTrips=mean(VehicleTrips,na.rm=T),
# #             TransitTripsPerCapita=mean(TransitTripsPerCapita,na.rm=T))%>% ungroup()
# 
# # write.csv(hh4_group,"h4group.csv", row.names=F)
# 
# # hh5<-fread("Household_2010_AP.csv") %>% # filter(Azone=="Baker") %>% 
# #   mutate(scenario="four") %>%
# #   rowwise() %>%
# #   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
# #                               between(Income, 23794,50289) ~ "$24k-$50k",
# #                               between(Income, 50289,86286) ~ "$50k-$86k",
# #                               between(Income, 86286,148656) ~ "$86k-$148k",
# #                               Income > 148656 ~ "More than $148k"),
# #          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
# #          adult_group=case_when(adults_no==0 ~ "0 adults",
# #                                adults_no==1 ~ "1 adult",
# #                                adults_no==2 ~ "2 adults",
# #                                adults_no>=3 ~ "3+ adults"),
# #          children_no=sum(c(Age15to19,Age0to14)),
# #          children_group=case_when(children_no==0 ~ "0 children",
# #                                   children_no %in% c(1,2) ~ "1-2 children",
# #                                   children_no>= 3 ~ "3+ children"),
# #          composition=paste(adult_group, children_group, sep=", "))
# 
# # hh5_group<-hh5 %>%
# #   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat)%>%
# #   summarise(DvmtPerCapita=mean(DvmtPerCapita,na.rm=T),
# #             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
# #             DailyCO2e=mean(DailyCO2e,na.rm=T),
# #             DailyGGE=mean(DailyGGE,na.rm=T),
# #             VehicleTrips=mean(VehicleTrips,na.rm=T),
# #             TransitTripsPerCapita=mean(TransitTripsPerCapita,na.rm=T))%>% ungroup()
# 
# # write.csv(hh5_group,"h5group.csv", row.names=F)

# 
# data.frame(weighting=c(1,2,1,3,4),
#            goal=c("sust","mob","equity","stew","safety")
# ) %>% mutate(total=sum(weighting), norm=weighting/total)
#   
# data.frame(weighting=c('pol_pri_OwnCostPropHhLess25K',
#                             'pol_pri_OwnCostProp',
#                             'pol_pri_OwnCostPropHh25Kto50K',
#                             'pol_pri_BikePMTPerPrsnUrban',
#                             'pol_pri_WalkPMTPerPrsnUrban',
#                             'pol_pri_TransitTripsPerCapita',
#                             'pol_pri_TransitPMTPerPrsnUrban',
#                             'pol_pri_BikeTripsPerCapita',
#                             'pol_pri_WalkTripsPerCapita',
#                             'pol_pri_HouseholdDvmtPerPrsn',
#                             'pol_pri_TotalDvmtPerPrsn',
#                             'pol_pri_ArtDvmtPropExtCong_SalemKeizer',
#                             'pol_pri_FwyExtCongTTI_Metro',
#                             'pol_pri_ArtExtCongTTI_Metro',
#                             'pol_pri_AutoFatalCrash_Metro',
#                             'pol_pri_AutoFatalUrban',
#                             'pol_pri_AutoInjuryUrban',
#                             'pol_pri_AutoFatalRura',
#                             'pol_pri_PresAdapt_22',
#                             'pol_pri_TotalCO2e',
#                             'pol_pri_HouseholdCO2ePerPrsn',
#                             'pol_pri_HouseholdCO2ePerMileRural',
#                             'pol_pri_BusCO2eRate_Metro',
#                             'pol_pri_HvyTrkAveUrbanCO2eRate_Metro'),
#  policy=c("Eq11","Eq12","Eq13","Mob11","Mob12","Mob13","Mob14","Mob15","Mob17","Mob21","Mob22","Mob41","Mob42","Mob43","Sfty11","Sfty12","Sfty13","Sfty14","Goal","Sus11","Sus12","Sus13","Sus21","Sus22"))  %>% mutate(total=sum(weighting), norm=weighting/total)
# 


# results() %>% mutate(
#   score= 
#     (OwnCostPropHhLess25K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
#                                     eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
#                                     eq_pol_weighting() %>% filter(policy=="Eq11") %>% pull(norm))) +
#     (OwnCostProp_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
#                            eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
#                            eq_pol_weighting() %>% filter(policy=="Eq12") %>% pull(norm)))+
#     (OwnCostPropHh25Kto50K_norm * (goal_weighting() %>% filter(goal=="equity") %>% pull(norm) * 
#                                      eq_obj_weighting() %>% filter(obj=="eq1") %>% pull(norm) * 
#                                      eq_pol_weighting() %>% filter(policy=="Eq13") %>% pull(norm)))+
#     (BikePMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                    mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
#                                    mob1_pol_weighting() %>% filter(policy=="Mob11") %>% pull(norm)))+
#     (WalkPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                    mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
#                                    mob1_pol_weighting() %>% filter(policy=="Mob12") %>% pull(norm)))+
#     (TransitTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                      mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
#                                      mob1_pol_weighting() %>% filter(policy=="Mob13") %>% pull(norm)))+
#     (TransitPMTPerPrsnUrban_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                       mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
#                                       mob1_pol_weighting() %>% filter(policy=="Mob14") %>% pull(norm)))+
#     (BikeTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                   mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
#                                   mob1_pol_weighting() %>% filter(policy=="Mob15") %>% pull(norm)))+
#     (WalkTripsPerCapita_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                   mob_obj_weighting() %>% filter(obj=="mob1") %>% pull(norm) * 
#                                   mob1_pol_weighting() %>% filter(policy=="Mob17") %>% pull(norm)))+
#     (HouseholdDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                     mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
#                                     mob2_pol_weighting() %>% filter(policy=="Mob21") %>% pull(norm))) + 
#     (TotalDvmtPerPrsn_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                 mob_obj_weighting() %>% filter(obj=="mob2") %>% pull(norm) * 
#                                 mob2_pol_weighting() %>% filter(policy=="Mob22") %>% pull(norm))) + 
#     (ArtDvmtPropExtCong_SalemKeizer_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                               mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
#                                               mob3_pol_weighting() %>% filter(policy=="Mob41") %>% pull(norm))) + 
#     (FwyExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                    mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
#                                    mob3_pol_weighting() %>% filter(policy=="Mob42") %>% pull(norm))) + 
#     (ArtExtCongTTI_Metro_norm * (goal_weighting() %>% filter(goal=="mob") %>% pull(norm) * 
#                                    mob_obj_weighting() %>% filter(obj=="mob4") %>% pull(norm) * 
#                                    mob3_pol_weighting() %>% filter(policy=="Mob43") %>% pull(norm))) +
#     (AutoFatalCrash_Metro_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
#                                     sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
#                                     sfty_pol_weighting() %>% filter(policy=="Sfty11") %>% pull(norm))) + 
#     (AutoFatalUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
#                               sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
#                               sfty_pol_weighting() %>% filter(policy=="Sfty12") %>% pull(norm))) + 
#     (AutoInjuryUrban_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
#                                sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
#                                sfty_pol_weighting() %>% filter(policy=="Sfty13") %>% pull(norm))) + 
#     (AutoFatalRural_norm * (goal_weighting() %>% filter(goal=="safety") %>% pull(norm) * 
#                               sfty_obj_weighting() %>% filter(obj=="sfty1") %>% pull(norm) * 
#                               sfty_pol_weighting() %>% filter(policy=="Sfty14") %>% pull(norm))) + 
#     (TotalCO2e_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
#                          sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
#                          sus1_pol_weighting() %>% filter(policy=="Sus11") %>% pull(norm))) + 
#     (HouseholdCO2ePerPrsn_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
#                                     sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
#                                     sus1_pol_weighting() %>% filter(policy=="Sus12") %>% pull(norm))) + 
#     (HouseholdCO2ePerMileRural_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
#                                          sus_obj_weighting() %>% filter(obj=="sus1") %>% pull(norm) * 
#                                          sus1_pol_weighting() %>% filter(policy=="Sus13") %>% pull(norm))) +
#     (BusCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
#                                  sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
#                                  sus2_pol_weighting() %>% filter(policy=="Sus21") %>% pull(norm))) +
#     (HvyTrkAveUrbanCO2eRate_Metro_norm * (goal_weighting() %>% filter(goal=="sust") %>% pull(norm) * 
#                                             sus_obj_weighting() %>% filter(obj=="sus2") %>% pull(norm) * 
#                                             sus2_pol_weighting() %>% filter(policy=="Sus22") %>% pull(norm)))+
#     (`PresAdapt$2022_norm` * (goal_weighting() %>% filter(goal=="stew") %>% pull(norm) * 
#                                 pres_obj_weighting() %>% filter(obj=="goal") %>% pull(norm) * 
#                                 pres_pol_weighting() %>% filter(policy=="Goal") %>% pull(norm)))
# ) %>%
#   arrange(desc(score)) 
