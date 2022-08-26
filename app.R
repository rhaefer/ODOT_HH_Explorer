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

###### Data Prep - Ignore ######

# https://nhts.ornl.gov/tables09/CodebookPage.aspx?id=960
# 
# lc <- data.frame(code=c(1,2,3,4,9,10),
#                  meaning=c("one adult, no children","2+ adults, no children","one adult, youngest child 0-5","2+ adults, youngest child 0-5","one adult, retired, no children","2+ adults, retired, no children"))

# inc_cat <- data.frame(values=round(unname(quantile(hh1$Income, probs = seq(0, 1, 1/5))),0),
#                       income_cat=c("","low","low-med","med","med-high","high"))

#data<-read_csv("data.csv")

# hh1<-fread("C:/Users/reid.haefer/OneDrive - Resource Systems Group, Inc/Documents/projects/ODOT/Household_Common.csv") %>% mutate(scenario="common") %>% filter(Year==2015) %>% rowwise() %>%
#   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
#                               between(Income, 23794,50289) ~ "$24k-$50k",
#                               between(Income, 50289,86286) ~ "$50k-$86k",
#                               between(Income, 86286,148656) ~ "$86k-$148k",
#                               Income > 148656 ~ "More than $148k"),
#          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
#          adult_group=case_when(adults_no==0 ~ "0 adults",
#                                adults_no==1 ~ "1 adult",
#                                adults_no==2 ~ "2 adults",
#                                adults_no>=3 ~ "3+ adults"),
#          children_no=sum(c(Age15to19,Age0to14)),
#          children_group=case_when(children_no==0 ~ "0 children",
#                                   children_no %in% c(1,2) ~ "1-2 children",
#                                   children_no>= 3 ~ "3+ children"),
#          composition=paste(adult_group, children_group, sep=", "))
# 
# hh2<-fread("C:/Users/reid.haefer/OneDrive - Resource Systems Group, Inc/Documents/projects/ODOT/Household_Common_MM.csv") %>% mutate(scenario="mm_common") %>% filter(Year==2015) %>%
#   rowwise() %>%
#   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
#                               between(Income, 23794,50289) ~ "$24k-$50k",
#                               between(Income, 50289,86286) ~ "$50k-$86k",
#                               between(Income, 86286,148656) ~ "$86k-$148k",
#                               Income > 148656 ~ "More than $148k"),
#          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
#          adult_group=case_when(adults_no==0 ~ "0 adults",
#                                adults_no==1 ~ "1 adult",
#                                adults_no==2 ~ "2 adults",
#                                adults_no>=3 ~ "3+ adults"),
#          children_no=sum(c(Age15to19,Age0to14)),
#          children_group=case_when(children_no==0 ~ "0 children",
#                                   children_no %in% c(1,2) ~ "1-2 children",
#                                   children_no>= 3 ~ "3+ children"),
#          composition=paste(adult_group, children_group, sep=", "))
# 
# hh3<-fread("C:/Users/reid.haefer/OneDrive - Resource Systems Group, Inc/Documents/projects/ODOT/Household_STS_MM.csv") %>% mutate(scenario="mm_STS", BikePMT=as.numeric(BikePMT))  %>% filter(Year==2050) %>%
#   rowwise() %>%
#   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
#                               between(Income, 23794,50289) ~ "$24k-$50k",
#                               between(Income, 50289,86286) ~ "$50k-$86k",
#                               between(Income, 86286,148656) ~ "$86k-$148k",
#                               Income > 148656 ~ "More than $148k"),
#          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
#          adult_group=case_when(adults_no==0 ~ "0 adults",
#                                adults_no==1 ~ "1 adult",
#                                adults_no==2 ~ "2 adults",
#                                adults_no>=3 ~ "3+ adults"),
#          children_no=sum(c(Age15to19,Age0to14)),
#          children_group=case_when(children_no==0 ~ "0 children",
#                                   children_no %in% c(1,2) ~ "1-2 children",
#                                   children_no>= 3 ~ "3+ children"),
#          composition=paste(adult_group, children_group, sep=", "))
# 
# hh4<-fread("C:/Users/reid.haefer/OneDrive - Resource Systems Group, Inc/Documents/projects/ODOT/Household_STS.csv") %>% mutate(scenario="STS") %>% filter(Year==2050) %>%
#   rowwise() %>%
#   mutate(income_cat=case_when(Income < 23794 ~ "Less than $24k",
#                               between(Income, 23794,50289) ~ "$24k-$50k",
#                               between(Income, 50289,86286) ~ "$50k-$86k",
#                               between(Income, 86286,148656) ~ "$86k-$148k",
#                               Income > 148656 ~ "More than $148k"),
#          adults_no=sum(c(Age20to29,Age30to54,Age55to64,Age65Plus)),
#          adult_group=case_when(adults_no==0 ~ "0 adults",
#                                adults_no==1 ~ "1 adult",
#                                adults_no==2 ~ "2 adults",
#                                adults_no>=3 ~ "3+ adults"),
#          children_no=sum(c(Age15to19,Age0to14)),
#          children_group=case_when(children_no==0 ~ "0 children",
#                                   children_no %in% c(1,2) ~ "1-2 children",
#                                   children_no>= 3 ~ "3+ children"),
#          composition=paste(adult_group, children_group, sep=", "))
# 
# hh1_group<-hh1 %>%
#   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat) %>%
#   summarise(Dvmt=mean(Dvmt,na.rm=T),
#             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
#             DailyCO2e=mean(DailyCO2e,na.rm=T),
#             DailyGGE=mean(DailyGGE,na.rm=T),
#             VehicleTrips=mean(VehicleTrips,na.rm=T),
#             TransitTrips=mean(TransitTrips,na.rm=T)) %>% ungroup()
# 
#  write.csv(hh1_group,"h1group.csv", row.names=F)

# hh2_group<-hh2 %>%
#   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat)%>%
#   summarise(Dvmt=mean(Dvmt,na.rm=T),
#             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
#             DailyCO2e=mean(DailyCO2e,na.rm=T),
#             DailyGGE=mean(DailyGGE,na.rm=T),
#             VehicleTrips=mean(VehicleTrips,na.rm=T),
#             TransitTrips=mean(TransitTrips,na.rm=T))%>% ungroup()
# write.csv(hh2_group,"h2group.csv", row.names=F)



# hh3_group<-hh3 %>%
#   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat) %>%
#   summarise(Dvmt=mean(Dvmt,na.rm=T),
#             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
#             DailyCO2e=mean(DailyCO2e,na.rm=T),
#             DailyGGE=mean(DailyGGE,na.rm=T),
#             VehicleTrips=mean(VehicleTrips,na.rm=T),
#             TransitTrips=mean(TransitTrips,na.rm=T))%>% ungroup()
# write.csv(hh3_group,"h3group.csv", row.names=F)

# hh4_group<-hh4 %>%
#   group_by(Azone,HhSize,Workers,HouseType,LocType,Vehicles,Drivers, composition,income_cat)%>%
#   summarise(Dvmt=mean(Dvmt,na.rm=T),
#             AveVehCostPM=mean(AveVehCostPM,na.rm=T),
#             DailyCO2e=mean(DailyCO2e,na.rm=T),
#             DailyGGE=mean(DailyGGE,na.rm=T),
#             VehicleTrips=mean(VehicleTrips,na.rm=T),
#             TransitTrips=mean(TransitTrips,na.rm=T))%>% ungroup()
# write.csv(hh4_group,"h4group.csv", row.names=F)


###### Read in Summarized VE Outputs ######

hh1_group<-read_csv("h5group.csv") %>%
  mutate(scenario="hh1")

hh2_group<-read_csv("h1group.csv") %>%
  mutate(scenario="hh2")

hh3_group<-read_csv("h2group.csv") %>%
  mutate(scenario="hh3")

hh4_group<-read_csv("h3group.csv")%>%
  mutate(scenario="h4")

hh5_group<-read_csv("h4group.csv") %>%
  mutate(scenario="hh5")

all<- bind_rows(hh1_group, hh2_group, hh3_group, hh4_group) %>%
  filter(!composition %in% c("0 adults, 1-2 children","0 adults, 3+ children"))

counties <- unique(all$Azone)
hh_size <- unique(all$HhSize)
inc <- unique(all$income_cat)

linebreaks <- function(n){HTML(strrep(br(), n))}

############ Shiny Tool Code ##################

ui <- dashboardPage(skin="black", 
                    dashboardHeader(title="Oregon Household Scenario Explorer",titleWidth = 450),
                    dashboardSidebar(),
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
                               tabPanel("Summary Data",
                        box(title="Select Household Characteristics",
                                   column(1,pickerInput("input_azone","County" ,choices=counties, selected=counties,options = list(`actions-box` = TRUE),multiple = T)), 
                                   column(1,pickerInput("input_hh","Household Size", choices=hh_size, selected=hh_size, options = list(`actions-box` = TRUE),multiple = T)), 
                                   column(2,pickerInput("input_lifecycle","Household Composition", choices=sort(unique(all$composition)), selected=unique(all$composition),options = list(`actions-box` = TRUE),multiple = T)),
                                   column(2,pickerInput("input_income","Household Income", choices=inc, selected=inc,options = list(`actions-box` = TRUE),multiple = T)), 
                                   column(1,pickerInput("input_workers","# of Workers", choices=unique(all$Workers), selected=unique(all$Workers),options = list(`actions-box` = TRUE),multiple = T)), 
                                   column(1,pickerInput("input_house_type","House Type", choices=unique(all$HouseType), selected=unique(all$HouseType) ,options = list(`actions-box` = TRUE),multiple = T)),
                                   column(1,pickerInput("input_loc_type","Location Type", choices=unique(all$LocType), selected=unique(all$LocType),options = list(`actions-box` = TRUE),multiple = T)),
                                   column(1,pickerInput("input_vehicles","# of Vehicles", choices=unique(all$Vehicles), selected=unique(all$Vehicles), options = list(`actions-box` = TRUE),multiple = T)),
                                   column(1,pickerInput("input_drivers","# of Drivers", choices=unique(all$Drivers), selected=unique(all$Drivers) ,options = list(`actions-box` = TRUE),multiple = T)),
                                   width=12),
                      # fluidRow(box(title="Common 2015 - Household Summary", status="primary", solidHeader = F,
                      #              column(2,valueBoxOutput("dvmt_1", width=12) %>% withSpinner()),
                      #              column(2,valueBoxOutput("veh_trips_1", width=12)),
                      #              column(2,valueBoxOutput("tran_trips_1", width=12)),
                      #              column(2,valueBoxOutput("gge_1", width=12)),
                      #              column(2,valueBoxOutput("cost_1", width=12)),
                      #              column(2,valueBoxOutput("co2_1", width=12)),
                      #              width=12)
                      # ),
                      fluidRow(box(title="Base (Adopted Plan 2010)",status="success", solidHeader = F,
                                   column(2,valueBoxOutput("dvmt_1", width=12)),  
                                   column(2,valueBoxOutput("veh_trips_1", width=12)),
                                   column(2,valueBoxOutput("tran_trips_1", width=12)),
                                   column(2,valueBoxOutput("gge_1", width=12)),
                                   column(2,valueBoxOutput("cost_1", width=12)),
                                   column(2,valueBoxOutput("co2_1", width=12)),
                                   width=12)
                      ),
                      fluidRow(box(title="Reduce_Emissions (2050)", status="primary", solidHeader = F,
                                   column(2,valueBoxOutput("dvmt_2", width=12)),  
                                   column(2,valueBoxOutput("veh_trips_2", width=12)),
                                   column(2,valueBoxOutput("tran_trips_2", width=12)),
                                   column(2,valueBoxOutput("gge_2", width=12)),
                                   column(2,valueBoxOutput("cost_2", width=12)),
                                   column(2,valueBoxOutput("co2_2", width=12)),
                                   width=12)
                      ),
                      fluidRow(box(title="Good Repair (2050)", status="success",solidHeader = F,
                                   column(2,valueBoxOutput("dvmt_3", width=12)),  
                                   column(2,valueBoxOutput("veh_trips_3", width=12)),
                                   column(2,valueBoxOutput("tran_trips_3", width=12)),
                                   column(2,valueBoxOutput("gge_3", width=12)),
                                   column(2,valueBoxOutput("cost_3", width=12)),
                                   column(2,valueBoxOutput("co2_3", width=12)),
                                   width=12)),
                      fluidRow(box(title="Travel Options (2050)", status="success",solidHeader = F,
                                   column(2,valueBoxOutput("dvmt_4", width=12)),  
                                   column(2,valueBoxOutput("veh_trips_4", width=12)),
                                   column(2,valueBoxOutput("tran_trips_4", width=12)),
                                   column(2,valueBoxOutput("gge_4", width=12)),
                                   column(2,valueBoxOutput("cost_4", width=12)),
                                   column(2,valueBoxOutput("co2_4", width=12)),
                                   width=12)),
                      fluidRow(box(title="Balanced Future (2050)", status="success",solidHeader = F,
                                   column(2,valueBoxOutput("dvmt_5", width=12)),
                                   column(2,valueBoxOutput("veh_trips_5", width=12)),
                                   column(2,valueBoxOutput("tran_trips_5", width=12)),
                                   column(2,valueBoxOutput("gge_5", width=12)),
                                   column(2,valueBoxOutput("cost_5", width=12)),
                                   column(2,valueBoxOutput("co2_5", width=12)),
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
  base_values <- reactive({
    hh1_group %>% filter(Azone %in%input$input_azone & 
                           HhSize %in%input$input_hh & 
                           composition %in%input$input_lifecycle & 
                           income_cat %in%input$input_income & 
                           Workers %in%input$input_workers & 
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           Vehicles %in%input$input_vehicles&
                           Drivers %in%input$input_drivers) %>% summarise(Dvmt=round(mean(Dvmt, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTrips=round(mean(VehicleTrips, na.rm=T),2),
                                                                          TransitTrips=round(mean(TransitTrips, na.rm=T),2))
  })
  # scenario 1
  output$dvmt_1 <-renderValueBox({
    valueBox(base_values() %>% pull(Dvmt), "Daily VMT", width=12, color="navy")
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
    valueBox(base_values() %>% pull(VehicleTrips), "Vehicle Trips", width=12, color="navy")
  })
  output$tran_trips_1 <-renderValueBox({
    valueBox(base_values() %>% pull(TransitTrips), "Transit Trips", width=12, color="navy")
  })
  # scenario 2
  values2<- reactive({
    hh2_group %>% filter(Azone %in%input$input_azone & 
                           HhSize %in%input$input_hh & 
                           composition %in%input$input_lifecycle & 
                           income_cat %in%input$input_income & 
                           Workers %in%input$input_workers & 
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           Vehicles %in%input$input_vehicles&
                           Drivers %in%input$input_drivers) %>% summarise(Dvmt=round(mean(Dvmt, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTrips=round(mean(VehicleTrips, na.rm=T),2),                                                                      TransitTrips=round(mean(TransitTrips, na.rm=T),2))
  })
  output$dvmt_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(Dvmt)-base_values() %>% pull(Dvmt))/base_values() %>% pull(Dvmt)), "Daily VMT", width=12, color="navy")
  })
  output$gge_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(DailyGGE)-base_values() %>% pull(DailyGGE))/base_values() %>% pull(DailyGGE)), "Daily GHG", width=12, color="navy")
  })
  output$cost_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(AveVehCostPM)-base_values() %>% pull(AveVehCostPM))/base_values() %>% pull(AveVehCostPM)), "Vehicle Cost", width=12, color="navy")
  })
  output$co2_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(DailyCO2e)-base_values() %>% pull(DailyCO2e))/base_values() %>% pull(DailyCO2e)), "Daily CO2", width=12, color="navy")
  })
  output$veh_trips_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(VehicleTrips)-base_values() %>% pull(VehicleTrips))/base_values() %>% pull(VehicleTrips)), "Vehicle Trips", width=12, color="navy")
  })
  output$tran_trips_2 <-renderValueBox({
    valueBox(percent((values2() %>% pull(TransitTrips)-base_values() %>% pull(TransitTrips))/base_values() %>% pull(TransitTrips)), "Transit Trips", width=12, color="navy")
  })
  # scenario 3
  values3<- reactive({
    hh3_group %>% filter(Azone %in%input$input_azone & 
                           HhSize %in%input$input_hh & 
                           composition %in%input$input_lifecycle & 
                           income_cat %in%input$input_income & 
                           Workers %in%input$input_workers & 
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           Vehicles %in%input$input_vehicles&
                           Drivers %in%input$input_drivers) %>% summarise(Dvmt=round(mean(Dvmt, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTrips=round(mean(VehicleTrips, na.rm=T),2),                                                                      TransitTrips=round(mean(TransitTrips, na.rm=T),2))
  })
  output$dvmt_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(Dvmt)-base_values() %>% pull(Dvmt))/base_values() %>% pull(Dvmt)), "Daily VMT", width=12, color="navy")
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
    valueBox(percent((values3() %>% pull(VehicleTrips)-base_values() %>% pull(VehicleTrips))/base_values() %>% pull(VehicleTrips)), "Vehicle Trips", width=12, color="navy")
  })
  output$tran_trips_3 <-renderValueBox({
    valueBox(percent((values3() %>% pull(TransitTrips)-base_values() %>% pull(TransitTrips))/base_values() %>% pull(TransitTrips)), "Transit Trips", width=12, color="navy")
  })
  # scenario 4
  values4<- reactive({
    hh4_group %>% filter(Azone %in%input$input_azone & 
                           HhSize %in%input$input_hh & 
                           composition %in%input$input_lifecycle & 
                           income_cat %in%input$input_income & 
                           Workers %in%input$input_workers & 
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           Vehicles %in%input$input_vehicles&
                           Drivers %in%input$input_drivers) %>% summarise(Dvmt=round(mean(Dvmt, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTrips=round(mean(VehicleTrips, na.rm=T),2),                                                                      TransitTrips=round(mean(TransitTrips, na.rm=T),2))
  })
  output$dvmt_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(Dvmt)-base_values() %>% pull(Dvmt))/base_values() %>% pull(Dvmt)), "Daily VMT", width=12, color="navy")
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
    valueBox(percent((values4() %>% pull(VehicleTrips)-base_values() %>% pull(VehicleTrips))/base_values() %>% pull(VehicleTrips)), "Vehicle Trips", width=12, color="navy")
  })
  output$tran_trips_4 <-renderValueBox({
    valueBox(percent((values4() %>% pull(TransitTrips)-base_values() %>% pull(TransitTrips))/base_values() %>% pull(TransitTrips)), "Transit Trips", width=12, color="navy")
  })
 # scenario 5
  values5<- reactive({
    hh5_group %>% filter(Azone %in%input$input_azone &
                           HhSize %in%input$input_hh &
                           composition %in%input$input_lifecycle &
                           income_cat %in%input$input_income &
                           Workers %in%input$input_workers &
                           HouseType %in%input$input_house_type &
                           LocType %in%input$input_loc_type &
                           Vehicles %in%input$input_vehicles&
                           Drivers %in%input$input_drivers) %>% summarise(Dvmt=round(mean(Dvmt, na.rm=T),2),
                                                                          DailyGGE=round(mean(DailyGGE, na.rm=T),2),
                                                                          AveVehCostPM=round(mean(AveVehCostPM, na.rm=T),2),
                                                                          DailyCO2e=round(mean(DailyCO2e, na.rm=T),2),
                                                                          VehicleTrips=round(mean(VehicleTrips, na.rm=T),2),                                                                      TransitTrips=round(mean(TransitTrips, na.rm=T),2))
  })
  output$dvmt_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(Dvmt)-base_values() %>% pull(Dvmt))/base_values() %>% pull(Dvmt)), "Daily VMT", width=12, color="navy")
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
    valueBox(percent((values5() %>% pull(VehicleTrips)-base_values() %>% pull(VehicleTrips))/base_values() %>% pull(VehicleTrips)), "Vehicle Trips", width=12, color="navy")
  })
  output$tran_trips_5 <-renderValueBox({
    valueBox(percent((values5() %>% pull(TransitTrips)-base_values() %>% pull(TransitTrips))/base_values() %>% pull(TransitTrips)), "Transit Trips", width=12, color="navy")
  })
}
shinyApp(ui, server)

