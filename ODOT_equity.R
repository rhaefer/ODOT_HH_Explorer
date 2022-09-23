library(tidyverse)
library(shiny)
library(data.table)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(DT)
library(sf)
library(mapview)
library(tigris)
library(leaflet)
library(fuzzyjoin)
library(maps)
library(stringr)
library(ggpubr)
theme_set(theme_pubr())

data(county.fips)
hh <-fread("C:\\Users\\reid.haefer\\OneDrive - Resource Systems Group, Inc\\Documents\\projects\\ODOT\\Household_Common.csv") %>%
  filter(Year==2010)

bzone_vars<-read_csv("C:\\Users\\reid.haefer\\OneDrive - Resource Systems Group, Inc\\Documents\\projects\\ODOT\\ODOT_equity\\bzone_SLD_vars.csv")
area_type_gab_bg <-st_read("C://Users//reid.haefer//OneDrive - Resource Systems Group, Inc//Documents//projects//general//OR_AreaTypes","OR_AreaType") %>%
  st_transform(crs=4326)

eq_high <- st_read(dsn="C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Equity","High_State_Equity") %>%
  st_transform(crs=4326) %>%
  mutate(eq_cat="high")
eq_low <- st_read(dsn="C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Equity","Low_State_Equity2") %>%
  st_transform(crs=4326)%>%
  mutate(eq_cat="low")
eq_low_med <- st_read(dsn="C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Equity","LowMed_State_Equity") %>%
  st_transform(crs=4326)%>%
  mutate(eq_cat="low_med")
eq_med_high <- st_read(dsn="C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Equity","MedHigh_State_Equity") %>%
  st_transform(crs=4326)%>%
  mutate(eq_cat="med_high")
equity<-bind_rows(eq_high, eq_low, eq_low_med, eq_med_high)


pt_shiny<-read_csv('C:\\Users\\reid.haefer\\OneDrive - Resource Systems Group, Inc\\Documents\\projects\\ODOT\\ODOT_HH_Explorer\\place_type_output.csv') %>%
  mutate(area_type_shiny=case_when(AreaType=="Suburb/Town" ~ "Outer",
                                   AreaType=="Low Density/Rural" ~ "Fringe",
                                   AreaType=="Close In Community" ~ "Inner",
                                   AreaType=="Regional Center" ~ "Center")) %>%
  select(GEOID10, area_type_shiny)

test<-equity %>% mutate(fips=paste0(STATEFP,COUNTYFP)) %>%
  select(GEOID, eq_cat, fips) %>%
  left_join(county.fips %>% mutate(fips=as.character(fips)), by="fips") %>% 
  left_join(pt_shiny, by=c("GEOID"="GEOID10")) %>%
  mutate(county=gsub(".*,","",polyname),
         county=str_to_title(county),
         id=paste(county, area_type_shiny, sep="-"))




## VE area types and ODOT block group equity analysis

state_sum <- data.frame(Dvmt_q1=quantile(hh$Dvmt, 0.25),
                        Dvmt_q2=quantile(hh$Dvmt, 0.5),
                        Dvmt_q3=quantile(hh$Dvmt, 0.75),
                        TransitTrips_q1=quantile(hh$TransitTrips, 0.25),
                        TransitTrips_q2=quantile(hh$TransitTrips, 0.5),
                        TransitTrips_q3=quantile(hh$TransitTrips, 0.75),
                        BikeTrips_q1=quantile(hh$BikeTrips, 0.25),
                        BikeTrips_q2=quantile(hh$BikeTrips, 0.5),
                        BikeTrips_q3=quantile(hh$BikeTrips, 0.75),
                        WalkTrips_q1=quantile(hh$WalkTrips, 0.25),
                        WalkTrips_q2=quantile(hh$WalkTrips, 0.5),
                        WalkTrips_q3=quantile(hh$WalkTrips, 0.75),
                        OwnCost_q1=quantile(hh$OwnCost, 0.25),
                        OwnCost_q2=quantile(hh$OwnCost, 0.5),
                        OwnCost_q3=quantile(hh$OwnCost, 0.75),
                        DailyCO2e_q1=quantile(hh$DailyCO2e, 0.25),
                        DailyCO2e_q2=quantile(hh$DailyCO2e, 0.5),
                        DailyCO2e_q3=quantile(hh$DailyCO2e, 0.75)) %>% 
  pivot_longer(cols=everything()) %>%
  mutate(category=str_sub(name, end=-4))
state_sum

hh_bzone_vars<-hh %>%# join hh file to bzone vars table
  left_join(bzone_vars, by=c("Bzone"="bzone")) %>% 
  mutate(atype=str_to_title(atype),
         id=paste(azone, atype, sep="-")) 


## VE datastore values and hh outputs

hh_a_type1 %>% ggplot(aes(Dvmt_q3, D1C)) + geom_point() + geom_smooth()

hh_a_type1 %>% ggplot(aes(Dvmt_q3, D3bpo4)) + geom_point() + geom_smooth()

hh_a_type1 %>% ggplot(aes(Dvmt_q3, D4C)) + geom_point() + geom_smooth()

hh_a_type1 %>% ggplot(aes(BikeTrips_q3, D1D)) + geom_point() + geom_smooth()

hh_a_type<- hh_bzone_vars %>%
  group_by(id) %>%
  summarise(
    Dvmt_total=sum(Dvmt,na.rm=T),
    Dvmt_mean=mean(Dvmt,na.rm=T),
    Dvmt_q1=quantile(Dvmt, 0.25),
    Dvmt_q2=quantile(Dvmt, 0.5),
    Dvmt_q3=quantile(Dvmt, 0.75),
    TransitTrips_mean=mean(TransitTrips, na.rm=T),
    TransitTrips_q1=quantile(TransitTrips, 0.25),
    TransitTrips_q2=quantile(TransitTrips, 0.5),
    TransitTrips_q3=quantile(TransitTrips, 0.75),
    BikeTrips_mean=mean(BikeTrips, na.rm=T),
    BikeTrips_q1=quantile(BikeTrips, 0.25),
    BikeTrips_q2=quantile(BikeTrips, 0.5),
    BikeTrips_q3=quantile(BikeTrips, 0.75),
    WalkTrips_mean=mean(WalkTrips,na.rm=T),
    WalkTrips_q1=quantile(WalkTrips, 0.25),
    WalkTrips_q2=quantile(WalkTrips, 0.5),
    WalkTrips_q3=quantile(WalkTrips, 0.75),
    OwnCost_mean=mean(OwnCost,na.rm=T),
    OwnCost_total=sum(OwnCost,na.rm=T),
    OwnCost_q1=quantile( OwnCost, 0.25),
    OwnCost_q2=quantile( OwnCost, 0.5),
    OwnCost_q3=quantile( OwnCost, 0.75),
    DailyCO2e_mean=mean(DailyCO2e,na.rm=T),
    DailyCO2e_q1=quantile( DailyCO2e, 0.25),
    DailyCO2e_q2=quantile(DailyCO2e, 0.5),
    DailyCO2e_q3=quantile( DailyCO2e, 0.75),
    households=n(),
    population=sum(HhSize),
    households=sum(households,na.rm=T)) %>%
  mutate(dvmt_per_capita=Dvmt_total/population,
         owner_cost_per_capita=OwnCost_total/population)


hh_a_type_bg<-test %>%
  # area_type_gab_bg %>% mutate(fips=paste0(STATEFP10,COUNTYFP10)) %>% select(fips, GEOID10, VEAreaType) %>%
  # left_join(county.fips %>% mutate(fips=as.character(fips)), by="fips") %>% 
  # mutate(county=gsub(".*,","",polyname),
  #        county=str_to_title(county),
  #        id=paste(county, VEAreaType, sep="-")) %>%
  left_join(hh_a_type, by="id")

missing<-hh_a_type %>% mutate(test="test") %>% left_join(test, by="id") %>% filter(is.na(fips)) %>% View()

hh_equity<-equity %>%
  left_join(hh_a_type_bg %>% data.frame(), by="GEOID")

st_write(dsn="C:\\Users\\reid.haefer\\OneDrive - Resource Systems Group, Inc\\Documents\\projects\\ODOT\\ODOT_equity\\GIS", obj=hh_equity, layer="equity_final", driver="ESRI Shapefile")

hh_equitydf<- hh_equity %>% data.frame()

sum_mean<-hh_equitydf %>%
  #filter(Final != 0) %>% 
  group_by(id,Dvmt_mean,TransitTrips_mean,WalkTrips_mean,OwnCost_mean,DailyCO2e_mean, population, households,dvmt_per_capita, owner_cost_per_capita) %>%
  summarise(equity_mean=weighted.mean(StateEquit, Total_Popu), 
            Age_Over64_mean=weighted.mean(Age_Over64, Total_Popu),
            PovertySta_mean=weighted.mean(PovertySta, Total_Popu),
            NonWhite_o_mean=weighted.mean(NonWhite_o, Total_Popu),
            Disability_mean=weighted.mean(Disability, Total_Popu),
            equity_q3=quantile(StateEquit, 0.75), 
            Age_Over64_q3=quantile(Age_Over64, 0.75),
            PovertySta_q3=quantile(PovertySta, 0.75),
            NonWhite_o_q3=quantile(NonWhite_o, 0.75),
            Disability_q3=quantile(Disability, 0.75)) %>%
  filter(!is.na(Dvmt_mean)) %>%
  select(id,Dvmt_mean,TransitTrips_mean,WalkTrips_mean,OwnCost_mean ,equity_mean, everything())

# Poverty Status & VMT

a<-sum_mean %>% ggplot(aes(PovertySta_mean,dvmt_per_capita)) + geom_point() + geom_smooth() + ggtitle("Poverty Status (Mean) & DVMT Per Capita") + xlab("Poverty Status (Weighted Mean)") + ylab("DVMT Per Capita")

b<-sum_mean %>% ggplot(aes(PovertySta_q3,dvmt_per_capita)) + geom_point() + geom_smooth() + ggtitle("Poverty Status (Q3) & DVMT Per Capita") + xlab("Poverty Status (3rd Quartile)") + ylab("DVMT Per Capita")

one <- ggarrange(a, b, 
                       labels = c("",""),
                       ncol = 2, nrow = 1)
one

linearMod <- lm(PovertySta_q3 ~ dvmt_per_capita, data=sum_mean)
summary(linearMod)

linearMod <- lm(PovertySta_mean ~ dvmt_per_capita, data=sum_mean)
summary(linearMod)

# State Equity & VMT

f<-sum_mean %>% ggplot(aes(equity_mean,dvmt_per_capita)) + geom_point() + geom_smooth() + ggtitle("State Equity (Mean) & DVMT Per Capita") + xlab("State Equity (Weighted Mean)") + ylab("DVMT Per Capita")

g<-sum_mean %>% ggplot(aes(equity_q3,dvmt_per_capita)) + geom_point() + geom_smooth() + ggtitle("State Equity (Q3) & DVMT Per Capita") + xlab("State Equity (3rd Quartile)") + ylab("DVMT Per Capita")

four <- ggarrange(f, g, 
                 labels = c("",""),
                 ncol = 2, nrow = 1)
four

linearMod <- lm(equity_q3 ~ dvmt_per_capita, data=sum_mean)
summary(linearMod)

glance(linearMod)

linearMod <- lm(equity_mean ~ dvmt_per_capita, data=sum_mean)
summary(linearMod)

# Elderly Status & VMT

c<-sum_mean %>% ggplot(aes(Age_Over64_mean,dvmt_per_capita)) + geom_point() + geom_smooth() + ggtitle("Elderly Population (Mean) & DVMT Per Capita") + xlab("Elderly Population (Weighted Mean)") + ylab("DVMT Per Capita")

d<-sum_mean %>% ggplot(aes(Age_Over64_q3,dvmt_per_capita)) + geom_point() + geom_smooth() + ggtitle("Elderly Population (Q3) & DVMT Per Capita") + xlab("Elderly Population (3rd Quartile)") + ylab("DVMT Per Capita")

two <- ggarrange(c, d, 
                 labels = c("",""),
                 ncol = 2, nrow = 1)
two

linearMod <- lm(Age_Over64_q3 ~ dvmt_per_capita, data=sum_mean)
summary(linearMod)

linearMod <- lm(Age_Over64_mean ~ dvmt_per_capita, data=sum_mean)
summary(linearMod)

# Poverty Status & Walk Trips

e<-sum_mean %>% ggplot(aes(PovertySta_mean,WalkTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Poverty Status (Mean) & Walk Trips") + xlab("Poverty Status (Weighted Mean)") + ylab("Walk Trips")

f<-sum_mean %>% ggplot(aes(PovertySta_q3,WalkTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Poverty Status (Q3) & Walk Trips") + xlab("Poverty Status (3rd Quartile)") + ylab("Walk Trips")

three <- ggarrange(e, f, 
                 labels = c("",""),
                 ncol = 2, nrow = 1)
three

linearMod <- lm(PovertySta_mean ~ WalkTrips_mean, data=sum_mean)
summary(linearMod)

linearMod <- lm(PovertySta_q3 ~ WalkTrips_mean, data=sum_mean)
summary(linearMod)

# Poverty Status & transit Trips

e<-sum_mean %>% ggplot(aes(PovertySta_mean,TransitTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Poverty Status (Mean) & Transit Trips") + xlab("Poverty Status (Weighted Mean)") + ylab("Transit Trips")

f<-sum_mean %>% ggplot(aes(PovertySta_q3,TransitTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Poverty Status (Q3) & Transit Trips") + xlab("Poverty Status (3rd Quartile)") + ylab("Transit Trips")

three <- ggarrange(e, f, 
                   labels = c("",""),
                   ncol = 2, nrow = 1)
three

linearMod <- lm(PovertySta_mean ~ TransitTrips_mean, data=sum_mean)
summary(linearMod)

linearMod <- lm(PovertySta_q3 ~ WalkTrips_mean, data=sum_mean)
summary(linearMod)

# disabiliity & Transit Trips

e<-sum_mean %>% ggplot(aes(Disability_mean,TransitTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Disability (Mean) & Transit Trips") + xlab("Disability (Weighted Mean)") + ylab("Transit Trips")

f<-sum_mean %>% ggplot(aes(Disability_q3,TransitTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Disability (Q3) & Transit Trips") + xlab("Disability (3rd Quartile)") + ylab("DVMT Per Capita")

three <- ggarrange(e, f, 
                   labels = c("",""),
                   ncol = 2, nrow = 1)
three

linearMod <- lm(Disability_mean ~ TransitTrips_mean, data=sum_mean)
summary(linearMod)

linearMod <- lm(Disability_q3 ~ TransitTrips_mean, data=sum_mean)
summary(linearMod)



# race & vehicle cost

e<-sum_mean %>% ggplot(aes(NonWhite_o_mean,OwnCost_mean)) + geom_point() + geom_smooth() + ggtitle("Race (Non-White) (Mean) & Vehicle Cost") + xlab("Race (Non-White)  (Weighted Mean)") + ylab("Vehicle Cost")

f<-sum_mean %>% ggplot(aes(NonWhite_o_q3,OwnCost_mean)) + geom_point() + geom_smooth() + ggtitle("Race (Non-White) & Vehicle Cost") + xlab("Race (Non-White) (3rd Quartile)") + ylab("Vehicle Cost")

three <- ggarrange(e, f, 
                   labels = c("",""),
                   ncol = 2, nrow = 1)
three

# race & Transit trips

e<-sum_mean %>% ggplot(aes(NonWhite_o_mean,TransitTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Race (Non-White) (Mean) & Transit Trips") + xlab("Race (Non-White)  (Weighted Mean)") + ylab("Transit Trips")

f<-sum_mean %>% ggplot(aes(NonWhite_o_q3,TransitTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Race (Non-White) (Q3) & Transit Trips") + xlab("Race (Non-White) (3rd Quartile)") + ylab("Transit Trips")

three <- ggarrange(e, f, 
                   labels = c("",""),
                   ncol = 2, nrow = 1)
three

linearMod <- lm(NonWhite_o_mean ~ TransitTrips_mean, data=sum_mean)
summary(linearMod)

linearMod <- lm(NonWhite_o_q3 ~ TransitTrips_mean, data=sum_mean)
summary(linearMod)

# state equity & CO2

e<-sum_mean %>% ggplot(aes(equity_mean,DailyCO2e_mean)) + geom_point() + geom_smooth() + ggtitle("State Equity (Mean) & CO2") + xlab("State Equity (Weighted Mean)") + ylab("CO2")

f<-sum_mean %>% ggplot(aes(equity_q3,DailyCO2e_mean)) + geom_point() + geom_smooth() + ggtitle("State Equity (Mean) & CO2") + xlab("State Equity (3rd Quartile)") + ylab("CO2")

three <- ggarrange(e, f, 
                   labels = c("",""),
                   ncol = 2, nrow = 1)
three

linearMod <- lm(Disability_mean ~ WalkTrips_mean, data=sum_mean)
summary(linearMod)

linearMod <- lm(PovertySta_q3 ~ WalkTrips_mean, data=sum_mean)
summary(linearMod)

# Poverty Status & Transit Trips

e<-sum_mean %>% ggplot(aes(Disability_mean,WalkTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Poverty Status (Mean) & Walk Trips") + xlab("Poverty Status (Weighted Mean)") + ylab("Walk Trips")

f<-sum_mean %>% ggplot(aes(Disability_q3,WalkTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Poverty Status (Mean) & Walk Trips") + xlab("Poverty Status (3rd Quartile)") + ylab("DVMT Per Capita")

three <- ggarrange(e, f, 
                   labels = c("",""),
                   ncol = 2, nrow = 1)
three
#######################

sum_mean %>% ggplot(aes(dvmt_per_capita,PovertySta_mean)) + geom_point() + geom_smooth() + ggtitle("Dvmt Per Capita & Poverty Status (Weighted Mean)")

sum_mean %>% ggplot(aes(owner_cost_per_capita,PovertySta_mean)) + geom_point() + geom_smooth() + ggtitle("Owner Cost Per Capita & Poverty Status (Weighted Mean)")

sum_mean %>% ggplot(aes(dvmt_per_capita,equity_mean)) + geom_point() + geom_smooth() + ggtitle("Dvmt Per Capita & state Equity (Weighted Mean)")

linearMod <- lm(PovertySta_mean ~ dvmt_per_capita, data=sum_mean)
summary(linearMod)

linearMod <- lm(PovertySta_mean ~ owner_cost_per_capita, data=sum_mean)
summary(linearMod)

sum_q3<-hh_equitydf %>%
  #filter(Final != 0) %>% 
  group_by(id,Dvmt_mean,TransitTrips_mean,WalkTrips_mean,OwnCost_mean,DailyCO2e_mean, population, households,dvmt_per_capita, owner_cost_per_capita) %>%
  summarise(equity=mean(StateEquit), 
            Age_Over64=weighted.mean(Age_Over64, Total_Popu),
            PovertySta=weighted.mean(PovertySta, Total_Popu),
            NonWhite_o=weighted.mean(NonWhite_o, Total_Popu),
            Disability=weighted.mean(Disability, Total_Popu))

# correlation
cor(sum_mean$equity, sum_mean$TransitTrips_mean)

# build linear model
linearMod <- lm(equity ~ TransitTrips_mean, data=sum_mean)
# linear model-> equity = 1.109213 + ( -0.002616 * Dvmt)
#statistical significance
summary(linearMod)

modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["Dvmt", "Estimate"]  # get beta estimate for Dvmt
std.error <- modelCoeffs["Dvmt", "Std. Error"]  # get std.error for Dvmt
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(sum)-ncol(sum))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

t_value
p_value
f_statistic
model_p

a<-ggplot(sum_mean, aes(equity, Dvmt_mean)) + geom_point() + geom_smooth() + ggtitle("Dvmt & Equity") + theme_minimal()
b<-ggplot(sum_mean, aes(equity, TransitTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Transit Trips & Equity") + theme_minimal()
c<-ggplot(sum_mean, aes(equity, WalkTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Walk Trips & Equity") + theme_minimal()
d<-ggplot(sum_mean, aes(equity, OwnCost_mean)) + geom_point() + geom_smooth() + ggtitle("Owner Cost & Equity") + theme_minimal()
e<-ggplot(sum_mean, aes(equity, DailyCO2e_mean)) + geom_point() + geom_smooth() + ggtitle("C02e & Equity") + theme_minimal()
figure <- ggarrange(a, b, c, d, e,
                    labels = c("","","","",""),
                    ncol = 3, nrow = 2)
figure


# poverty status and walk trips
ggplot(sum_mean, aes(PovertySta, WalkTrips_mean)) + geom_point() + geom_smooth() + ggtitle("Walk Trips & Poverty Status") + theme_minimal()

# poverty status and walk trips
ggplot(sum_q3, aes(Disability, Dvmt_q3)) + geom_point() + geom_smooth() + ggtitle("Walk Trips & Poverty Status") + theme_minimal()


a3<-ggplot(sum_q3, aes(equity, Dvmt_q3)) + geom_point() + geom_smooth() + ggtitle("Dvmt & Equity") + theme_minimal()
b3<-ggplot(sum_q3, aes(equity, TransitTrips_q3)) + geom_point() + geom_smooth() + ggtitle("Transit Trips & Equity") + theme_minimal()
c3<-ggplot(sum_q3, aes(equity, WalkTrips_q3)) + geom_point() + geom_smooth() + ggtitle("Walk Trips & Equity") + theme_minimal()
d3<-ggplot(sum_q3, aes(equity, OwnCost_q3)) + geom_point() + geom_smooth() + ggtitle("Owner Cost & Equity") + theme_minimal()
e3<-ggplot(sum_q3, aes(equity, DailyCO2e_q3)) + geom_point() + geom_smooth() + ggtitle("C02e & Equity") + theme_minimal()
figure_q3 <- ggarrange(a3, b3, c3, d3, e3,
                       labels = c("","","","",""),
                       ncol = 3, nrow = 2)
figure_q3



st_write(dsn="C:\\Users\\reid.haefer\\OneDrive - Resource Systems Group, Inc\\Documents\\projects\\ODOT\\ODOT_equity\\GIS",
         obj=hh_equity, layer="hh_equity", driver="ESRI Shapefile")

hh_equity %>% data.frame() %>% group_by(eq_cat, id) %>% # average of all block groups by equity category
  summarise(Dvmt=mean(Dvmt),
            TransitTrips=mean(TransitTrips), 
            WalkTrips=mean(WalkTrips) , 
            OwnCost=mean(OwnCost), 
            DailyCO2e=mean(DailyCO2e), 
            equity=mean(Final,na.rm=T)) %>% group_by(eq_cat) %>% # average of all block groups by equity category
  summarise(Dvmt=mean(Dvmt),
            TransitTrips=mean(TransitTrips), 
            WalkTrips=mean(WalkTrips) , 
            OwnCost=mean(OwnCost), 
            DailyCO2e=mean(DailyCO2e))

hh_equity %>% mapview(zcol="Dvmt")

test <- hh_equity %>% data.frame() %>% count(eq_cat, id, households, Dvmt, TransitTrips,name="block groups")

## SLD and ODOT equity

place_ODOT_block<-st_read("C://Users//reid.haefer//OneDrive - Resource Systems Group, Inc//Documents//projects//ODOT//ODOT_equity","PlaceTypes") %>% st_transform(crs=4326)

smart_bg<-st_read(dsn="C:/Users/reid.haefer/OneDrive - Resource Systems Group, Inc/Documents/projects/general/SmartLocationDatabaseV3/SmartLocationDatabase.gdb","EPA_SLD_Database_V3") %>%
  st_transform(crs=4326) %>% filter(STATEFP == "41")

equity_sld<-equity %>% select(GEOID,Age_Under1,Age_Over64,Limited_En,White_Alon,NonWhite_o,Total_Popu,Disability,PovertySta,No_Vehicle,OverCrowdi,TransDisIn,StateEquit,X2Equity,Final,eq_cat) %>%
  left_join(
    smart_bg %>% data.frame() %>% select(GEOID10,D1B,D1C,D2A_EPHHM,D2A_JPHH,D2A_WRKEMP,D3BPO4,D4C,D1D), by=c("GEOID"="GEOID10")
  ) 

eq_sld_df <- equity_sld %>% data.frame() %>% select(-geometry)

sld_labels <- data.frame(var=c("D4C","D1B","D1C" ,"D2A_EPHH","D2A_JPHH","D3BPO4","D2A_WRKEMP","D1D"),
                         label=c("Transit Frequency","Population Density","Employment Density","Employment and household entropy","Jobs Per Household","Ped Friendly Intersection Density", "Household Workers Per Job","Activity Density (employment + HUs)" ))

##
eq_sld_sum <- data.frame(Disability_q1=quantile(equity_sld$Disability, 0.25),
                         Disability_q2=quantile(equity_sld$Disability, 0.5),
                         Disability_q3=quantile(equity_sld$Disability, 0.75),
                         Age_Over64_q1=quantile(equity_sld$Age_Over64, 0.25),
                         Age_Over64_q2=quantile(equity_sld$Age_Over64, 0.5),
                         Age_Over64_q3=quantile(equity_sld$Age_Over64, 0.75),
                         PovertySta_q1=quantile(equity_sld$PovertySta, 0.25),
                         PovertySta_q2=quantile(equity_sld$PovertySta, 0.5),
                         PovertySta_q3=quantile(equity_sld$PovertySta, 0.75),
                         Limited_En_q1=quantile(equity_sld$Limited_En, 0.25),
                         Limited_En_q2=quantile(equity_sld$Limited_En, 0.5),
                         Limited_En_q3=quantile(equity_sld$Limited_En, 0.75),
                         D4C_q1=quantile(equity_sld$D4C, 0.25),
                         D4C_q2=quantile(equity_sld$D4C, 0.5),
                         D4C_q3=quantile(equity_sld$D4C, 0.75),
                         D3BPO4_q1=quantile(equity_sld$D3BPO4, 0.25),
                         D3BPO4_q2=quantile(equity_sld$D3BPO4, 0.5),
                         D3BPO4_q3=quantile(equity_sld$D3BPO4, 0.75),
                         D2A_JPHH_q1=quantile(equity_sld$D2A_JPHH, 0.25),
                         D2A_JPHH_q2=quantile(equity_sld$D2A_JPHH, 0.5),
                         D2A_JPHH_q3=quantile(equity_sld$D2A_JPHH, 0.75)) %>% 
  pivot_longer(cols=everything()) %>%
  mutate(category=str_sub(name, end=-4))
eq_sld_sum


# poverty status (4th quartile) & employment density

eq_sld_df%>% filter(PovertySta < 2500) %>%  filter(PovertySta > (eq_sld_sum %>% filter(name=='PovertySta_q3') %>% pull(value))) %>%
  ggplot(aes(PovertySta, D1C)) + geom_point() + geom_smooth() + ggtitle("poverty status (4th quartile) & employment density") + theme_minimal()

# poverty status (4th quartile) & transit fequency

eq_sld_df %>% filter(D4C!=-99999.00 & PovertySta < 2500) %>%  filter(PovertySta > (eq_sld_sum %>% filter(name=='PovertySta_q3') %>% pull(value))) %>%
  ggplot(aes(PovertySta, D4C)) + geom_point() + geom_smooth() + ggtitle("poverty status (4th quartile) & transit") + theme_minimal()

# older population (4th quartile) & ped friendly intersections

eq_sld_df %>%  filter(Age_Over64 > (eq_sld_sum %>% filter(name=='Age_Over64_q3') %>% pull(value))) %>%
  ggplot(aes(Age_Over64, D3BPO4)) + geom_point() + geom_smooth() + ggtitle("Older Population (4th quartile) & ped friendly intersections") + theme_minimal()

# older population (4th quartile) & transit

eq_sld_df%>% filter(D4C!=-99999.00)  %>%  filter(Age_Over64 > (eq_sld_sum %>% filter(name=='Age_Over64_q3') %>% pull(value))) %>% 
  ggplot(aes(Age_Over64, D4C)) + geom_point() + geom_smooth() + ggtitle("Older Population (4th quartile) & transit") + theme_minimal()


## multivariate regression

model <- lm(PovertySta~D1B+D1C, data = eq_sld_df)


st_write(dsn = "C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Place Types", obj = area_type_gab_bg, layer="Block_Group_Area_Type", driver="ESRI Shapefile")

smart_area<-smart_bg %>%
  left_join(area_type_gab_bg %>% data.frame() %>% select(-geometry), by="GEOID10") %>%
  mutate(fips=paste0(STATEFP,COUNTYFP)) %>%
  left_join(county.fips %>% mutate(fips=as.character(fips)), by="fips") %>%
  mutate(county=gsub(".*,","",polyname),
         county=str_to_title(county),
         id=paste(county, VEAreaType, sep="-"))



# smart %>% 
#   filter(COUNTYFP== "001") %>%
#   select(D1B) %>%
#   #mutate(D1B=round(D1B * 639.9994,5)) %>% 
#   mapview()


smart_bzone<-smart_area %>%
  left_join(bzone_vars %>% group_by(id) %>% summarise(bzones=paste(bzone, collapse=" ,"), Dvmt=mean(Dvmt,na.r=T)), by="id")



data<-smart_bzone %>%
  #filter(COUNTYFP10=="005") %>% 
  group_by(id) %>%
  summarise(Dvmt=mean(Dvmt,na.rm=T)) 

st_write(dsn = "C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Place Types", obj = data, layer="Dvmt_County_Area_Type", driver="ESRI Shapefile")

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    +-
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))
title <- tags$div(
  tag.map.title, HTML("Dvmt by County Area Type")
)  

pal <- colorNumeric(
  palette = "Blues",
  domain = data$Dvmt)  

leaflet() %>% addPolygons(data=data,  weight=1, color="white", smoothFactor = 0.2, fillOpacity = 1,
                          fillColor = ~pal(Dvmt),
                          popup=paste0("<b>","County Area Type: ","</b>", data$id , "<br>",
                                       "<b>","dVMT: ","</b>", round(data$Dvmt,0)),
                          highlight = highlightOptions(
                            weight = 2,
                            fillOpacity = 1,
                            color = "red",
                            opacity = .5,
                            bringToFront = TRUE,
                            sendToBack = TRUE)) %>% addTiles() %>%
  addControl(title, position = "topleft", className="map-title") 



smart_bzone %>% mapview(zcol="id")

mapshot(map, url = paste0(getwd(), "/county_areatype_vmt.html"), selfcontained = FALSE)

htmlwidgets::saveWidget(map@map, "test.html", selfcontained=TRUE)

# smart_df<-smart %>% data.frame() %>%
#   mutate(D1B=round(D1B * 639.9994,5),
#          D1C=round(D1C * 639.9994,6),
#          D2A_EPHHM=round(D2A_EPHHM,7),
#          D2A_JPHH=round(D2A_JPHH,8),
#          D2A_WRKEMP=round(D2A_WRKEMP,9),
#          D3BPO4=round(D3BPO4,8),
#          D4C=round(D4C,9)) %>%
#   select(GEOID10, D1B,D1C,D2A_EPHHM,D2A_JPHH,D2A_WRKEMP,D3BPO4,D4C)





eq_high <- st_read(dsn="C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Equity","High_State_Equity") %>%
  st_transform(crs=4326)
eq_low <- st_read(dsn="C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Equity","Low_State_Equity2") %>%
  st_transform(crs=4326)
eq_low_med <- st_read(dsn="C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Equity","LowMed_State_Equity") %>%
  st_transform(crs=4326)
eq_med_high <- st_read(dsn="C:\\Users\\reid.haefer\\Resource Systems Group, Inc\\Projects - Documents\\OR\\21068-ODOT_OHP\\GIS\\Equity","MedHigh_State_Equity") %>%
  st_transform(crs=4326)

equity<-bind_rows(eq_high, eq_low, eq_low_med, eq_med_high)




OR_counties<-counties(state="OR")%>%
  st_transform(crs=4326)

OR_counties %>% mapview()

sum<-all %>% group_by(scenario,Azone) %>%
  summarise(Dvmt=mean(Dvmt,na.rm=T),
            AveVehCostPM=mean(AveVehCostPM,na.rm=T),
            DailyCO2e=mean(DailyCO2e,na.rm=T),
            DailyGGE=mean(DailyGGE,na.rm=T),
            VehicleTrips=mean(VehicleTrips,na.rm=T),
            TransitTrips=mean(TransitTrips,na.rm=T))

common_2015 <- OR_counties %>% select(NAME) %>% left_join(
  sum %>% filter(scenario=="hh1"), by=c("NAME"="Azone")
) 

mm_sts_2050 <- OR_counties %>% select(NAME) %>% left_join(
  sum %>% filter(scenario=="hh3"), by=c("NAME"="Azone")
) 

conpal1 <- colorNumeric(palette = "Blues", domain = common_2015$Dvmt, na.color = "black")

conpal2 <- colorNumeric(palette = "Blues", domain = mm_sts_2050$Dvmt, na.color = "black")


tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    +-
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))
title <- tags$div(
  tag.map.title, HTML("Oregon Equity & HH VE Map")
)  

leaflet() %>% 
  addPolygons(data=common_2015, fillColor = ~conpal1(common_2015$Dvmt), fillOpacity = 1, color="black", weight=1, group="Average HH dVMT - Common 2015", 
              popup =paste0("<b>","County: ","</b>", common_2015$NAME , "<br>",
                            "<b>","dVMT: ","</b>", round(common_2015$Dvmt,0)
              )) %>% 
  addPolygons(data=mm_sts_2050, fillColor = ~conpal2(mm_sts_2050$Dvmt), fillOpacity = 1, color="black", weight=1, group="Average HH dVMT - MM STS 2050", 
              popup =paste0("<b>","County: ","</b>", mm_sts_2050$NAME , "<br>",
                            "<b>","dVMT: ","</b>", round(mm_sts_2050$Dvmt,0)
              )) %>% 
  addTiles() %>%
  addPolygons(data=eq_low, fillColor = "#581b18", fillOpacity = .7,weight=1, color="white", group="low equity") %>% 
  addPolygons(data=eq_low_med, fillColor = "#dca8a5", fillOpacity = .7,weight=1, color="white", group="med-low equity") %>% 
  addPolygons(data=eq_med_high, fillColor = "#9fe5b5", fillOpacity = .7,weight=1, color="white", group="med-high equity") %>% 
  addPolygons(data=eq_high, fillColor = "#18582c", fillOpacity = .7,weight=1, color="white", group="high equity") %>% 
  addLayersControl(
    # baseGroups = c("OSM (default)"),
    overlayGroups = c("Average HH dVMT - Common 2015", "Average HH dVMT - MM STS 2050","low equity", "med-low equity", "med-high equity", "high equity"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% hideGroup(c("Average HH dVMT - MM STS 2050","low equity", "med-low equity", "med-high equity", "high equity")) %>%
  addControl(title, position = "topleft", className="map-title") 



