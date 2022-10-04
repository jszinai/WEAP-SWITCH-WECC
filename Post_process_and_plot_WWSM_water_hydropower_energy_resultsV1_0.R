
#Script to process and create plots and maps of results from WEAP runs

library(tidyverse)
library(lubridate)
library(stringr)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(raster)
library(rgdal)
library(sp)
library(sf)
library(readxl)
########################

## WEAP directory:
WEAP_run_id <- "WWSM_WEAPSwitch_V1_0"

mapping_file <- "Mapping_WEAP_Objects_V1_0.xlsx"

#update with your directories here:
# weap_dir <- "/Users/juliaszinai/Dropbox/Linux_work/WEAP_data_and_scripts/"
weap_dir_results <- "/Users/juliaszinai/Dropbox/Linux_work/WEAP_data_and_scripts/WEAP_results/WWSM_WEAPSwitch_V1_0/"

#list of 15 climate scenarios run for WWSM V1 submitted manuscript
scenario_list <- c("RefLOCA", "CCSM","HadGEM2-ES","CanESM","CESM1-CAM5","ACCESS-1.0","CNRM-CM5",
                   "GFDL-ESM2M","bcc-csm1-1", "MPI-ESM-LR", "MIROC5", "HadGEM2-CC", "GFDL-CM3" ,
                   "CMCC-CMS"  , "CMCC-CM", "CESM1-BGC")

climate_scenario_list <- c("Reference","CCSM","HadGEM2-ES","CanESM","CESM1-CAM5","ACCESS-1.0","CNRM-CM5",
                           "GFDL-ESM2M","bcc-csm1-1", "MPI-ESM-LR", "MIROC5", "HadGEM2-CC", "GFDL-CM3" ,
                           "CMCC-CMS"  , "CMCC-CM", "CESM1-BGC")

#colorblind friendly palette
cbpal <- c("#999999","#004949","#009292","#ff6db6","#ffb6db",
           "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
           "#920000","#924900","#db6d00","#24ff24","#ffff6d")

#####
#Hydropower

WEAP_hydro_data_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number,"_Hydropower Generation",".csv", sep="")
  WEAP_data <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/",WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
  
  #renaming some column names
  WEAP_data <- rename(WEAP_data, "Year" = "X.Columns...Year", "Month" = "Timestep")
  WEAP_data$Max.Gigawatt.Hour. <- NULL
  WEAP_data$Min.Gigawatt.Hour. <- NULL
  WEAP_data$Sum.Gigawatt.Hour. <- NULL
  WEAP_data$Median.Gigawatt.Hour. <- NULL
  WEAP_data$SD.Gigawatt.Hour. <- NULL
  WEAP_data$Mean.Gigawatt.Hour. <- NULL
  WEAP_data$RMS.Gigawatt.Hour. <- NULL
  
  #transpose to long format
  WEAP_data_long <- gather(WEAP_data, WEAP_generator_name_raw, Generation_GWh, 3:ncol(WEAP_data))
  
  #parse generator name from column
  #removing GWh unit label
  WEAP_generator_name <- str_replace(WEAP_data_long$WEAP_generator_name_raw, ".Gigawatt.Hour.","\\.")
  #replacing . with space and removing trailing space
  WEAP_generator_name1 <- str_trim(str_replace_all(WEAP_generator_name, "\\."," "), side = "right")
  #checking length
  WEAP_generator_name1 <- cbind(WEAP_generator_name1, str_length(WEAP_generator_name1))
  WEAP_generator_name1 <- as.data.frame(WEAP_generator_name1)
  colnames(WEAP_generator_name1) <- c("WEAP_generator_name", "length")
  WEAP_generator_name1$length <- NULL
  #binding corrected name and deleting old name
  WEAP_data_long <- cbind(WEAP_data_long, WEAP_generator_name1)
  WEAP_data_long$WEAP_generator_name_raw <- NULL
  
  #dropping spin up and later years
  WEAP_data_long <- WEAP_data_long %>% filter(Year > 2015 & Year < 2056)
  
  WEAP_data_long$Scenario <- scenario
  WEAP_data_long$Scenario_number <- scenario
  
  WEAP_data_long$Date <- as.Date(paste(WEAP_data_long$Year, WEAP_data_long$Month, "1", sep="-"),format = "%Y-%m-%d")
  
  WEAP_hydro_data_long_all_scenarios <- rbind(WEAP_hydro_data_long_all_scenarios, WEAP_data_long)
  
}


WEAP_hydro_monthly_total_all_scenarios <- WEAP_hydro_data_long_all_scenarios %>% group_by(Scenario, Scenario_number, Year, Month, Date) %>% summarize(Generation_GWh = sum(Generation_GWh))

#CSV of monthly hydropower generation summed across all WECC
write.csv(WEAP_hydro_monthly_total_all_scenarios, file = paste(weap_dir_results,"/Results_plots/", "WEAP_total_WECC_monthly_hydro_generation_all_scenarios", ".csv", sep=""))

#total WECC WEAP hydropower generation annually for each scenario
hydro_annual_total_all_scenarios <- WEAP_hydro_data_long_all_scenarios %>% group_by(Scenario, Year) %>% summarize(Generation_GWh = sum(Generation_GWh))
hydro_annual_total_all_scenarios$Year <- as.numeric(hydro_annual_total_all_scenarios$Year)

write.csv(hydro_annual_total_all_scenarios, file = paste(weap_dir_results,"/Results_plots/", "WEAP_total_WECC_annual_hydro_generation_all_scenarios", ".csv", sep=""))

##FIGURE 7C with ALL YEARS (2020 - 2050)
# #seasonal shift in 2050 monthly average generation
WEAP_hydro_monthly_avg_allyrs_all_scenarios_1 <- WEAP_hydro_monthly_total_all_scenarios %>% filter(Year>=2020 & Year<=2050) 
WEAP_hydro_monthly_avg_allyrs_all_scenarios <- WEAP_hydro_monthly_avg_allyrs_all_scenarios_1 %>% group_by(Scenario, Month) %>% summarize(Generation_GWh = mean(Generation_GWh))

#seasonal shift in 2050 monthly average generation (But Reference scenario is black and all climate scenarios are grey)
cols <- c('Reference' = 'black', 'CC' = 'grey')
WEAP_hydro_monthly_avg_allyrs_all_scenarios$scenario_color <- ifelse(WEAP_hydro_monthly_avg_allyrs_all_scenarios$Scenario == "Reference", "Reference", "CC")

png(paste(weap_dir_results,"/Results_plots/", "Fig7C_CWEAP_total_WECC_2020_to_2050monthly_avg_hydro_generation_Ref_vs_all_scenarios", ".png", sep=""), width=900, height=600, res=100)

ggplot(data=WEAP_hydro_monthly_avg_allyrs_all_scenarios, aes(x=factor(Month), y=Generation_GWh,  group=Scenario, colour = scenario_color)) +
  geom_line(size = 1.1) + xlab("Month") + ylab("Generation (GWh)") + ggtitle("Total WECC 2020 - 2050 average monthly hydropower generation") + 
  theme_bw() + 
  theme(panel.grid.minor.y = element_line(color = "grey"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=0, size=14), axis.title.x = element_text(size=16), 
        legend.position="bottom", legend.text = element_text(size = 16), legend.title = element_text(size = 16), plot.title = element_text(size=18, hjust=0.5), aspect.ratio = 1) +
  scale_colour_manual(values = cols, name = "Scenario", labels=c("15 GCM Scenarios", "Reference (historical climate)"))

dev.off()


#Hydropower annual exceedance for all scenarios
hydro_exceedance <- read.table(file=paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/","Hydropower Generation Exceedance.csv",sep=""), skip = 3, header = TRUE, sep =',')

hydro_exceedance <- hydro_exceedance %>% filter(Scenario != "Sum" & Scenario != "Min" & Scenario != "Max" & Scenario != "Mean" & Scenario != "Median" & Scenario != "SD" & Scenario != "RMS" )

hydro_exceedance_long <- gather(hydro_exceedance, "Percent_of_time_exceeded", "Annual_Generation_GWh", X2.:X98.)


hydro_exceedance_long$Percent_of_time_exceeded2 <- str_replace(hydro_exceedance_long$Percent_of_time_exceeded, ".","\\.") 
hydro_exceedance_long$Percent_of_time_exceeded2 <- str_trim(str_replace_all(hydro_exceedance_long$Percent_of_time_exceeded2, "\\.",""), side = "right")
hydro_exceedance_long$Percent_of_time_exceeded2 <- as.numeric(hydro_exceedance_long$Percent_of_time_exceeded2)
hydro_exceedance_long$Percent_of_time_exceeded <- hydro_exceedance_long$Percent_of_time_exceeded2/100
hydro_exceedance_long$Percent_of_time_exceeded2 <- NULL

###FIGURE 7B
#plot of Hydropower annual exceedance for all scenarios with Ref scenario black and all GCM scenarios grey
cols <- c('Reference' = 'black', 'CC' = 'grey')
hydro_exceedance_long$scenario_color <- ifelse(hydro_exceedance_long$Scenario == "RefLOCA", "Reference", "CC")

png(paste(weap_dir_results,"/Results_plots/", "Fig7B_WEAP_hydro_generation_annual_perc_exceeded_Ref_vs_GCMs", ".png", sep=""), width=900, height=600, res=100)

ggplot(data=hydro_exceedance_long, aes(x=Percent_of_time_exceeded, y=Annual_Generation_GWh,  group=Scenario, colour = scenario_color)) +
  geom_line(size = 1.1) + xlab("Percent of Time Exceeded") + ylab("Annual Generation (GWh)") + ggtitle("Total WECC annual hydropower generation exceedance") + 
  theme_bw() + 
  theme(panel.grid.minor.y = element_line(color = "grey"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  axis.line = element_line(colour = "black"), 
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=0, size=14), axis.title.x = element_text(size=16), 
        legend.position="bottom", legend.text = element_text(size = 16), legend.title = element_text(size = 16), plot.title = element_text(size=18, hjust=0.5), aspect.ratio = 1)  + 
  scale_colour_manual(values = cols, name = "Scenario", labels=c("15 GCM Scenarios", "Reference (historical climate)")) +
  scale_x_continuous(labels = scales::percent)

dev.off()


#######


#Water supplies delivered (water use) by sector, from all sources, all demand sites

WEAP_supplydel_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  # i = 1
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number, "_Supply Delivered All Demand Sites",".csv", sep="")
  WEAP_data <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
  
  #renaming some column names
  WEAP_data <- rename(WEAP_data, "Year" = "X.Columns...Year", "Month" = "Timestep")
  WEAP_data$Max.Acre.foot. <- NULL
  WEAP_data$Min.Acre.foot. <- NULL
  WEAP_data$Sum.Acre.foot. <- NULL
  WEAP_data$Median.Acre.foot. <- NULL
  WEAP_data$SD.Acre.foot. <- NULL
  WEAP_data$Mean.Acre.foot. <- NULL
  WEAP_data$RMS.Acre.foot. <- NULL
  
  #transpose to long format
  WEAP_data_long <- gather(WEAP_data, WEAP_object_name_raw, Supply_delivered_AF, 3:ncol(WEAP_data))

  #parse object name from column
  #removing acre foot unit label
  WEAP_object_name <- str_replace(WEAP_data_long$WEAP_object_name_raw, ".Acre.foot.","\\.")
  #replacing . with space and removing trailing space
  WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
  #checking length
  WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
  WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
  colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
  WEAP_object_name1$length <- NULL
  #binding corrected name and deleting old name
  WEAP_data_long <- cbind(WEAP_data_long, WEAP_object_name1)
  WEAP_data_long$WEAP_object_name_raw <- NULL
  
  #dropping spin up and later years
  WEAP_data_long <- WEAP_data_long %>% filter(Year > 2015 & Year < 2056)
  
  WEAP_data_long$Scenario <- scenario
  WEAP_data_long$Scenario_number <- scenario
  
  WEAP_data_long$Date <- as.Date(paste(WEAP_data_long$Year, WEAP_data_long$Month, "1", sep="-"),format = "%Y-%m-%d")
  
  WEAP_supplydel_long_all_scenarios <- rbind(WEAP_supplydel_long_all_scenarios, WEAP_data_long)
  
}


#read in mapping file with sectors for each demand object
WEAP_object_demand_sector_mapping <- read_excel(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", mapping_file, sep=""), sheet = "WEAPobject_demand_sector_mappin", col_names = TRUE)

WEAP_supplydel_sector_all_scenarios <- left_join(WEAP_supplydel_long_all_scenarios, WEAP_object_demand_sector_mapping, c("WEAP_object_name" = "WEAP_object_name"))

#checking for missing demand site names
missingWEAP_supplydel_sector_all_scenarios <- WEAP_supplydel_sector_all_scenarios %>% filter(is.na(Demand_sector))
table(missingWEAP_supplydel_sector_all_scenarios$WEAP_object_name)

#sum monthly supply deliveries by sector and scenario
WEAP_supplydel_monthly_sector_all_scenarios <- WEAP_supplydel_sector_all_scenarios %>% group_by(Scenario, Demand_sector, Year, Month, Date) %>% summarize(Supply_delivered_AF = sum(Supply_delivered_AF,na.rm=TRUE))

#sum annually by sector across all nodes for each scenario
WEAP_supplydel_annual_total_by_sector_scenario <- WEAP_supplydel_sector_all_scenarios %>% group_by(Year, Demand_sector, Scenario) %>% summarize(Supply_delivered_AF = sum(Supply_delivered_AF,na.rm=TRUE))

#output annual supply deliveries by sector for all scenarios to CSV
write.csv(WEAP_supplydel_annual_total_by_sector_scenario, file = paste(weap_dir_results,"/Results_plots/", "WEAP_total_WECC_supplies_delivered_by_sector_all_scenarios", ".csv", sep=""))


##FIGURE 6A
#Box plot of supply deliveries by sector 
#convert from acre feet to cubic km
WEAP_supplydel_annual_total_by_sector_scenario$Supply_delivered_km3 <- WEAP_supplydel_annual_total_by_sector_scenario$Supply_delivered_AF/(810714)

#reorder factors
WEAP_supplydel_annual_total_by_sector_scenario$Ref_vs_CC_Scenario <- ifelse(WEAP_supplydel_annual_total_by_sector_scenario$Scenario == "Reference", "Reference", "Climate Scenarios")

WEAP_supplydel_annual_total_by_sector_scenario$Ref_vs_CC_Scenario <- ordered(WEAP_supplydel_annual_total_by_sector_scenario$Ref_vs_CC_Scenario, levels = c("Reference","Climate Scenarios"))

#facet plot of box and whiskers by source and sector for Reference vs climate scenarios

#box and whiskers by sector of deltas by decade
png(paste(weap_dir_results,"/Results_plots/", "Fig6A_FACET_SupplyDeliveries_Box_plot_of_by_Sector_by_Scenario",".png", sep=""), width=1000, height=800, res=100)

ggplot(data = WEAP_supplydel_annual_total_by_sector_scenario, aes(Ref_vs_CC_Scenario, Supply_delivered_km3)) + 
  geom_boxplot(outlier.shape = 1, fill = "grey") + 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  facet_grid(Demand_sector ~ ., scales="free_y") + 
  labs(y = expression(paste("Supply Deliveries ", (km^3))), x = "Scenario") +
  ggtitle("Total Annual Supply Deliveries by Sector") +
  theme_bw() + 
  theme(panel.grid.minor.y = element_line(color = "grey"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"), strip.text.y = element_text(size = 16),strip.text.x = element_text(size = 16),
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=0, size=14), axis.title.x = element_text(size=16), 
        legend.position="right", legend.text = element_text(size = 16), legend.title = element_text(size = 16), plot.title = element_text(size=18, hjust=0.5)) 

dev.off()


##FIGURE 6C
#Supply Deliveries from GW annual exceedance for all scenarios for agriculture

GW_exceedance <- read.table(file=paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/","GW Supply Delivered Exceedance.csv",sep=""), skip = 3, header = TRUE, sep =',')

GW_exceedance <- GW_exceedance %>% filter(Scenario != "Sum" & Scenario != "Min" & Scenario != "Max" & Scenario != "Mean" & Scenario != "Median" & Scenario != "SD" & Scenario != "RMS" )

GW_exceedance_long <- gather(GW_exceedance, "Percent_of_time_exceeded", "Annual_Deliveries_cubic_meters", X2.:X98.)

GW_exceedance_long$Percent_of_time_exceeded2 <- str_replace(GW_exceedance_long$Percent_of_time_exceeded, ".","\\.") 
GW_exceedance_long$Percent_of_time_exceeded2 <- str_trim(str_replace_all(GW_exceedance_long$Percent_of_time_exceeded2, "\\.",""), side = "right")
GW_exceedance_long$Percent_of_time_exceeded2 <- as.numeric(GW_exceedance_long$Percent_of_time_exceeded2)
GW_exceedance_long$Percent_of_time_exceeded <- GW_exceedance_long$Percent_of_time_exceeded2/100
GW_exceedance_long$Percent_of_time_exceeded2 <- NULL

GW_exceedance_long$Annual_Deliveries_km3 <- GW_exceedance_long$Annual_Deliveries_cubic_meters/(10^9)

#plot of GW annual exceedance for all scenarios with Ref scenario black and all GCM scenarios grey
cols <- c('Reference' = 'black', 'CC' = 'grey')
GW_exceedance_long$scenario_color <- ifelse(GW_exceedance_long$Scenario == "RefLOCA", "Reference", "CC")

png(paste(weap_dir_results,"/Results_plots/", "Fig6C_WEAP_GW_supply_del_annual_perc_exceeded_Ref_vs_GCMs", ".png", sep=""), width=1000, height=800, res=100)

ggplot(data=GW_exceedance_long, aes(x=Percent_of_time_exceeded, y=Annual_Deliveries_km3,  group=Scenario, colour = scenario_color)) +
  geom_line(size = 1.1) +
  labs(y = expression(paste("Supply Deliveries ", (km^3))), x = "Percent of Time Exceeded") +
  ggtitle("Annual exceedence of groundwater supply deliveries") + 
  theme_bw() + 
  theme(panel.grid.minor.y = element_line(color = "grey"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=0, size=14), axis.title.x = element_text(size=16), 
        legend.position="bottom", legend.text = element_text(size = 16), legend.title = element_text(size = 16), plot.title = element_text(size=18, hjust=0.5), aspect.ratio = 1)  + 
  scale_colour_manual(values = cols, name = "Scenario", labels=c("15 GCM Scenarios", "Reference (historical climate)")) +
  scale_x_continuous(labels = scales::percent)

dev.off()

#####SUPPLY DELIVERED FROM GW AND NON_GW SOURCES TO DEMAND SITES BY SECTOR

#For FIGURE 6B and FIGURE 6D

#MUNI supply deliveries, separated out by total and surface water deliveries (GW deliveries are calculated as the difference between total and sw)
#These are outputs of a VBA script that goes through the WEAP deliveries to the Muni "treatment plant" objects from the various transmission links that connect to GW and SW sources (no reuse is included here)

muni_directory <- paste(weap_dir_results, WEAP_run_id,"data/Results/Muni", sep="")

#get list of file names in MUNI directory, these are the gw and total supply delivered volumes by muni
setwd(muni_directory)

muni_file_list <- list(list.files(path=".", pattern=NULL, all.files=FALSE,
                                  full.names=FALSE))
#this will get the list of the munis
muni_file_df <- do.call(rbind, lapply(muni_file_list, as.data.frame))
muni_file_df$file_name <- muni_file_df$`X[[i]]`

All_MUNI_SW <- data.frame()
All_MUNI_Total <- data.frame()
for (i in 1:length(muni_file_df$file_name)){
  
  #determine which files are GW + SW or just SW
  muni_file_df$source1[i] <- substr(muni_file_df$file_name[i],str_locate(muni_file_df$file_name[i],"_MUNI_")[2]+1,str_locate(muni_file_df$file_name[i],"_MUNI_")[2] + 2)
  muni_file_df$source[i] <- muni_file_df$source1[i]
  muni_file_df$source[i] <- ifelse(muni_file_df$source1[i] == "GW", "Total GW and SW","SW")
  muni_file_df$SWITCH_loadzone[i] <- substr(muni_file_df$file_name[i],1,str_locate(muni_file_df$file_name[i],"_MUNI_")[1]-1)
  muni_file_df$Scenario[i] <- gsub(".*[_]([^.]+)[.].*", "\\1", muni_file_df$file_name[i])
  
  #start with surface water supply delivered, and  loop through the file names, read into R, print the Muni name
  #then read in total supply delivered and loop through file names
  
  if(muni_file_df$source[i] == "SW"){
    muni_sw <- read.table(paste(muni_file_df$file_name[i]), skip = 1, header = FALSE, sep =',')
    muni_sw$SWITCH_loadzone <- muni_file_df$SWITCH_loadzone[i]
    muni_sw <- rename(muni_sw, SW_Supply_delivered_cubic_meter = V3)
    muni_sw$Scenario <- muni_file_df$Scenario[i]
    
    All_MUNI_SW <- rbind(All_MUNI_SW, muni_sw)
  }else if(muni_file_df$source[i] == "Total GW and SW"){
    muni_total <- read.table(paste(muni_file_df$file_name[i]), skip = 1, header = FALSE, sep =',')
    muni_total$SWITCH_loadzone <- muni_file_df$SWITCH_loadzone[i]
    muni_total <- rename(muni_total, Total_Supply_delivered_cubic_meter = V3)
    muni_total$Scenario <- muni_file_df$Scenario[i]
    
    All_MUNI_Total <- rbind(All_MUNI_Total, muni_total)
  }
  
}
All_MUNI_SW <- rename(All_MUNI_SW, Year = V1, Month = V2)
All_MUNI_Total <- rename(All_MUNI_Total, Year = V1, Month = V2)

write.csv(muni_file_df, file = paste(weap_dir_results,"/Results_plots/", "MUNI_file_list", ".csv", sep=""), row.names = FALSE)

#join surface water and total supply delivered by muni

All_MUNI_all_source <- merge(x=All_MUNI_SW,y=All_MUNI_Total,by=c("Year"="Year","Month" = "Month", "SWITCH_loadzone" = "SWITCH_loadzone" , "Scenario"="Scenario"),all=TRUE)

#if total is NA, total = surface water (no gw deliveries in other words)
All_MUNI_all_source$Total_Supply_delivered_cubic_meter <- ifelse(is.na(All_MUNI_all_source$Total_Supply_delivered_cubic_meter), All_MUNI_all_source$SW_Supply_delivered_cubic_meter, All_MUNI_all_source$Total_Supply_delivered_cubic_meter)

#calculate difference in deliveries to get gw delivered
All_MUNI_all_source$GW_Supply_delivered_cubic_meter <- All_MUNI_all_source$Total_Supply_delivered_cubic_meter - All_MUNI_all_source$SW_Supply_delivered_cubic_meter 

##dropping spin up and later years
All_MUNI_all_source <- All_MUNI_all_source %>% filter(Year > 2015 & Year < 2056)

#read in mapping file with load zones and states
WEAP_lz_state_mapping <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "load_zones", col_names = TRUE)

#join with list of load zones and states
All_MUNI_all_source <- left_join(All_MUNI_all_source, WEAP_lz_state_mapping, c("SWITCH_loadzone"= "LOAD_ZONE"))
All_MUNI_all_source$zone_ccs_distance_km <- NULL
All_MUNI_all_source$zone_dbid <- NULL
All_MUNI_all_source$Demand_sector <- "Urban"

#transpose results
All_MUNI_all_source_long <- gather(All_MUNI_all_source, Source, Supply_deliveries_cubic_meter, Total_Supply_delivered_cubic_meter, GW_Supply_delivered_cubic_meter, SW_Supply_delivered_cubic_meter)
All_MUNI_all_source_long$Source <- ifelse(All_MUNI_all_source_long$Source == "Total_Supply_delivered_cubic_meter", "Total", All_MUNI_all_source_long$Source)
All_MUNI_all_source_long$Source <- ifelse(All_MUNI_all_source_long$Source == "GW_Supply_delivered_cubic_meter", "GW", All_MUNI_all_source_long$Source)
All_MUNI_all_source_long$Source <- ifelse(All_MUNI_all_source_long$Source == "SW_Supply_delivered_cubic_meter", "SW", All_MUNI_all_source_long$Source)


#read in supply delivered results by GW and non-GW sources to get ag data by source

#Supply Deliveries by demand site from GW sources
GW_supplydel_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number, "_GW Supply Delivered All Demand Sites",".csv", sep="")
  gw_supplydel_all_sites <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')

  #GW deliveries
  #renaming some column names
  gw_supplydel_all_sites <- rename(gw_supplydel_all_sites, "Year" = "X.Columns...Year", "Month" = "Timestep")
  gw_supplydel_all_sites$Max.Cubic.Meter. <- NULL
  gw_supplydel_all_sites$Min.Cubic.Meter. <- NULL
  gw_supplydel_all_sites$Sum.Cubic.Meter. <- NULL
  gw_supplydel_all_sites$Median.Cubic.Meter. <- NULL
  gw_supplydel_all_sites$SD.Cubic.Meter. <- NULL
  gw_supplydel_all_sites$Mean.Cubic.Meter. <- NULL
  gw_supplydel_all_sites$RMS.Cubic.Meter. <- NULL
  
  #transpose to long format
  gw_supplydel_all_sites_long <- gather(gw_supplydel_all_sites, WEAP_object_name_raw, GW_Supply_delivered_cubic_meter, 3:ncol(gw_supplydel_all_sites))
  
  #parse object name from column
  #removing cubic meter unit label
  WEAP_object_name <- str_replace(gw_supplydel_all_sites_long$WEAP_object_name_raw, ".Cubic.Meter.","\\.")
  #replacing . with space and removing trailing space
  WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
  #checking length
  WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
  WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
  colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
  WEAP_object_name1$length <- NULL
  #binding corrected name and deleting old name
  gw_supplydel_all_sites_long <- cbind(gw_supplydel_all_sites_long, WEAP_object_name1)
  gw_supplydel_all_sites_long$WEAP_object_name_raw <- NULL
  
  #dropping spin up and later years
  gw_supplydel_all_sites_long <- gw_supplydel_all_sites_long %>% filter(Year > 2015 & Year < 2056)
  
  gw_supplydel_all_sites_long$Scenario <- scenario
  gw_supplydel_all_sites_long$Scenario_number <- scenario
  
  #join with mapping file to get load zones and states
  WEAP_demandsite_state_mapping <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "WEAPobject_demand_sector_mappin", col_names = TRUE)
  
  gw_supplydel_all_sites <- left_join(gw_supplydel_all_sites_long, WEAP_demandsite_state_mapping, c("WEAP_object_name"="WEAP_object_name"))
  gw_supplydel_all_sites$Demand_sector <- ifelse(gw_supplydel_all_sites$Demand_sector == "Urban Indoor" | gw_supplydel_all_sites$Demand_sector == "Urban Outdoor", "Urban", gw_supplydel_all_sites$Demand_sector)
  
  GW_supplydel_long_all_scenarios <- rbind(GW_supplydel_long_all_scenarios, gw_supplydel_all_sites)

}

#Supply Deliveries by demand site from Surface Water sources
SW_supplydel_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number, "_SW Supply Delivered All Demand Sites",".csv", sep="")
  SW_supplydel_all_sites <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
  
  #SW deliveries
  #renaming some column names
  SW_supplydel_all_sites <- rename(SW_supplydel_all_sites, "Year" = "X.Columns...Year", "Month" = "Timestep")
  SW_supplydel_all_sites$Max.Cubic.Meter. <- NULL
  SW_supplydel_all_sites$Min.Cubic.Meter. <- NULL
  SW_supplydel_all_sites$Sum.Cubic.Meter. <- NULL
  SW_supplydel_all_sites$Median.Cubic.Meter. <- NULL
  SW_supplydel_all_sites$SD.Cubic.Meter. <- NULL
  SW_supplydel_all_sites$Mean.Cubic.Meter. <- NULL
  SW_supplydel_all_sites$RMS.Cubic.Meter. <- NULL
  
  #transpose to long format
  SW_supplydel_all_sites_long <- gather(SW_supplydel_all_sites, WEAP_object_name_raw, SW_Supply_delivered_cubic_meter, 3:ncol(SW_supplydel_all_sites))
  
  #parse object name from column
  #removing acre foot unit label
  WEAP_object_name <- str_replace(SW_supplydel_all_sites_long$WEAP_object_name_raw, ".Cubic.Meter.","\\.")
  #replacing . with space and removing trailing space
  WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
  #checking length
  WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
  WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
  colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
  WEAP_object_name1$length <- NULL
  #binding corrected name and deleting old name
  SW_supplydel_all_sites_long <- cbind(SW_supplydel_all_sites_long, WEAP_object_name1)
  SW_supplydel_all_sites_long$WEAP_object_name_raw <- NULL
  
  #dropping spin up and later years
  SW_supplydel_all_sites_long <- SW_supplydel_all_sites_long %>% filter(Year > 2015 & Year < 2056)
  
  SW_supplydel_all_sites_long$Scenario <- scenario
  SW_supplydel_all_sites_long$Scenario_number <- scenario
  
  #join with mapping file to get load zones and states
  WEAP_demandsite_state_mapping <- read_excel(paste(weap_dir_results,mapping_file, sep=""), sheet = "WEAPobject_demand_sector_mappin", col_names = TRUE)
  
  SW_supplydel_all_sites <- left_join(SW_supplydel_all_sites_long, WEAP_demandsite_state_mapping, c("WEAP_object_name"="WEAP_object_name"))
  SW_supplydel_all_sites$Demand_sector <- ifelse(SW_supplydel_all_sites$Demand_sector == "Urban Indoor" | SW_supplydel_all_sites$Demand_sector == "Urban Outdoor", "Urban", SW_supplydel_all_sites$Demand_sector)
  
  SW_supplydel_long_all_scenarios <- rbind(SW_supplydel_long_all_scenarios, SW_supplydel_all_sites)
  
}

#join sw with gw deliveries to calculate total by load zone and state
total_supplydel_all_sites <- merge(x=SW_supplydel_long_all_scenarios,y=GW_supplydel_long_all_scenarios,by=c("Scenario"="Scenario","Scenario_number" = "Scenario_number","Year" = "Year", "Month" = "Month", "SWITCH_loadzone"= "SWITCH_loadzone","SWITCH_loadzone_id"="SWITCH_loadzone_id", "State" = "State", "Demand_sector" = "Demand_sector", "WEAP_object_name"="WEAP_object_name"),all=TRUE)

#total supply is surface plus gw supply
total_supplydel_all_sites$SW_Supply_delivered_cubic_meter <- ifelse(is.na(total_supplydel_all_sites$SW_Supply_delivered_cubic_meter), 0, total_supplydel_all_sites$SW_Supply_delivered_cubic_meter)
total_supplydel_all_sites$GW_Supply_delivered_cubic_meter <- ifelse(is.na(total_supplydel_all_sites$GW_Supply_delivered_cubic_meter), 0, total_supplydel_all_sites$GW_Supply_delivered_cubic_meter)

total_supplydel_all_sites$Total_Supply_delivered_cubic_meter <- total_supplydel_all_sites$SW_Supply_delivered_cubic_meter + total_supplydel_all_sites$GW_Supply_delivered_cubic_meter

#transpose
total_supplydel_all_sites_long <- gather(total_supplydel_all_sites, Source, Supply_deliveries_cubic_meter, Total_Supply_delivered_cubic_meter, GW_Supply_delivered_cubic_meter, SW_Supply_delivered_cubic_meter)
total_supplydel_all_sites_long$Source <- ifelse(total_supplydel_all_sites_long$Source == "Total_Supply_delivered_cubic_meter", "Total", total_supplydel_all_sites_long$Source)
total_supplydel_all_sites_long$Source <- ifelse(total_supplydel_all_sites_long$Source == "GW_Supply_delivered_cubic_meter", "GW", total_supplydel_all_sites_long$Source)
total_supplydel_all_sites_long$Source <- ifelse(total_supplydel_all_sites_long$Source == "SW_Supply_delivered_cubic_meter", "SW", total_supplydel_all_sites_long$Source)

#aggregate up to load zone and sector
total_supplydel_lz_sector_long <- total_supplydel_all_sites_long %>% group_by(Scenario, Year, Month, SWITCH_loadzone, State, Demand_sector, Source) %>% summarize(Supply_deliveries_cubic_meter = sum(Supply_deliveries_cubic_meter, na.rm = TRUE))

#filter out just Ag because the urban demand sites do not have the supply deliveries by source and those have already have been proccessed in the previous step
Ag_total_total_supplydel_lz_long <- total_supplydel_lz_sector_long %>% filter(Demand_sector == "Ag")

#append to muni (aka urban) supply delivered by source
MUNI_sector_total_deliveries <- All_MUNI_all_source_long %>% dplyr::select(Scenario, Year, Month, SWITCH_loadzone, State, Demand_sector, Source, Supply_deliveries_cubic_meter)

MUNI_AG_supplydel_lz_sector_long <- rbind(Ag_total_total_supplydel_lz_long, MUNI_sector_total_deliveries)

#standardize scenario names
MUNI_AG_supplydel_lz_sector_long$Scenario <- ifelse(MUNI_AG_supplydel_lz_sector_long$Scenario == "ACCESS-1", "ACCESS-1.0", MUNI_AG_supplydel_lz_sector_long$Scenario)
MUNI_AG_supplydel_lz_sector_long$Scenario <- ifelse(MUNI_AG_supplydel_lz_sector_long$Scenario == "RefLOCA", "Reference", MUNI_AG_supplydel_lz_sector_long$Scenario)

#calculate monthly deliveries by sector and source and load zone and state
sum_monthly_MUNI_AG_supplydel_lz_sector <- MUNI_AG_supplydel_lz_sector_long %>% group_by(Scenario, Year, Month, SWITCH_loadzone, State, Demand_sector, Source) %>% summarize(Supply_deliveries_cubic_meter = sum(Supply_deliveries_cubic_meter, na.rm = TRUE))

#summarize by year, sector and state and source
annual_MUNI_AG_supplydel_state_sector_source <- MUNI_AG_supplydel_lz_sector_long %>% group_by(Scenario, Year, State, Demand_sector, Source) %>% summarize(Supply_deliveries_cubic_meter = sum(Supply_deliveries_cubic_meter, na.rm = TRUE))

write.csv(annual_MUNI_AG_supplydel_state_sector_source, file = paste(weap_dir_results,"/Results_plots/", "WEAP_annual_MUNI_AG_supplydel_state_sector_source_CC", ".csv", sep=""), row.names = FALSE)

#summarize by sector and source

annual_MUNI_AG_supplydel_sector <- MUNI_AG_supplydel_lz_sector_long %>% group_by(Scenario, Year, Demand_sector, Source) %>% summarize(Supply_deliveries_cubic_meter = sum(Supply_deliveries_cubic_meter, na.rm = TRUE))

write.csv(annual_MUNI_AG_supplydel_sector, file = paste(weap_dir_results,"/Results_plots/", "Annual_MUNI_AG_supplydel_by_sector_sourceRef_and_CC_scenarios", ".csv", sep=""), row.names = FALSE)

#FIGURE 6B
annual_MUNI_AG_supplydel_sector$Ref_vs_CC_Scenario <- ifelse(annual_MUNI_AG_supplydel_sector$Scenario == "Reference", "Reference", "Climate Scenarios")

#reorder factors
annual_MUNI_AG_supplydel_sector$Ref_vs_CC_Scenario <- ordered(annual_MUNI_AG_supplydel_sector$Ref_vs_CC_Scenario, levels = c("Reference","Climate Scenarios"))

annual_MUNI_AG_supplydel_sector$Supply_delivered_km3 <- annual_MUNI_AG_supplydel_sector$Supply_deliveries_cubic_meter/(10^9)

#facet plot of box and whiskers by source and sector for Reference vs climate scenarios

#free scales for each sector
png(paste(weap_dir_results,"/Results_plots/", "Fig6B_FACET_SupplyDeliveries_Box_plot_of_by_Source_By_Sector_by_Scenario",".png", sep=""), width=1000, height=800, res=100)

ggplot(data = subset(annual_MUNI_AG_supplydel_sector, Source != "Total"), aes(Ref_vs_CC_Scenario, Supply_delivered_km3)) + 
  geom_boxplot(outlier.shape = 1, fill = "grey") + 
  facet_grid(Demand_sector ~ Source, scales="free_y") + 
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  labs(y = expression(paste("Supply Deliveries ", (km^3))), x = "Scenario") +
  ggtitle("Total Annual Supply Deliveries by Sector and Source") +
  theme_bw() + 
  theme(panel.grid.minor.y = element_line(color = "grey"), panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"), strip.text.y = element_text(size = 16),strip.text.x = element_text(size = 16),
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=0, size=14), axis.title.x = element_text(size=16), 
        legend.position="right", legend.text = element_text(size = 16), legend.title = element_text(size = 16), plot.title = element_text(size=18, hjust=0.5)) 

dev.off()

annual_MUNI_AG_supplydel_sector$Ref_vs_CC_Scenario <- ifelse(annual_MUNI_AG_supplydel_sector$Scenario == "Reference", "Reference", "Climate Scenarios")
#reorder factors
annual_MUNI_AG_supplydel_sector$Ref_vs_CC_Scenario <- ordered(annual_MUNI_AG_supplydel_sector$Ref_vs_CC_Scenario, levels = c("Reference","Climate Scenarios"))

#Figure 6D
#FACET PLOT OF SUPPLY DELIVERIES FOR GW VS SW for AG vs Urban for each climate scenario

#average supply deliveries by decade
annual_MUNI_AG_supplydel_sector2 <- annual_MUNI_AG_supplydel_sector 

#bin the years into decades
annual_MUNI_AG_supplydel_sector2$Decade <- 2060
annual_MUNI_AG_supplydel_sector2$Decade <-ifelse(annual_MUNI_AG_supplydel_sector2$Year < 2016, 2010, annual_MUNI_AG_supplydel_sector2$Decade)
annual_MUNI_AG_supplydel_sector2$Decade <-ifelse(annual_MUNI_AG_supplydel_sector2$Year >= 2016 & annual_MUNI_AG_supplydel_sector2$Year < 2026, 2020, annual_MUNI_AG_supplydel_sector2$Decade)
annual_MUNI_AG_supplydel_sector2$Decade <-ifelse(annual_MUNI_AG_supplydel_sector2$Year >= 2026 & annual_MUNI_AG_supplydel_sector2$Year < 2036, 2030, annual_MUNI_AG_supplydel_sector2$Decade)
annual_MUNI_AG_supplydel_sector2$Decade <-ifelse(annual_MUNI_AG_supplydel_sector2$Year >= 2036 & annual_MUNI_AG_supplydel_sector2$Year < 2046, 2040, annual_MUNI_AG_supplydel_sector2$Decade)
annual_MUNI_AG_supplydel_sector2$Decade <-ifelse(annual_MUNI_AG_supplydel_sector2$Year >= 2046 & annual_MUNI_AG_supplydel_sector2$Year < 2056, 2050, annual_MUNI_AG_supplydel_sector2$Decade)

#calculate decadal average supply deliveries by sector and source
decade_MUNI_AG_supplydel_sector <- annual_MUNI_AG_supplydel_sector2 %>% group_by(Scenario, Decade, Demand_sector, Source, Ref_vs_CC_Scenario) %>% summarize(Supply_delivered_km3 = mean(Supply_delivered_km3), 
                                                                                                                                        Supply_deliveries_cubic_meter = mean(Supply_deliveries_cubic_meter))

Refdecade_MUNI_AG_supplydel_sector <- decade_MUNI_AG_supplydel_sector %>% filter(Scenario == "Reference")
Refdecade_MUNI_AG_supplydel_sector <- rename(Refdecade_MUNI_AG_supplydel_sector, Ref_Supply_delivered_km3 = Supply_delivered_km3, Ref_Supply_deliveries_cubic_meter = Supply_deliveries_cubic_meter)
Refdecade_MUNI_AG_supplydel_sector$Scenario <- NULL
Refdecade_MUNI_AG_supplydel_sector$Ref_vs_CC_Scenario <- NULL

decadeDelta_MUNI_AG_supplydel_sector <- left_join(decade_MUNI_AG_supplydel_sector, Refdecade_MUNI_AG_supplydel_sector, c("Decade" = "Decade", "Demand_sector" = "Demand_sector", "Source" = "Source"))
#Decadal average delta supply del by sector and source
decadeDelta_MUNI_AG_supplydel_sector$Delta_Supply_delivered_km3 <- decadeDelta_MUNI_AG_supplydel_sector$Supply_delivered_km3 - decadeDelta_MUNI_AG_supplydel_sector$Ref_Supply_delivered_km3
decadeDelta_MUNI_AG_supplydel_sector$Delta_Supply_deliveries_cubic_meter <- decadeDelta_MUNI_AG_supplydel_sector$Supply_deliveries_cubic_meter - decadeDelta_MUNI_AG_supplydel_sector$Ref_Supply_deliveries_cubic_meter

#FIGURE Fig6D OF SUPPLY DEL DELTAS BY DECADE, SECTOR, SOURCE

png(paste(weap_dir_results,"/Results_plots/", "Fig6D_Delta_DecadeFACET_3x3SupplyDeliveries_Bar_Plot_By_Sector_by_Scenario",".png", sep=""), width=1000, height=800, res=100)

ggplot(data = subset(decadeDelta_MUNI_AG_supplydel_sector,  Scenario !="Reference" & Decade != 2020), aes(x = Scenario, y = Delta_Supply_delivered_km3, fill = Demand_sector)) + 
  geom_bar(stat="identity", colour="black") + 
  facet_grid(Source ~ Decade) + 
  scale_fill_manual(name = "Sector", values=c("#009E73", "#99FF33")) +
  labs(y = expression(paste("Delta Supply Deliveries", (km^3))), x = "Scenario") +
  ggtitle("Decadal Annual Average Difference in Supply Deliveries from Reference Scenario") +
  theme_bw() + 
  theme(panel.grid.minor.y = element_line(color = "grey"), panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"), strip.text.y = element_text(size = 16),strip.text.x = element_text(size = 16),
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=90, size=12), axis.title.x = element_text(size=16), 
        legend.position="right", legend.text = element_text(size = 16), legend.title = element_text(size = 16), plot.title = element_text(size=18, hjust=0.2)) 

dev.off()

##Figure 4C Summary of GCM changes in precip and temperature

#join with mapping file to get load zones and states
GCM_summary <- read_excel(paste(weap_dir_results, "GCM_summary.xlsx", sep=""), sheet = "GCM summary", col_names = TRUE, skip = 1)
GCM_summary_long <- gather(GCM_summary, degree_change, degrees_delta, 2:5)
GCM_summary_long$Year <- str_sub(GCM_summary_long$degree_change,1,4)
GCM_summary_long2 <- gather(GCM_summary, precip_change, precip_perc_delta, 10:13)
GCM_summary_long2$Year <- str_sub(GCM_summary_long2$precip_change,1,4)

GCM_summary_long3 <- left_join(GCM_summary_long, GCM_summary_long2, c("Year"="Year", "GCM"="GCM", "2020"="2020","2030"="2030","2040"="2040","2050"="2050"))
GCM_summary_long3$GCM <- ifelse(GCM_summary_long3$GCM == "Ensemble Mean", "Overall Ensemble Mean", GCM_summary_long3$GCM)

#colorblind friendly palette
cbpal3 <- c("#999999","#009292","#00CCCC","#ff6db6","#ffb6db",
                      "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                      "#920000","#924900","#db6d00","#24ff24","#ffff6d", "#000000")

#FIGURE 4C
png(paste(weap_dir_results,"/Results_plots/", "Fig4C_GCM_precip_temp_decade_average_delta",".png", sep=""), width=1700, height=500, res=100)

ylab<- expression(paste('Change in Average Monthly Temperature (',~degree,'C)',sep=""))
xlab<-"Change in Total Monthly Precipitation (%)"
ggplot(data=subset(GCM_summary_long3, Year != 2020), aes(x=precip_perc_delta, y=degrees_delta,  group=GCM, color = GCM)) +
  geom_point(size=3) + labs(x=xlab, y=ylab) + 
  ggtitle("Decadal precipitation and temperature deltas from climate scenarios vs. historical data across WUS") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.text.y = element_text(size = 16),strip.text.x = element_text(size = 16),
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=0, size=14), axis.title.x = element_text(size=16), 
        legend.position="right", legend.text = element_text(size = 12), legend.title = element_text(size = 14), plot.title = element_text(size=18, hjust=0.5), 
        legend.direction = "vertical", legend.box = "vertical", panel.spacing = unit(1, "lines")) + 
  scale_colour_manual(name="Scenario", values=cbpal3) + #using colorblind-friendly palette
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(.~Year, ncol = 3) 

dev.off()

######ENERGY USE RESULTS#########
#######
#Transmission energy, mainly for groundwater

WEAP_TransmissionLinkEnergy_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  # i = 1
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number, "_Transmission Link Electricity Use Monthly",".csv", sep="")
  WEAP_data <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
  
  #renaming some column names
  WEAP_data <- rename(WEAP_data, "Year" = "X.Columns...Year", "Month" = "Timestep")
  WEAP_data$Max.Gigawatt.Hour. <- NULL
  WEAP_data$Min.Gigawatt.Hour. <- NULL
  WEAP_data$Sum.Gigawatt.Hour. <- NULL
  WEAP_data$Median.Gigawatt.Hour. <- NULL
  WEAP_data$SD.Gigawatt.Hour. <- NULL
  WEAP_data$Mean.Gigawatt.Hour. <- NULL
  WEAP_data$RMS.Gigawatt.Hour. <- NULL
  
  #transpose to long format
  WEAP_data_long <- gather(WEAP_data, WEAP_object_name_raw, Electricity_use_GWh, 3:ncol(WEAP_data))
  
  #parse object name from column
  #removing acre foot unit label
  WEAP_object_name <- str_replace(WEAP_data_long$WEAP_object_name_raw, ".Gigawatt.Hour.","\\.")
  #replacing . with space and removing trailing space
  WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
  #checking length
  WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
  WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
  colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
  WEAP_object_name1$length <- NULL
  #binding corrected name and deleting old name
  WEAP_data_long <- cbind(WEAP_data_long, WEAP_object_name1)
  WEAP_data_long$WEAP_object_name_raw <- NULL
  
  #dropping spin up and later years
  WEAP_data_long <- WEAP_data_long %>% filter(Year > 2015 & Year < 2056)
  
  WEAP_data_long$Scenario <- scenario
  WEAP_data_long$Scenario_number <- scenario
  
  WEAP_data_long$Date <- as.Date(paste(WEAP_data_long$Year, WEAP_data_long$Month, "1", sep="-"),format = "%Y-%m-%d")
  
  WEAP_TransmissionLinkEnergy_long_all_scenarios <- rbind(WEAP_TransmissionLinkEnergy_long_all_scenarios, WEAP_data_long)
  
}

#read in mapping file with sectors for each demand object
WEAP_object_trans_link_mapping <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "TransmissionLink_mapping", col_names = TRUE)
  
WEAP_TransmissionLinkEnergy_long_all_scenarios <- left_join(WEAP_TransmissionLinkEnergy_long_all_scenarios, WEAP_object_trans_link_mapping, c("WEAP_object_name" = "WEAP_object_name"))

#checking for missing data
missingWEAP_TransmissionLinkEnergy_long_all_scenarios <- WEAP_TransmissionLinkEnergy_long_all_scenarios %>% filter(is.na(`Included Energy`))
table(missingWEAP_TransmissionLinkEnergy_long_all_scenarios$WEAP_object_name)

#######
#Diversion energy

WEAP_DiversionEnergy_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  # i = 1
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number, "_Diversion Electricity Use Monthly",".csv", sep="")
  WEAP_data <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
  
  #renaming some column names
  WEAP_data <- rename(WEAP_data, "Year" = "X.Columns...Year", "Month" = "Timestep")
  WEAP_data$Max.Gigawatt.Hour. <- NULL
  WEAP_data$Min.Gigawatt.Hour. <- NULL
  WEAP_data$Sum.Gigawatt.Hour. <- NULL
  WEAP_data$Median.Gigawatt.Hour. <- NULL
  WEAP_data$SD.Gigawatt.Hour. <- NULL
  WEAP_data$Mean.Gigawatt.Hour. <- NULL
  WEAP_data$RMS.Gigawatt.Hour. <- NULL
  
  #transpose to long format
  WEAP_data_long <- gather(WEAP_data, WEAP_object_name_raw, Electricity_use_GWh, 3:ncol(WEAP_data))
  
  #parse object name from column
  #removing acre foot unit label
  WEAP_object_name <- str_replace(WEAP_data_long$WEAP_object_name_raw, ".Gigawatt.Hour.","\\.")
  #replacing . with space and removing trailing space
  WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
  #checking length
  WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
  WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
  colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
  WEAP_object_name1$length <- NULL
  #binding corrected name and deleting old name
  WEAP_data_long <- cbind(WEAP_data_long, WEAP_object_name1)
  WEAP_data_long$WEAP_object_name_raw <- NULL
  
  #dropping spin up and later years
  WEAP_data_long <- WEAP_data_long %>% filter(Year > 2015 & Year < 2056)
  
  WEAP_data_long$Scenario <- scenario
  WEAP_data_long$Scenario_number <- scenario
  
  WEAP_data_long$Date <- as.Date(paste(WEAP_data_long$Year, WEAP_data_long$Month, "1", sep="-"),format = "%Y-%m-%d")
  
  WEAP_DiversionEnergy_long_all_scenarios <- rbind(WEAP_DiversionEnergy_long_all_scenarios, WEAP_data_long)
  
}

#read in mapping file with sectors for each demand object
WEAP_object_diversion_mapping <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "Diversion_mapping", col_names = TRUE)

WEAP_DiversionEnergy_long_all_scenarios <- left_join(WEAP_DiversionEnergy_long_all_scenarios, WEAP_object_diversion_mapping, c("WEAP_object_name" = "WEAP_object_name"))

missingDiversion_all_scenarios <- WEAP_DiversionEnergy_long_all_scenarios %>% filter(is.na(`Included Energy`))
table(missingDiversion_all_scenarios$WEAP_object_name)

#sum by scenario across all diversion links
diversion_energy_annual_total_by_scenarios <- WEAP_DiversionEnergy_long_all_scenarios %>% group_by(Year, Scenario) %>% summarize(Electricity_use_GWh = sum(Electricity_use_GWh))

diversion_energy_annual_total_by_scenarios$Year <- as.numeric(diversion_energy_annual_total_by_scenarios$Year)


######
#Demand site energy (distribution and muni treatment)

WEAP_DemandSiteEnergy_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  # i = 1
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number, "_Demand Site Electricity Use Monthly",".csv", sep="")
  WEAP_data <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
  
  #renaming some column names
  WEAP_data <- rename(WEAP_data, "Year" = "X.Columns...Year", "Month" = "Timestep")
  WEAP_data$Max.Gigawatt.Hour. <- NULL
  WEAP_data$Min.Gigawatt.Hour. <- NULL
  WEAP_data$Sum.Gigawatt.Hour. <- NULL
  WEAP_data$Median.Gigawatt.Hour. <- NULL
  WEAP_data$SD.Gigawatt.Hour. <- NULL
  WEAP_data$Mean.Gigawatt.Hour. <- NULL
  WEAP_data$RMS.Gigawatt.Hour. <- NULL
  
  #transpose to long format
  WEAP_data_long <- gather(WEAP_data, WEAP_object_name_raw, Electricity_use_GWh, 3:ncol(WEAP_data))
  
  #parse object name from column
  #removing acre foot unit label
  WEAP_object_name <- str_replace(WEAP_data_long$WEAP_object_name_raw, ".Gigawatt.Hour.","\\.")
  #replacing . with space and removing trailing space
  WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
  #checking length
  WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
  WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
  colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
  WEAP_object_name1$length <- NULL
  #binding corrected name and deleting old name
  WEAP_data_long <- cbind(WEAP_data_long, WEAP_object_name1)
  WEAP_data_long$WEAP_object_name_raw <- NULL
  
  #dropping spin up and later years
  WEAP_data_long <- WEAP_data_long %>% filter(Year > 2015 & Year < 2056)
  
  WEAP_data_long$Scenario <- scenario
  WEAP_data_long$Scenario_number <- scenario
  
  WEAP_data_long$Date <- as.Date(paste(WEAP_data_long$Year, WEAP_data_long$Month, "1", sep="-"),format = "%Y-%m-%d")
  
  WEAP_DemandSiteEnergy_long_all_scenarios <- rbind(WEAP_DemandSiteEnergy_long_all_scenarios, WEAP_data_long)
  
}

#read in mapping file with sectors for each demand object
WEAP_object_demand_mapping <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "DemandSite_energy_mapping", col_names = TRUE)

WEAP_DemandSiteEnergy_long_all_scenarios <- left_join(WEAP_DemandSiteEnergy_long_all_scenarios, WEAP_object_demand_mapping, c("WEAP_object_name" = "WEAP_object_name"))

#checking for missing links
missing_WEAP_DemandSiteEnergy_long_all_scenarios <- WEAP_DemandSiteEnergy_long_all_scenarios %>% filter(is.na(`Included Energy`))
table(missing_WEAP_DemandSiteEnergy_long_all_scenarios$WEAP_object_name)

#removing Domestic Heating because it is miscalculated, and Domestic Energy because is double counting the energy for MuniTreat and DistEnergy (since it is on all Supply Delivered for each red dot which is for Commercial and Industrial + Domestic)
WEAP_DemandSiteEnergy_long_all_scenarios <- WEAP_DemandSiteEnergy_long_all_scenarios%>% filter(Sector != "Domestic" & Sector != "Domestic Heating")
table(WEAP_DemandSiteEnergy_long_all_scenarios$`Included Energy`, WEAP_DemandSiteEnergy_long_all_scenarios$Sector)


##SUPPLY DELIVERED for post-processing Domestic Water Heating energy use

#######
#Water supplies delivered (water use) by sector

WEAP_supplydel_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  # i = 1
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number, "_Supply Delivered All Demand Sites",".csv", sep="")
  WEAP_data <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
  
  #renaming some column names
  WEAP_data <- rename(WEAP_data, "Year" = "X.Columns...Year", "Month" = "Timestep")
  WEAP_data$Max.Acre.foot. <- NULL
  WEAP_data$Min.Acre.foot. <- NULL
  WEAP_data$Sum.Acre.foot. <- NULL
  WEAP_data$Median.Acre.foot. <- NULL
  WEAP_data$SD.Acre.foot. <- NULL
  WEAP_data$Mean.Acre.foot. <- NULL
  WEAP_data$RMS.Acre.foot. <- NULL
  
  #transpose to long format
  WEAP_data_long <- gather(WEAP_data, WEAP_object_name_raw, Supply_delivered_AF, 3:ncol(WEAP_data))
  
  #parse object name from column
  #removing acre foot unit label
  WEAP_object_name <- str_replace(WEAP_data_long$WEAP_object_name_raw, ".Acre.foot.","\\.")
  #replacing . with space and removing trailing space
  WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
  #checking length
  WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
  WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
  colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
  WEAP_object_name1$length <- NULL
  #binding corrected name and deleting old name
  WEAP_data_long <- cbind(WEAP_data_long, WEAP_object_name1)
  WEAP_data_long$WEAP_object_name_raw <- NULL
  
  #dropping spin up and later years
  WEAP_data_long <- WEAP_data_long %>% filter(Year > 2015 & Year < 2056)
  
  WEAP_data_long$Scenario <- scenario
  WEAP_data_long$Scenario_number <- scenario
  
  WEAP_data_long$Date <- as.Date(paste(WEAP_data_long$Year, WEAP_data_long$Month, "1", sep="-"),format = "%Y-%m-%d")
  
  WEAP_supplydel_long_all_scenarios <- rbind(WEAP_supplydel_long_all_scenarios, WEAP_data_long)
  
}


#read in mapping file with sectors for each demand object
WEAP_object_demand_sector_mapping <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "WEAPobject_demand_sector_mappin", col_names = TRUE)

WEAP_supplydel_sector_all_scenarios <- left_join(WEAP_supplydel_long_all_scenarios, WEAP_object_demand_sector_mapping, c("WEAP_object_name" = "WEAP_object_name"))

#monthly by SWITCH load zone and sector
WEAP_supplydel_monthly_sector_lz_all_scenarios <- WEAP_supplydel_sector_all_scenarios %>% group_by(Scenario, Demand_sector, Year, Month, Date, WEAP_object_name, SWITCH_loadzone) %>% summarize(Supply_delivered_AF = sum(Supply_delivered_AF))

#filtering to just have urban indoor
WEAP_Urban_Indoor_supplydel_monthly_all_scenarios <- WEAP_supplydel_monthly_sector_lz_all_scenarios %>% filter(Demand_sector == "Urban Indoor")

#read in mapping file with PIU and CIU and Muni Elec Sat sectors for each demand object by load zone
WEAP_PIU_CIU_mapping <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "PIU_or_CIU", col_names = TRUE)
WEAP_PIU_mapping <- WEAP_PIU_CIU_mapping %>% filter(`Level 4...` == "PIU")
WEAP_PIU_mapping <- rename(WEAP_PIU_mapping, PIU = PIU_or_CIU, Perc_PIU = Perc_PIU_or_CIU)

WEAP_Urban_Indoor_supplydel_monthly_all_scenarios <- left_join(WEAP_Urban_Indoor_supplydel_monthly_all_scenarios, WEAP_PIU_mapping, c("SWITCH_loadzone" = "Level 3"))

#drop CAN and MEX
WEAP_Urban_Indoor_supplydel_monthly_all_scenarios <- WEAP_Urban_Indoor_supplydel_monthly_all_scenarios %>% filter(SWITCH_loadzone != "CAN_BC" & SWITCH_loadzone !="CAN_ALB" & SWITCH_loadzone !="MEX_BAJA")
#Calculate monthly Domestic Heating based on Supply Delivered AF * Liters to AF * hot water share * PIU perc of total PIU + CIU water * electric sat perc * specific heat for water * 1/3600 convert to kWh * degrees T * 1/efficiency
WEAP_Urban_Indoor_supplydel_monthly_all_scenarios$Domestic_Heating_kWh <- WEAP_Urban_Indoor_supplydel_monthly_all_scenarios$Supply_delivered_AF * 1.233*10^6 * 0.33 * 
  WEAP_Urban_Indoor_supplydel_monthly_all_scenarios$Perc_PIU * WEAP_Urban_Indoor_supplydel_monthly_all_scenarios$MuniElecSat_Value * (4.2/3600) * 40 * 1/0.9

WEAP_Urban_Indoor_supplydel_monthly_all_scenarios$Domestic_Heating_GWh <- WEAP_Urban_Indoor_supplydel_monthly_all_scenarios$Domestic_Heating_kWh/10^6

# #sum annually by sector across all nodes for each scenario
# 
#read in mapping file with states for each load zone
load_zones <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "load_zones", col_names = TRUE)
load_zones$SWITCH_load_zone_id <- load_zones$zone_dbid
load_zones$zone_ccs_distance_km <- NULL
load_zones$zone_dbid <- NULL
load_zones$...4 <- NULL
load_zones$...5 <- NULL

WEAP_Urban_Indoor_supplydel_monthly_all_scenarios <- left_join(WEAP_Urban_Indoor_supplydel_monthly_all_scenarios, load_zones, c("SWITCH_loadzone" = "LOAD_ZONE"))

WEAP_Urban_Indoor_supplydel_monthly_all_scenarios$Demand_sector <-"Urban Indoor (Domestic Heating)"
WEAP_Urban_Indoor_supplydel_monthly_all_scenarios$`Included Energy` <- "Domestic Heating"

#renaming columns for consistency with other dataframes
WEAP_Urban_Indoor_supplydel_monthly_all_scenarios <- rename(WEAP_Urban_Indoor_supplydel_monthly_all_scenarios, Electricity_use_GWh = Domestic_Heating_GWh, SWITCH_load_zone = SWITCH_loadzone, Sector = Demand_sector, Scenario = Scenario.x)
DomesticHeatingEnergy_monthly_all_scenarios <- WEAP_Urban_Indoor_supplydel_monthly_all_scenarios %>% dplyr::select(Scenario, Year, Month, WEAP_object_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Electricity_use_GWh)

### For graphing WECC total energy use for demandsite and domestic heating
#sum water heating energy use across WECC and by year
WEAP_Domestic_Heating_annual_total_all_scenarios <- DomesticHeatingEnergy_monthly_all_scenarios %>% group_by(Year, Scenario, Sector) %>% summarize(Electricity_use_GWh = sum(Electricity_use_GWh))

#####
#Wastewater treatment energy from Return Flows

WEAP_ReturnFlowEnergy_long_all_scenarios <- data.frame()

for (i in 1:length(scenario_list)){
  
  # i = 1
  scenario_number <- scenario_list[[i]]
  
  scenario <- climate_scenario_list[[i]]
  
  #read in WEAP outputs
  WEAP_input_filename <- paste(scenario_number, "_Electricity Use Return Flows Monthly",".csv", sep="")
  WEAP_data <- read.table(paste(weap_dir_results,WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
  
  #renaming some column names
  WEAP_data <- rename(WEAP_data, "Year" = "X.Columns...Year", "Month" = "Timestep")
  WEAP_data$Max.Gigawatt.Hour. <- NULL
  WEAP_data$Min.Gigawatt.Hour. <- NULL
  WEAP_data$Sum.Gigawatt.Hour. <- NULL
  WEAP_data$Median.Gigawatt.Hour. <- NULL
  WEAP_data$SD.Gigawatt.Hour. <- NULL
  WEAP_data$Mean.Gigawatt.Hour. <- NULL
  WEAP_data$RMS.Gigawatt.Hour. <- NULL
  
  #transpose to long format
  WEAP_data_long <- gather(WEAP_data, WEAP_object_name_raw, Electricity_use_GWh, 3:ncol(WEAP_data))
  
  #parse object name from column
  #removing acre foot unit label
  WEAP_object_name <- str_replace(WEAP_data_long$WEAP_object_name_raw, ".Gigawatt.Hour.","\\.")
  #replacing . with space and removing trailing space
  WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
  #checking length
  WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
  WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
  colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
  WEAP_object_name1$length <- NULL
  #binding corrected name and deleting old name
  WEAP_data_long <- cbind(WEAP_data_long, WEAP_object_name1)
  WEAP_data_long$WEAP_object_name_raw <- NULL
  
  #dropping spin up and later years
  WEAP_data_long <- WEAP_data_long %>% filter(Year > 2015 & Year < 2056)
  
  WEAP_data_long$Scenario <- scenario
  WEAP_data_long$Scenario_number <- scenario
  
  WEAP_data_long$Date <- as.Date(paste(WEAP_data_long$Year, WEAP_data_long$Month, "1", sep="-"),format = "%Y-%m-%d")
  
  WEAP_ReturnFlowEnergy_long_all_scenarios <- rbind(WEAP_ReturnFlowEnergy_long_all_scenarios, WEAP_data_long)
  
}

#read in mapping file with sectors for each demand object
WEAP_object_returnflow_link_mapping <- read_excel(paste(weap_dir_results, mapping_file, sep=""), sheet = "ReturnFlowWW_mapping", col_names = TRUE)

WEAP_ReturnFlowEnergy_long_all_scenarios <- left_join(WEAP_ReturnFlowEnergy_long_all_scenarios, WEAP_object_returnflow_link_mapping, c("WEAP_object_name" = "WEAP_object_name"))

#checking for missing links
missing_WEAP_ReturnFlowEnergy_long_all_scenarios <- WEAP_ReturnFlowEnergy_long_all_scenarios %>% filter(is.na(`Included Energy`))
table(missing_WEAP_ReturnFlowEnergy_long_all_scenarios$WEAP_object_name)

#sum sectoral energy by scenario and by year across all zones 
ReturnFlowEnergy_annual_total_all_scenarios <- WEAP_ReturnFlowEnergy_long_all_scenarios %>% group_by(Year, Scenario, Sector)  %>% summarize(Electricity_use_GWh = sum(Electricity_use_GWh))
ReturnFlowEnergy_annual_total_all_scenarios$Year <- as.numeric(ReturnFlowEnergy_annual_total_all_scenarios$Year)

######
#Total monthly, load zone, energy balance impact

#Hydropower

#monthly average reference hydropower
Hydropower_monthly_reference <- WEAP_hydro_data_long_all_scenarios %>% filter(Scenario == "Reference")
Hydropower_monthly_reference <- rename(Hydropower_monthly_reference, Reference_Generation_GWh = Generation_GWh)
#period mean monthly hydropower reference
Hydropower_avg_monthly_reference <- Hydropower_monthly_reference %>% group_by(Month, WEAP_generator_name) %>% summarize(Reference_Generation_GWh = mean(Reference_Generation_GWh))
#filtering out just climate scenarios 
Hydropower_monthly_CC_scenarios <- WEAP_hydro_data_long_all_scenarios %>% filter(Scenario != "Reference")
#calculating decadal average hydropower generation for each climate scenario
Hydropower_monthly_CC_scenarios <- Hydropower_monthly_CC_scenarios %>% filter(Year > 2015 & Year < 2056)

Hydropower_monthly_CC_scenarios$Decade <- 2060
Hydropower_monthly_CC_scenarios$Decade <-ifelse(Hydropower_monthly_CC_scenarios$Year < 2016, 2010, Hydropower_monthly_CC_scenarios$Decade)
Hydropower_monthly_CC_scenarios$Decade <-ifelse(Hydropower_monthly_CC_scenarios$Year >= 2016 & Hydropower_monthly_CC_scenarios$Year < 2026, 2020, Hydropower_monthly_CC_scenarios$Decade)
Hydropower_monthly_CC_scenarios$Decade <-ifelse(Hydropower_monthly_CC_scenarios$Year >= 2026 & Hydropower_monthly_CC_scenarios$Year < 2036, 2030, Hydropower_monthly_CC_scenarios$Decade)
Hydropower_monthly_CC_scenarios$Decade <-ifelse(Hydropower_monthly_CC_scenarios$Year >= 2036 & Hydropower_monthly_CC_scenarios$Year < 2046, 2040, Hydropower_monthly_CC_scenarios$Decade)
Hydropower_monthly_CC_scenarios$Decade <-ifelse(Hydropower_monthly_CC_scenarios$Year >= 2046 & Hydropower_monthly_CC_scenarios$Year < 2056, 2050, Hydropower_monthly_CC_scenarios$Decade)

Hydropower_decadeAvg_monthly_CC_scenarios <- Hydropower_monthly_CC_scenarios %>% group_by(Scenario, Decade, Month, WEAP_generator_name) %>% summarize(CC_Generation_GWh = mean(Generation_GWh))

#joining decadal monthly averages from CC scenarios with period monthly average from historical
Hydropower_decadeAvg_monthly_CC_scenarios <- left_join(Hydropower_decadeAvg_monthly_CC_scenarios, Hydropower_avg_monthly_reference, c("Month"="Month", "WEAP_generator_name" = "WEAP_generator_name"))

#join list of generators with load zone mapping
WEAP_SWITCH_hydro_mapping <- read.csv(paste(weap_dir_results,"WEAP_SWITCH_mapping_with_avg_annual_generation.csv", sep=""), stringsAsFactors = FALSE)

Hydropower_decadeAvg_monthly_CC_scenarios <- left_join(Hydropower_decadeAvg_monthly_CC_scenarios, WEAP_SWITCH_hydro_mapping, c("WEAP_generator_name" = "WEAP_name"))

#remove NAs, duplicates, and non-generating generators and just keep switch load zone id and name
Hydropower_decadeAvg_monthly_CC_scenarios <- Hydropower_decadeAvg_monthly_CC_scenarios[complete.cases(Hydropower_decadeAvg_monthly_CC_scenarios[ , 7]), ]

Hydropower_decadeAvg_monthly_CC_scenarios <- Hydropower_decadeAvg_monthly_CC_scenarios[!duplicated(Hydropower_decadeAvg_monthly_CC_scenarios[c(1,2,3,4,5)]),]

Hydropower_decadeAvg_monthly_CC_scenarios <-Hydropower_decadeAvg_monthly_CC_scenarios %>% filter(not_generating_in_WEAP == 0)

#calculate delta in generation between CC scenarios and Reference
Hydropower_decadeAvg_monthly_CC_scenarios$Delta_Generation_GWh <- Hydropower_decadeAvg_monthly_CC_scenarios$CC_Generation_GWh - Hydropower_decadeAvg_monthly_CC_scenarios$Reference_Generation_GWh

#ANnual delta, and then calculate ensemble average delta by generator for mapping
Generator_Hydropower_decade_annual_CC_scenarios <- Hydropower_decadeAvg_monthly_CC_scenarios %>% group_by(Scenario, Decade, load_zone_id, load_zone_name, WEAP_generator_name, latitude, longitude) %>% summarize(Delta_Generation_GWh = sum(Delta_Generation_GWh), 
                                                                                                                                                                                                                  CC_Generation_GWh = sum(CC_Generation_GWh), 
                                                                                                                                                                                                                  Reference_Generation_GWh = sum(Reference_Generation_GWh))
#annual average by scenario
Generator_Hydropower_decadeAvg_annual_CC <- Generator_Hydropower_decade_annual_CC_scenarios %>% group_by(Scenario, load_zone_id, load_zone_name, WEAP_generator_name, latitude, longitude) %>% summarize(Delta_Generation_GWh = mean(Delta_Generation_GWh), 
                                                                                                                                                                                                         CC_Generation_GWh = mean(CC_Generation_GWh), 
                                                                                                                                                                                                         Reference_Generation_GWh = mean(Reference_Generation_GWh))

#ensemble average delta annual
Generator_EnsembleHydropower_decadeAvg_annual_CC <- Generator_Hydropower_decadeAvg_annual_CC %>% group_by(load_zone_id, load_zone_name, WEAP_generator_name, latitude, longitude) %>% summarize(Delta_Generation_GWh = mean(Delta_Generation_GWh), 
                                                                                                                                                                                                CC_Generation_GWh = mean(CC_Generation_GWh), 
                                                                                                                                                                                                Reference_Generation_GWh = mean(Reference_Generation_GWh))


#sum total delta generation, CC generation and Reference generation by load zone
Hydropower_lz_monthly_CC_delta <- Hydropower_decadeAvg_monthly_CC_scenarios %>% group_by(Scenario, Decade, Month, load_zone_id, load_zone_name) %>% summarize(Delta_Electricity_GWh = sum(Delta_Generation_GWh), 
                                                                                                                                                              CC_Electricity_GWh = sum(CC_Generation_GWh), 
                                                                                                                                                              Reference_Electricity_GWh = sum(Reference_Generation_GWh))
#calculate percentage change delta generation by load zone
Hydropower_lz_monthly_CC_delta$monthly_delta_perc <- Hydropower_lz_monthly_CC_delta$Delta_Electricity_GWh / Hydropower_lz_monthly_CC_delta$Reference_Electricity_GWh

#Energy Demand

#monthly average reference transmission link, diversion, demand site, wastewater energy
TransmissionLinkEnergy_monthly_reference <- WEAP_TransmissionLinkEnergy_long_all_scenarios %>% filter(Scenario == "Reference")
TransmissionLinkEnergy_monthly_reference <- rename(TransmissionLinkEnergy_monthly_reference, Reference_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name)
TransmissionLinkEnergy_monthly_reference <- TransmissionLinkEnergy_monthly_reference %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Reference_Electricity_use_GWh)
TransmissionLinkEnergy_monthly_reference$Energy_Use_Category <- "Transmission Link"

DiversionEnergy_monthly_reference <- WEAP_DiversionEnergy_long_all_scenarios %>% filter(Scenario == "Reference")
DiversionEnergy_monthly_reference <- rename(DiversionEnergy_monthly_reference, Reference_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name )
DiversionEnergy_monthly_reference <- DiversionEnergy_monthly_reference %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Reference_Electricity_use_GWh)
DiversionEnergy_monthly_reference$Energy_Use_Category <- "Diversion"

DemandSiteEnergy_monthly_reference <- WEAP_DemandSiteEnergy_long_all_scenarios %>% filter(Scenario == "Reference" )
DemandSiteEnergy_monthly_reference <- rename(DemandSiteEnergy_monthly_reference, Reference_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name)
DemandSiteEnergy_monthly_reference <- DemandSiteEnergy_monthly_reference %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Reference_Electricity_use_GWh)
DemandSiteEnergy_monthly_reference$Energy_Use_Category <- "Demand Site"

DomesticHeatingEnergy_monthly_reference <- DomesticHeatingEnergy_monthly_all_scenarios  %>% filter(Scenario == "Reference")
DomesticHeatingEnergy_monthly_reference <- rename(DomesticHeatingEnergy_monthly_reference, Reference_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name )
DomesticHeatingEnergy_monthly_reference <- DomesticHeatingEnergy_monthly_reference %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Reference_Electricity_use_GWh)
DomesticHeatingEnergy_monthly_reference$Energy_Use_Category <- "Domestic Heating"
DomesticHeatingEnergy_monthly_reference$Date <- NULL

#return flow/wastewater treatment energy

ReturnFlowEnergy_monthly_reference <- WEAP_ReturnFlowEnergy_long_all_scenarios %>% filter(Scenario == "Reference")
ReturnFlowEnergy_monthly_reference <- rename(ReturnFlowEnergy_monthly_reference, Reference_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name)
ReturnFlowEnergy_monthly_reference <- ReturnFlowEnergy_monthly_reference %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Reference_Electricity_use_GWh)
ReturnFlowEnergy_monthly_reference$Energy_Use_Category <- "Return Flow"

#combine all monthly reference energy
All_EnergyUse_monthly_reference <- rbind(TransmissionLinkEnergy_monthly_reference, DiversionEnergy_monthly_reference, DemandSiteEnergy_monthly_reference, DomesticHeatingEnergy_monthly_reference, ReturnFlowEnergy_monthly_reference)

#combine commercial and industrial and domestic heating into Urban Indoor category to be consistent
All_EnergyUse_monthly_reference$Sector <- ifelse(All_EnergyUse_monthly_reference$Sector %in% c("Commercial and Industrial"), "Urban", All_EnergyUse_monthly_reference$Sector)

#Read in CC scenario energy demand files:  transmission link, diversion, demand site, wastewater energy
TransmissionLinkEnergy_monthly_CC <- WEAP_TransmissionLinkEnergy_long_all_scenarios %>% filter(Scenario != "Reference")
TransmissionLinkEnergy_monthly_CC <- rename(TransmissionLinkEnergy_monthly_CC, CC_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name)
TransmissionLinkEnergy_monthly_CC <- TransmissionLinkEnergy_monthly_CC %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector,`Included Energy`, CC_Electricity_use_GWh)
TransmissionLinkEnergy_monthly_CC$Energy_Use_Category <- "Transmission Link"

DiversionEnergy_monthly_CC <- WEAP_DiversionEnergy_long_all_scenarios %>% filter(Scenario != "Reference")
DiversionEnergy_monthly_CC <- rename(DiversionEnergy_monthly_CC, CC_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name)
DiversionEnergy_monthly_CC <- DiversionEnergy_monthly_CC %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, CC_Electricity_use_GWh)
DiversionEnergy_monthly_CC$Energy_Use_Category <- "Diversion"

DemandSiteEnergy_monthly_CC <- WEAP_DemandSiteEnergy_long_all_scenarios %>% filter(Scenario != "Reference")
DemandSiteEnergy_monthly_CC <- rename(DemandSiteEnergy_monthly_CC, CC_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name)
DemandSiteEnergy_monthly_CC <- DemandSiteEnergy_monthly_CC %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, CC_Electricity_use_GWh)
DemandSiteEnergy_monthly_CC$Energy_Use_Category <- "Demand Site"

DomesticHeatingEnergy_monthly_CC<- DomesticHeatingEnergy_monthly_all_scenarios  %>% filter(Scenario != "Reference")
DomesticHeatingEnergy_monthly_CC <- rename(DomesticHeatingEnergy_monthly_CC, CC_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name)
DomesticHeatingEnergy_monthly_CC <- DomesticHeatingEnergy_monthly_CC %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, CC_Electricity_use_GWh)
DomesticHeatingEnergy_monthly_CC$Energy_Use_Category <- "Domestic Heating"
DomesticHeatingEnergy_monthly_CC$Date <- NULL

ReturnFlowEnergy_monthly_CC <- WEAP_ReturnFlowEnergy_long_all_scenarios %>% filter(Scenario != "Reference")
ReturnFlowEnergy_monthly_CC <- rename(ReturnFlowEnergy_monthly_CC, CC_Electricity_use_GWh = Electricity_use_GWh, WEAP_name = WEAP_object_name)
ReturnFlowEnergy_monthly_CC <- ReturnFlowEnergy_monthly_CC %>% dplyr::select(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector,`Included Energy`, CC_Electricity_use_GWh)
ReturnFlowEnergy_monthly_CC$Energy_Use_Category <- "Return Flow"

#combine all monthly CC energy
All_EnergyUse_monthly_CC <- rbind(TransmissionLinkEnergy_monthly_CC, DiversionEnergy_monthly_CC, DemandSiteEnergy_monthly_CC, DomesticHeatingEnergy_monthly_CC, ReturnFlowEnergy_monthly_CC)

All_EnergyUse_monthly_CC$Sector <- ifelse(All_EnergyUse_monthly_CC$Sector %in%c("Commercial and Industrial"), "Urban", All_EnergyUse_monthly_CC$Sector)

#calculate decadal average by object
#reference
All_EnergyUse_monthly_reference <- All_EnergyUse_monthly_reference %>% filter(Year > 2015 & Year < 2056)

All_EnergyUse_monthly_reference$Decade <- 2060
All_EnergyUse_monthly_reference$Decade <-ifelse(All_EnergyUse_monthly_reference$Year < 2016, 2010, All_EnergyUse_monthly_reference$Decade)
All_EnergyUse_monthly_reference$Decade <-ifelse(All_EnergyUse_monthly_reference$Year >= 2016 & All_EnergyUse_monthly_reference$Year < 2026, 2020, All_EnergyUse_monthly_reference$Decade)
All_EnergyUse_monthly_reference$Decade <-ifelse(All_EnergyUse_monthly_reference$Year >= 2026 & All_EnergyUse_monthly_reference$Year < 2036, 2030, All_EnergyUse_monthly_reference$Decade)
All_EnergyUse_monthly_reference$Decade <-ifelse(All_EnergyUse_monthly_reference$Year >= 2036 & All_EnergyUse_monthly_reference$Year < 2046, 2040, All_EnergyUse_monthly_reference$Decade)
All_EnergyUse_monthly_reference$Decade <-ifelse(All_EnergyUse_monthly_reference$Year >= 2046 & All_EnergyUse_monthly_reference$Year < 2056, 2050, All_EnergyUse_monthly_reference$Decade)

#average monthly Reference energy use by decade
All_EnergyUse_decadeAvg_monthly_reference <- All_EnergyUse_monthly_reference %>% group_by(Decade, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Energy_Use_Category) %>% 
  summarize(Reference_Electricity_use_GWh = mean(Reference_Electricity_use_GWh, na.rm=TRUE))

#sum energy use reference by year (not decade)
Yearly_All_EnergyUse_reference <- All_EnergyUse_monthly_reference %>% group_by(Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Energy_Use_Category) %>% 
  summarize(Reference_Electricity_use_GWh = sum(Reference_Electricity_use_GWh, na.rm=TRUE))

#CC scenarios
All_EnergyUse_monthly_CC <- All_EnergyUse_monthly_CC %>% filter(Year > 2015 & Year < 2056)

All_EnergyUse_monthly_CC$Decade <- 2060
All_EnergyUse_monthly_CC$Decade <-ifelse(All_EnergyUse_monthly_CC$Year < 2016, 2010, All_EnergyUse_monthly_CC$Decade)
All_EnergyUse_monthly_CC$Decade <-ifelse(All_EnergyUse_monthly_CC$Year >= 2016 & All_EnergyUse_monthly_CC$Year < 2026, 2020, All_EnergyUse_monthly_CC$Decade)
All_EnergyUse_monthly_CC$Decade <-ifelse(All_EnergyUse_monthly_CC$Year >= 2026 & All_EnergyUse_monthly_CC$Year < 2036, 2030, All_EnergyUse_monthly_CC$Decade)
All_EnergyUse_monthly_CC$Decade <-ifelse(All_EnergyUse_monthly_CC$Year >= 2036 & All_EnergyUse_monthly_CC$Year < 2046, 2040, All_EnergyUse_monthly_CC$Decade)
All_EnergyUse_monthly_CC$Decade <-ifelse(All_EnergyUse_monthly_CC$Year >= 2046 & All_EnergyUse_monthly_CC$Year < 2056, 2050, All_EnergyUse_monthly_CC$Decade)

#average monthly CC energy use by decade
All_EnergyUse_decadeAvg_monthly_CC <- All_EnergyUse_monthly_CC %>% group_by(Scenario, Decade, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Energy_Use_Category) %>% 
  summarize(CC_Electricity_use_GWh = mean(CC_Electricity_use_GWh, na.rm=TRUE))

#sum monthly energy use reference by year (not decade)

Yearly_EnergyUse_CC <- All_EnergyUse_monthly_CC %>% group_by(Scenario, Year, Month, WEAP_name, SWITCH_load_zone_id, SWITCH_load_zone, Sector, `Included Energy`, Energy_Use_Category) %>% 
  summarize(CC_Electricity_use_GWh = sum(CC_Electricity_use_GWh,  na.rm=TRUE))

#join total annual energy use by sector and load zone of CC scenarios with reference
Yearly_EnergyUse_CC_Reference <- left_join(Yearly_EnergyUse_CC, Yearly_All_EnergyUse_reference, c("Year"="Year", "Month"="Month", "WEAP_name" = "WEAP_name", 
                                                                                                                                    "SWITCH_load_zone_id" = "SWITCH_load_zone_id", "SWITCH_load_zone"="SWITCH_load_zone", "Sector"="Sector", 
                                                                                                                                    "Included Energy"="Included Energy", "Energy_Use_Category"="Energy_Use_Category"))
#calculate monthly delta in energy use as CC - Reference electricity use
Yearly_EnergyUse_CC_Reference$Delta_Electricity_GWh <- Yearly_EnergyUse_CC_Reference$CC_Electricity_use_GWh - Yearly_EnergyUse_CC_Reference$Reference_Electricity_use_GWh

#sum monthly deltas by year, load zone, sector
Delta_Annual_EnergyUse_lz_sector <- Yearly_EnergyUse_CC_Reference %>% group_by(Scenario, Year, SWITCH_load_zone_id, SWITCH_load_zone, Sector) %>% 
  summarize(Delta_Electricity_GWh = sum(Delta_Electricity_GWh), CC_Electricity_use_GWh = sum(CC_Electricity_use_GWh), Reference_Electricity_use_GWh = sum(Reference_Electricity_use_GWh))

#sum to get WECC annual deltas by year, sector
Delta_Annual_EnergyUse_WECC_sector <- Delta_Annual_EnergyUse_lz_sector %>% group_by(Scenario, Year, Sector) %>% 
  summarize(Delta_Electricity_GWh = sum(Delta_Electricity_GWh), CC_Electricity_use_GWh = sum(CC_Electricity_use_GWh), Reference_Electricity_use_GWh = sum(Reference_Electricity_use_GWh))

#output csv
write.csv(Delta_Annual_EnergyUse_WECC_sector, paste(weap_dir_results,"/Results_plots/", "WECC_Annual_Delta_EnergyUse_by_sector_scenario",".csv", sep=""))

###FIGURE 8
##box plot of total energy use for reference vs. climate scenarios by sector

#transpose
Delta_Annual_EnergyUse_WECC_sector_long <- gather(Delta_Annual_EnergyUse_WECC_sector, Electricity_metric, Electricity_GWh,4:6)
Delta_Annual_EnergyUse_WECC_sector_long <- Delta_Annual_EnergyUse_WECC_sector_long %>% filter(Sector != "Reservoir Operations")
Delta_Annual_EnergyUse_WECC_sector_long$CC_vs_Reference <- ifelse(Delta_Annual_EnergyUse_WECC_sector_long$Electricity_metric == "Reference_Electricity_use_GWh","Reference", "Climate Scenarios")
Delta_Annual_EnergyUse_WECC_sector_long2 <-    Delta_Annual_EnergyUse_WECC_sector_long [!duplicated(Delta_Annual_EnergyUse_WECC_sector_long[c(2,3,4,5,6)]),]

#reorder factors
Delta_Annual_EnergyUse_WECC_sector_long2$CC_vs_Reference <- ordered(Delta_Annual_EnergyUse_WECC_sector_long2$CC_vs_Reference, levels = c("Reference","Climate Scenarios"))
Delta_Annual_EnergyUse_WECC_sector_long2$Sector <- ifelse(Delta_Annual_EnergyUse_WECC_sector_long2$Sector %in%c("Urban"), "Domestic and C&I (Indoor + Outdoor)", Delta_Annual_EnergyUse_WECC_sector_long2$Sector)
Delta_Annual_EnergyUse_WECC_sector_long2$Sector <- ifelse(Delta_Annual_EnergyUse_WECC_sector_long2$Sector %in%c("Urban Indoor (Domestic Heating)"), "Domestic Heating", Delta_Annual_EnergyUse_WECC_sector_long2$Sector)

png(paste(weap_dir_results,"/Results_plots/", "Fig8_Box_plot_of_Annual_Electricity_Use_By_Sector_CC_vs_Ref",".png", sep=""), width=2000, height=800, res=100)

p <- ggplot(data = subset(Delta_Annual_EnergyUse_WECC_sector_long2, Electricity_metric != "Delta_Electricity_GWh"), aes(CC_vs_Reference, Electricity_GWh))
p + geom_boxplot(fill = "grey") + facet_wrap(Sector ~ ., scales="free_y") + ylab("Annual Electricity Use by Water Sector (GWh)") + xlab("Scenario") + 
  ggtitle("Annual electricity use from Reference and Climate scenarios, by sector") +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  theme_bw() + 
  theme(panel.grid.minor.y = element_line(color = "grey"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"), strip.text.y = element_text(size = 16),strip.text.x = element_text(size = 16),
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=0, size=14), axis.title.x = element_text(size=16), 
        legend.position="right", legend.text = element_text(size = 16), legend.title = element_text(size = 16), plot.title = element_text(size=18, hjust=0.5)) 
dev.off()

### for other plots (quadrant, sectoral box plots, etc)

#Joining the decadal monthly average usage for Reference vs CC scenarios 
#join CC scenarios with reference
Delta_All_EnergyUse_decadeAvg_monthly <- left_join(All_EnergyUse_decadeAvg_monthly_CC, All_EnergyUse_decadeAvg_monthly_reference, c("Decade"="Decade", "Month"="Month", "WEAP_name" = "WEAP_name", 
                                                                                                                                    "SWITCH_load_zone_id" = "SWITCH_load_zone_id", "SWITCH_load_zone"="SWITCH_load_zone", "Sector"="Sector", 
                                                                                                                                    "Included Energy"="Included Energy", "Energy_Use_Category"="Energy_Use_Category"))

#sum annual energy delta across all energy demand files, for annual energy demand related to water in WECC
#calculate monthly delta in energy use as CC - Reference electricity use
Delta_All_EnergyUse_decadeAvg_monthly$Delta_Electricity_GWh <- Delta_All_EnergyUse_decadeAvg_monthly$CC_Electricity_use_GWh - Delta_All_EnergyUse_decadeAvg_monthly$Reference_Electricity_use_GWh

#output csv
write.csv(Delta_All_EnergyUse_decadeAvg_monthly, paste(weap_dir_results,"/Results_plots/", "Delta_All_EnergyUse_decadeAvg_monthly",".csv", sep=""))

#sum deltas for each month and load zone
EnergyUse_lz_monthly_CC_delta <- Delta_All_EnergyUse_decadeAvg_monthly %>% group_by(Scenario, Decade, Month, SWITCH_load_zone_id, SWITCH_load_zone, Sector, Energy_Use_Category) %>% 
  summarize(Delta_Electricity_GWh = sum(Delta_Electricity_GWh),  CC_Electricity_GWh = sum(CC_Electricity_use_GWh), Reference_Electricity_GWh = sum(Reference_Electricity_use_GWh))

#drop Reservoir Operations category (not an energy use)
EnergyUse_lz_monthly_CC_delta <- EnergyUse_lz_monthly_CC_delta %>% filter(Sector != "Reservoir Operations")

#add to hydropower monthly and decadal deltas
Hydropower_lz_monthly_CC_delta <- rename(Hydropower_lz_monthly_CC_delta, SWITCH_load_zone_id = load_zone_id, SWITCH_load_zone = load_zone_name)
Hydropower_lz_monthly_CC_delta$Sector <- "Hydropower"
Hydropower_lz_monthly_CC_delta$Energy_Use_Category <- "Hydropower Generation"
Hydropower_lz_monthly_CC_delta <- Hydropower_lz_monthly_CC_delta %>%dplyr::select(Scenario, Decade, Month, SWITCH_load_zone_id, SWITCH_load_zone, 
                                                                                  Sector, Energy_Use_Category, Delta_Electricity_GWh, CC_Electricity_GWh,Reference_Electricity_GWh) 

#Change in Water-related energy balance calculated as change in supply (hydropower) minus change in demand (energy use for water)

#stack energy use and hydropower deltas
EnergyUse_Hydro_lz_monthly_delta <- rbind(EnergyUse_lz_monthly_CC_delta, Hydropower_lz_monthly_CC_delta)

#annual (sum of monthly average for the Decade) lz deltas for EnergyUse and Hydro by Sector
EnergyUse_Hydro_lz_sector_annual_delta <- EnergyUse_Hydro_lz_monthly_delta %>% group_by(Scenario, Decade, SWITCH_load_zone_id, SWITCH_load_zone, Sector) %>% summarize(Delta_Electricity_GWh = sum(Delta_Electricity_GWh),
                                                                                                                                                                       CC_Electricity_GWh = sum(CC_Electricity_GWh),
                                                                                                                                                                       Reference_Electricity_GWh = sum(Reference_Electricity_GWh))
#annual (sum of monthly average for the Decade) WECC total deltas for EnergyUse and Hydro by Sector
EnergyUse_Hydro_WECC_sector_annual_delta <- EnergyUse_Hydro_lz_monthly_delta %>% group_by(Scenario, Decade, Sector) %>% summarize(Delta_Electricity_GWh = sum(Delta_Electricity_GWh),
                                                                                                                                                                       CC_Electricity_GWh = sum(CC_Electricity_GWh),
                                                                                                                                                                       Reference_Electricity_GWh = sum(Reference_Electricity_GWh))


#Calculate total annual energy balance change (Hydropower annual average delta - Energy demand annual average delta)
Hydropower_lz_monthly_CC_delta2 <- Hydropower_lz_monthly_CC_delta
Hydropower_lz_monthly_CC_delta2 <- rename(Hydropower_lz_monthly_CC_delta2, Delta_Generation_GWh = Delta_Electricity_GWh, 
                                          Reference_Generation_GWh = Reference_Electricity_GWh, CC_Generation_GWh = CC_Electricity_GWh)
Hydropower_lz_monthly_CC_delta2$Sector <- NULL
Hydropower_lz_monthly_CC_delta2$Energy_Use_Category <- NULL

#join energy use and hydropower to calculate total energy balance by load zone and month for each decade and CC scenario

#calculate total change in energy demand by load zone
total_lz_EnergyUse_monthly_delta <- EnergyUse_lz_monthly_CC_delta %>% group_by(Scenario, Decade, Month, SWITCH_load_zone_id, SWITCH_load_zone) %>% summarize(Delta_Electricity_use_GWh = sum(Delta_Electricity_GWh), 
                                                                                                                                                                     CC_Electricity_use_GWh=sum(CC_Electricity_GWh),
                                                                                                                                                                     Reference_Electricity_use_GWh=sum(Reference_Electricity_GWh))

#joining total delta of hydropower by month and lz with total delta energy use by month and lz
Lz_monthly_energy_balance_change <- left_join(total_lz_EnergyUse_monthly_delta, Hydropower_lz_monthly_CC_delta2, c("Scenario"="Scenario", "Decade"="Decade", "Month"= "Month", 
                                                                                                                "SWITCH_load_zone_id"="SWITCH_load_zone_id", "SWITCH_load_zone"="SWITCH_load_zone"))

#some zones don't have hydropower so make that 0 instead of NA
Lz_monthly_energy_balance_change$Delta_Generation_GWh <- ifelse(is.na(Lz_monthly_energy_balance_change$Delta_Generation_GWh), 0,  Lz_monthly_energy_balance_change$Delta_Generation_GWh)
Lz_monthly_energy_balance_change$CC_Generation_GWh <- ifelse(is.na(Lz_monthly_energy_balance_change$CC_Generation_GWh), 0,  Lz_monthly_energy_balance_change$CC_Generation_GWh)
Lz_monthly_energy_balance_change$Reference_Generation_GWh <- ifelse(is.na(Lz_monthly_energy_balance_change$Reference_Generation_GWh), 0,  Lz_monthly_energy_balance_change$Reference_Generation_GWh)

#Calculate monthly energy balance change (Hydropower delta - Energy demand delta) by load zone
Lz_monthly_energy_balance_change$Total_monthly_change_energy_balance_GWh <- Lz_monthly_energy_balance_change$Delta_Generation_GWh - Lz_monthly_energy_balance_change$Delta_Electricity_use_GWh

#calculate annual energy balance change (Hydropower delta - Energy demand delta) by load zone
Lz_annual_energy_balance_change <- Lz_monthly_energy_balance_change %>% group_by(Scenario, Decade, SWITCH_load_zone_id, SWITCH_load_zone) %>% summarize(Total_annual_change_energy_balance_GWh = sum(Total_monthly_change_energy_balance_GWh),
                                                                                                                                                        Delta_Electricity_use_GWh = sum(Delta_Electricity_use_GWh), 
                                                                                                                                                        Delta_Generation_GWh = sum(Delta_Generation_GWh),
                                                                                                                                                        Reference_Generation_GWh = sum(Reference_Generation_GWh),
                                                                                                                                                        CC_Generation_GWh = sum(CC_Generation_GWh),
                                                                                                                                                        Reference_Electricity_use_GWh = sum(Reference_Electricity_use_GWh),
                                                                                                                                                        CC_Electricity_use_GWh = sum(CC_Electricity_use_GWh))

#calculate percentage change delta generation and energy use annually by load zone
Lz_annual_energy_balance_change$Annual_Delta_Generation_perc <- Lz_annual_energy_balance_change$Delta_Generation_GWh / Lz_annual_energy_balance_change$Reference_Generation_GWh
Lz_annual_energy_balance_change$Annual_Delta_Electricity_use_perc <- Lz_annual_energy_balance_change$Delta_Electricity_use_GWh / Lz_annual_energy_balance_change$Reference_Electricity_use_GWh

#if percentage changes are NaN (because there is no hydropower included in that load zone or there is not energy use modeled in that load zone (like Mexico or Canadian load zones), change percentage to 0)
Lz_annual_energy_balance_change$Annual_Delta_Generation_perc <- ifelse(is.nan(Lz_annual_energy_balance_change$Annual_Delta_Generation_perc), 0, Lz_annual_energy_balance_change$Annual_Delta_Generation_perc)
Lz_annual_energy_balance_change$Annual_Delta_Electricity_use_perc <- ifelse(is.nan(Lz_annual_energy_balance_change$Annual_Delta_Electricity_use_perc), 0, Lz_annual_energy_balance_change$Annual_Delta_Electricity_use_perc)

#facet plot of percentage change between hydropower and energy demand by load zone for each climate model
#colorblind friendly palette

cbpal2 <- c("#999999","#004949","#009292","#ff6db6","#ffb6db",
           "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
           "#920000","#924900","#db6d00","#24ff24","#ffcc00")

#calculate the total absolute and percentage deltas in hydropower and energy use across all the load zones (WECC total) and change in energy balance

WECCtotal_annual_energy_balance_change <- Lz_monthly_energy_balance_change %>% group_by(Scenario, Decade) %>% summarize(Total_annual_change_energy_balance_GWh = sum(Total_monthly_change_energy_balance_GWh),
                                                                                                                       Delta_Electricity_use_GWh = sum(Delta_Electricity_use_GWh), 
                                                                                                                       Delta_Generation_GWh = sum(Delta_Generation_GWh),
                                                                                                                       Reference_Generation_GWh = sum(Reference_Generation_GWh),
                                                                                                                       CC_Generation_GWh = sum(CC_Generation_GWh),
                                                                                                                       Reference_Electricity_use_GWh = sum(Reference_Electricity_use_GWh),
                                                                                                                       CC_Electricity_use_GWh = sum(CC_Electricity_use_GWh))

#calculate percentage change delta generation and energy use annually for WECC total
WECCtotal_annual_energy_balance_change$Annual_Delta_Generation_perc <- WECCtotal_annual_energy_balance_change$Delta_Generation_GWh / WECCtotal_annual_energy_balance_change$Reference_Generation_GWh
WECCtotal_annual_energy_balance_change$Annual_Delta_Electricity_use_perc <- WECCtotal_annual_energy_balance_change$Delta_Electricity_use_GWh / WECCtotal_annual_energy_balance_change$Reference_Electricity_use_GWh


#making the delta of energy balance change change in energy demand minus change in generation so that the size of hte dots make more sense
WECCtotal_annual_energy_balance_change$Positive_or_negative_annual_balance_change <- ifelse(WECCtotal_annual_energy_balance_change$Total_annual_change_energy_balance_GWh >= 0 , "Positive (Surplus)", "Negative (Shortage)")

#Figure 9
#WECC total plot of percentage change between hydropower and energy demand for each climate model, absolute balance change
png(paste(weap_dir_results,"/Results_plots/", "Fig9_WECC_facet_plot_Scatter_annual_hydro_energyuse_perc_and_Pos_OR_Negative_balance_change_all_scenarios",".png", sep=""), width=1700, height=1000, res=100)

ggplot(data=subset(WECCtotal_annual_energy_balance_change, Decade != 2020), aes(x=Annual_Delta_Electricity_use_perc, y=Annual_Delta_Generation_perc,  group=Scenario, color = Scenario, 
                                                                                size = abs(Total_annual_change_energy_balance_GWh), shape = as.factor(Positive_or_negative_annual_balance_change))) +
  geom_point() + xlab("Annual % Change in Water-Related Electricity Use from Reference") + ylab("Annual % Change in Hydropower from Reference") + 
  ggtitle("Average annual \u0394 in water-related electricity use and hydropower generation from Reference scenario, WECC total") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.text.y = element_text(size = 16),strip.text.x = element_text(size = 16),
        axis.text.y = element_text(angle=0, size=14), axis.title.y = element_text(size=16), axis.text.x = element_text(angle=0, size=14), axis.title.x = element_text(size=16), 
        legend.position="right", legend.text = element_text(size = 12), legend.title = element_text(size = 14), plot.title = element_text(size=18, hjust=0.1), 
        legend.direction = "vertical", legend.box = "vertical", panel.spacing = unit(1, "lines")) + 
  scale_size_continuous(name ="|Energy balance \u0394| (GWh)", breaks = c(100, 1000,5000, 10000, 20000)) + 
  scale_shape_manual(name = "Shortage or surplus energy balance", values=c(15, 17)) + 
  scale_colour_manual(values=cbpal2) + #using colorblind-friendly palette
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
   guides(color = guide_legend(override.aes = list(size=5)), shape = guide_legend(override.aes = list(size=5))) +
  coord_fixed(ratio = 1) +
  facet_wrap(.~Decade, ncol = 3) 

dev.off()

