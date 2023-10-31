#Author: Josh Roll 
#Date: 9/2020 

#Load libraries
#-----------------------
	#Correct lib path
	.libPaths("C:/Program Files/R/R-4.1.2/library")
	library(ggplot2)
	library(dplyr)
	library(scales)
	library(tigris)
	library(tidycensus)
	library(acs)
	library(zipcode)
	library(arcgisbinding)
	library(tidyr)
	library(rgeos)
	library(rgdal)

	#install.packages("PUMSutils")
	
#Define custom scripts functions
#------------------------------
	#Function that simplifies loading .RData objects
	assignLoad <- function(filename){
       load(filename)
       get(ls()[ls() != "filename"])
    }	

	#Set working directory
	setwd("//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/Equity and Pedestrian Crashes")

	#Set Census API Key
	Developer_Key <- ""
		
	#Function to rename selected census ACS variable
	rename_ACS_vars <- function(dat, geo){
		#Reform variables and select columns of interest
		#Rename data file
		Census.. <- dat
		#Create logic for geography level
		#County----
		if(geo == "County"){Census.. <- Census.. %>% mutate(County =  trimws(gsub(" County, Oregon","",NAME)))}
		#Urban Area----
		if(geo == "Urban Area"){Census.. <- Census.. %>% mutate(Urban_Area_Id = GEOID, Urban_Area =  Cluster_City)}
		#Zip Code----
		if(geo == "Zip Code"){Census.. <- Census.. %>% mutate(Zip_Code = GEOID)}
		#Tract----
		if(geo == "Tract"){Census.. <- Census.. %>% mutate(Tract = GEOID, County = trimws(gsub(" County","",sapply(strsplit(as.character(NAME), ","), function(x) x[[2]]))))}
		#Block group
		if(geo == "Block Group"){Census.. <- Census.. %>% mutate(Block_Group = GEOID, County = trimws(gsub(" County","",sapply(strsplit(as.character(NAME), ","), function(x) x[[3]]))))}

		#Rename all other elements
		Census..  <- Census.. %>% mutate(
			#Total Population
			#Population = B17001_001E,
			Population = B01003_001E,
			#Poverty
			Poverty = B17021_002E,
			#Poverty under 200% of poverty level
			Poverty_200 = C17002_002E + C17002_003E + C17002_004E + C17002_005E  + C17002_006E + C17002_007E,
			#SNAP
			SNAP_Hh = B22010_002E,
			#Households eith 1 or more disabled persons
			Disability_Hh = B22010_003E + B22010_006E,
			#Disability population (only works for tract)
			Disability = B18101_004E + B18101_007E + B18101_010E + B18101_013E + B18101_016E + B18101_019E + B18101_023E + B18101_026E + B18101_029E + B18101_032E + B18101_035E + B18101_038E,
			#Population in work force with a disability
			Disability_20_64 = B23024_003E + B23024_018E,
			#Limited English proficiency
			#Limited_English  = B16004_021E + B16004_022E + B16004_023E + B16004_043E + B16004_044E + B16004_045E + B16004_065E + B16004_066E + B16004_067E,
			Limited_English  =  B16004_007E + B16004_008E + B16004_012E + B16004_013E + B16004_017E +B16004_018E + B16004_022E +B16004_023E +
				B16004_029E +B16004_030E + B16004_034E +B16004_035E + B16004_039E +B16004_040E +B16004_044E + B16004_045E +
				B16004_051E +B16004_052E + B16004_056E +B16004_057E + B16004_061E +B16004_062E + B16004_066E +B16004_067E,
			#Total Households
			Total_Hh = B22010_001E,
			#Race
			#White = B02001_002E, 
			White = B03002_003E,	Black = B02001_003E,	AIAN = B02001_004E, Asian = B02001_005E, NHPI = B02001_006E,
				Mult_Race = (B02001_007E + B02001_008E + B02001_009E + B02001_010E), Latino = B03002_012E, 
				Bipoc = B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E + B02001_008E + B02001_009E + B02001_010E,
			Other_Race = B02001_004E +  B02001_006E,
			#Income 
			Median_Income = B19013_001E,
			#Age 
			#Male
			Age_Under5_Male = B01001_003E, Age_5_9_Male = B01001_004E, Age_10_14_Male = B01001_005E, Age_15_17_Male = B01001_006E, Age_18_19_Male = B01001_007E, Age_20_Male = B01001_008E, Age_21_Male = B01001_009E,
			Age_22_24_Male = B01001_010E, Age_25_29_Male = B01001_011E, Age_30_34_Male = B01001_012E, Age_35_39_Male = B01001_013E, Age_40_44_Male  = B01001_014E, Age_45_49_Male = B01001_015E,
			Age_50_54_Male = B01001_016E,	Age_55_59_Male = B01001_017E, Age_60_61_Male = B01001_018E, Age_62_64_Male = B01001_019E, Age_65_66_Male = B01001_020E, Age_67_69_Male = B01001_021E, 
			Age_70_74_Male = B01001_022E, 	Age_75_79_Male = B01001_023E, Age_80_84_Male = B01001_024E, Age_Over_84_Male = B01001_025E,
			#Female
			Age_Under5_Female = B01001_027E, Age_5_9_Female = B01001_028E, Age_10_14_Female = B01001_029E, Age_15_17_Female = B01001_030E, Age_18_19_Female = B01001_031E, Age_20_Female = B01001_032E, Age_21_Female = B01001_033E,
			Age_22_24_Female = B01001_034E, Age_25_29_Female = B01001_035E, Age_30_34_Female = B01001_036E, Age_35_39_Female = B01001_037E, Age_40_44_Female  = B01001_038E, Age_45_49_Female = B01001_039E,
			Age_50_54_Female = B01001_040E,	Age_55_59_Female = B01001_041E, Age_60_61_Female = B01001_042E, Age_62_64_Female = B01001_043E, Age_65_66_Female = B01001_044E, Age_67_69_Female = B01001_045E, 
			Age_70_74_Female = B01001_046E, 	Age_75_79_Female = B01001_047E, Age_80_84_Female = B01001_048E, Age_Over_84_Female = B01001_049E,
			#Vehicle availability
			#Vehicle_0 = B08201_002E , Vehicle_1 = B08201_003E, Vehicle_2 = B08201_004E, Vehicle_3 = B08201_005E, Vehicle_4_Plus = B08201_006E, #THis wasnt working for block groups 
			Vehicle_0 = B25044_003E + B25044_010E , Vehicle_1 = B25044_004E + B25044_011E, Vehicle_2 = B25044_005E + B25044_012E, Vehicle_3 = B25044_006E + B25044_013E, 
				Vehicle_4_Plus = B25044_007E + B25044_014E +  B25044_008E +  B25044_015E,
			#Commute mode
			Jtw_Motor_Vehicle = B08301_002E, Jtw_Transit =  B08301_010E, Jtw_Bike = B08301_018E, Jtw_Walk = B08301_019E,
			Workers = B08301_001E, 
			#Unemployment
			Labor_Force = B23025_002E, Unemployment = B23025_005E,
			#Crowded housing
			Crowded_Housing_Hh = B25014_005E + B25014_006E + B25014_007E + B25014_011E + B25014_012E + B25014_013E
		) %>% 
			mutate(Bipoc = Population - White)
		#Create household proportions]
		Census..  <- Census.. %>% mutate(SNAP_Hh_Prop = SNAP_Hh / Total_Hh, Disability_Hh_Prop = Disability_Hh / Total_Hh,
			Vehicle_0_Prop = Vehicle_0 / Total_Hh,  Vehicle_1_Prop = Vehicle_1 / Total_Hh,  Vehicle_2_Prop = Vehicle_2 / Total_Hh,  Vehicle_3_Prop = Vehicle_3 / Total_Hh,
			Vehicle_4_Plus_Prop = Vehicle_4_Plus / Total_Hh, Crowded_Housing_Hh_Prop = Crowded_Housing_Hh / Total_Hh
		)
		#Add male + female populations 
		Census.. <- Census..%>% mutate(Age_Under5 = Age_Under5_Male + Age_Under5_Female, Age_5_9 = Age_5_9_Male + Age_5_9_Female, Age_10_14 = Age_10_14_Male + Age_10_14_Female, 
			Age_15_17 = Age_15_17_Male + Age_15_17_Female, Age_18_19 = Age_18_19_Male + Age_18_19_Female, Age_20 = Age_20_Male + Age_20_Female, Age_21 = Age_21_Male + Age_21_Female,
		Age_22_24 = Age_22_24_Male + Age_22_24_Female, Age_25_29 = Age_25_29_Male + Age_25_29_Female, Age_30_34 = Age_30_34_Male + Age_30_34_Female, Age_35_39 = Age_35_39_Male + Age_35_39_Female,
		Age_40_44  = Age_40_44_Male + Age_40_44_Female, Age_45_49 = Age_45_49_Male + Age_45_49_Female, Age_50_54 = Age_50_54_Male + Age_50_54_Female, Age_55_59 = Age_55_59_Male + Age_55_59_Female,
		Age_60_61 = Age_60_61_Male + Age_60_61_Female, Age_62_64 = Age_62_64_Male + Age_62_64_Female, Age_65_66 = Age_65_66_Male + Age_65_66_Female, Age_67_69 = Age_67_69_Male + Age_67_69_Female, 
		Age_70_74 = Age_70_74_Male + Age_70_74_Female, 	Age_75_79 = Age_75_79_Male + Age_75_79_Female, Age_80_84 = Age_80_84_Male + Age_80_84_Female, Age_Over_84 = Age_Over_84_Male + Age_Over_84_Female)
		
		#Create cohorts
		Census.. <- Census..%>% mutate(Age_Under14 = Age_Under5 + Age_5_9 + Age_10_14, Age_Under19 = Age_Under5 + Age_5_9 + Age_10_14 + Age_15_17 + Age_18_19,
			Age_20_24 = Age_20 + Age_21 + Age_22_24, Age_25_34 = Age_25_29 + Age_30_34, Age_35_44 = Age_35_39 + Age_40_44, Age_45_54 = Age_45_49 + Age_50_54, Age_55_64 = Age_55_59 + Age_60_61 + Age_62_64, 
			Age_Over_64 = Age_65_66 + Age_67_69 + Age_70_74 + Age_75_79 + Age_80_84 + Age_Over_84,
			#Male
			Age_Under14_Male = Age_Under5_Male + Age_5_9_Male + Age_10_14_Male, Age_Under19_Male = Age_Under5_Male + Age_5_9_Male + Age_10_14_Male + Age_15_17_Male + Age_18_19_Male,
			Age_20_24_Male = Age_20_Male + Age_21_Male + Age_22_24_Male, Age_25_34_Male = Age_25_29_Male + Age_30_34_Male, Age_35_44_Male = Age_35_39_Male + Age_40_44_Male, Age_45_54_Male = Age_45_49_Male + Age_50_54_Male,
			Age_55_64_Male = Age_55_59_Male + Age_60_61_Male + Age_62_64_Male, Age_Over_64_Male = 
			Age_65_66_Male + Age_67_69_Male + Age_70_74_Male + Age_75_79_Male + Age_80_84_Male + Age_Over_84_Male,
			#Female
			Age_Under14_Female = Age_Under5_Female + Age_5_9_Female + Age_10_14_Female, Age_Under19_Female = Age_Under5_Female + Age_5_9_Female + Age_10_14_Female + Age_15_17_Female + Age_18_19_Female,
			Age_20_24_Female = Age_20_Female + Age_21_Female + Age_22_24_Female, Age_25_34_Female = Age_25_29_Female + Age_30_34_Female, Age_35_44_Female = Age_35_39_Female + Age_40_44_Female, Age_45_54_Female = Age_45_49_Female + Age_50_54_Female,
			Age_55_64_Female = Age_55_59_Female + Age_60_61_Female + Age_62_64_Female, Age_Over_64_Female = 
			Age_65_66_Female + Age_67_69_Female + Age_70_74_Female + Age_75_79_Female + Age_80_84_Female + Age_Over_84_Female
		)	
		
	
	
		#Create cohorts proportions
		Vars. <- c("Poverty","Poverty_200","Limited_English","Age_Under5","Age_Under14_Male","Age_Under19_Male", "Age_20_24_Male","Age_25_34_Male","Age_35_44_Male","Age_45_54_Male",
			"Age_55_64_Male","Age_Over_64_Male","Age_Under14_Female","Age_Under19_Female", "Age_20_24_Female","Age_25_34_Female","Age_35_44_Female","Age_45_54_Female",
			"Age_55_64_Female","Age_Over_64_Female","Age_Under14", "Age_Under19", "Age_20_24","Age_25_34","Age_35_44","Age_45_54",
			"Age_55_64","Age_Over_64")
		for(var in Vars.){
			Census..[paste(var,"_Prop",sep="")] <- Census..[,var] / Census..$Population
		}
		
		#Create proportions
		Census.. <- Census..%>% mutate(White_Prop = White / Population, Black_Prop = Black / Population, AIAN_Prop = AIAN / Population,
			Asian_Prop = Asian / Population, NHPI_Prop = NHPI / Population, Mult_Race_Prop = Mult_Race / Population, Latino_Prop = Latino / Population, Bipoc_Prop = Bipoc / Population,
			Jtw_Motor_Vehicle_Prop = Jtw_Motor_Vehicle / Workers, Jtw_Transit_Prop = Jtw_Transit / Workers, Jtw_Bike_Prop = Jtw_Bike / Workers, Jtw_Walk_Prop = Jtw_Walk / Workers, 
			Unemployment_Rate = Unemployment / Labor_Force, Disability_Prop = Disability / Population
		)
		#Define the final selected variables
		#################################
		Select_Vars. <- c(
				"Population",			
			"Poverty","Poverty_Prop","Poverty_200","Poverty_200_Prop","Limited_English",	"Limited_English_Prop",
			"SNAP_Hh","Disability","Disability_Prop","Disability_Hh","Disability_20_64","SNAP_Hh_Prop", "Disability_Hh_Prop",
			"White", "Black","AIAN","Asian","NHPI","Mult_Race","Latino","Bipoc",
			"White_Prop", "Black_Prop", "AIAN_Prop", "Asian_Prop", "NHPI_Prop", "Mult_Race_Prop", "Latino_Prop", "Bipoc_Prop",
			"Median_Income","Total_Hh",	"Age_Under5_Male","Age_5_9_Male","Age_10_14_Male","Age_15_17_Male","Age_18_19_Male","Age_20_Male","Age_21_Male",
			"Age_22_24_Male","Age_25_29_Male","Age_30_34_Male","Age_35_39_Male","Age_40_44_Male","Age_45_49_Male",
			"Age_50_54_Male","Age_55_59_Male","Age_60_61_Male","Age_62_64_Male","Age_65_66_Male","Age_67_69_Male", 
			"Age_70_74_Male","Age_75_79_Male","Age_80_84_Male","Age_Over_84_Male", 		
			"Age_Under5_Female","Age_5_9_Female","Age_10_14_Female","Age_15_17_Female","Age_18_19_Female","Age_20_Female","Age_21_Female",
			"Age_22_24_Female","Age_25_29_Female","Age_30_34_Female","Age_35_39_Female","Age_40_44_Female","Age_45_49_Female" ,
			"Age_50_54_Female","Age_55_59_Female","Age_60_61_Female","Age_62_64_Female","Age_65_66_Female","Age_67_69_Female", 
			"Age_70_74_Female","Age_75_79_Female","Age_80_84_Female","Age_Over_84_Female",
			"Age_Under5","Age_5_9","Age_10_14","Age_15_17","Age_18_19","Age_20", "Age_21",
			"Age_22_24","Age_25_29","Age_30_34","Age_35_39","Age_40_44","Age_45_49",
			"Age_50_54","Age_55_59","Age_60_61","Age_62_64","Age_65_66","Age_67_69", 
			"Age_70_74","Age_75_79","Age_80_84","Age_Over_84",
			"Age_Under14","Age_Under19","Age_20_24",  "Age_25_34",  "Age_35_44",  "Age_45_54",  "Age_55_64",  "Age_Over_64","Age_Under14_Male","Age_Under19_Male","Age_20_24_Male", 
			"Age_25_34_Male",  "Age_35_44_Male",  "Age_45_54_Male",  "Age_55_64_Male",  "Age_Over_64_Male","Age_Under14_Female" ,"Age_Under19_Female", "Age_20_24_Female","Age_25_34_Female","Age_35_44_Female","Age_45_54_Female" , 
			"Age_55_64_Female","Age_Over_64_Female", "Age_Under14_Male_Prop","Age_Under19_Male_Prop","Age_20_24_Male_Prop", 
			"Age_25_34_Male_Prop",  "Age_35_44_Male_Prop",  "Age_45_54_Male_Prop",  "Age_55_64_Male_Prop",  "Age_Over_64_Male_Prop","Age_Under14_Female_Prop", "Age_Under19_Female_Prop", "Age_20_24_Female_Prop",
			"Age_25_34_Female_Prop","Age_35_44_Female_Prop","Age_45_54_Female_Prop","Age_55_64_Female_Prop","Age_Over_64_Female_Prop", "Age_Under5_Prop","Age_Under14_Prop", "Age_Under19_Prop", "Age_20_24_Prop",
			"Age_25_34_Prop", "Age_35_44_Prop", "Age_45_54_Prop", "Age_55_64_Prop", "Age_Over_64_Prop",
			

			#Vehicle availability
			"Vehicle_0","Vehicle_1","Vehicle_2","Vehicle_3","Vehicle_4_Plus","Vehicle_0_Prop","Vehicle_1_Prop","Vehicle_2_Prop","Vehicle_3_Prop","Vehicle_4_Plus_Prop",
			#Commute mode
			"Jtw_Motor_Vehicle","Jtw_Transit","Jtw_Bike","Jtw_Walk","Workers",
			"Jtw_Motor_Vehicle_Prop","Jtw_Transit_Prop","Jtw_Bike_Prop","Jtw_Walk_Prop",
			#Unemployment
			"Labor_Force", "Unemployment", "Unemployment_Rate",
			#Crowded Housing
			"Crowded_Housing_Hh","Crowded_Housing_Hh_Prop"
			
		)
		#Add logic for geography
		if(geo == "County"){Select_Vars. <- c("County",Select_Vars.)}
		if(geo == "Urban Area"){Select_Vars. <- c("Urban_Area_Id","Urban_Area","NAME",Select_Vars.)}
		if(geo == "Zip Code"){Select_Vars. <- c("Zip_Code",Select_Vars.)}
		if(geo == "Tract"){Select_Vars. <- c("Tract","County",Select_Vars.)}
		if(geo == "Block Group"){Select_Vars. <- c("Block_Group","County",Select_Vars.)}
			
			
		#Return data
		Census..[,Select_Vars.]
	}
	
	
	#Write function to rename and prepare population and age census variable names
	############################
	all_age_race_rename <- function(dat){
		#Rename variables to meaningful names
		dat <- mutate(dat,
			#White 
			#######
			#Male---
			Age_Under5_Male_W = B01001H_003E, Age_5_9_Male_W = B01001H_004E, Age_10_14_Male_W = B01001H_005E, Age_15_17_Male_W = B01001H_006E, Age_18_19_Male_W = B01001H_007E, Age_20_24_Male_W = B01001H_008E,
			Age_25_29_Male_W = B01001H_009E, Age_30_34_Male_W = B01001H_010E, Age_35_44_Male_W = B01001H_011E, Age_45_54_Male_W = B01001H_012E, Age_55_64_Male_W = B01001H_013E,	
			Age_65_74_Male_W = B01001H_014E, Age_75_84_Male_W = B01001H_015E, Age_Over_84_Male_W = B01001H_016E, 
			#Female---
			Age_Under5_Female_W = B01001H_018E, Age_5_9_Female_W = B01001H_019E, Age_10_14_Female_W = B01001H_020E, Age_15_17_Female_W = B01001H_021E, Age_18_19_Female_W = B01001H_022E, Age_20_24_Female_W = B01001H_023E,
			Age_25_29_Female_W = B01001H_024E, Age_30_34_Female_W = B01001H_025E, Age_35_44_Female_W = B01001H_026E, Age_45_54_Female_W = B01001H_027E, Age_55_64_Female_W = B01001H_028E,	
			Age_65_74_Female_W = B01001H_029E, Age_75_84_Female_W = B01001H_030E, Age_Over_84_Female_W = B01001H_031E, 
			#Black
			#######
			#Male---
			Age_Under5_Male_B = B01001B_003E, Age_5_9_Male_B = B01001B_004E, Age_10_14_Male_B = B01001B_005E, Age_15_17_Male_B = B01001B_006E, Age_18_19_Male_B = B01001B_007E, Age_20_24_Male_B = B01001B_008E,
			Age_25_29_Male_B = B01001B_009E, Age_30_34_Male_B = B01001B_010E, Age_35_44_Male_B = B01001B_011E, Age_45_54_Male_B = B01001B_012E, Age_55_64_Male_B = B01001B_013E,	
			Age_65_74_Male_B = B01001B_014E, Age_75_84_Male_B = B01001B_015E, Age_Over_84_Male_B = B01001B_016E, 
			#Female---
			Age_Under5_Female_B = B01001B_018E, Age_5_9_Female_B = B01001B_019E, Age_10_14_Female_B = B01001B_020E, Age_15_17_Female_B = B01001B_021E, Age_18_19_Female_B = B01001B_022E, Age_20_24_Female_B = B01001B_023E,
			Age_25_29_Female_B = B01001B_0724E, Age_30_34_Female_B = B01001B_025E, Age_35_44_Female_B = B01001B_026E, Age_45_54_Female_B = B01001B_027E, Age_55_64_Female_B = B01001B_028E,	
			Age_65_74_Female_B = B01001B_029E, Age_75_84_Female_B = B01001B_030E, Age_Over_84_Female_B = B01001B_031E, 
			#AIAN
			#######
			#Male---
			Age_Under5_Male_AIAN = B01001C_003E, Age_5_9_Male_AIAN = B01001C_004E, Age_10_14_Male_AIAN = B01001C_005E, Age_15_17_Male_AIAN = B01001C_006E, Age_18_19_Male_AIAN = B01001C_007E, Age_20_24_Male_AIAN = B01001C_008E,
			Age_25_29_Male_AIAN = B01001C_009E, Age_30_34_Male_AIAN = B01001C_010E, Age_35_44_Male_AIAN = B01001C_011E, Age_45_54_Male_AIAN = B01001C_012E, Age_55_64_Male_AIAN = B01001C_013E,	
			Age_65_74_Male_AIAN = B01001C_014E, Age_75_84_Male_AIAN = B01001C_015E, Age_Over_84_Male_AIAN = B01001C_016E, 
			#Female---
			Age_Under5_Female_AIAN = B01001C_018E, Age_5_9_Female_AIAN = B01001C_019E, Age_10_14_Female_AIAN = B01001C_020E, Age_15_17_Female_AIAN = B01001C_021E, Age_18_19_Female_AIAN = B01001C_022E, Age_20_24_Female_AIAN = B01001C_023E,
			Age_25_29_Female_AIAN = B01001C_024E, Age_30_34_Female_AIAN = B01001C_025E, Age_35_44_Female_AIAN = B01001C_026E, Age_45_54_Female_AIAN = B01001C_027E, Age_55_64_Female_AIAN = B01001C_028E,	
			Age_65_74_Female_AIAN = B01001C_029E, Age_75_84_Female_AIAN = B01001C_030E, Age_Over_84_Female_AIAN = B01001C_031E, 
			#Asian
			#######
			#Male---
			Age_Under5_Male_A = B01001D_003E, Age_5_9_Male_A = B01001D_004E, Age_10_14_Male_A = B01001D_005E, Age_15_17_Male_A = B01001D_006E, Age_18_19_Male_A = B01001D_007E, Age_20_24_Male_A = B01001D_008E,
			Age_25_29_Male_A = B01001D_009E, Age_30_34_Male_A = B01001D_010E, Age_35_44_Male_A = B01001D_011E, Age_45_54_Male_A = B01001D_012E, Age_55_64_Male_A = B01001D_013E,	
			Age_65_74_Male_A = B01001D_014E, Age_75_84_Male_A = B01001D_015E, Age_Over_84_Male_A = B01001D_016E, 
			#Female---
			Age_Under5_Female_A = B01001D_018E, Age_5_9_Female_A = B01001D_019E, Age_10_14_Female_A = B01001D_020E, Age_15_17_Female_A = B01001D_021E, Age_18_19_Female_A = B01001D_022E, Age_20_24_Female_A = B01001D_023E,
			Age_25_29_Female_A = B01001D_024E, Age_30_34_Female_A = B01001D_025E, Age_35_44_Female_A = B01001D_026E, Age_45_54_Female_A = B01001D_027E, Age_55_64_Female_A = B01001D_028E,	
			Age_65_74_Female_A = B01001D_029E, Age_75_84_Female_A = B01001D_030E, Age_Over_84_Female_A = B01001D_031E, 
			#NHPI
			#######
			#Male---
			Age_Under5_Male_NHPI = B01001E_003E, Age_5_9_Male_NHPI = B01001E_004E, Age_10_14_Male_NHPI = B01001E_005E, Age_15_17_Male_NHPI = B01001E_006E, Age_18_19_Male_NHPI = B01001E_007E, Age_20_24_Male_NHPI = B01001E_008E,
			Age_25_29_Male_NHPI = B01001E_009E, Age_30_34_Male_NHPI = B01001E_010E, Age_35_44_Male_NHPI = B01001E_011E, Age_45_54_Male_NHPI = B01001E_012E, Age_55_64_Male_NHPI = B01001E_013E,	
			Age_65_74_Male_NHPI = B01001E_014E, Age_75_84_Male_NHPI = B01001E_015E, Age_Over_84_Male_NHPI = B01001E_016E, 
			#Female---
			Age_Under5_Female_NHPI = B01001E_018E, Age_5_9_Female_NHPI = B01001E_019E, Age_10_14_Female_NHPI = B01001E_020E, Age_15_17_Female_NHPI = B01001E_021E, Age_18_19_Female_NHPI = B01001E_022E, Age_20_24_Female_NHPI = B01001E_023E,
			Age_25_29_Female_NHPI = B01001E_024E, Age_30_34_Female_NHPI = B01001E_025E, Age_35_44_Female_NHPI = B01001E_026E, Age_45_54_Female_NHPI = B01001E_027E, Age_55_64_Female_NHPI = B01001E_028E,	
			Age_65_74_Female_NHPI = B01001E_029E, Age_75_84_Female_NHPI = B01001E_030E, Age_Over_84_Female_NHPI = B01001E_031E, 
			#Latinx
			#######
			#Male---
			Age_Under5_Male_L = B01001I_003E, Age_5_9_Male_L = B01001I_004E, Age_10_14_Male_L = B01001I_005E, Age_15_17_Male_L = B01001I_006E, Age_18_19_Male_L = B01001I_007E, Age_20_24_Male_L = B01001I_008E,
			Age_25_29_Male_L = B01001I_009E, Age_30_34_Male_L = B01001I_010E, Age_35_44_Male_L = B01001I_011E, Age_45_54_Male_L = B01001I_012E, Age_55_64_Male_L = B01001I_013E,	
			Age_65_74_Male_L = B01001I_014E, Age_75_84_Male_L = B01001I_015E, Age_Over_84_Male_L = B01001I_016E, 
			#Female---
			Age_Under5_Female_L = B01001I_018E, Age_5_9_Female_L = B01001I_019E, Age_10_14_Female_L = B01001I_020E, Age_15_17_Female_L = B01001I_021E, Age_18_19_Female_L = B01001I_022E, Age_20_24_Female_L = B01001I_023E,
			Age_25_29_Female_L = B01001I_024E, Age_30_34_Female_L = B01001I_025E, Age_35_44_Female_L = B01001I_026E, Age_45_54_Female_L = B01001I_027E, Age_55_64_Female_L = B01001I_028E,	
			Age_65_74_Female_L = B01001I_029E, Age_75_84_Female_L = B01001I_030E, Age_Over_84_Female_L = B01001I_031E
		)
			
		#Sum male and female
		#---------------------------
		dat <- mutate(dat,
			#White
			Age_Under5_W = Age_Under5_Male_W + Age_Under5_Female_W, Age_5_9_W = Age_5_9_Male_W + Age_5_9_Female_W, Age_10_14_W = Age_10_14_Male_W + Age_10_14_Female_W,
				Age_15_17_W = Age_15_17_Male_W + Age_15_17_Female_W, Age_18_19_W = Age_18_19_Male_W + Age_18_19_Female_W, Age_20_24_W = Age_20_24_Male_W + Age_20_24_Female_W,
				Age_25_29_W = Age_25_29_Male_W + Age_25_29_Female_W, Age_30_34_W = Age_30_34_Male_W + Age_30_34_Female_W, Age_35_44_W = Age_35_44_Male_W + Age_35_44_Female_W,
				Age_45_54_W = Age_45_54_Male_W + Age_45_54_Female_W, Age_55_64_W = Age_55_64_Male_W + Age_55_64_Female_W, Age_65_74_W = Age_65_74_Male_W + Age_65_74_Female_W,
				 Age_75_84_W =  Age_75_84_Male_W +  Age_75_84_Female_W, Age_Over_84_W = Age_Over_84_Male_W + Age_Over_84_Female_W,
			#Black
			Age_Under5_B = Age_Under5_Male_B + Age_Under5_Female_B, Age_5_9_B = Age_5_9_Male_B + Age_5_9_Female_B, Age_10_14_B = Age_10_14_Male_B + Age_10_14_Female_B,
				Age_15_17_B = Age_15_17_Male_B + Age_15_17_Female_B, Age_18_19_B = Age_18_19_Male_B + Age_18_19_Female_B, Age_20_24_B = Age_20_24_Male_B + Age_20_24_Female_B,
				Age_25_29_B = Age_25_29_Male_B + Age_25_29_Female_B, Age_30_34_B = Age_30_34_Male_B + Age_30_34_Female_B, Age_35_44_B = Age_35_44_Male_B + Age_35_44_Female_B,
				Age_45_54_B = Age_45_54_Male_B + Age_45_54_Female_B, Age_55_64_B = Age_55_64_Male_B + Age_55_64_Female_B, Age_65_74_B = Age_65_74_Male_B + Age_65_74_Female_B,
				 Age_75_84_B =  Age_75_84_Male_B +  Age_75_84_Female_B, Age_Over_84_B = Age_Over_84_Male_B + Age_Over_84_Female_B,
			#AIAN
			Age_Under5_AIAN = Age_Under5_Male_AIAN + Age_Under5_Female_AIAN, Age_5_9_AIAN = Age_5_9_Male_AIAN + Age_5_9_Female_AIAN, Age_10_14_AIAN = Age_10_14_Male_AIAN + Age_10_14_Female_AIAN,
				Age_15_17_AIAN = Age_15_17_Male_AIAN + Age_15_17_Female_AIAN, Age_18_19_AIAN = Age_18_19_Male_AIAN + Age_18_19_Female_AIAN, Age_20_24_AIAN = Age_20_24_Male_AIAN + Age_20_24_Female_AIAN,
				Age_25_29_AIAN = Age_25_29_Male_AIAN + Age_25_29_Female_AIAN, Age_30_34_AIAN = Age_30_34_Male_AIAN + Age_30_34_Female_AIAN, Age_35_44_AIAN = Age_35_44_Male_AIAN + Age_35_44_Female_AIAN,
				Age_45_54_AIAN = Age_45_54_Male_AIAN + Age_45_54_Female_AIAN, Age_55_64_AIAN = Age_55_64_Male_AIAN + Age_55_64_Female_AIAN, Age_65_74_AIAN = Age_65_74_Male_AIAN + Age_65_74_Female_AIAN,
				 Age_75_84_AIAN =  Age_75_84_Male_AIAN +  Age_75_84_Female_AIAN, Age_Over_84_AIAN = Age_Over_84_Male_AIAN + Age_Over_84_Female_AIAN,
			#Asian
			Age_Under5_A = Age_Under5_Male_A + Age_Under5_Female_A, Age_5_9_A = Age_5_9_Male_A + Age_5_9_Female_A, Age_10_14_A = Age_10_14_Male_A + Age_10_14_Female_A,
				Age_15_17_A = Age_15_17_Male_A + Age_15_17_Female_A, Age_18_19_A = Age_18_19_Male_A + Age_18_19_Female_A, Age_20_24_A = Age_20_24_Male_A + Age_20_24_Female_A,
				Age_25_29_A = Age_25_29_Male_A + Age_25_29_Female_A, Age_30_34_A = Age_30_34_Male_A + Age_30_34_Female_A, Age_35_44_A = Age_35_44_Male_A + Age_35_44_Female_A,
				Age_45_54_A = Age_45_54_Male_A + Age_45_54_Female_A, Age_55_64_A = Age_55_64_Male_A + Age_55_64_Female_A, Age_65_74_A = Age_65_74_Male_A + Age_65_74_Female_A,
				 Age_75_84_A =  Age_75_84_Male_A +  Age_75_84_Female_A, Age_Over_84_A = Age_Over_84_Male_A + Age_Over_84_Female_A,
			#NHPI
			Age_Under5_NHPI = Age_Under5_Male_NHPI + Age_Under5_Female_NHPI, Age_5_9_NHPI = Age_5_9_Male_NHPI + Age_5_9_Female_NHPI, Age_10_14_NHPI = Age_10_14_Male_NHPI + Age_10_14_Female_NHPI,
				Age_15_17_NHPI = Age_15_17_Male_NHPI + Age_15_17_Female_NHPI, Age_18_19_NHPI = Age_18_19_Male_NHPI + Age_18_19_Female_NHPI, Age_20_24_NHPI = Age_20_24_Male_NHPI + Age_20_24_Female_NHPI,
				Age_25_29_NHPI = Age_25_29_Male_NHPI + Age_25_29_Female_NHPI, Age_30_34_NHPI = Age_30_34_Male_NHPI + Age_30_34_Female_NHPI, Age_35_44_NHPI = Age_35_44_Male_NHPI + Age_35_44_Female_NHPI,
				Age_45_54_NHPI = Age_45_54_Male_NHPI + Age_45_54_Female_NHPI, Age_55_64_NHPI = Age_55_64_Male_NHPI + Age_55_64_Female_NHPI, Age_65_74_NHPI = Age_65_74_Male_NHPI + Age_65_74_Female_NHPI,
				 Age_75_84_NHPI =  Age_75_84_Male_NHPI +  Age_75_84_Female_NHPI, Age_Over_84_NHPI = Age_Over_84_Male_NHPI + Age_Over_84_Female_NHPI,
			#LAtinx
			Age_Under5_L = Age_Under5_Male_L + Age_Under5_Female_L, Age_5_9_L = Age_5_9_Male_L + Age_5_9_Female_L, Age_10_14_L = Age_10_14_Male_L + Age_10_14_Female_L,
				Age_15_17_L = Age_15_17_Male_L + Age_15_17_Female_L, Age_18_19_L = Age_18_19_Male_L + Age_18_19_Female_L, Age_20_24_L = Age_20_24_Male_L + Age_20_24_Female_L,
				Age_25_29_L = Age_25_29_Male_L + Age_25_29_Female_L, Age_30_34_L = Age_30_34_Male_L + Age_30_34_Female_L, Age_35_44_L = Age_35_44_Male_L + Age_35_44_Female_L,
				Age_45_54_L = Age_45_54_Male_L + Age_45_54_Female_L, Age_55_64_L = Age_55_64_Male_L + Age_55_64_Female_L, Age_65_74_L = Age_65_74_Male_L + Age_65_74_Female_L,
				 Age_75_84_L =  Age_75_84_Male_L +  Age_75_84_Female_L, Age_Over_84_L = Age_Over_84_Male_L + Age_Over_84_Female_L
		)	
		#Return data frame
		dat
	}
	#Create a zipcode lookup table
	## Download source file, unzip and extract into table
	#ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
	#temp <- tempfile()
	#download.file(ZipCodeSourceFile , temp)
	#Zip_Codes.. <- read.table(unz(temp, "US.txt"), sep="\t")
	#unlink(temp)
	#names(Zip_Codes..) = c("CountryCode", "zip", "PlaceName", 
	#	"AdminName1", "AdminCode1", "AdminName2", "AdminCode2", 
	#	"AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")
	#save(Zip_Codes.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Documentation/Zip_Code_Lookup.RData")
	#Load 
	Zip_Codes.. <- assignLoad(file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Documentation/Zip_Code_Lookup.RData")
	#Select just oregon zip
	Zip_Codes.. <- filter(Zip_Codes.., AdminCode1 =="OR")
	
#Grab and perpare Census Data
#----------------------------------------------------------------
	#ACS Data look up 
	########################
	#acs.lookup(keyword = "B08301", endyear = 201)
	#x <- acs.lookup(table.name = "B16004_006E", endyear = 2015)
	x <-  acs.lookup(endyear=2018, table.number="B23024",case.sensitive=F)
	x <-  acs.lookup(endyear=2018, table.name = "disability",case.sensitive=F)
	B18101
	#Initialize Census Key
	#api.key.install(Developer_Key)
	#Define census variables to download 
	#Census table name sexplained https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html
	
	Vars. <- c(
		#Total Population---
		#"B17001_001",
		"B01003_001",
		#Population below Poverty--- 
		"B17021_002", 
		#Poverty at under 200% and below of Poverty Threshold
		"C17002_002", "C17002_003", "C17002_004" ,"C17002_005","C17002_006","C17002_007",
		#Population with disability and Receive Food stamps
		#SNAP households, SNAP and household with 1 or more disabled persons: SNAP but no disabled persons: No SNAP but 1 or more disabled persons
		"B22010_002","B22010_003", "B22010_004",  "B22010_006",
		#Disabled Population
		"B18101_004","B18101_007","B18101_010","B18101_013","B18101_016","B18101_019","B18101_023","B18101_026","B18101_029","B18101_032","B18101_035","B18101_038E",
		#Disabled workers (ages 20-64)
		"B23024_003","B23024_018",
		#English proficiency
		#"B16004_021E +B16004_022E +B16004_023E +B16004_043E +B16004_044E +B16004_045E +B16004_065E +B16004_066E +B16004_067",		
		"B16004_007","B16004_008","B16004_012","B16004_013","B16004_017","B16004_018","B16004_022","B16004_023",
		"B16004_029","B16004_030","B16004_034","B16004_035","B16004_039","B16004_040","B16004_044","B16004_045",
		"B16004_051","B16004_052","B16004_056","B16004_057","B16004_061","B16004_062","B16004_066","B16004_067",
		#Total housholds
		"B22010_001",
		#Population by Race---
		#White, Black or African American, American Indian & Alaska Native, Asian, Hawaiin or other pacific islander, "Some other race alone", "Two or more races:" ,
		#"Two or more races: Two races including Some other race","Two or more races: Two races excluding Some other race, and three or more races", Hispanic or Latino
		#"B02001_002",Was the white variable
		"B03002_003","B02001_003","B02001_004", "B02001_005", "B02001_006", "B02001_007","B02001_008", "B02001_009","B02001_010","B03002_012",
		#Median Income---
		"B19013_001",
		#Population by Age/Sex---
		#Male/Femal Under 5 years, 5 to 9 years, 10 to 14 years, 15 to 17 years, 18 and 19 years, 20 years, 21 years, 22 to 24 years, 25 to 29 years, 30 to 34 years, 35 to 39 years
		#40 to 44 years, 45 to 49 years, 50 to 54 years, 55 to 59 years, 60 and 61 years, 62 to 64 years, 65 and 66 years, 67 to 69 years, 70 to 74 years, 
		#"75 to 79 years", "80 to 84 years",  "85 years and over"
		#Male
		c(paste("B01001_",str_pad(seq(3,25),3,pad="0"),sep=""),
		#Female 
		paste("B01001_",str_pad(seq(27,49),3,pad="0"),sep="")),
		#Vehicles available
		#"B08014_002"," B08014_003","B08014_004","B08014_005","B08014_006","B08014_007",
		#"B08201_002","B08201_003","B08201_004","B08201_005","B08201_006", this wasnt working for block groups 
		"B25044_003", "B25044_010", "B25044_004", "B25044_011", "B25044_005", "B25044_012", "B25044_006", "B25044_013", "B25044_007", "B25044_014", "B25044_008", "B25044_015",
		#Workers
		"B08301_001",
		#Journey to work----
		#Car, truck, van---
		"B08301_002",
		#Public transit
		"B08301_010",
		#Motorcycle, Bicycle, Walk
		"B08301_017","B08301_018","B08301_019",
		#Unemployment (Workers in labor force, unemployed labor force
		"B23025_002", "B23025_005",
		#Crowded Housing households
		"B25014_005", "B25014_006", "B25014_007", "B25014_011", "B25014_012", "B25014_013"
		
	)
	
	
	
	
#Cycle through all geographies to retreive data for all years
#-----------------------------------------------------------	
	#Define years to cycle through for acs data
	Acs_Years. <- 2012:2021
	#County 
	######################################
	#Create an object to store retreived data
	Store_Census.. <- data.frame()
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "county", variables = Vars., state = "OR", output = "wide", year = acs_year, geometry = F, key = 	Developer_Key)		
		#Run custom rename function
		Census.. <- mutate(rename_ACS_vars(Load_Census..,geo = "County"), Year = acs_year)
		#Append year and store in one data set
		Store_Census.. <- rbind(Store_Census..,Census..)
	}
	#Save to directory 
	save(Store_Census.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/County_Census_2006_2021_5yr.RData")
	
	
	#Tract 
	######################################
	#Create an object to store retreived data
	Store_Census.. <- data.frame()
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "tract", variables = Vars., state = "OR", output = "wide", year = acs_year, geometry = F, key =	Developer_Key)
		#Run custom rename function
		Census.. <- mutate(rename_ACS_vars(Load_Census.., geo = "Tract"), Year = acs_year)
		#Append year and store in one data set
		Store_Census.. <- rbind(Store_Census..,Census..)
	}

	#Save to directory 
	save(Store_Census.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Tract_Census_2008_2021_5yr.RData")

		
	#Urban Area
	######################################
	#Define years
	Acs_Years. <- 2012:2018	
	#Develop a list of Oregon only urban areas to select out of national urban area 
	Usa_Urban_Area.. <- get_acs(geography = "urban area", variables ="B17001_001", output = "wide", year = 2018, geometry = FALSE, show_call = TRUE, key = 	Developer_Key)
	OR_Urban_Area.. <- Usa_Urban_Area.. %>%
	  separate(NAME, into=c("Cluster_City", "Tail"), sep=",", remove=FALSE) %>%
	  mutate(Tail=str_trim(Tail)) %>%
	  separate(Tail, into=c("States", "U", "AC", "Year"), sep=" ") %>%
	  filter(str_detect(States, "OR")) %>%
	  dplyr::select(-c(U, AC, Year))
	#Remove some urban areas on border
	OR_Urban_Area.. <- OR_Urban_Area.. %>% filter(!(Cluster_City%in%c("Weiser,","Longview,","Walla Walla")))
	OR_Urban_Areas. <- OR_Urban_Area..$Cluster_City
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "urban area", variables = Vars., output = "wide", year = acs_year, geometry = FALSE, key =Developer_Key)
		#Convert to spatial 
		#Census_Sp <- as(Load_Census_Sf, "Spatial")
		Census.. <- Load_Census..
		#Get only the Oregon 
		Census.. <- filter(Census.. %>%  separate(NAME, into=c("Cluster_City", "Tail"), sep=",", remove=FALSE) %>%  mutate(Tail=str_trim(Tail)) %>%
			separate(Tail, into=c("States", "U", "AC", "Year"), sep=" "), Cluster_City%in%OR_Urban_Areas. & States%in%c("OR","OR--WA","OR--ID") )
		#Run custom rename function
		Census.. <- mutate(rename_ACS_vars(Census.., geo = "Urban Area"), Year = acs_year)
		#Append year and store in one data set
		if(acs_year == Acs_Years.[1]){
			Store_Census.. <-Census..
		} else {
			Store_Census.. <- rbind(Store_Census..,Census..)
		}
	}
	#Save to directory 
	save(Store_Census.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Urban_Areas_Census_2012_2018_5yr.RData")
		
	#Zip code
	######################################
	Acs_Years. <- 2011:2018
	#Create an object to store retreived data
	Store_Census.. <- data.frame()
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "zcta", variables = Vars., output = "wide", year = acs_year, geometry = F, key = 	Developer_Key)
		#Select only Oregon zipcodes
		Census.. <- filter(Load_Census.., GEOID%in%Zip_Codes..$zip)
		#Run custom rename function
		Census.. <- mutate(rename_ACS_vars(Census.., geo = "Zip Code"), Year = acs_year)
		#Append year and store in one data set
		Store_Census.. <- rbind(Store_Census..,Census..)
	}
	#Save to directory 
	save(Store_Census.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Zip_Code_Census_2007_2018_5yr.RData")
	
	#Block group
	######################################
	Acs_Years. <- 2013:2021
	#Create an object to store retreived data
	Store_Census.. <- data.frame()
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "block group", variables = Vars.,  state = "OR", output = "wide", year = acs_year, geometry = F, key =  Developer_Key)
		#Run custom rename function
		Census.. <- mutate(rename_ACS_vars(Load_Census.., geo = "Block Group"), Year = acs_year)
		#Append year and store in one data set
		Store_Census.. <- rbind(Store_Census..,Census..)
	}
	
	#Save to directory 
	save(Store_Census.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Block_Groups_Census_2009_2021_5yr.RData")
	

#---------------------------------------------------------------------------------------------------------------	
#---------------------------------------------------------------------------------------------------------------	
#Pull population and age by race information for FARS analysis
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

	#Develop list of variables
	Vars. <- c(
		#White by age
		paste("B01001H_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#Black by age
		paste("B01001B_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#American Indian
		paste("B01001C_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#Asian
		paste("B01001D_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#NAtive Hawwain Pacific Islander
		paste("B01001E_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)],
		#Hispanic
		paste("B01001I_",str_pad(seq(1,31),3,pad="0"),sep="")[c(3:16,18:31)]
	)
		

	#Urban Area
	######################################
	#Define years
	Acs_Years. <- 2012:2019
	#Develop a list of Oregon only urban areas to select out of national urban area 
	Usa_Urban_Area.. <- get_acs(geography = "urban area", variables ="B17001_001", output = "wide", year = 2018, geometry = FALSE, show_call = TRUE, key = 	Developer_Key)
	OR_Urban_Area.. <- Usa_Urban_Area.. %>%
	  separate(NAME, into=c("Cluster_City", "Tail"), sep=",", remove=FALSE) %>%
	  mutate(Tail=str_trim(Tail)) %>%
	  separate(Tail, into=c("States", "U", "AC", "Year"), sep=" ") %>%
	  filter(str_detect(States, "OR")) %>%
	  dplyr::select(-c(U, AC, Year))
	#Remove some urban areas on border
	OR_Urban_Area.. <- OR_Urban_Area.. %>% filter(!(Cluster_City%in%c("Weiser,","Longview,","Walla Walla")))
	OR_Urban_Areas. <- OR_Urban_Area..$Cluster_City
	
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "urban area", variables = Vars., output = "wide", year = acs_year, geometry = FALSE, key =Developer_Key)
		#Convert to spatial 
		#Census_Sp <- as(Load_Census_Sf, "Spatial")
		Census.. <- Load_Census..
		#Get only the Oregon 
		Census.. <- filter(Census.. %>%  separate(NAME, into=c("Cluster_City", "Tail"), sep=",", remove=FALSE) %>%  mutate(Tail=str_trim(Tail)) %>%
			separate(Tail, into=c("States", "U", "AC", "Year"), sep=" "), Cluster_City%in%OR_Urban_Areas. & States%in%c("OR","OR--WA","OR--ID") )
		#Run custom rename function
		Census.. <- mutate(all_age_race_rename(Census..), Year = acs_year)[,c(1:4,7,344:595)]
		#Append year and store in one data set
		if(acs_year == Acs_Years.[1]){
			Store_Census.. <-Census..
		} else {
			Store_Census.. <- rbind(Store_Census..,Census..)
		}
	}
	#Save to directory 
	save(Store_Census.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Urban_Areas_Age_Sex_2012_2019_5yr.RData")
		
	#State - 5 year
	######################################
	#Define years
	Acs_Years. <- 2009:2019
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "state", variables = Vars., output = "wide", year = acs_year, geometry = FALSE, key =Developer_Key)
		#Convert to spatial 
		#Census_Sp <- as(Load_Census_Sf, "Spatial")
		Census.. <- Load_Census..
		#Run custom rename function
		Census.. <- mutate(all_age_race_rename(Census..), Year = acs_year, State = NAME)[,c(1:2,339:592)]
		#Append year and store in one data set
		if(acs_year == Acs_Years.[1]){
			Store_Census.. <-Census..
		} else {
			Store_Census.. <- rbind(Store_Census..,Census..)
		}
	}
	#Save to directory 
	save(Store_Census.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/State_Age_Sex_2009_2019_5yr.RData")
	
	#State - 1 Year
	######################################
	#Define years
	Acs_Years. <- 2009:2018	
	for(acs_year in Acs_Years.){
		#Get the data for Oregon
		Load_Census.. <- get_acs(geography = "state", variables = Vars., output = "wide", year = acs_year,  survey = "acs1", geometry = FALSE, key =Developer_Key)
		#Convert to spatial 
		#Census_Sp <- as(Load_Census_Sf, "Spatial")
		Census.. <- Load_Census..
		#Run custom rename function
		Census.. <- mutate(all_age_race_rename(Census..), Year = acs_year, State = NAME)[,c(1:2,339:592)]
		#Append year and store in one data set
		if(acs_year == Acs_Years.[1]){
			Store_Census.. <-Census..
		} else {
			Store_Census.. <- rbind(Store_Census..,Census..)
		}
	}
	#Save to directory 
	save(Store_Census.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/State_Age_Sex_2009_2018_1yr.RData")
		
		Load_Census.. <- get_acs(geography = "state", variables = c("B23025_001","B23025_002","B23025_004","B23025_005"), survey = "acs1", output = "wide", year =2018,   geometry = FALSE, key =Developer_Key)
		

	B23025_001
	
#-----------------------------------------------------------------------------------
#Calculate Urban area propotion of census geos
#-----------------------------------------------------------------------------------
	#Ensure proper projection
	#######################
	#Create a proijection file for other files
	Projection_Utm <-"+proj=utm +zone=10 +datum=WGS84 +units=m"
	Projection_LatLong <-"+proj=longlat +zone=10 +datum=WGS84"
	
	#Block Groups 
	#########################
	
	
	#2000---
	#Init a geodatabase file
	#fgdb <-    "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/Spatial/Oregon_Census_Data.gdb"
	#2010---
	#Load_Block_Groups_00_Sp <-  readOGR(dsn=fgdb,layer= "Oregon_Block_Groups_2000") 
	#Ensure proper projections
	#Block_Groups_00_Sp <- spTransform(Load_Block_Groups_00_Sp, CRS(Projection_Utm))
	#Rename block group names
	#Block_Groups_00_Sp@data <- mutate(Block_Groups_00_Sp@data, BLKGROUP = str_pad(BLKGROUP, 3, pad = "0")) %>% mutate(Block_Group = paste(STATE, COUNTY, TRACT, BLKGROUP,sep=""))
		

	
	#2010---
	#Init a geodatabase file
	fgdb <-    "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/Spatial/Oregon_Census_Data.gdb"
	#2010---
	Load_Block_Groups_10_Sp <-  readOGR(dsn=fgdb,layer= "Oregon_Block_Groups_2010") 
	#Ensure proper projections
	Block_Groups_10_Sp <- spTransform(Load_Block_Groups_10_Sp, CRS(Projection_Utm))
	#Rename block group names
	Block_Groups_10_Sp@data <- mutate(Block_Groups_10_Sp@data, Block_Group = GEOID10, County_Code = COUNTYFP10, Land_Area_Sq_Me = ALAND10, Land_Area_Sq_Ft =  ALAND10 * 10.7639, Land_Area_Sq_Mi =  (ALAND10 * 10.7639) * 3.587006427915519e-8)
	
	
	#Tracts
	#########################
	#2010---
	Load_Tracts_10_Sp <-  readOGR(dsn=fgdb,layer= "Oregon_Tracts_2010") 
	#Ensure proper projections
	Load_Tracts_10_Sp <- spTransform(Load_Tracts_10_Sp, CRS(Projection_Utm))
	#Rename block group names
	Load_Tracts_10_Sp@data <- mutate(Load_Tracts_10_Sp@data, Tract = GEOID,   Land_Area_Sq_Me = ALAND, Land_Area_Sq_Ft =  ALAND * 10.7639, Land_Area_Sq_Mi =  (ALAND * 10.7639) * 3.587006427915519e-8)
	#2000
	#----
	#Load_Tracts_00_Sp <-  readOGR(dsn=fgdb,layer= "Oregon_Tracts_2000") 
	#Ensure proper projections
	#Load_Tracts_00_Sp <- spTransform(Load_Tracts_00_Sp, CRS(Projection_Utm))
	#Rename block group names
	#Load_Tracts_00_Sp@data <- mutate(Load_Tracts_00_Sp@data, Tract = CTIDFP00,   Land_Area_Sq_Me = ALAND00, Land_Area_Sq_Ft =  ALAND00 * 10.7639, Land_Area_Sq_Mi =  (ALAND00 * 10.7639) * 3.587006427915519e-8)
	

	#Urban Areas
	########################
	#2000---
	Load_Urban_Areas_00_Sp <-  readOGR(dsn=fgdb,layer= "USA_Urban_Areas_2000")
	#2000-----
	Urban_Areas_Sp <- spTransform(Load_Urban_Areas_00_Sp, CRS(Projection_Utm))
	Urban_Areas_Sp@data  <- mutate(Urban_Areas_Sp@data %>%  separate(NAME, into=c("Cluster_City", "State"), sep=",", remove=FALSE), State=str_trim(State), Urban_Area = as.character(Cluster_City))
	Urban_Areas_00_Sp <- Urban_Areas_Sp[Urban_Areas_Sp@data$State%in%c("OR", "OR--ID", "OR--WA"),]
	#2010---
	Load_Urban_Areas_10_Sp <-  readOGR(dsn=fgdb,layer= "USA_Urban_Areas_2010")
	Urban_Areas_Sp <- spTransform(Load_Urban_Areas_10_Sp, CRS(Projection_Utm))
	Urban_Areas_Sp@data  <- mutate(Urban_Areas_Sp@data %>%  separate(NAME10, into=c("Cluster_City", "State"), sep=",", remove=FALSE), State=str_trim(State), Urban_Area = as.character(Cluster_City))
	Urban_Areas_10_Sp <- Urban_Areas_Sp[Urban_Areas_Sp@data$State%in%c("OR", "OR--ID", "OR--WA"),]

  
	#Block Groups
	#################################
	#2000--------
	Poly_Sp <- Block_Groups_00_Sp
	Poly_Split_Sp <- raster::intersect(Poly_Sp, Urban_Areas_00_Sp)
	#Calculate area within an urban area
	Poly_Split_Sp@data$Urban_Area_Me <- gArea(Poly_Split_Sp,byid=TRUE)
	#Summ that area by census geo
	Sum_Poly_Split.. <- Poly_Split_Sp@data %>% group_by(Block_Group) %>% summarise(Urban_Area_Me = sum(Urban_Area_Me))
	#Join back to poly gon 
	Poly_Sp@data <- left_join(Poly_Sp@data, Sum_Poly_Split.., by = "Block_Group")
	#Add urban area name
	#Poly_Sp@data <- left_join(Poly_Sp@data, Poly_Split_Sp@data[,c("Block_Group","Urban_Area")], by = "Block_Group")
	Poly_Sp@data$Urban_Area <- Poly_Split_Sp@data$Urban_Area[match(Poly_Sp@data$Block_Group, Poly_Split_Sp@data$Block_Group)]
	Poly_Sp@data$Urban_Area[(is.na(Poly_Sp@data$Urban_Area))] <- "Rural"
	#Convert nas to 0 as they indicate completely rural
	Poly_Sp@data$Urban_Area_Me[is.na(Poly_Sp@data$Urban_Area_Me)] <- 0
	#Calcualte urban percentage of land area
	Poly_Sp@data <- mutate(Block_Group =  paste(STATE, COUNTY, TRACT,BLKGROUP,sep=""),Poly_Sp@data, Urban_Area_Prop = Urban_Area_Me / (Land_Area_Sq_Me))
	#Convert is.infinite to 0 as they indicate completely water geos
	Poly_Sp@data$Urban_Area_Prop [is.infinite(Poly_Sp@data$Urban_Area_Prop )] <- 0
	Poly_Sp@data$Urban_Area_Prop [is.na(Poly_Sp@data$Urban_Area_Prop )] <- 0
	#Store urban proportions for joining above
	save(Poly_Sp, file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Urban Proportions/Block_Groups_Urban_Prop_2000.RData")
	
	#2010---------
	Poly_Sp <- Block_Groups_10_Sp
	Poly_Split_Sp <- raster::intersect(Poly_Sp, Urban_Areas_10_Sp)
	#Calculate area within an urban area
	Poly_Split_Sp@data$Urban_Area_Me <- gArea(Poly_Split_Sp,byid=TRUE)
	#Summ that area by census geo
	Sum_Poly_Split.. <- Poly_Split_Sp@data %>% group_by(Block_Group) %>% summarise(Urban_Area_Me = sum(Urban_Area_Me))
	#Join back to poly gon 
	Poly_Sp@data <- left_join(Poly_Sp@data, Sum_Poly_Split.., by = "Block_Group")
	#Add urban area name
	#Poly_Sp@data <- left_join(Poly_Sp@data, Poly_Split_Sp@data[,c("Block_Group","Urban_Area")], by = "Block_Group")
	Poly_Sp@data$Urban_Area <- Poly_Split_Sp@data$Urban_Area[match(Poly_Sp@data$Block_Group, Poly_Split_Sp@data$Block_Group)]
	Poly_Sp@data$Urban_Area[(is.na(Poly_Sp@data$Urban_Area))] <- "Rural"
	#Convert nas to 0 as they indicate completely rural
	Poly_Sp@data$Urban_Area_Me[is.na(Poly_Sp@data$Urban_Area_Me)] <- 0
	#Calcualte urban percentage of land area
	Poly_Sp@data <- mutate(Block_Group = GEOID10,Poly_Sp@data, Urban_Area_Prop = Urban_Area_Me / (ALAND10 + AWATER10))
	#Convert is.infinite to 0 as they indicate completely water geos
	Poly_Sp@data$Urban_Area_Prop [is.infinite(Poly_Sp@data$Urban_Area_Prop )] <- 0
	Poly_Sp@data$Urban_Area_Prop [is.na(Poly_Sp@data$Urban_Area_Prop )] <- 0
	#Store urban proportions for joining above
	save(Poly_Sp, file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Urban Proportions/Block_Groups_Urban_Prop_2010.RData")
	
	#Tracts
	#################################
	#2000----
	Poly_Sp <- Load_Tracts_00_Sp
	Poly_Split_Sp <- raster::intersect(Poly_Sp,Urban_Areas_00_Sp)
	#Calculate area within an urban area
	Poly_Split_Sp@data$Urban_Area_Me <- gArea(Poly_Split_Sp,byid=TRUE)
	#Summ that area by census geo
	Sum_Poly_Split.. <- Poly_Split_Sp@data %>% group_by(Tract) %>% summarise(Urban_Area_Me = sum(Urban_Area_Me))
	#Join back to poly gon 
	Poly_Sp@data <- left_join(Poly_Sp@data, Sum_Poly_Split.., by = "Tract")
	#Add urban area name
	#Poly_Sp@data <- left_join(Poly_Sp@data, Poly_Split_Sp@data[,c("Block_Group","Urban_Area")], by = "Block_Group")
	Poly_Sp@data$Urban_Area <- Poly_Split_Sp@data$Urban_Area[match(Poly_Sp@data$Tract, Poly_Split_Sp@data$Tract)]
	Poly_Sp@data$Urban_Area[(is.na(Poly_Sp@data$Urban_Area))] <- "Rural"
	#Convert nas to 0 as they indicate completely rural
	Poly_Sp@data$Urban_Area_Me[is.na(Poly_Sp@data$Urban_Area_Me)] <- 0
	#Calcualte urban percentage of land area
	Poly_Sp@data <- mutate(Tract = CTIDFP00, Poly_Sp@data, Urban_Area_Prop = Urban_Area_Me / (ALAND00 + AWATER00))
	#Convert is.infinite to 0 as they indicate completely water geos
	Poly_Sp@data$Urban_Area_Prop [is.infinite(Poly_Sp@data$Urban_Area_Prop )] <- 0
	Poly_Sp@data$Urban_Area_Prop [is.na(Poly_Sp@data$Urban_Area_Prop )] <- 0
	#Store urban proportions for joining above
	save(Poly_Sp, file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Urban Proportions/Tracts_Urban_Prop_2000.RData")
		
	#2010----
	Poly_Sp <- Load_Tracts_10_Sp
	Poly_Split_Sp <- raster::intersect(Poly_Sp,Urban_Areas_10_Sp)
	#Calculate area within an urban area
	Poly_Split_Sp@data$Urban_Area_Me <- gArea(Poly_Split_Sp,byid=TRUE)
	#Summ that area by census geo
	Sum_Poly_Split.. <- Poly_Split_Sp@data %>% group_by(Tract) %>% summarise(Urban_Area_Me = sum(Urban_Area_Me))
	#Join back to poly gon 
	Poly_Sp@data <- left_join(Poly_Sp@data, Sum_Poly_Split.., by = "Tract")
	#Add urban area name
	#Poly_Sp@data <- left_join(Poly_Sp@data, Poly_Split_Sp@data[,c("Block_Group","Urban_Area")], by = "Block_Group")
	Poly_Sp@data$Urban_Area <- Poly_Split_Sp@data$Urban_Area[match(Poly_Sp@data$Tract, Poly_Split_Sp@data$Tract)]
	Poly_Sp@data$Urban_Area[(is.na(Poly_Sp@data$Urban_Area))] <- "Rural"
	#Convert nas to 0 as they indicate completely rural
	Poly_Sp@data$Urban_Area_Me[is.na(Poly_Sp@data$Urban_Area_Me)] <- 0
	#Calcualte urban percentage of land area
	Poly_Sp@data <- mutate(Tract = GEOID, Poly_Sp@data, Urban_Area_Prop = Urban_Area_Me / (ALAND + AWATER))
	#Convert is.infinite to 0 as they indicate completely water geos
	Poly_Sp@data$Urban_Area_Prop [is.infinite(Poly_Sp@data$Urban_Area_Prop )] <- 0
	Poly_Sp@data$Urban_Area_Prop [is.na(Poly_Sp@data$Urban_Area_Prop )] <- 0
	#Store urban proportions for joining above
	save(Poly_Sp, file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Urban Proportions/Tracts_Urban_Prop_2010.RData")
	


