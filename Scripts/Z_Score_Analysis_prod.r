#Author: Josh Roll 
#Date: 5/12/2021
#Subject:  Z-score analysis to analyze Pedestrain Injuries 
#Description: This script uses a standardized scoring method to categorize Census tracts in Oregon into categories based on their concentration of poverty and BIPOC populations.  
#Commonly referred to as a Social Vulnerability Index, this categorization is then used to understand pedestrian injury rate disparies and pathways to these disparities that include
#traffic expsoure and built environmental variables

#Version notes:

#Load libraries
#------------------------------
	#Establish library path
	library(tigris)
	library(dplyr)
	library(rgdal)
	library(htmlwidgets)
	library(sf)
	library(stringr)
	library(leaflet)
	library(htmlwidgets)
	library(tidyr)
	library(ggplot2)
	
	


	
#Set environmanetal 
#-----------------
  options( scipen = 10 )

#Define custom scripts functions
#------------------------------
	#Function that simplifies loading .RData objects
	assignLoad <- function(filename){
       load(filename)
       get(ls()[ls() != "filename"])
    }	
	
	#Function to create bin labels
	label_Bins <- function(Breaks.){
		Labels_ <- list()
		for(i in 1:(length(Breaks.)-1)){
			Labels_[[i]] <- paste(Breaks.[[i]], Breaks.[[i + 1]], sep="-")
		}
		#Return result
		unlist(Labels_)
	}

#Load Data
#------------------------------------
	#Recent period
	##########################
	Load_Tracts.. <- assignLoad(file = "//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Data/Crash Modeling/Tracts_Data_2014-2018.RData")
	#Use only tracts with land	
	Tracts.. <- filter(Load_Tracts.., ALAND > 0) 
	#Tracts.. %>% group_by(Tract) %>% summarise(Non_Drive_Prop = sum(Jtw_Transit +  Jtw_Walk + Jtw_Bike ) / sum(Workers))
	#Calcualte z scores
	Tracts.. <- #left_join(Tracts.., Tracts.. %>% group_by(Tract) %>% summarise(Non_Drive_Prop = sum(Jtw_Transit +  Jtw_Walk + Jtw_Bike ) / sum(Workers))) %>% 
		Tracts.. %>%
		mutate(
			z_Poverty = as.numeric(scale(Poverty_Prop, center = TRUE, scale = TRUE)),
			z_Bipoc = as.numeric(scale(Bipoc_Prop, center = TRUE, scale = TRUE)),
			#z_Disability_Hh = as.numeric(scale(Disability_Hh_Prop, center = TRUE, scale = TRUE)) ,
			#z_Limited_English = as.numeric(scale(Limited_English, center = TRUE, scale = TRUE)) ,
			#z_Age_Over_64 = as.numeric(scale(Age_Over_64_Prop, center = TRUE, scale = TRUE)),
			#z_Non_Drive_Prop = as.numeric(scale(Non_Drive_Prop, center = TRUE, scale = TRUE)) ,
			#z_Vehicle_0_Prop = as.numeric(scale(Vehicle_0_Prop, center = TRUE, scale = TRUE)) ,
			#z_Vmt_All_Arterial_Density = as.numeric(scale(Vmt_All_Arterial_Density, center = TRUE, scale = TRUE)) ,
			#Miles_Non_Int_45_Plus_Density = as.numeric(scale(Miles_Non_Int_45_Plus_Density, center = TRUE, scale = TRUE)) ,
			#Mean_Transit_Stops_Density = as.numeric(scale(Mean_Transit_Stops_Density, center = TRUE, scale = TRUE)) ,
			#Index = z_Poverty + z_Bipoc 
			Index = z_Poverty + z_Bipoc 
			#Index = z_Non_Drive_Prop + z_Vehicle_0_Prop + z_Vmt_All_Arterial_Density + Mean_Transit_Stops_Density
	)  
			 
	#Create categories
	Tracts..$Index_Cat <- cut(Tracts..$Index, breaks = c(-10,-1,0,1,25), labels = c("Lowest","Low","Moderate","High"))
	Tracts..$Index_Cat <- as.character(Tracts..$Index_Cat)
	Tracts..$Index_Cat[is.na(Tracts..$Index_Cat)] <- "No Population" 
	Tracts..$Index_Cat <- factor(Tracts..$Index_Cat, levels = c("No Population","Lowest","Low","Moderate","High"))
	#Add z score
	#Join with life expectancy
	Tracts.. <- left_join(Tracts.., Life_Exp.., by= "Tract")
	#Summarise data	
	Index_Summary.. <- rbind(Tracts.. %>% group_by(Index_Cat) %>% summarise(
		Population = sum(Population),
		Ped_KABC_Mean = mean(Ped_KABC), Ped_KABC = sum(Ped_KABC), Ped_KA_Mean = mean(Ped_KA), Ped_KA = sum(Ped_KA),
		Ped_KABC_Rate = Ped_KABC / Population * 100000, Ped_KA_Rate = Ped_KA / Population*100000,
		Non_Drive_Prop = sum(Jtw_Transit +  Jtw_Walk + Jtw_Bike ) / sum(Workers),Vehicle_0_Prop = mean(Vehicle_0_Prop),
		Vmt_All_Arterial  = mean(Vmt_All_Arterial_Density)*1e6 , Miles_Non_Int_45_Plus_Density  = mean(Miles_Non_Int_45_Plus_Density) *100,Mean_Transit_Stops = mean(Mean_Transit_Stops_Density),
		Poverty_Prop = mean(Poverty_Prop), Bipoc_Prop = mean(Bipoc_Prop),
		Age_Over_64_Prop = mean(Age_Over_64_Prop), 
		),
		#Create Oregon average row
		cbind(Index_Cat = "Oregon Average", Tracts.. %>% group_by() %>% 	summarise(Population = sum(Population),
		Ped_KABC_Mean = mean(Ped_KABC), Ped_KABC = sum(Ped_KABC), Ped_KA_Mean = mean(Ped_KA),Ped_KA = sum(Ped_KA),
		Ped_KABC_Rate = Ped_KABC / Population * 100000, Ped_KA_Rate = Ped_KA / Population*100000, 
		Non_Drive_Prop = sum(Jtw_Transit +  Jtw_Walk + Jtw_Bike ) / sum(Workers),Vehicle_0_Prop = mean(Vehicle_0_Prop),
		Vmt_All_Arterial  = mean(Vmt_All_Arterial_Density)*1e6 , Miles_Non_Int_45_Plus_Density  = mean(Miles_Non_Int_45_Plus_Density) *100, Mean_Transit_Stops = mean(Mean_Transit_Stops_Density),
		Poverty_Prop = mean(Poverty_Prop), Bipoc_Prop = mean(Bipoc_Prop), 
		Age_Over_64_Prop = mean(Age_Over_64_Prop)
	)))
	Ped_KABCO
	#Write to file
	###########################
	save(Tracts.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Data/Social Vulnerability Index/SVI_2014-2018.RData")
	write.csv(Tracts.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Data/Social Vulnerability Index/SVI_2014-2018.csv")
		
	#Summarise by index value
	Index_Summary.. <- rbind(Tracts.. %>% group_by(Index_Cat) %>% summarise(
		Population = sum(Population),
		Ped_KABCO_Mean = mean(Ped_KABCO), Ped_KABCO = sum(Ped_KABCO), Ped_KA_Mean = mean(Ped_KA), Ped_KA = sum(Ped_KA),
		Ped_KABCO_Rate = Ped_KABCO / Population * 100000, Ped_KA_Rate = Ped_KA / Population*100000,
		Non_Drive_Prop = sum(Jtw_Transit +  Jtw_Walk + Jtw_Bike ) / sum(Workers),Vehicle_0_Prop = mean(Vehicle_0_Prop),
		Vmt_All_Arterial  = mean(Vmt_All_Arterial_Density)*1e6 , Miles_Non_Int_45_Plus_Density  = mean(Miles_Non_Int_45_Plus_Density) *100,Mean_Transit_Stops = mean(Mean_Transit_Stops_Density),
		Poverty_Prop = mean(Poverty_Prop), Bipoc_Prop = mean(Bipoc_Prop),
		Age_Over_64_Prop = mean(Age_Over_64_Prop)
		),
		#Create Oregon average row
		cbind(Index_Cat = "Oregon Average", Tracts.. %>% group_by() %>% 	summarise(Population = sum(Population),
		Ped_KABCO_Mean = mean(Ped_KABCO), Ped_KABCO = sum(Ped_KABCO), Ped_KA_Mean = mean(Ped_KA),Ped_KA = sum(Ped_KA),
		Ped_KABCO_Rate = Ped_KABCO / Population * 100000, Ped_KA_Rate = Ped_KA / Population*100000, 
		Non_Drive_Prop = sum(Jtw_Transit +  Jtw_Walk + Jtw_Bike ) / sum(Workers),Vehicle_0_Prop = mean(Vehicle_0_Prop),
		Vmt_All_Arterial  = mean(Vmt_All_Arterial_Density)*1e6 , Miles_Non_Int_45_Plus_Density  = mean(Miles_Non_Int_45_Plus_Density) *100, Mean_Transit_Stops = mean(Mean_Transit_Stops_Density),
		Poverty_Prop = mean(Poverty_Prop), Bipoc_Prop = mean(Bipoc_Prop), 
		Age_Over_64_Prop = mean(Age_Over_64_Prop)
	))) %>% dplyr::select(c(Index_Cat,Poverty_Prop, Bipoc_Prop, Ped_KA_Rate, Ped_KABCO_Rate, Ped_KA, Ped_KABCO,Ped_KA_Mean, Ped_KABCO_Mean, Vmt_All_Arterial,
		 Miles_Non_Int_45_Plus_Density, Non_Drive_Prop, Mean_Transit_Stops,  Vehicle_0_Prop,Population))
	Index_Summary_1.. <- Index_Summary..
	#Format for table
	x <- t(Index_Summary..)
	#Format for table
	x <- t(Index_Summary..)
	Index_Summary_1.. <- Index_Summary..
	
	#Data descriptives for report
	#####################
	dat <- rbind(dplyr::select(mutate(Tracts..[,c("Tract","Poverty_Prop","Index_Cat")],Measure = Poverty_Prop, Type = "Poverty %"),-Poverty_Prop),
			dplyr::select(mutate(Tracts..[,c("Tract","Bipoc_Prop","Index_Cat")],Measure = Bipoc_Prop, Type = "BIPOC %"),-Bipoc_Prop),
			#dplyr::select(mutate(Index..[,c("Tract","Disability_Hh_Prop","Index_Cat")],Measure = Disability_Hh_Prop, Type = "Disability %"),-Disability_Hh_Prop),
			#dplyr::select(mutate(Index..[,c("Tract","Limited_English_Prop","Index_Cat")],Measure = Limited_English_Prop, Type = "Limted English Proficiency %"),-Limited_English_Prop),
			#dplyr::select(mutate(Index..[,c("Tract","Age_Over_64_Prop","Index_Cat")],Measure = Age_Over_64_Prop, Type = "Population 65+ %"),-Age_Over_64_Prop),
			dplyr::select(mutate(Tracts..[,c("Tract","Index","Index_Cat")],Measure = Index, Type = "Composite SVI Index"),-Index))
	 group_by(Type) %>% summarise(Mean = mean(Measure)),by = "Type")
	
	
	#Chart the index values to determine cut lines for categories
	#############################
	dat <- filter(dat, Type == "Composite SVI Index" & !(Index_Cat%in%"No Population") )
	#dat <- mutate(filter(dat,!(Index_Cat%in%"No Population") ), 
	
	ggplot(dat, aes(Measure)) +
		geom_histogram(binwidth = 0.1, aes(fill = Index_Cat)) + 
		facet_wrap(~Type, scales = "free",nrow = 3) +
		ylab("Count of Tracts") + xlab("Composite Z-Score") +
		ggtitle("Distribution of Composite Z-Scores for Race/Ethnicity & Income Index (REII)\n2014-2018") + 
		#scale_fill_manual( values=c(Colors.)) +
		theme(text = element_text(size = 16)) +
		#geom_hline(yintercept=	State_Avg_K_Rate., linetype = "dashed", color = "grey42", size = 1.25) +
		#Center plot
		theme(plot.title = element_text(hjust = 0.5)) + 
		guides(fill=guide_legend(title="Race/Ethnicity & \nIncome (REII) Index\nCategory")) +
		theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
		theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=16)) +
		theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
		theme(axis.title.x=element_text(size=16)) +
		theme(strip.text.x = element_text(size = 14)) +
		geom_vline(xintercept=c(0), linetype="dashed", color = "black", size=1) +
		geom_vline(xintercept=c(-1,1), linetype="twodash", color = "black", size=1) 
		#annotate("text", aes(x = Mean), y = 65, label = "Oregon Average", size = 8.5)
		#geom_text(aes(x = 0), y = 65, label = "Oregon Average", size = 8.5) 
	
	#Create a chart of rates by index value 
	#############################
	#NMake a copy of data 
	dat <- mutate(filter(Index_Summary_1.., !(Index_Cat%in%c("No Population","Oregon Average"))), Ped_KA_Rate_Label = round(Ped_KA_Rate,1))
	#Iregon average
	Avg_Rate <- filter(Index_Summary.., Index_Cat=="Oregon Average")$Ped_KA_Rate
	Colors. <- colorRampPalette(c("grey","blue", "red"))(4)
	dat$Avg_Rate <- Avg_Rate
	dat1 <- dat
	
	#Chart for report 
	####################
	ggplot(dat, aes(x = Index_Cat, y = Ped_KA_Rate)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Index_Cat))  +
	  geom_text(aes(x = Index_Cat, y = Ped_KA_Rate, label = Ped_KA_Rate_Label),fontface = "bold", size = 8, position = position_nudge(y = 1.0)) +
	  ylab("Injuries per 100,000 people") + xlab("Social Vulnerability Index") +
	  ggtitle("Pedestrian Fatal & Severe Injury Rate by\n Social Vulnerability Index\n in Oregon\n2014 - 2018") + 
	  scale_fill_manual( values=c(Colors.)) +
	   theme(text = element_text(size = 16)) +
	  #geom_hline(yintercept=	State_Avg_K_Rate., linetype = "dashed", color = "grey42", size = 1.25) +
	  #Center plot
	  theme(plot.title = element_text(hjust = 0.5)) + 
	  guides(fill=guide_legend(title="Social Vulnerability Index")) +
	  theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
	  theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=16)) +
	  theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
	  theme(axis.title.x=element_text(size=16)) +
	  theme(strip.text.x = element_text(size = 14)) +
	  geom_hline(yintercept=Avg_Rate, linetype="dashed", color = "black", size=1) +
	  annotate("text", x = 1.1, y = Avg_Rate+1.5, label = "Oregon Average", size = 8.5)
	
	
	#Chart for Equity Series 
	####################
	mypal <- colorFactor("Spectral", domain =1:5, na.color = "grey")
	Colors. <- 	mypal(c(1:5))
	Colors. <- 	mypal(c(2,3,4,5))
	
	
	ggplot(dat, aes(x = Index_Cat, y = Ped_KA_Rate)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Index_Cat))  +
	  geom_text(aes(x = Index_Cat, y = Ped_KA_Rate, label = Ped_KA_Rate_Label),fontface = "bold", size = 8, position = position_nudge(y = 1.0)) +
	  ylab("Injuries per 100,000 people") + xlab("Level of Concentration of Low income and BIPOC Populations") +
	  ggtitle("Pedestrian Fatal & Severe Injury Rate by\n Low Income & BIPOC Populations Concentration Level\n in Oregon\n2014 - 2018") + 
	  scale_fill_manual( values=c(Colors.)) +
	   theme(text = element_text(size = 16)) +
	  #geom_hline(yintercept=	State_Avg_K_Rate., linetype = "dashed", color = "grey42", size = 1.25) +
	  #Center plot
	  theme(plot.title = element_text(hjust = 0.5)) + 
	  guides(fill=guide_legend(title="Concentration Level")) +
	  theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
	  theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=16)) +
	  theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
	  theme(axis.title.x=element_text(size=16)) +
	  theme(strip.text.x = element_text(size = 14)) +
	  geom_hline(yintercept=Avg_Rate, linetype="dashed", color = "black", size=1) +
	  annotate("text", x = 1.1, y = Avg_Rate+1.5, label = "Oregon Average", size = 8.5)
	  
	
	#Chart for Equity Series 
	####################
	mypal <- colorFactor("Spectral", domain =1:5, na.color = "grey")
	Colors. <- 	mypal(c(1:5))
	Colors. <- 	mypal(c(2,3,4,5))
	
	
	ggplot(dat, aes(x = Index_Cat, y = Ped_KA_Rate)) + 
		geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Index_Cat))  +
		geom_text(aes(x = Index_Cat, y = Ped_KA_Rate, label = Ped_KA_Rate_Label),fontface = "bold", size = 8, position = position_nudge(y = 1.0)) +
		ylab("Injuries per 100,000 people") + xlab("Level of Concentration of Low income and BIPOC Populations") +
		ggtitle("Pedestrian Fatal & Severe Injury Rate by\n Low Income & BIPOC Populations Concentration Level\n in Oregon\n2014 - 2018") + 
		scale_fill_manual( values=c(Colors.)) +
		theme(text = element_text(size = 16)) +
		#geom_hline(yintercept=	State_Avg_K_Rate., linetype = "dashed", color = "grey42", size = 1.25) +
		theme(plot.title = element_text(hjust = 0.5)) + 
		guides(fill=guide_legend(title="Concentration Level")) +
		theme(legend.text=element_text(size=20),legend.title=element_text(size=20)) +
		theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20)) +
		theme(strip.text.x = element_text(size = 2)) +
		theme(text = element_text(size = 20))  +
		geom_hline(yintercept=Avg_Rate, linetype="dashed", color = "black", size=1) +
		annotate("text", x = 1.1, y = Avg_Rate+1.5, label = "Oregon Average", size = 8.5)

#------------------------------------------------------------------------------------
#Historic period - 2008-2012
#------------------------------------------------------------------------------------
	#Load data
	Load_Tracts.. <- assignLoad(file = "//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Data/Crash Modeling/Tracts_Data_2008-2012.RData")
	#Select only tracts with land
	Tracts.. <- filter(Load_Tracts.., ALAND >0)
	#Calcualte standarized scores
	Tracts.. <- Tracts.. %>% 
		mutate(
		z_Poverty = as.numeric(scale(Poverty_Prop, center = TRUE, scale = TRUE)),
		z_Bipoc = as.numeric(scale(Bipoc_Prop, center = TRUE, scale = TRUE)),
		z_Disability_Hh = as.numeric(scale(Disability_Hh_Prop, center = TRUE, scale = TRUE)) ,
		z_Limited_English = as.numeric(scale(Limited_English, center = TRUE, scale = TRUE)) ,
		z_Age_Over_64 = as.numeric(scale(Age_Over_64_Prop, center = TRUE, scale = TRUE)) ,
		Index = z_Poverty + z_Bipoc 
	)  
	#Create categories for the index
	Tracts..$Index_Cat <- cut(Tracts..$Index, breaks = c(-9.0,-1,0,1,13), labels = c("Lowest","Low","Moderate","High"))
	Tracts..$Index_Cat <- as.character(Tracts..$Index_Cat)
	Tracts..$Index_Cat[is.na(Tracts..$Index_Cat)] <- "No Population" 
	Tracts..$Index_Cat <- factor(Tracts..$Index_Cat, levels = c("No Population","Lowest","Low","Moderate","High"))
		
	#Write to file
	save(Tracts.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Data/Social Vulnerability Index/SVI_2008-2012.RData")
	write.csv(Tracts.., file = "//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Data/Social Vulnerability Index/SVI_2008-2012.csv")	
	
	#Summarise by index value
	Index_Summary.. <- rbind(Tracts.. %>% group_by(Index_Cat) %>% summarise(
		Population = sum(Population),
		Ped_KABCO_Mean = mean(Ped_KABCO), Ped_KABCO = sum(Ped_KABCO), Ped_KA_Mean = mean(Ped_KA), Ped_KA = sum(Ped_KA),
		Ped_KABCO_Rate = Ped_KABCO / Population * 100000, Ped_KA_Rate = Ped_KA / Population*100000,
		Non_Drive_Prop = sum(Jtw_Transit +  Jtw_Walk + Jtw_Bike ) / sum(Workers),Vehicle_0_Prop = mean(Vehicle_0_Prop),
		Vmt_All_Arterial  = mean(Vmt_All_Arterial_Density)*1e6 , Miles_Non_Int_45_Plus_Density  = mean(Miles_Non_Int_45_Plus_Density) *100,Mean_Transit_Stops = mean(Mean_Transit_Stops_Density),
		Poverty_Prop = mean(Poverty_Prop), Bipoc_Prop = mean(Bipoc_Prop),
		Age_Over_64_Prop = mean(Age_Over_64_Prop)
		),
		#Create Oregon average row
		cbind(Index_Cat = "Oregon Average", Tracts.. %>% group_by() %>% 	summarise(Population = sum(Population),
		Ped_KABCO_Mean = mean(Ped_KABCO), Ped_KABCO = sum(Ped_KABCO), Ped_KA_Mean = mean(Ped_KA),Ped_KA = sum(Ped_KA),
		Ped_KABCO_Rate = Ped_KABCO / Population * 100000, Ped_KA_Rate = Ped_KA / Population*100000, 
		Non_Drive_Prop = sum(Jtw_Transit +  Jtw_Walk + Jtw_Bike ) / sum(Workers),Vehicle_0_Prop = mean(Vehicle_0_Prop),
		Vmt_All_Arterial  = mean(Vmt_All_Arterial_Density)*1e6 , Miles_Non_Int_45_Plus_Density  = mean(Miles_Non_Int_45_Plus_Density) *100, Mean_Transit_Stops = mean(Mean_Transit_Stops_Density),
		Poverty_Prop = mean(Poverty_Prop), Bipoc_Prop = mean(Bipoc_Prop), 
		Age_Over_64_Prop = mean(Age_Over_64_Prop)
	))) %>% dplyr::select(c(Index_Cat,Poverty_Prop, Bipoc_Prop, Ped_KA_Rate, Ped_KABCO_Rate, Ped_KA, Ped_KABCO,Ped_KA_Mean, Ped_KABCO_Mean, Vmt_All_Arterial,
		 Miles_Non_Int_45_Plus_Density, Non_Drive_Prop, Mean_Transit_Stops,  Vehicle_0_Prop,Population))
	Index_Summary_2.. <- Index_Summary..
	#Format for table
	x <- t(Index_Summary..)
	Tracts.. %>% group_by(Index_Cat) %>% summarise(Tract_Count = length(Tract))
	#Create a chart
	#############################
	#NMake a copy of data 
	dat <- mutate(filter(Index_Summary.., !(Index_Cat%in%c("No Population","Oregon Average"))), Ped_KA_Rate_Label = round(Ped_KA_Rate,1))
	#Iregon average
	Avg_Rate <- filter(Index_Summary.., Index_Cat=="Oregon Average")$Ped_KA_Rate
	Colors. <- colorRampPalette(c("grey","blue", "red"))(4)
	dat$Avg_Rate <- Avg_Rate
	dat2 <- dat
#	Plot <-
	ggplot(dat, aes(x = Index_Cat, y = Ped_KA_Rate)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Index_Cat))  +
	  geom_text(aes(x = Index_Cat, y = Ped_KA_Rate, label = Ped_KA_Rate_Label),fontface = "bold", size = 8, position = position_nudge(y = 1.5)) +
	    ylab("Injuries per 100,000 people") + xlab("Concentration Index") +
	  ggtitle("Pedestrian Fatal & Severe Injury Rate by\n \n in Oregon\n2008-2012") + 
	  scale_fill_manual( values=c(Colors.)) +
	   theme(text = element_text(size = 16)) +
	  #geom_hline(yintercept=	State_Avg_K_Rate., linetype = "dashed", color = "grey42", size = 1.25) +
	  #Center plot
	  theme(plot.title = element_text(hjust = 0.5)) + 
	  guides(fill=guide_legend(title="Social Vulnerability Index")) +
	  theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
	  theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=16)) +
	  theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
	  theme(axis.title.x=element_text(size=16)) +
	  theme(strip.text.x = element_text(size = 14)) +
	  geom_hline(yintercept=Avg_Rate, linetype="dashed", color = "black", size=1) +
	  annotate("text", x = 1.1, y = Avg_Rate+1.5, label = "Oregon Average", size = 8.5)
	
	
	#Show both periods
	##############################
	#Make a copy of data 
	dat1 <- 	Index_Summary_1..
	dat2 <- 	Index_Summary_2..
	dat <- rbind(cbind(dat1[,colnames(dat2)],Year = "Period: 2014-2018"),
		cbind(dat2,Year = "Period: 2008-2012")) %>% mutate(Ped_KA_Rate_Label = round(Ped_KA_Rate,1),Ped_KABCO_Rate_Label = round(Ped_KABCO_Rate,1))
	dat <- left_join(dat, data.frame(Avg_Rate = filter(dat, Index_Cat=="Oregon Average")$Ped_KA_Rate, Year = filter(dat, Index_Cat=="Oregon Average")$Year))
	dat <- dat[dat$Index_Cat != "Oregon Average",]
	Colors. <- colorRampPalette(c("grey","blue", "red"))(4)
	mypal <- colorFactor("Spectral", domain =1:5, na.color = "grey")
	Colors. <- 	mypal(c(1:5))
	Colors. <- 	mypal(c(2,3,4,5))
	
	#Fatal and severe injury rate--------------
#	Plot <-
	ggplot(dat, aes(x = Index_Cat, y = Ped_KA_Rate)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Index_Cat))  +
	  geom_text(aes(x = Index_Cat, y = Ped_KA_Rate, label = Ped_KA_Rate_Label),fontface = "bold", size = 10, position = position_nudge(y = 1.0)) +
	  ylab("Injuries per 100,000 people") + xlab("Concentration Index") +
	  ggtitle("Changes in\nPedestrian Fatal & Severe Injury Rate by\n Low Income & BIPOC Populations Concentration Level\n in Oregon") + 
	  scale_fill_manual( values=c(Colors.)) +
	   theme(text = element_text(size = 16)) +
	  facet_wrap(~Year, nrow = 1) +
	  #Center plot
	  theme(plot.title = element_text(hjust = 0.5)) + 
	  guides(fill=guide_legend(title="Concentration Level")) +
	  theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20)) +
	  theme(strip.text.x = element_text(size = 2)) +
	  theme(text = element_text(size = 20))  +
	  geom_hline(aes(yintercept=Avg_Rate), linetype="dashed", color = "black", size=1) +
	  geom_text(aes(x = 1.1, y = Avg_Rate + 1.5), label = "Oregon Average", size = 8.5)
	#All injury rate--------------
	dat1 <- 	Index_Summary_1..
	dat2 <- 	Index_Summary_2..
	dat <- rbind(cbind(dat1[,colnames(dat2)],Year = "Period: 2014-2018"),
		cbind(dat2,Year = "Period: 2008-2012")) %>% mutate(Ped_KA_Rate_Label = round(Ped_KA_Rate,1),Ped_KABCO_Rate_Label = round(Ped_KABCO_Rate,1))
	dat <- left_join(dat, data.frame(Avg_Rate = filter(dat, Index_Cat=="Oregon Average")$Ped_KABCO_Rate, Year = filter(dat, Index_Cat=="Oregon Average")$Year))
	dat <- dat[dat$Index_Cat != "Oregon Average",]
	Colors. <- colorRampPalette(c("grey","blue", "red"))(4)
	mypal <- colorFactor("Spectral", domain =1:5, na.color = "grey")
	Colors. <- 	mypal(c(1:5))
	Colors. <- 	mypal(c(2,3,4,5))
	ggplot(dat, aes(x = Index_Cat, y = Ped_KABCO_Rate)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Index_Cat))  +
	  geom_text(aes(x = Index_Cat, y = Ped_KABCO_Rate +5, label = Ped_KABCO_Rate_Label),fontface = "bold", size = 10, position = position_nudge(y = 1.0)) +
	  ylab("Injuries per 100,000 people") + xlab("Concentration Index") +
	  ggtitle("Changes in\nPedestrian Injury Rate by\n Low Income & BIPOC Populations Concentration Level\n in Oregon") + 
	  scale_fill_manual( values=c(Colors.)) +
	   theme(text = element_text(size = 16)) +
	  facet_wrap(~Year, nrow = 1) +
	  #Center plot
	  theme(plot.title = element_text(hjust = 0.5)) + 
	  guides(fill=guide_legend(title="Concentration Level")) +
	  theme(axis.text.x=element_text(size=20),axis.title.x=element_text(size=20)) +
	  theme(strip.text.x = element_text(size = 2)) +
	  theme(text = element_text(size = 20))  +
	  geom_hline(aes(yintercept=Avg_Rate), linetype="dashed", color = "black", size=1) +
	  geom_text(aes(x = 1.1, y = Avg_Rate + 6.9), label = "Oregon Average", size = 8.5)
	
	
	#Show change in rate
	##############################
	#Make a copy of data 
	dat1 <- 	Index_Summary_1..#rbind(cbind(Index_Summary_1..[,c("Index_Cat")], Index_Summary_1..[,c("Population","Ped_KA_Rate","Ped_KABCO_Rate")], Index_Summary_2..[,c("Population","Ped_KA_Rate","Ped_KABCO_Rate")]))
	dat2 <- 	Index_Summary_2..
	dat <- rbind(cbind(dat1[,colnames(dat2)],Year = "Period: 2014-2018"),
		cbind(dat2,Year = "Period: 2008-2012")) %>% mutate(Ped_KA_Rate_Label = round(Ped_KA_Rate,1))
	dat <- left_join(dat, data.frame(Avg_Rate = filter(dat, Index_Cat=="Oregon Average")$Ped_KA_Rate, Year = filter(dat, Index_Cat=="Oregon Average")$Year))
	dat <- dat[dat$Index_Cat != "Oregon Average",]
	Colors. <- colorRampPalette(c("grey","blue", "red"))(4)
	
#	Plot <-
	ggplot(dat, aes(x = Index_Cat, y = Ped_KA_Rate)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Index_Cat))  +
	  geom_text(aes(x = Index_Cat, y = Ped_KA_Rate, label = Ped_KA_Rate_Label),fontface = "bold", size = 8, position = position_nudge(y = 1.0)) +
	  ylab("Injuries per 100,000 people") + xlab("Race/Ethnicity Concentration Index") +
	  ggtitle("Changes in\nPedestrian Fatal & Severe Injury Rate by\n Concentration Index\n in Oregon") + 
	  scale_fill_manual( values=c(Colors.)) +
	   theme(text = element_text(size = 16)) +
	  facet_wrap(~Year, nrow = 1) +
	  #Center plot
	  theme(plot.title = element_text(hjust = 0.5)) + 
	  guides(fill=guide_legend(title="Social Vulnerability Index")) +
	  theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
	  theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=16)) +
	  theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
	  theme(axis.title.x=element_text(size=16)) +
	  theme(strip.text.x = element_text(size = 14)) +
	  geom_hline(aes(yintercept=Avg_Rate), linetype="dashed", color = "black", size=1) +
	  #annotate("text", x = 1.1, y = Avg_Rate+1.5, label = "Oregon Average", size = 8.5)
	  geom_text(aes(x = 1.1, y = Avg_Rate + 1.5), label = "Oregon Average", size = 8.5)
	
	
	
	
	#Show just the 
	
	#Show change in Select metrics
	##############################
	#Make a copy of data 
	dat1 <- 	Index_Summary_1..
	dat2 <- 	Index_Summary_2..
	dat <- rbind(cbind(dat1[,colnames(dat2)],Year = "Period: 2014-2018"),
		cbind(dat2,Year = "Period: 2008-2012")) %>% mutate(Ped_KA_Rate_Label = round(Ped_KA_Rate,1))
	
	dat <- rbind(dplyr::select(cbind(dat[,c("Index_Cat","Poverty_Prop","Year")],Measure = "Poverty %",Type = "Social Vulnerability Factors") %>% mutate(Value = Poverty_Prop),c(-Poverty_Prop)),
		dplyr::select(cbind(dat[,c("Index_Cat","Bipoc_Prop","Year")],Measure = "Black, Indigenous & People of Color %",Type = "BIPOC %")%>% mutate(Value = Bipoc_Prop),c(-Bipoc_Prop)),
		dplyr::select(cbind(dat[,c("Index_Cat","Ped_KA_Rate","Year")],Measure = "Pedestrian\nFatal & Severe Injuries Rate",Type = "Pedestrian Injury")%>% mutate(Value = Ped_KA_Rate),c(-Ped_KA_Rate)),
		dplyr::select(cbind(dat[,c("Index_Cat","Ped_KABCO_Rate","Year")],Measure = "Pedestrian\nAll Injuries Rate",Type = "Pedestrian Injury")%>% mutate(Value =Ped_KABCO_Rate),c(-Ped_KABCO_Rate)),
		dplyr::select(cbind(dat[,c("Index_Cat","Non_Drive_Prop","Year")],Measure = "Walk + Transit + Bike Commute %",Type = "Non_Vehicle Commuting")%>% mutate(Value = Non_Drive_Prop),c(-Non_Drive_Prop)),
		dplyr::select(cbind(dat[,c("Index_Cat","Vehicle_0_Prop","Year")],Measure = "% Households with No Vehicle",Type = "Vehicle Qwnership")%>% mutate(Value = Vehicle_0_Prop),c(-Vehicle_0_Prop)),
		dplyr::select(cbind(dat[,c("Index_Cat","Vmt_All_Arterial","Year")],Measure = "VMT on Arterials Density (Millions per Sqmi)",Type = "Vehicle Exposure")%>% mutate(Value = Vmt_All_Arterial),c(-Vmt_All_Arterial)),
		dplyr::select(cbind(dat[,c("Index_Cat","Mean_Transit_Stops","Year")],Measure = "Transit Stops per Sqmi",Type = "Transit")%>% mutate(Value = Mean_Transit_Stops),c(-Mean_Transit_Stops)),
		dplyr::select(cbind(dat[,c("Index_Cat","Miles_Non_Int_45_Plus_Density","Year")],Measure = "High Speed (45 mph +) Roadway Density\nNon-Freeway/Highway",Type = "Speed")%>% mutate(Value = Miles_Non_Int_45_Plus_Density),c(-Miles_Non_Int_45_Plus_Density)))
	dat <- dat[dat$Index_Cat != "Oregon Average",]
	
	Colors. <- colorRampPalette(c("grey","blue", "red"))(4)
	#SHow just the rate changes
	dat1 <-filter(dat, Measure%in%c("Pedestrian\nFatal & Severe Injuries Rate","Pedestrian\nAll Injuries Rate"))
	dat1$Measure <- factor(dat1$Measure,levels =  c("Pedestrian\nFatal & Severe Injuries Rate","Pedestrian\nAll Injuries Rate"))
	
	ggplot(dat1, aes(x = Index_Cat, y = Value)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Year)) +  
		ylab("Pedestrian Injury Rate per 100,000") + xlab("Social Vulnerability Index") +
		ggtitle("Changes in\nPedestrian Injury Rate by Social Vulnerability Index\n in Oregon Over Time") + 
		# scale_fill_manual( values=c(Colors.)) +
		theme(text = element_text(size = 16)) +
		facet_wrap(~Measure,  scales = "free", nrow = 1) +
		#scale_y_continuous(labels = scales::percent) +
		#Center plot
		theme(plot.title = element_text(hjust = 0.5)) + 
		 guides(fill=FALSE) +
		theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
		theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=16)) +
		theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
		theme(axis.title.x=element_text(size=16)) +
		theme(strip.text.x = element_text(size = 14)) 
	
	
	#Percetnage measures
	dat1 <-filter(dat, Measure%in%c("Poverty %","Black, Indigenous & People of Color %", "Walk + Transit + Bike Commute %","% Households with No Vehicle"))
	dat1$Measure <- factor(dat1$Measure,levels =  c("Poverty %","Black, Indigenous & People of Color %", "Walk + Transit + Bike Commute %","% Households with No Vehicle"))
	Plot1 <-
	ggplot(dat1, aes(x = Index_Cat, y = Value)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Year)) +  
		ylab("for Presented Measure                                        ") + xlab("Concentration Level") +
		ggtitle("Changes in\nPedestrian Fatal & Severe Injury Rate by\n Concentration Level\n in Oregon") + 
		# scale_fill_manual( values=c(Colors.)) +
		theme(text = element_text(size = 16)) +
		facet_wrap(~Measure,  scales = "free", nrow = 1) +
		scale_y_continuous(labels = scales::percent) +
		#Center plot
		theme(plot.title = element_text(hjust = 0.5)) + 
		 guides(fill=FALSE) +
		theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
		theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=16)) +
		theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
		theme(axis.title.x=element_text(size=16)) +
		theme(strip.text.x = element_text(size = 14)) 
	#Non percentage measures
	dat2 <-filter(dat, Measure%in%c("Pedestrian\nFatal & Severe Injuries Rate", "VMT on Arterials Density (Millions per Sqmi)","High Speed (45 mph +) Roadway Density\nNon-Freeway/Highway","Transit Stops per Sqmi"))
	dat2$Measure <- factor(dat2$Measure,levels =  c("Pedestrian\nFatal & Severe Injuries Rate", "VMT on Arterials Density (Millions per Sqmi)","High Speed (45 mph +) Roadway Density\nNon-Freeway/Highway","Transit Stops per Sqmi"))
	Plot2 <-
	ggplot(dat2, aes(x = Index_Cat, y = Value)) + 
	  geom_bar(stat="identity", width=.5, position = "dodge",aes(fill = Year)) +  
		ylab("                                                       Mean Tract Value") + xlab("Concentration Level") +
		ggtitle("") + 
		# scale_fill_manual( values=c(Colors.)) +
		theme(text = element_text(size = 16)) +
		facet_wrap(~Measure,  scales = "free",nrow=1) +
		scale_y_continuous(labels = scales::comma) +
		#Center plot
		theme(plot.title = element_text(hjust = 0.5)) + 
		theme(legend.position="bottom") +
		
		theme(legend.text=element_text(size=16),legend.title=element_text(size=16)) +
		theme(axis.text.y=element_text(size=16),axis.title.y=element_text(size=16)) +
		theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=16)) +
		theme(axis.title.x=element_text(size=16)) +
		theme(strip.text.x = element_text(size = 14)) 

	#Dual chart
	gridExtra::grid.arrange(Plot1, Plot2)


#Map data
#-----------------------------------------
	#Init projection files
	Projection_LatLong <- "+proj=longlat +zone=10 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
	Projection_Utm <- "+proj=utm +zone=10 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
	#Load spatial data
	fgdb <-    "//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/Spatial/Oregon_Census_Data.gdb"
	Load_Tracts_10_Sp <-  readOGR(dsn=fgdb,layer= "Oregon_Tracts_2010") 
	#Ensure proper projections
	Load_Tracts_10_Sp <- spTransform(Load_Tracts_10_Sp, CRS(Projection_Utm))
	#Rename block group names
	Load_Tracts_10_Sp@data <- mutate(Load_Tracts_10_Sp@data, Tract = GEOID,   Land_Area_Sq_Me = ALAND, Land_Area_Sq_Ft =  ALAND * 10.7639, Land_Area_Sq_Mi =  (ALAND * 10.7639) * 3.587006427915519e-8)
	#Load tracts with urban area prop calculate
	Tract_Urban_Prop_Sp <- assignLoad("//wpdotfill09/R_VMP3_USERS/tdb069/Data/Census/Data/SPR 841/Urban Proportions/Tracts_Urban_Prop_2010.RData")
	#Join
	Load_Tracts_10_Sp@data <- left_join(Load_Tracts_10_Sp@data,Tract_Urban_Prop_Sp@data[,c("Tract","Urban_Area_Prop","Urban_Area")],by = "Tract")
	#Load tract data with index
	Tracts.. <-  file = "//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Data/Social Vulnerability Index/SVI_2014-2018.RData")

	#Load data from file
	#Summary.. <- assignLoad(file = "//wpdotfill09/R_VMP3_USERS/tdb069/Data/OLCC/Data/Processed/OLCC_Block_Groups_2000_2020.RData")
	dat <- Load_Tracts_10_Sp
	#Join summary data
	dat@data <- 	left_join(dat@data,Tracts.., by = "Tract") %>% mutate(Non_Drive_Commute_Prop = Jtw_Transit_Prop + Jtw_Walk_Prop + Jtw_Bike_Prop,
		Ped_KA_Rate = Ped_KA / Population *100000,Ped_KABCO_Rate = Ped_KABCO / Population *100000)
	#Transform
	dat <- spTransform(		dat, CRS(Projection_LatLong))
	dat <- dat[!(is.na(dat@data$Population)),]
	#Create palette
	#rc1 <- colorRampPalette(colors = c("lightblue", "red"), space = "Lab")(4)
	
	mypal <- colorFactor("Spectral", domain = dat@data$Index_Cat, na.color = "grey")

	Map <- leaflet() %>% addTiles() %>%
	  addPolygons(data=dat, smoothFactor = 0.2, fillOpacity = .5,	color = ~mypal(Index_Cat),
				  popup = paste("Tract ", dat@data$Tract,"<br>",
						"Index Category", dat@data$Index_Cat,"<br>",
						"Pedestrian Fatal & Severe Injury Rate", round(dat@data$Ped_KA_Rate,1),"<br>",
						"All Pedestrian Injury Rate", round(dat@data$Ped_KABCO_Rate,1),"<br>",
						"Pedestrian Fatal & Severe Injuries", round(dat@data$Ped_KA,1),"<br>",
						"All Pedestrian Injuries", round(dat@data$Ped_KABCO,1),"<br>",
						"Poverty %", round(dat@data$Poverty_Prop,3)*100,"<br>",
						"BIPOC %", round(dat@data$Non_White_Prop,3)*100,"<br>",
						"Non-Drive Commute %", round(dat@data$Non_Drive_Commute_Prop,3)*100,"<br>",
						"Arterial Vmt Density", round(dat@data$Vmt_All_Arterial_Density*1e6,0),"<br>",
						"High Speed Roads", round(dat@data$Miles_Non_Int_45_Plus*100,0),"<br>",
						"Zero Vehicle Households %", round(dat@data$Vehicle_0_Prop,3)*100,"<br>",
						"Population", dat@data$Population)) %>%
	  addLegend("bottomright",  pal = mypal, values = dat@data$Index_Cat,title = "Concentration Level",    opacity = 1)	
	
	
	#Save file
	saveWidget(Map, file="//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Reports/Dynamic Maps/Tract_Concentration_Maps_2014-2018.html", selfcontained = T)
					      
		
	#Ped injury rate map
	###########################
	dat <- dat[!(is.na(dat@data$Population)),]
	dat <- dat[!(is.infinite(dat@data$Ped_KA_Rate)) ,]
	Breaks. <- c(0,7,quantile(dat@data$Ped_KA_Rate, prob = seq(0, 1, length = 21), type = 5, na.rm=T)[11:21])
	Bin_Labels. <- label_Bins(round(Breaks.,0))
	dat$Ped_KA_Rate_Cat <- cut(dat$Ped_KA_Rate, breaks = Breaks., labels = Bin_Labels.)
	rc1 <- colorRampPalette(colors = c("white","blue", "red"), space = "Lab")(10)
	#mypal <- colorFactor("Spectral", domain = dat@data$Ped_KA_Rate_Cat, na.color = "grey")
	mypal <- colorFactor(rc1, domain = dat@data$Ped_KA_Rate_Cat, na.color = "grey")
	#mypal <- colorNumeric("RdYlBu", domain = 1:12, na.color = "grey")
	#mypal <- colorNumeric(rc1, domain = dat@data$Ped_KA_Rate, na.color =  "grey")
	Map <- leaflet() %>% addTiles() %>%
	  addPolygons(data=dat, smoothFactor = 0.2, fillOpacity = .5,	color = ~mypal(Ped_KA_Rate_Cat),
				  popup = paste("Tract ", dat@data$Tract,"<br>",
						"Index Category", dat@data$Index_Cat,"<br>",
						"Pedestrian Fatal & Severe Injury Rate", round(dat@data$Ped_KA_Rate,1),"<br>",
						"All pedestrian Injury Rate", round(dat@data$Ped_KABCO_Rate,1),"<br>",
						"Poverty %", round(dat@data$Poverty_Prop,3)*100,"<br>",
						"BIPOC %", round(dat@data$Non_White_Prop,3)*100,"<br>",
						"Non-Drive Commute %", round(dat@data$Non_Drive_Commute_Prop,3)*100,"<br>",
						"Arterial Vmt Density", round(dat@data$Vmt_All_Arterial_Density*1e6,0),"<br>",
						"Zero Vehicle Households %", round(dat@data$Vehicle_0_Prop,3)*100,"<br>",
						"Population", dat@data$Population)) %>%
	  addLegend("bottomright",  pal = mypal, values = dat@data$Ped_KA_Rate_Cat,title = "Fatal and Severe Injury Rate",    opacity = 1)	
	
	
	#Save file
	saveWidget(Map, file="//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Reports/Dynamic Maps/Ped_KA_Rate_Cat_2014-2018.html", selfcontained = T)
			
	
	Tracts.. <- assignLoad("//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Data/Social Vulnerability Index/SVI_2014-2018.RData")
		
		
	Quantile_Summary.. <- Tracts.. %>% group_by(Index_Cat) %>% summarize(Ped_KABCO_Median = median(Ped_KABCO),Ped_KA_Median = median(Ped_KA))
	
		
	#Join to tracts
	Tracts.. <- left_join(Tracts.., Quantile_Summary.., by = c("Index_Cat")) 
	Tracts..  <- Tracts.. %>%
		mutate(Ped_KA_Qnt = ifelse(Ped_KA <= Ped_KA_Median, "Low","High"),
		Ped_KABCO_Qnt = ifelse(Ped_KABCO <= Ped_KABCO_Median, "Low","High")) 
	#Sumamrise
	x <- Tracts.. %>% group_by(Index_Cat, Ped_KA_Qnt) %>% summarise(Ped_KA = sum(Ped_KA), Ped_KABCO = sum(Ped_KABCO), Vmt_All_Arterial_Density = mean(Vmt_All_Arterial_Density), Transit_Stops = mean(Mean_Transit_Stops),
		Tracts = length(Tract), Population = sum(Population), Bipoc_Prop = mean(Bipoc_Prop), Poverty_Prop = mean(Poverty_Prop)) %>% mutate(Ped_KA_Rate = (Ped_KA * 100000) / Population,Ped_KABCO_Rate = (Ped_KABCO * 100000) / Population)
		
	x = 
	sum(filter(Tracts.., Index_Cat =="High" & Ped_KA_Qnt =="High")$Ped_KA)	
	sum(Tracts..$Ped_KA)
	sum(filter(Tracts.., Index_Cat =="High" & Ped_KA_Qnt =="High")$Ped_KA)	/
	sum(Tracts..$Ped_KA)
	
	sum(filter(Tracts.., Index_Cat =="High" & Ped_KABCO_Qnt =="High")$Ped_KABCO)	/
	sum(Tracts..$Ped_KABCO)
	
	nrow(filter(Tracts.., Index_Cat =="High" & Ped_KABCO_Qnt =="High"))
	
	
	#Ped injury rate map
	###########################
	dat <- Load_Tracts_10_Sp
	#Join summary data
	dat@data <- 	left_join(dat@data,Tracts.., by = "Tract") %>% mutate(Non_Drive_Commute_Prop = Jtw_Transit_Prop + Jtw_Walk_Prop + Jtw_Bike_Prop, Ped_KA
		Ped_KA_Qnt_Index = paste(Index_Cat, Ped_KA_Qnt, sep="-"))
	dat@data$Ped_KA_Qnt_Index <- factor(dat@data$Ped_KA_Qnt_Index, levels = rev(c("High-High","High-Low","Moderate-High","Moderate-Low","Low-High","Low-Low","Lowest-High","Lowest-Low")))


	#Transform
	dat <- spTransform(		dat, CRS(Projection_LatLong))
	dat <- dat[!(is.na(dat@data$Population)),]
#	dat <- dat[!(is.infinite(dat@data$Ped_KA_Rate)) ,]
	Breaks. <- c(0,7,quantile(dat@data$Ped_KA_Rate, prob = seq(0, 1, length = 21), type = 5, na.rm=T)[11:21])
	Bin_Labels. <- label_Bins(round(Breaks.,0))
	#dat$Ped_KA_Rate_Cat <- cut(dat$Ped_KA_Rate, breaks = Breaks., labels = Bin_Labels.)
	rc1 <- colorRampPalette(colors = c("white","blue", "red"), space = "Lab")(10)
	#mypal <- colorFactor("Spectral", domain = dat@data$Ped_KA_Rate_Cat, na.color = "grey")
	mypal <- colorFactor(rc1, domain = dat@data$Ped_KA_Qnt_Index, na.color = "grey")
	#mypal <- colorNumeric("RdYlBu", domain = 1:12, na.color = "grey")
	#mypal <- colorNumeric(rc1, domain = dat@data$Ped_KA_Rate, na.color =  "grey")
	Map <- leaflet() %>% addTiles() %>%
	  addPolygons(data=dat, smoothFactor = 0.2, fillOpacity = .5,	color = ~mypal(Ped_KA_Qnt_Index),
				  popup = paste("Tract ", dat@data$Tract,"<br>",
						"Index Category", dat@data$Index_Cat,"<br>",
						"Median Category", dat@data$PEd_KA_Qnt,"<br>",
						"Pedestrian Fatal & Severe Injury", round(dat@data$Ped_KA),"<br>",
						"Pedestrian Injury", round(dat@data$Ped_KABCO),"<br>",
						#"All pedestrian Injury Rate", round(dat@data$Ped_KABCO_Rate,1),"<br>",
						"Poverty %", round(dat@data$Poverty_Prop,3)*100,"<br>",
						"BIPOC %", round(dat@data$Bipoc_Prop,3)*100,"<br>",
						"Non-Drive Commute %", round(dat@data$Non_Drive_Commute_Prop,3)*100,"<br>",
						"Arterial Vmt Density", round(dat@data$Vmt_All_Arterial_Density*1e6,0),"<br>",
						"Zero Vehicle Households %", round(dat@data$Vehicle_0_Prop,3)*100,"<br>",
						"Population", dat@data$Population)) %>%
	  addLegend("bottomright",  pal = mypal, values = dat@data$Ped_KA_Qnt_Index,title = "Concentration Index",    opacity = 1)	
	
	
	#Save file
	saveWidget(Map, file="//wpdotfill09/R_VMP3_USERS/tdb069/Crash Analysis/SPR 841/Reports/Dynamic Maps/Index_Cat_With_Qnt_2014-2018.html", selfcontained = T)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	