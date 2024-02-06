


# Packages ----

library(ggplot2)
library(tidyverse)
library(webshot)
library(cowplot)
library(gt)
library(magick)


# Directory 

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Tennis/4. Matchs_Length")




# Data ----

for(i in 0:25){
	
download.file(url      = paste("https://github.com/JeffSackmann/tennis_atp/raw/master/atp_matches_",1998 + i,".csv",
															 sep = ""),
							destfile = paste("ATP_Matches_",1998 + i,".csv", sep = ""))
	
	
assign(paste("ATP_", 1998 + i, sep = ""), 
			 read.csv(paste("ATP_Matches_",1998 + i,".csv", sep = ""))
			 )
	
}



# Data processing ----


# Variables selection

for(i in 0:25){
	
assign(paste("ATP_", 1998 + i, sep = ""),
			 get(paste("ATP_", 1998 + i, sep = "")) %>% select(tourney_name, surface, tourney_date, winner_name,
			 																									 loser_name, score, best_of, minutes))
	
}



# For this viz, I need only the matches with the best of 3 sets for a better comparison

for(i in 0:25){
	
assign(paste("ATP_", 1998 + i, sep = ""),
			 get(paste("ATP_", 1998 + i, sep = "")) %>% filter(best_of == 3))
	
}


# Bind rows to work on a single dataframe (I could have done it before...)

ATP_df <- bind_rows(mget(paste("ATP_",1998:2023, sep="")))
rm(list = paste("ATP_",1998:2023,sep = ""))



 
# Remove matches which did not end in a "classic" way

ATP_df = ATP_df[-c(grep("RET", ATP_df$score),which(ATP_df$minutes == 0),
									 grep("DEF", ATP_df$score),grep("Def", ATP_df$score)
									 ), ]


# Remove matches that do not have time values

ATP_df = ATP_df[-which(is.na(ATP_df$minutes)), ]


# Remove matches which are not classic tournaments (e.g. netx gen because the sets format is different)

ATP_df = ATP_df[-grep("NextGen Finals", ATP_df$tourney_name), ]


# There seems to be inconsistent time values. To try to remedy this I will 
# break down the score and add up the games of each set and therefore the match. Next,
# I will establish a threshold for average time per game and surface that can potentially highlight outliers better.
# Example : Arnaud Clement vs Max Mirnyi at Stockholm in 220 minutes for the score 6-3 6-2...
# It seems impossible for 17 games in a match.

ATP_df = separate(ATP_df, col = score, into=c('Set_1', 'Set_2','Set_3'), sep = ' ')
ATP_df$Set_3[which(is.na(ATP_df$Set_3))] = ""


# Remove score of tie breaks

for(i in 1:nrow(ATP_df)){
	
if(nchar(ATP_df$Set_1[i]) > 3){ATP_df$Set_1[i] = substr(ATP_df$Set_1[i],1,3)}
if(nchar(ATP_df$Set_2[i]) > 3){ATP_df$Set_2[i] = substr(ATP_df$Set_2[i],1,3)}
if(nchar(ATP_df$Set_3[i]) > 3){ATP_df$Set_3[i] = substr(ATP_df$Set_3[i],1,3)}

}




# It's a personal project not at all important so I'm going to check manually some matches...
# I will manually replace the values with those found via the FlashScore website


ATP_df[ATP_df$tourney_name == "Miami Masters" & 
			 ATP_df$winner_name == "Frances Tiafoe" & 
			 ATP_df$loser_name == "David Goffin" & 
			 ATP_df$tourney_date == "20190318",
			 "minutes"] = 130

ATP_df[ATP_df$tourney_name == "Shenzhen" & 
			 ATP_df$winner_name == "Denis Shapovalov" & 
			 ATP_df$loser_name == "Ilya Ivashka" & 
			 ATP_df$tourney_date == "20180924",
			 "minutes"] = 127

ATP_df[ATP_df$tourney_name == "Munich" & 
			 ATP_df$winner_name == "Hyeon Chung" & 
			 ATP_df$loser_name == "Martin Klizan" & 
			 ATP_df$tourney_date == "20170501",  
			 "minutes"] = 137

ATP_df[ATP_df$tourney_name == "Cincinnati Masters" & 
			 ATP_df$winner_name == "Steve Johnson" & 
			 ATP_df$loser_name == "Federico Delbonis" & 
			 ATP_df$tourney_date == "20160815",
			 "minutes"] = 116
	
ATP_df[ATP_df$tourney_name == "Sydney" & 
			 ATP_df$winner_name == "Gilles Muller" & 
			 ATP_df$loser_name == "Jeremy Chardy" & 
			 ATP_df$tourney_date == "20160111",  
			 "minutes"] = 118

ATP_df[ATP_df$tourney_name == "Beijing Olympics" & 
			 ATP_df$winner_name == "Michael Llodra" & 
			 ATP_df$loser_name == "Radek Stepanek" & 
			 ATP_df$tourney_date == "20080811",  
			 "Set_3"] = "11-9"

ATP_df[ATP_df$tourney_name == "Beijing Olympics" & 
			 ATP_df$winner_name == "Fernando Gonzalez" & 
			 ATP_df$loser_name == "James Blake" & 
			 ATP_df$tourney_date == "20080811",  
			 "Set_3"] = "11-9"

ATP_df[ATP_df$tourney_name == "London Olympics" & 
			 ATP_df$winner_name == "Jo-Wilfried Tsonga" & 
			 ATP_df$loser_name == "Milos Raonic" & 
			 ATP_df$tourney_date == "20120725",  
			 "Set_3"] = "25-23"

ATP_df[ATP_df$tourney_name == "London Olympics" & 
			 ATP_df$winner_name == "Roger Federer" & 
			 ATP_df$loser_name == "Juan Martin del Potro" & 
			 ATP_df$tourney_date == "20120725",  
			 "Set_3"] = "19-17"



# Create the sum of games by set

ATP_df$Number_Games_Set_1 = as.numeric(substr(ATP_df$Set_1,1,1)) + as.numeric(substr(ATP_df$Set_1,3,3))
ATP_df$Number_Games_Set_2 = as.numeric(substr(ATP_df$Set_2,1,1)) + as.numeric(substr(ATP_df$Set_2,3,3))
ATP_df$Number_Games_Set_3 = as.numeric(substr(ATP_df$Set_3,1,1)) + as.numeric(substr(ATP_df$Set_3,3,3))
ATP_df$Number_Games_Set_3[which(nchar(ATP_df$Set_3) == 5)] = as.numeric(substr(ATP_df$Set_3[which(nchar(ATP_df$Set_3) == 5)],1,2)) + as.numeric(substr(ATP_df$Set_3[which(nchar(ATP_df$Set_3) == 5)],4,5))
ATP_df$Number_Games_Set_3[which(nchar(ATP_df$Set_3) == 4)] = as.numeric(substr(ATP_df$Set_3[which(nchar(ATP_df$Set_3) == 4)],1,2)) + as.numeric(substr(ATP_df$Set_3[which(nchar(ATP_df$Set_3) == 4)],4,4))
ATP_df$Number_Games_Set_3[which(is.na(ATP_df$Number_Games_Set_3))] = 0


# Sum all sets

ATP_df$Number_Games_Match = as.numeric(ATP_df$Number_Games_Set_1) + as.numeric(ATP_df$Number_Games_Set_2) + as.numeric(ATP_df$Number_Games_Set_3)
	
	

# The average time played per game is approximately 4.25 minutes, excluding the 2 values that are far too extreme to be true (987 and 1146)
# mean(ATP_df$minutes[which(ATP_df$minutes < 290)]) / mean(ATP_df$Number_Games_Match[which(ATP_df$minutes < 290)])
# but it's not super precise because the average speed per game also depends on the number of games played and the surface ! 
# A 6-0 6-0 match will have a lower average than a close match in 3 sets... so I decide to do 3 confidence intervals 
# to find the outliers who perhaps have a false value.
# The number of games values in the match vary between 10 and 39, so I will make the following classes : 
# [10;16] , [17,26] , [27;43] and [44;66] by surface


# Be careful though, the use of this type of confidence interval is for the following data
# a normal distribution, which is not quite the case here
# So my approach is not very rigorous but we have to do something to try to find values probably wrong

# ggdensity(ATP_df_final$minutes, fill = "lightgray")
# ggqqplot(ATP_df_final$minutes)
# ATP_df_final[sample(x = 1:nrow(ATP_df_final),size = 5000,replace = F),] %>% group_by(surface,group_games) %>% shapiro_test(minutes)


# Hard court and Carpet

Hard_Carpet_df_10_16_games = ATP_df %>% filter(between(Number_Games_Match,10,16), surface %in% c("Carpet","Hard"))
Hard_Carpet_df_17_26_games = ATP_df %>% filter(between(Number_Games_Match,17,26), surface %in% c("Carpet","Hard"))
Hard_Carpet_df_27_43_games = ATP_df %>% filter(between(Number_Games_Match,27,43), surface %in% c("Carpet","Hard"))
Hard_Carpet_df_44_66_games = ATP_df %>% filter(between(Number_Games_Match,44,66), surface %in% c("Carpet","Hard"))

Hard_Carpet_df_10_16_games$ratio_time_number_games = Hard_Carpet_df_10_16_games$minutes / Hard_Carpet_df_10_16_games$Number_Games_Match
Hard_Carpet_df_17_26_games$ratio_time_number_games = Hard_Carpet_df_17_26_games$minutes / Hard_Carpet_df_17_26_games$Number_Games_Match
Hard_Carpet_df_27_43_games$ratio_time_number_games = Hard_Carpet_df_27_43_games$minutes / Hard_Carpet_df_27_43_games$Number_Games_Match
Hard_Carpet_df_44_66_games$ratio_time_number_games = Hard_Carpet_df_44_66_games$minutes / Hard_Carpet_df_44_66_games$Number_Games_Match


Hard_Carpet_df_10_16_games$Threshold_Time_4sd_sup_10_16_games[Hard_Carpet_df_10_16_games$minutes < 300] = Hard_Carpet_df_10_16_games$Number_Games_Match[Hard_Carpet_df_10_16_games$minutes < 300] * mean(Hard_Carpet_df_10_16_games$ratio_time_number_games[Hard_Carpet_df_10_16_games$minutes < 300]) +  4*sd(Hard_Carpet_df_10_16_games$minutes[Hard_Carpet_df_10_16_games$minutes < 300])
Hard_Carpet_df_10_16_games$Threshold_Time_4sd_inf_10_16_games[Hard_Carpet_df_10_16_games$minutes < 300] = Hard_Carpet_df_10_16_games$Number_Games_Match[Hard_Carpet_df_10_16_games$minutes < 300] * mean(Hard_Carpet_df_10_16_games$ratio_time_number_games[Hard_Carpet_df_10_16_games$minutes < 300]) -  4*sd(Hard_Carpet_df_10_16_games$minutes[Hard_Carpet_df_10_16_games$minutes < 300])

Hard_Carpet_df_17_26_games$Threshold_Time_4sd_sup_17_26_games[Hard_Carpet_df_17_26_games$minutes < 300] = Hard_Carpet_df_17_26_games$Number_Games_Match[Hard_Carpet_df_17_26_games$minutes < 300] * mean(Hard_Carpet_df_17_26_games$ratio_time_number_games[Hard_Carpet_df_17_26_games$minutes < 300]) +  4*sd(Hard_Carpet_df_17_26_games$minutes[Hard_Carpet_df_17_26_games$minutes < 300])
Hard_Carpet_df_17_26_games$Threshold_Time_4sd_inf_17_26_games[Hard_Carpet_df_17_26_games$minutes < 300] = Hard_Carpet_df_17_26_games$Number_Games_Match[Hard_Carpet_df_17_26_games$minutes < 300] * mean(Hard_Carpet_df_17_26_games$ratio_time_number_games[Hard_Carpet_df_17_26_games$minutes < 300]) -  4*sd(Hard_Carpet_df_17_26_games$minutes[Hard_Carpet_df_17_26_games$minutes < 300])

Hard_Carpet_df_27_43_games$Threshold_Time_4sd_sup_27_43_games[Hard_Carpet_df_27_43_games$minutes < 300] = Hard_Carpet_df_27_43_games$Number_Games_Match[Hard_Carpet_df_27_43_games$minutes < 300] * mean(Hard_Carpet_df_27_43_games$ratio_time_number_games[Hard_Carpet_df_27_43_games$minutes < 300]) +  4*sd(Hard_Carpet_df_27_43_games$minutes[Hard_Carpet_df_27_43_games$minutes < 300])
Hard_Carpet_df_27_43_games$Threshold_Time_4sd_inf_27_43_games[Hard_Carpet_df_27_43_games$minutes < 300] = Hard_Carpet_df_27_43_games$Number_Games_Match[Hard_Carpet_df_27_43_games$minutes < 300] * mean(Hard_Carpet_df_27_43_games$ratio_time_number_games[Hard_Carpet_df_27_43_games$minutes < 300]) -  4*sd(Hard_Carpet_df_27_43_games$minutes[Hard_Carpet_df_27_43_games$minutes < 300])

Hard_Carpet_df_44_66_games$Threshold_Time_4sd_sup_44_66_games[Hard_Carpet_df_44_66_games$minutes < 300] = Hard_Carpet_df_44_66_games$Number_Games_Match[Hard_Carpet_df_44_66_games$minutes < 300] * mean(Hard_Carpet_df_44_66_games$ratio_time_number_games[Hard_Carpet_df_44_66_games$minutes < 300]) +  4*sd(Hard_Carpet_df_44_66_games$minutes[Hard_Carpet_df_44_66_games$minutes < 300])
Hard_Carpet_df_44_66_games$Threshold_Time_4sd_inf_44_66_games[Hard_Carpet_df_44_66_games$minutes < 300] = Hard_Carpet_df_44_66_games$Number_Games_Match[Hard_Carpet_df_44_66_games$minutes < 300] * mean(Hard_Carpet_df_44_66_games$ratio_time_number_games[Hard_Carpet_df_44_66_games$minutes < 300]) -  4*sd(Hard_Carpet_df_44_66_games$minutes[Hard_Carpet_df_44_66_games$minutes < 300])



Hard_Carpet_df_10_16_games$Bool_Threshold_Time_4sd = ifelse(between(Hard_Carpet_df_10_16_games$minutes,
																														Hard_Carpet_df_10_16_games$Threshold_Time_4sd_inf_10_16_games,
																														Hard_Carpet_df_10_16_games$Threshold_Time_4sd_sup_10_16_games),
																										T,
																										F)

Hard_Carpet_df_17_26_games$Bool_Threshold_Time_4sd = ifelse(between(Hard_Carpet_df_17_26_games$minutes,
																														Hard_Carpet_df_17_26_games$Threshold_Time_4sd_inf_17_26_games,
																														Hard_Carpet_df_17_26_games$Threshold_Time_4sd_sup_17_26_games),
																										T,
																										F)

Hard_Carpet_df_27_43_games$Bool_Threshold_Time_4sd = ifelse(between(Hard_Carpet_df_27_43_games$minutes,
																														Hard_Carpet_df_27_43_games$Threshold_Time_4sd_inf_27_43_games,
																														Hard_Carpet_df_27_43_games$Threshold_Time_4sd_sup_27_43_games),
																										T,
																										F)

# Hard_Carpet_df_44_66_games$Bool_Threshold_Time_4sd = ifelse(between(Hard_Carpet_df_44_66_games$minutes,
# 																														Hard_Carpet_df_44_66_games$Threshold_Time_4sd_inf_44_66_games,
# 																														Hard_Carpet_df_44_66_games$Threshold_Time_4sd_sup_44_66_games),
# 																										T,
# 																										F)

colnames(Hard_Carpet_df_17_26_games) = colnames(Hard_Carpet_df_10_16_games)
colnames(Hard_Carpet_df_27_43_games) = colnames(Hard_Carpet_df_10_16_games)
#colnames(Hard_Carpet_df_44_66_games) = colnames(Hard_Carpet_df_10_16_games)


Hard_Carpet_df = bind_rows(Hard_Carpet_df_10_16_games,
									         Hard_Carpet_df_17_26_games,
									         Hard_Carpet_df_27_43_games,
													 Hard_Carpet_df_44_66_games)

rm(Hard_Carpet_df_10_16_games,Hard_Carpet_df_17_26_games,Hard_Carpet_df_27_43_games,Hard_Carpet_df_44_66_games)



# Clay court

Clay_df_10_16_games = ATP_df %>% filter(between(Number_Games_Match,10,16), surface %in% c("Clay"))
Clay_df_17_26_games = ATP_df %>% filter(between(Number_Games_Match,17,26), surface %in% c("Clay"))
Clay_df_27_43_games = ATP_df %>% filter(between(Number_Games_Match,27,43), surface %in% c("Clay"))
Clay_df_44_66_games = ATP_df %>% filter(between(Number_Games_Match,44,66), surface %in% c("Clay"))

Clay_df_10_16_games$ratio_time_number_games = Clay_df_10_16_games$minutes / Clay_df_10_16_games$Number_Games_Match
Clay_df_17_26_games$ratio_time_number_games = Clay_df_17_26_games$minutes / Clay_df_17_26_games$Number_Games_Match
Clay_df_27_43_games$ratio_time_number_games = Clay_df_27_43_games$minutes / Clay_df_27_43_games$Number_Games_Match
Clay_df_44_66_games$ratio_time_number_games = Clay_df_44_66_games$minutes / Clay_df_44_66_games$Number_Games_Match


Clay_df_10_16_games$Threshold_Time_4sd_sup_10_16_games[Clay_df_10_16_games$minutes < 300] = Clay_df_10_16_games$Number_Games_Match[Clay_df_10_16_games$minutes < 300] * mean(Clay_df_10_16_games$ratio_time_number_games[Clay_df_10_16_games$minutes < 300]) +  4*sd(Clay_df_10_16_games$minutes[Clay_df_10_16_games$minutes < 300])
Clay_df_10_16_games$Threshold_Time_4sd_inf_10_16_games[Clay_df_10_16_games$minutes < 300] = Clay_df_10_16_games$Number_Games_Match[Clay_df_10_16_games$minutes < 300] * mean(Clay_df_10_16_games$ratio_time_number_games[Clay_df_10_16_games$minutes < 300]) -  4*sd(Clay_df_10_16_games$minutes[Clay_df_10_16_games$minutes < 300])

Clay_df_17_26_games$Threshold_Time_4sd_sup_17_26_games[Clay_df_17_26_games$minutes < 300] = Clay_df_17_26_games$Number_Games_Match[Clay_df_17_26_games$minutes < 300] * mean(Clay_df_17_26_games$ratio_time_number_games[Clay_df_17_26_games$minutes < 300]) +  4*sd(Clay_df_17_26_games$minutes[Clay_df_17_26_games$minutes < 300])
Clay_df_17_26_games$Threshold_Time_4sd_inf_17_26_games[Clay_df_17_26_games$minutes < 300] = Clay_df_17_26_games$Number_Games_Match[Clay_df_17_26_games$minutes < 300] * mean(Clay_df_17_26_games$ratio_time_number_games[Clay_df_17_26_games$minutes < 300]) -  4*sd(Clay_df_17_26_games$minutes[Clay_df_17_26_games$minutes < 300])

Clay_df_27_43_games$Threshold_Time_4sd_sup_27_43_games[Clay_df_27_43_games$minutes < 300] = Clay_df_27_43_games$Number_Games_Match[Clay_df_27_43_games$minutes < 300] * mean(Clay_df_27_43_games$ratio_time_number_games[Clay_df_27_43_games$minutes < 300]) +  4*sd(Clay_df_27_43_games$minutes[Clay_df_27_43_games$minutes < 300])
Clay_df_27_43_games$Threshold_Time_4sd_inf_27_43_games[Clay_df_27_43_games$minutes < 300] = Clay_df_27_43_games$Number_Games_Match[Clay_df_27_43_games$minutes < 300] * mean(Clay_df_27_43_games$ratio_time_number_games[Clay_df_27_43_games$minutes < 300]) -  4*sd(Clay_df_27_43_games$minutes[Clay_df_27_43_games$minutes < 300])

Clay_df_44_66_games$Threshold_Time_4sd_sup_44_66_games[Clay_df_44_66_games$minutes < 300] = Clay_df_44_66_games$Number_Games_Match[Clay_df_44_66_games$minutes < 300] * mean(Clay_df_44_66_games$ratio_time_number_games[Clay_df_44_66_games$minutes < 300]) +  4*sd(Clay_df_44_66_games$minutes[Clay_df_44_66_games$minutes < 300])
Clay_df_44_66_games$Threshold_Time_4sd_inf_44_66_games[Clay_df_44_66_games$minutes < 300] = Clay_df_44_66_games$Number_Games_Match[Clay_df_44_66_games$minutes < 300] * mean(Clay_df_44_66_games$ratio_time_number_games[Clay_df_44_66_games$minutes < 300]) -  4*sd(Clay_df_44_66_games$minutes[Clay_df_44_66_games$minutes < 300])



Clay_df_10_16_games$Bool_Threshold_Time_4sd = ifelse(between(Clay_df_10_16_games$minutes,
																														Clay_df_10_16_games$Threshold_Time_4sd_inf_10_16_games,
																														Clay_df_10_16_games$Threshold_Time_4sd_sup_10_16_games),
																										T,
																										F)

Clay_df_17_26_games$Bool_Threshold_Time_4sd = ifelse(between(Clay_df_17_26_games$minutes,
																														Clay_df_17_26_games$Threshold_Time_4sd_inf_17_26_games,
																														Clay_df_17_26_games$Threshold_Time_4sd_sup_17_26_games),
																										T,
																										F)

Clay_df_27_43_games$Bool_Threshold_Time_4sd = ifelse(between(Clay_df_27_43_games$minutes,
																														Clay_df_27_43_games$Threshold_Time_4sd_inf_27_43_games,
																														Clay_df_27_43_games$Threshold_Time_4sd_sup_27_43_games),
																										T,
																										F)

# Clay_df_44_66_games$Bool_Threshold_Time_4sd = ifelse(between(Clay_df_44_66_games$minutes,
# 																														 Clay_df_44_66_games$Threshold_Time_4sd_inf_44_66_games,
# 																														 Clay_df_44_66_games$Threshold_Time_4sd_sup_44_66_games),
# 																										T,
# 																										F)

colnames(Clay_df_17_26_games) = colnames(Clay_df_10_16_games)
colnames(Clay_df_27_43_games) = colnames(Clay_df_10_16_games)
# colnames(Clay_df_44_66_games) = colnames(Clay_df_10_16_games)


Clay_df = bind_rows(Clay_df_10_16_games,
									  Clay_df_17_26_games,
									  Clay_df_27_43_games,
										Clay_df_44_66_games)

rm(Clay_df_10_16_games,Clay_df_17_26_games,Clay_df_27_43_games,Clay_df_44_66_games)




# Grass court


Grass_df_10_16_games = ATP_df %>% filter(between(Number_Games_Match,10,16), surface %in% c("Grass"))
Grass_df_17_26_games = ATP_df %>% filter(between(Number_Games_Match,17,26), surface %in% c("Grass"))
Grass_df_27_43_games = ATP_df %>% filter(between(Number_Games_Match,27,43), surface %in% c("Grass"))
Grass_df_44_66_games = ATP_df %>% filter(between(Number_Games_Match,44,66), surface %in% c("Grass"))

Grass_df_10_16_games$ratio_time_number_games = Grass_df_10_16_games$minutes / Grass_df_10_16_games$Number_Games_Match
Grass_df_17_26_games$ratio_time_number_games = Grass_df_17_26_games$minutes / Grass_df_17_26_games$Number_Games_Match
Grass_df_27_43_games$ratio_time_number_games = Grass_df_27_43_games$minutes / Grass_df_27_43_games$Number_Games_Match
Grass_df_44_66_games$ratio_time_number_games = Grass_df_44_66_games$minutes / Grass_df_44_66_games$Number_Games_Match


Grass_df_10_16_games$Threshold_Time_4sd_sup_10_16_games[Grass_df_10_16_games$minutes < 300] = Grass_df_10_16_games$Number_Games_Match[Grass_df_10_16_games$minutes < 300] * mean(Grass_df_10_16_games$ratio_time_number_games[Grass_df_10_16_games$minutes < 300]) +  4*sd(Grass_df_10_16_games$minutes[Grass_df_10_16_games$minutes < 300])
Grass_df_10_16_games$Threshold_Time_4sd_inf_10_16_games[Grass_df_10_16_games$minutes < 300] = Grass_df_10_16_games$Number_Games_Match[Grass_df_10_16_games$minutes < 300] * mean(Grass_df_10_16_games$ratio_time_number_games[Grass_df_10_16_games$minutes < 300]) -  4*sd(Grass_df_10_16_games$minutes[Grass_df_10_16_games$minutes < 300])

Grass_df_17_26_games$Threshold_Time_4sd_sup_17_26_games[Grass_df_17_26_games$minutes < 300] = Grass_df_17_26_games$Number_Games_Match[Grass_df_17_26_games$minutes < 300] * mean(Grass_df_17_26_games$ratio_time_number_games[Grass_df_17_26_games$minutes < 300]) +  4*sd(Grass_df_17_26_games$minutes[Grass_df_17_26_games$minutes < 300])
Grass_df_17_26_games$Threshold_Time_4sd_inf_17_26_games[Grass_df_17_26_games$minutes < 300] = Grass_df_17_26_games$Number_Games_Match[Grass_df_17_26_games$minutes < 300] * mean(Grass_df_17_26_games$ratio_time_number_games[Grass_df_17_26_games$minutes < 300]) -  4*sd(Grass_df_17_26_games$minutes[Grass_df_17_26_games$minutes < 300])

Grass_df_27_43_games$Threshold_Time_4sd_sup_27_43_games[Grass_df_27_43_games$minutes < 300] = Grass_df_27_43_games$Number_Games_Match[Grass_df_27_43_games$minutes < 300] * mean(Grass_df_27_43_games$ratio_time_number_games[Grass_df_27_43_games$minutes < 300]) +  4*sd(Grass_df_27_43_games$minutes[Grass_df_27_43_games$minutes < 300])
Grass_df_27_43_games$Threshold_Time_4sd_inf_27_43_games[Grass_df_27_43_games$minutes < 300] = Grass_df_27_43_games$Number_Games_Match[Grass_df_27_43_games$minutes < 300] * mean(Grass_df_27_43_games$ratio_time_number_games[Grass_df_27_43_games$minutes < 300]) -  4*sd(Grass_df_27_43_games$minutes[Grass_df_27_43_games$minutes < 300])

Grass_df_44_66_games$Threshold_Time_4sd_sup_44_66_games[Grass_df_44_66_games$minutes < 300] = Grass_df_44_66_games$Number_Games_Match[Grass_df_44_66_games$minutes < 300] * mean(Grass_df_44_66_games$ratio_time_number_games[Grass_df_44_66_games$minutes < 300]) +  4*sd(Grass_df_44_66_games$minutes[Grass_df_44_66_games$minutes < 300])
Grass_df_44_66_games$Threshold_Time_4sd_inf_44_66_games[Grass_df_44_66_games$minutes < 300] = Grass_df_44_66_games$Number_Games_Match[Grass_df_44_66_games$minutes < 300] * mean(Grass_df_44_66_games$ratio_time_number_games[Grass_df_44_66_games$minutes < 300]) -  4*sd(Grass_df_44_66_games$minutes[Grass_df_44_66_games$minutes < 300])


Grass_df_10_16_games$Bool_Threshold_Time_4sd = ifelse(between(Grass_df_10_16_games$minutes,
																														Grass_df_10_16_games$Threshold_Time_4sd_inf_10_16_games,
																														Grass_df_10_16_games$Threshold_Time_4sd_sup_10_16_games),
																										T,
																										F)

Grass_df_17_26_games$Bool_Threshold_Time_4sd = ifelse(between(Grass_df_17_26_games$minutes,
																														Grass_df_17_26_games$Threshold_Time_4sd_inf_17_26_games,
																														Grass_df_17_26_games$Threshold_Time_4sd_sup_17_26_games),
																										T,
																										F)

Grass_df_27_43_games$Bool_Threshold_Time_4sd = ifelse(between(Grass_df_27_43_games$minutes,
																														Grass_df_27_43_games$Threshold_Time_4sd_inf_27_43_games,
																														Grass_df_27_43_games$Threshold_Time_4sd_sup_27_43_games),
																										T,
																										F)

Grass_df_44_66_games$Bool_Threshold_Time_4sd = ifelse(between(Grass_df_44_66_games$minutes,
																														Grass_df_44_66_games$Threshold_Time_4sd_inf_44_66_games,
																														Grass_df_44_66_games$Threshold_Time_4sd_sup_44_66_games),
																										T,
																										F)

colnames(Grass_df_17_26_games) = colnames(Grass_df_10_16_games)
colnames(Grass_df_27_43_games) = colnames(Grass_df_10_16_games)
colnames(Grass_df_44_66_games) = colnames(Grass_df_10_16_games)


Grass_df = bind_rows(Grass_df_10_16_games,
									   Grass_df_17_26_games,
									   Grass_df_27_43_games,
										 Grass_df_44_66_games)

rm(Grass_df_10_16_games,Grass_df_17_26_games,Grass_df_27_43_games,Grass_df_44_66_games)



# Bind each dataframe

ATP_df_final = bind_rows(Grass_df,Clay_df,Hard_Carpet_df)
colnames(ATP_df_final)[16:17] = c("Threshold_Time_4sd_sup","Threshold_Time_4sd_inf")



	




ATP_df_final$Bool_Threshold_Time_4sd = ifelse(between(ATP_df_final$minutes,
																											ATP_df_final$Threshold_Time_4sd_inf,
																											ATP_df_final$Threshold_Time_4sd_sup),
																										T,
																										F)


ATP_df_final$hours = round(ATP_df_final$minutes / 60, 2)

# ATP_df_final = ATP_df_final %>% mutate(group_games = ifelse(between(Number_Games_Match,10,16),'10_16',
# 																														ifelse(between(Number_Games_Match,17,26),'17_26',	
# 																																	 ifelse(between(Number_Games_Match,27,43),'27_43','44_66'
# 																																	 			 )
# 																																	 
# 																																	 )
# 																														)
# 																			 )



# Filter on TRUE values

# Probably matches with correct values that are going to be removed but I can't let
# those that are extreme and false so too bad

ATP_df_final = ATP_df_final %>% filter(Bool_Threshold_Time_4sd == TRUE)


# Unfortunately, I notice that there are really a lot of false values that still remained. 
# Particularly on matches in 3 sets with values like 56 minutes for 27 games played. 
# If each game was blank this gives 108 points played, therefore 2 per minute (which is already not obvious) 
# in this case PERFECT which is impossible.
# I think it's reasonable to remove all 3-set matches that took place in less than 85 minutes.

ATP_df_final = ATP_df_final[-which(ATP_df_final$Set_3 != "" & ATP_df_final$minutes < 85),]



# Season variable

ATP_df_final$Season = substr(ATP_df_final$tourney_date,1,4)  
ATP_df_final$Season = as.numeric(ATP_df_final$Season)





# Plot ----

df_plot = ATP_df_final
df_plot = df_plot %>% filter(Season >= 2005)

# Trick to have more space between season lines

df_plot$Season[df_plot$Season > 2009] = df_plot$Season[df_plot$Season > 2009]  + as.numeric(substr(df_plot$Season[df_plot$Season > 2009],3,4)) 
df_plot$Season[df_plot$Season <= 2009] = df_plot$Season[df_plot$Season <= 2009]  + as.numeric(substr(df_plot$Season[df_plot$Season <= 2009],4,4))
df_plot$Season = df_plot$Season   + as.numeric(substr(df_plot$Season,3,4))




# 1st plot

first_plot = ggplot(df_plot, aes(minutes,Season)) + 
	geom_point(data  = df_plot %>% filter(Set_3 == "", (!df_plot$winner_name %in% c("Andy Murray")) & (!df_plot$loser_name %in% c("Andy Murray"))),
						 fill  = "#3D584D",
						 color = "#5D8473",
						 pch   = 21,
						 alpha = 0.45,
						 size  = 1.85) +
	geom_point(data  = df_plot %>% filter(Set_3 == "",df_plot$winner_name == "Andy Murray" | df_plot$loser_name == "Andy Murray"),
						 aes(x = minutes, y = Season - 1),
						 fill  = "#B22D0D", 
						 color = "#E83B11", 
						 pch   = 21,
						 alpha = 0.65, 
						 size  = 2.75) +
	theme_minimal() + 
	labs(x     = "\n \n Minutes",
			 y     = "",
			 title = "\n Matches in 2 sets (2 winning sets) \n") + 
	scale_x_continuous(breaks = seq(20,180,20),
										 labels = as.character(seq(20,180,20)),
										 limits = c(20,185)
										 ) + 
	scale_y_continuous(breaks = sort(unique(df_plot$Season)),
										 labels = as.character(seq(2005,2023,1)),
										 limits = c(2019,2092)
										 ) + 
	theme(panel.grid.major.y = element_blank(),
				panel.grid.major.x = element_line(colour    = "#B8C1BE", 
																					linetype  = 'dotted',
																					linewidth = 1),
				panel.grid.minor   = element_blank(),
				plot.background    = element_rect(fill  = "white", 
																					color = "white"), 
				plot.margin        = margin(10,25,20,10),
				plot.title         =  element_text(hjust = 0.5),
				axis.text.y        = element_text(margin = margin(0,-10,0,0), 
																					color  = "#27312E",
																					face   = "bold"),
				axis.text.x        = element_text(color = "#27312E",
																					face  = "bold"),
				axis.title.x       = element_text(color = "#583054", 
																					face  = "bold")
				)
 

# 2nd plot

second_plot = ggplot(df_plot, aes(minutes,Season)) + 
	geom_point(data  = df_plot %>% filter(Set_3 != "", (!df_plot$winner_name %in% c("Andy Murray")) & (!df_plot$loser_name %in% c("Andy Murray"))),
						 fill  = "#3D584D",
						 color = "#5D8473",
						 pch   = 21,
						 alpha = 0.45,
						 size  = 1.85) +
	geom_point(data  = df_plot %>% filter(Set_3 != "",df_plot$winner_name == "Andy Murray" | df_plot$loser_name == "Andy Murray"),
						 aes(x = minutes, y = Season - 1),
						 fill  = "#B22D0D", 
						 color = "#E83B11", 
						 pch   = 21,
						 alpha = 0.65, 
						 size  = 2.75) +
	theme_minimal() + 
	labs(x     = "\n \n Minutes",
			 y     = "",
			 title = "\n Matches in 3 sets (2 winning sets) \n") + 
	scale_x_continuous(breaks = seq(80,280,20),
										 labels = as.character(seq(80,280,20)),
										 limits = c(75,280)) + 
	scale_y_continuous(breaks   = sort(unique(df_plot$Season)),
										 labels   = as.character(seq(2005,2023,1)),
										 position = "right") + 
	theme(panel.grid.major.y = element_blank(),
				panel.grid.major.x = element_line(colour    = "#B8C1BE", 
																					linetype  = 'dotted',
																					linewidth = 1),
				panel.grid.minor   = element_blank(),
				plot.background    = element_rect(fill  = "white", 
																					color = "white"), 
				plot.title         = element_text(hjust = 0.5),
				plot.margin        = margin(10,10,20,25),
				axis.text.y        = element_text(margin = margin(0,0,0,0), 
																					color  = "#27312E",
																					face   = "bold"),
				axis.text.x        = element_text(color = "#27312E",
																					face  = "bold"),
				axis.title.x       = element_text(color = "#583054", 
																					face  = "bold")
				)





# Summarise informations for the tables viz

# Table 2 sets

Two_set_table = full_join(ATP_df_final %>% filter(Set_3 == "", minutes > 120) %>% group_by(winner_name) %>% count() %>% arrange(desc(n)),
				               	  ATP_df_final %>% filter(Set_3 == "", minutes > 120) %>% group_by(loser_name) %>% count() %>% arrange(desc(n)),
					                by = c('winner_name' = 'loser_name'))

Two_set_table$n.x[which(is.na(Two_set_table$n.x))] = 0
Two_set_table$n.y[which(is.na(Two_set_table$n.y))] = 0
Two_set_table$n                                    = Two_set_table$n.x + Two_set_table$n.y
Two_set_table           													 = Two_set_table[,c("winner_name","n")]
colnames(Two_set_table) 													 = c("Name","n")


# Table 3 sets

Three_set_table = full_join(ATP_df_final %>% filter(Set_3 != "", minutes > 180) %>% group_by(winner_name) %>% count() %>% arrange(desc(n)),
				                  	ATP_df_final %>% filter(Set_3 != "", minutes > 180) %>% group_by(loser_name) %>% count() %>% arrange(desc(n)),
					                  by = c('winner_name' = 'loser_name'))

Three_set_table$n.x[which(is.na(Three_set_table$n.x))] = 0
Three_set_table$n.y[which(is.na(Three_set_table$n.y))] = 0
Three_set_table$n                                      = Three_set_table$n.x + Three_set_table$n.y
Three_set_table                                        = Three_set_table[,c("winner_name","n")]
colnames(Three_set_table)                              = c("Name","n")


# Join the 2 tables 

Summary_Table                                      = full_join(Two_set_table,Three_set_table, by = "Name")
Summary_Table$n.x[which(is.na(Summary_Table$n.x))] = 0
Summary_Table$n.y[which(is.na(Summary_Table$n.y))] = 0
Summary_Table$n                                    = Summary_Table$n.x + Summary_Table$n.y
Summary_Table                                      = Summary_Table[,c("Name","n")]


# Player images

unique(c(tibble(Summary_Table) %>% arrange(desc(n)) %>% slice_head(n = 10) %>% pull(Name),
				 tibble(Three_set_table) %>% arrange(desc(n)) %>% slice_head(n = 10) %>% pull(Name),
				 tibble(Two_set_table) %>% arrange(desc(n)) %>% slice_head(n = 10) %>% pull(Name)))

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2022/05/25/15/47/nadal-head-2022-may.png",
							'Rafael Nadal.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2019/02/25/18/18/djokovic_head_ao19.png",
							'Novak Djokovic.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2022/05/25/15/37/murray-head-2022-may.png",
							'Andy Murray.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2018/04/02/01/20/ferrer_head_ao18.png",
							'David Ferrer.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2020/02/28/12/01/simon_head_ao20.png",
							'Gilles Simon.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2016/08/05/21/25/mathieu-head_ao-16.png",
							'Paul Henri Mathieu.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2016/08/05/21/55/ljubicic_i_headshot_lq.png",
							'Ivan Ljubicic.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2016/08/05/21/56/massu_n_headshot_lq.png",
							'Nicolas Massu.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2019/03/08/17/24/ramos_vinolas_head_ao19.png",
							'Albert Ramos.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2018/04/02/01/20/garcia_lopez_head_ao18.png",
							'Guillermo Garcia Lopez.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2016/08/05/21/27/berlocq-headshot_14.png",
							'Carlos Berlocq.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2023/03/14/15/48/lapentti_head_2023.png",
							'Nicolas Lapentti.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2020/02/28/12/13/wawrinka_head_ao20.png",
							'Stan Wawrinka.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2022/05/25/15/44/alcaraz-head-2022-may.png",
							'Carlos Alcaraz.png', 
							mode = 'wb')	

download.file("https://www.atptour.com/-/media/tennis/players/head-shot/2022/06/16/20/12/mannarino-head-2022-may.png",
							'Adrian Mannarino.png', 
							mode = 'wb')	


# TABLE TWO SET

tibble(Two_set_table) %>% 
	arrange(desc(n)) %>% 
	slice_head(n = 6) %>% 
	mutate(Player      = Name,
				 Activity    = c('Yes','Yes','Yes','No','No','No'),
				 Nationality = c('Spain','United Kingdom','Serbia','Spain','France','Chile'),
				 Age         = c(37,36,36,41,38,44)) %>% 
	select(Name,Nationality,Player,n,Activity,Age) %>%
  gt(id = "Two_set_table") %>%
  tab_header(title = md("<br>**Number of 2-set matches (> 2 hours)**<br><br>")) %>% 
  text_transform(
    locations = cells_body(columns = Name),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(55)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = Nationality),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(37)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = Activity),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(25)
      )
    }
  ) %>%
  cols_label(Name        = "",
  					 Player      = "",
  					 Activity    = "Active",
  					 Nationality = "",
  					 Age         = "Age") %>% 
  cols_width(
    Name        ~ px(85),
    Nationality ~ px(80),
    Player      ~ px(140),
    n           ~ px(50),
    Activity    ~ px(55),
    Age         ~ px(50),
  ) %>% 
  cols_align(
    align   = "center",
    columns = c(1,2,4,5,6)) %>%
	data_color(
    columns   = n,
    colors    = scales::col_numeric(
      palette = c("#f8eadb","#983737"),
      domain  = c(19,58)
    )
    ) %>%
   opt_css(
    css = "
#Two_set_table .gt_col_heading {
  color: #ffffff;
  background-color: #242A2F;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  padding-top: 15px;
  padding-bottom: 15px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#Two_set_table .gt_heading {
    background-color: #fae0b7;
    text-align: center;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
}

#Two_set_table .gt_col_headings {
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
}

#Two_set_table .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #bcbcbc;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#Two_set_table .gt_table_body {
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
}

"
) %>% gtsave("Two_set_table.html")

webshot2::webshot("Two_set_table.html", "Two_set_table.png",vwidth = 625) 




# TABLE THREE SET

tibble(Three_set_table) %>% 
	arrange(desc(n)) %>% 
	slice_head(n = 6) %>% 
	mutate(Player      = Name,
				 Activity    = c('Yes','Yes','No','Yes','No','Yes'),
				 Nationality = c('Spain','Serbia','Chile','United Kingdom','France','Spain'),
				 Age         = c(37,36,44,36,38,35)) %>% 
	select(Name,Nationality,Player,n,Activity,Age) %>%
  gt(id = "Three_set_table") %>%
  tab_header(title = md("<br>**Number of 3-set matches (> 3 hours)**<br><br>")) %>% 
  text_transform(
    locations = cells_body(columns = Name),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(55)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = Nationality),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(37)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = Activity),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(20)
      )
    }
  ) %>%
  cols_label(Name        = "",
  					 Player      = "",
  					 Activity    = "Active",
  					 Nationality = "",
  					 Age         = "Age") %>% 
  cols_width(
    Name        ~ px(85),
    Nationality ~ px(80),
    Player      ~ px(140),
    n           ~ px(50),
    Activity    ~ px(55),
    Age         ~ px(50)
  ) %>% 
  cols_align(
    align   = "center",
    columns = c(1,2,4,5,6)) %>%
	data_color(
    columns   = n,
    colors    = scales::col_numeric(
      palette = c("#f8eadb","#983737"),
      domain  = c(13,21)
    )
    ) %>%
   opt_css(
    css = "
#Three_set_table .gt_col_heading {
  color: #ffffff;
  background-color: #242A2F;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  padding-top: 15px;
  padding-bottom: 15px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#Three_set_table .gt_heading {
    background-color: #B7FACF;
    text-align: center;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
}

#Three_set_table .gt_col_headings {
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
}

#Three_set_table .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #bcbcbc;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#Three_set_table .gt_table_body {
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
}

"
) %>% gtsave("Three_set_table.html")

webshot2::webshot("Three_set_table.html", "Three_set_table.png",vwidth = 625) 



# TABLE SUMMARY

tibble(Summary_Table) %>% 
	arrange(desc(n)) %>% 
	slice_head(n = 6) %>% 
	mutate(Player      = Name,
				 Activity    = c('Yes','Yes','Yes','No','No','Yes'),
				 Nationality = c('Spain','Serbia','United Kingdom','Chile','France','Spain'),
				 Age         = c(37,36,36,44,38,35)) %>% 
	select(Name,Nationality,Player,n,Activity,Age) %>%
  gt(id = "Summary_table") %>%
  tab_header(title = md("<br>**Combination of both cases**<br><br>")) %>% 
  text_transform(
    locations = cells_body(columns = Name),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(55)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = Nationality),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(37)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = Activity),
    fn = function(x) {
      local_image(
        filename = paste(x,'.png',sep = "") ,
        height   = as.numeric(20)
      )
    }
  ) %>%
  cols_label(Name        = "",
  					 Player      = "",
  					 Activity    = "Active",
  					 Nationality = "",
  					 Age         = "Age") %>% 
  cols_width(
    Name        ~ px(85),
    Nationality ~ px(80),
    Player      ~ px(140),
    n           ~ px(50),
    Activity    ~ px(55),
    Age         ~ px(50)
  ) %>% 
  cols_align(
    align   = "center",
    columns = c(1,2,4,5,6)) %>%
	data_color(
    columns   = n,
    colors    = scales::col_numeric(
      palette = c("#f8eadb","#983737"),
      domain  = c(27,79)
    )
    ) %>%
   opt_css(
    css = "
#Summary_table .gt_col_heading {
  color: #ffffff;
  background-color: #242A2F;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  padding-top: 15px;
  padding-bottom: 15px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#Summary_table .gt_heading {
    background-color: #F0B7FA;
    text-align: center;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
}

#Summary_table .gt_col_headings {
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
}

#Summary_table .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #bcbcbc;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#Summary_table .gt_table_body {
    border-top-style: solid;
    border-top-width: 2px;
    border-top-color: #242A2F;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #242A2F;
    border-left-style: solid;
    border-left-width: 2px;
    border-left-color: #242A2F;
    border-right-style: solid;
    border-right-width: 2px;
    border-right-color: #242A2F;
}

"
) %>% gtsave("Summary_table.html")

webshot2::webshot("Summary_table.html", "Summary_table.png",vwidth = 625) 





# Save plot ----



ggsave(plot     = plot_grid(first_plot,second_plot,
														nrow = 1
														),
			 filename = "upper_plot.png", 
			 device   = "png",
			 width    = 16,
			 height   = 6, 
			 dpi      = 300)



white_plot = ggplot(data.frame(x = 200,y = 1100), aes(x = x, y = y)) + 
								scale_x_continuous(limits = c(0,450)) +
								scale_y_continuous(limits = c(0,1100)) +
								theme(panel.grid.major = element_blank(),
											panel.grid.minor = element_blank(),
											axis.text        = element_blank(),
											axis.title       = element_blank(),
											plot.background  = element_rect(fill  = "white", 
																											color = "white"),
											panel.background = element_rect(fill  = "white",
																											color = "white"),
											axis.ticks       = element_blank())

legend_plot = ggplot(data.frame(x = 0.5,y = 0.5), aes(x = x, y = y)) + 
									geom_point(color = "#B22D0D", size = 5, alpha = 0.5) + 
									scale_x_continuous(limits = c(0,1)) +
									scale_y_continuous(limits = c(0,1)) +
									theme(panel.grid.major = element_blank(),
												panel.grid.minor = element_blank(),
												axis.text        = element_blank(),
												axis.title       = element_blank(),
												plot.background  = element_rect(fill  = "white", 
																												color = "white"),
												panel.background = element_rect(fill  = "white",
																												color = "white"),
												axis.ticks       = element_blank())



ggsave(plot   = white_plot,"white_plot.png",
			 device = "png",
			 width  = 19,
			 height = 17)
ggsave(plot   = legend_plot,"legend_plot.png",
			 device = "png",
			 width  = 1,
			 height = 1)



image_write(image_read("white_plot.png") %>% 
	image_composite(image_scale(image_read("legend_plot.png"),"350"), offset = "+45+175") %>%
	image_composite(image_scale(image_read("legend_plot.png"),"350"), offset = "+5125+175") %>%
	image_composite(image_scale(image_read("upper_plot.png"),"5450"), offset = "+30+400") %>%
	image_composite(image_scale(image_read("Two_set_table.png"),"2000"), offset = "+0+2800") %>%
	image_composite(image_scale(image_read("Three_set_table.png"),"2000"), offset = "+1825+2800") %>%
	image_composite(image_scale(image_read("Summary_table.png"),"2000"), offset = "+3650+2800") %>%
	image_annotate("Andy Murray", size = 50, color = "#7C200A" ,location = "+300+315") %>%
	image_annotate("Andy Murray", size = 50, color = "#7C200A" ,location = "+4940+315") %>%
	image_annotate("Visualisation by DENIAUX Maxime | Data : SACKMANN Jeff | Icons : icones8 | Photos : ATP", size = 60, color = "#5D5A5A" ,location = "+1725+4950") %>%
	image_annotate("Marathon runners on the ATP circuit since 1998", size = 105, color = "black",weight = 700,location = "+1950+50"),
	path = "FINAL_PLOT.png", 
	format = "png") 






