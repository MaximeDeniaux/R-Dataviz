

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/8. Most_Decisive_Players")

# Packages ----

library(rvest)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(cowplot)
library(ggtext)
library(htmlwidgets)



# Data ----

# I will select the 20 best scorers from 22/23 of the 5 major championships



# Stats for the plot /90min


league_url <- c('https://www.transfermarkt.co.uk/ligue-1/scorerliste/wettbewerb/FR1/saison_id/2022',
								'https://www.transfermarkt.co.uk/premier-league/scorerliste/wettbewerb/GB1/saison_id/2022',
								'https://www.transfermarkt.co.uk/laliga/scorerliste/wettbewerb/ES1/saison_id/2022',
								'https://www.transfermarkt.co.uk/bundesliga/scorerliste/wettbewerb/L1/saison_id/2022',
								'https://www.transfermarkt.co.uk/serie-a/scorerliste/wettbewerb/IT1/saison_id/2022')

league_label <- c('Ligue 1','Premier League','Liga','Bundesliga','Serie A')




for(j in 1:length(league_url)){

print(paste("j : ",j))



# URL players
	
player_url <- read_html(league_url[j]) %>% html_nodes('.hauptlink a') %>% html_attr('href') %>% .[1:20] 



if(j == 1){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/plus/0?saison=2022&verein=&liga=&wettbewerb=FR1&pos=&trainer_id=",
																															 sep = "")
                                         }
}

if(j == 2){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/plus/0?saison=2022&verein=&liga=&wettbewerb=GB1&pos=&trainer_id=",
																															 sep = "")
                                         }
}

if(j == 3){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/plus/0?saison=2022&verein=&liga=&wettbewerb=ES1&pos=&trainer_id=",
																															 sep = "")
                                         }
}

if(j == 4){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/plus/0?saison=2022&verein=&liga=&wettbewerb=L1&pos=&trainer_id=",
																															 sep = "")
                                         }
}

if(j == 5){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/plus/0?saison=2022&verein=&liga=&wettbewerb=IT1&pos=&trainer_id=",
																															 sep = "")
                                         }
	        }




# Data Players

for(i in 1:length(player_url)){

print(paste("i : ",i))

goals <- ((read_html(player_url[i]) %>% 
														html_nodes('table') %>% 
														.[2] %>% 
														html_table(fill = T))[[1]][1,6:7] %>% as.numeric())[1]

assists <- ifelse(length((read_html(player_url[i]) %>% 
														html_nodes('table') %>% 
														.[2] %>% 
														html_table(fill = T))[[1]][1,6:7] %>% as.numeric()) == 2,
								 ((read_html(player_url[i]) %>% 
														html_nodes('table') %>% 
														.[2] %>% 
														html_table(fill = T))[[1]][1,6:7] %>% as.numeric())[2],
								 0)
								 

time <- as.numeric(gsub("'","",gsub("[.]","",(read_html(player_url[i]) %>% 
														html_nodes('table') %>% 
														.[2] %>% 
														html_table(fill = T))[[1]][1,9])))

data_player_i_ratio_90_min <- round((goals + assists) * 90 / time, digits = 2)

player_name <- gsub("[0-9]+","",
										gsub("            ","",
												 gsub("                    \n                                ","",
												 		 gsub("\n                                    \n                        #","",read_html(player_url[i]) %>% 
														html_nodes('h1') %>% 
														.[1] %>% html_text()))))


League_player <- league_label[j]
	
	
birth_date <- substr(gsub("                            ","",gsub("\n                                ","",read_html(player_url[i]) %>% 
														html_nodes(".data-header__items span") %>% 
														.[1] %>% html_text())),1,nchar(gsub("                            ","",gsub("\n                                ","",read_html(player_url[i]) %>% 
														html_nodes(".data-header__items span") %>% 
														.[1] %>% html_text())))-5)

Position <- gsub("                        ","",gsub("\n                            ","",read_html(player_url[i]) %>% 
														html_nodes(".data-header__items span") %>% 
														.[5] %>% html_text()))

if(j == 1 & i == 1){data <- data.frame(Player_Name = player_name,
																			 Birth_Date = birth_date,
																			 Position = Position,
																			 League_Player = League_player,
																		   Ratio_90_min = data_player_i_ratio_90_min,
																			 Time = time,
																			 Goals = goals,
																			 Assists = assists)}

if((j != 1) | (j == 1 & i != 1)){data <- bind_rows(data, data.frame(Player_Name = player_name,
																			 Birth_Date = birth_date,
																			 Position = Position,
																			 League_Player = League_player,
																		   Ratio_90_min = data_player_i_ratio_90_min,
																			 Time = time,
																			 Goals = goals,
																			 Assists = assists))}

}
}








# Stats for the plot /match


league_url <- c('https://www.transfermarkt.co.uk/ligue-1/scorerliste/wettbewerb/FR1/saison_id/2022',
								'https://www.transfermarkt.co.uk/premier-league/scorerliste/wettbewerb/GB1/saison_id/2022',
								'https://www.transfermarkt.co.uk/laliga/scorerliste/wettbewerb/ES1/saison_id/2022',
								'https://www.transfermarkt.co.uk/bundesliga/scorerliste/wettbewerb/L1/saison_id/2022',
								'https://www.transfermarkt.co.uk/serie-a/scorerliste/wettbewerb/IT1/saison_id/2022')

league_label <- c('Ligue 1','Premier League','Liga','Bundesliga','Serie A')

for(j in 1:length(league_url)){

print(paste("j : ",j))




# URL players
	
player_url <- read_html(league_url[j]) %>% html_nodes('.hauptlink a') %>% html_attr('href') %>% .[1:20] 




if(j == 1){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/saison/2022/verein/0/liga/0/wettbewerb/FR1/pos/0/trainer_id/0/plus/1",
																															 sep = "")
                                         }
}

if(j == 2){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/saison/2022/verein/0/liga/0/wettbewerb/GB1/pos/0/trainer_id/0/plus/1",
																															 sep = "")
                                         }
}

if(j == 3){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/saison/2022/verein/0/liga/0/wettbewerb/ES1/pos/0/trainer_id/0/plus/1",
																															 sep = "")
                                         }
}

if(j == 4){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/saison/2022/verein/0/liga/0/wettbewerb/L1/pos/0/trainer_id/0/plus/1",
																															 sep = "")
                                         }
}

if(j == 5){for(i in 1:length(player_url)){player_url[i] = paste("https://www.transfermarkt.co.uk/",
																															 strsplit(x = player_url,"/")[[i]][2],
																															 "/leistungsdatendetails/spieler/",
																															 strsplit(x = player_url,"/")[[i]][5],
																															 "/saison/2022/verein/0/liga/0/wettbewerb/IT1/pos/0/trainer_id/0/plus/1",
																															 sep = "")
                                         }
	        }




# Data Players

for(i in 1:length(player_url)){

print(paste("i : ",i))

df <- (read_html(player_url[i]) %>% html_nodes('table') %>% .[3] %>% html_table(fill = T))[[1]]
if(j == 4){df <- df[-nrow(df),c(1,8:10,15:16,18)]}	
if(j != 4){df <- df[-nrow(df),c(1,8:10,15:17)]}	
colnames(df) <- c('Matchday','Pos','Goals','Assist','In','Out','Time')

rsd <- sd(extract_numeric(df$Time),na.rm = T)/mean(extract_numeric(df$Time),na.rm = T)
df[is.na(extract_numeric(df$Time)),2:7] = ""
df$Game_Played     <- ifelse(df$Time == "",'No','Yes')	
df$Decisive_Action <- ifelse(df$Assist == "" & df$Goals == "","No","Yes")
ratio              <- round(as.numeric(table(df$Decisive_Action)[2]) /as.numeric(table(df$Game_Played)[which(names(table(df$Game_Played)) == "Yes")]),digits = 2)
if(length(which(df$Time == "")) != 0){df$Time[df$Time == ""] = 0}
mean_time_played  <- round(sum(extract_numeric(df$Time)) / as.numeric(table(df$Game_Played)[which(names(table(df$Game_Played)) == "Yes")]), digits = 2)

player_name <- gsub("[0-9]+","",
										gsub("            ","",
												 gsub("                    \n                                ","",
												 		 gsub("\n                                    \n                        #","",read_html(player_url[i]) %>% 
														html_nodes('h1') %>% 
														.[1] %>% html_text()))))
	
player_img <- read_html(player_url[i]) %>% html_nodes('.data-header__profile-image') %>% html_attr('src')
	
	

if(j == 1 & i == 1){data_bis <- data.frame(Player_Name = player_name,
																					 Ratio_Decisives_Actions =  ratio,
																					 Mean_Time_Played = mean_time_played,
																					 Player_Image = player_img,
																					 rsd = rsd
																					 )}

if((j != 1) | (j == 1 & i != 1)){data_bis <- bind_rows(data_bis, data.frame(Player_Name = player_name,
																																						Ratio_Decisives_Actions =  ratio,
																																						Mean_Time_Played = mean_time_played,
																																						Player_Image = player_img,
																																						rsd = rsd
																																						))}

}
}






# Data Processing ----

# Join informations

data <- data %>% mutate(Decisives_Actions_Nbr =  Goals + Assists)
data <- left_join(data,data_bis)


# Edit some wrong position values

data$Position[data$Player_Name == "Folarin Balogun"]  = "Centre-Forward"
data$Position[data$Player_Name == "Borja Iglesias"]  = "Centre-Forward"


# Factor type for League_Player

data$League_Player <- factor(data$League_Player, levels = unique(data$League_Player))


# Summary variable for position 

data <- data %>% mutate(Position_Summary = case_when(Position %in% c('Left-Back') ~ 'Defender',
																										 Position %in% c('Defensive Midfield','Central Midfield') ~ 'Midfielder',
																										 T ~ 'Attacker'
																										 )
												)



# Need to verify if there are NA values, sometimes I had issues with url players

# Variables class

data$Mean_Time_Played        <- as.numeric(data$Mean_Time_Played) 
data$Ratio_Decisives_Actions <- as.numeric(data$Ratio_Decisives_Actions) 


data$id = 1:100


# Plot ----

data$tooltip_plot_1 <- paste("<div class='main_div'>",
														 "<img class='image_player' src=",data$Player_Image,">",
														 "<p class='p_text'>", "<span class='span_text'>","Player : ","</span><span class='span_text_bis'>",data$Player_Name,"</span><br>",
														 "<span class='span_text'>","Position : ","</span><span class='span_text_bis'>",data$Position,"</span>","<span class='span_text'>","  |   League : ","</span><span class='span_text_bis'>",data$League_Player,"</span><br>",
														 "<span class='span_text'>","Time played : ","</span><span class='span_text_bis'>",data$Time," min","<span class='span_text'>"," (mean/match : ","</span>","<span class='span_text_bis'>",data$Mean_Time_Played," min","</span>","<span class='span_text'>",")","</span><br>",   
														 "<span class='span_text'>","Relative Stand. Dev. (time by match variable) : ","</span><span class='span_text_bis'>",round(data$rsd,digits = 2),"</span><br>",
														 "<span class='span_text'>","Goals : ","</span><span class='span_text_bis'>",data$Goals, "<span class='span_text'>","  -  Assists : ","</span>",data$Assists,"</span><br>",
														 "<span class='span_text'>","Decisives actions number : ","</span><span class='span_text_bis'>",data$Decisives_Actions_Nbr,"</span><br>",
														 "<span class='span_text'>","Decisives actions / 90 min : ","</span><span class='span_text_bis'>",data$Ratio_90_min,"</span></p></div>",
														 sep = "")


plot1 = ggplot() + 
	# Axis title
  annotate('text',
    x     = c(16,58),
    y     = c(1.55,0.31),
    label = c("Decisive actions / 90 min","Decisive actions (total)"),
    size  = 3.5, 
    color = "gray40"
  ) + 
	# Mean text
  annotate('text',
    x     = c(mean(data$Decisives_Actions_Nbr) - 0.85,48),
    y     = c(1.43,mean(data$Ratio_90_min) + 0.03),
    label = rep("mean",2),
    size  = 3.5, 
    color = c("darkred","#1A3C1A"), 
    angle = c(90,0)
  ) + 
	# Mean segment
	annotate("segment", 
					 x         = mean(data$Decisives_Actions_Nbr), 
					 xend      = mean(data$Decisives_Actions_Nbr), 
					 y         = 0.3, 
					 yend      = 1.5,
					 colour    = "darkred",
					 linewidth = 0.25
					 ) +
	annotate("segment", 
					 x         = 10, 
					 xend      = 50, 
					 y         = mean(data$Ratio_90_min), 
					 yend      = mean(data$Ratio_90_min),
					 colour    = "#1A3C1A",
					 linewidth = 0.25
					 ) +
  # Points
	geom_point_interactive(data = data,aes(x       = Decisives_Actions_Nbr, 
																				 y       = Ratio_90_min,
																				 tooltip = tooltip_plot_1,
																				 data_id = id), 
												 size  = 1.5, 
												 fill  = "#DC60DC", 
												 color = "gray30", 
												 shape = 21
												 ) + 
	scale_x_continuous(breaks = seq(10,50,10), 
										 labels = seq(10,50,10), 
										 limits = c(10,60), 
										 expand = c(0,0)
										 ) +
	scale_y_continuous(breaks = seq(0.3,1.5,0.2), 
										 labels = seq(0.3,1.5,0.2), 
										 limits = c(0.2,2), 
										 expand = c(0,0)
										 ) +
	coord_cartesian(clip = 'off', xlim = c(10,50), ylim = c(0.3,1.5)) +
	labs(x = "", y = "", fill = "", shape = "") +
	theme(panel.grid.minor = element_blank(),
				panel.grid.major = element_line(color     = 'gray90',
																				linetype  = 'dashed',
																				linewidth = 0.5),
				panel.background = element_rect(fill  = 'white',
																				color = 'gray90'),
				panel.border     = element_rect(colour = "gray50",
																				fill   = NA),
				plot.margin      = margin(c(50,150,50,10)),
				axis.ticks       = element_blank(), 
				legend.position  = 'none')




data$tooltip_plot_2 <- paste("<div class='main_div'>",
														 "<img class='image_player' src=",data$Player_Image,">",
														 "<p class='p_text'>", "<span class='span_text'>","Player : ","</span><span class='span_text_bis'>",data$Player_Name,"</span><br>",
														 "<span class='span_text'>","Position : ","</span><span class='span_text_bis'>",data$Position,"</span>","<span class='span_text'>","  |   League : ","</span><span class='span_text_bis'>",data$League_Player,"</span><br>",
														 "<span class='span_text'>","Time played : ","</span><span class='span_text_bis'>",data$Time," min","<span class='span_text'>"," (mean/match : ","</span>","<span class='span_text_bis'>",data$Mean_Time_Played," min","</span>","<span class='span_text'>",")","</span><br>",   
														 "<span class='span_text'>","Relative Stand. Dev. (time by match variable) : ","</span><span class='span_text_bis'>",round(data$rsd,digits = 2),"</span><br>",
														 "<span class='span_text'>","Goals : ","</span><span class='span_text_bis'>",data$Goals, "<span class='span_text'>","  -  Assists : ","</span>",data$Assists,"</span><br>",
														 "<span class='span_text'>","Decisives actions number : ","</span><span class='span_text_bis'>",data$Decisives_Actions_Nbr,"</span><br>",
														 "<span class='span_text'>","Decisives actions by match : ","</span><span class='span_text_bis'>",data$Ratio_Decisives_Actions,"</span></p></div>",
														 sep = "")



plot2 = ggplot() + 
	# Axis title
  annotate('text',
				   x     = c(18,58),
				   y     = c(0.825,0.205),
				   label = c("Decisive actions by match played","Decisive actions (total)"),
				   size  = 3.5, 
				   color = "gray40"
  ) + 
	# Mean text
  annotate('text',
  				 x      = c(mean(data$Decisives_Actions_Nbr) - 0.85,48),
           y      = c(0.765,mean(data$Ratio_Decisives_Actions) + 0.017),
				   label  = rep("mean",2),
				   size   = 3.5, 
				   color  = c("darkred","#1A3C1A"), 
				   angle  = c(90,0)
  ) + 
	# Mean segment
	annotate("segment", 
					 x         = mean(data$Decisives_Actions_Nbr), 
					 xend      = mean(data$Decisives_Actions_Nbr), 
					 y         = 0.2, 
					 yend      = 0.8,
					 colour    = "darkred",
					 linewidth = 0.25
					 ) +
	annotate("segment", 
					 x         = 10, 
					 xend      = 50, 
					 y         = mean(data$Ratio_Decisives_Actions), 
					 yend      = mean(data$Ratio_Decisives_Actions),
					 colour    = "#1A3C1A",
					 linewidth = 0.25
					 ) +
  # Points
	geom_point_interactive(data = data,aes(x       = Decisives_Actions_Nbr, 
																				 y       = Ratio_Decisives_Actions,
																				 tooltip = tooltip_plot_2,
																				 data_id = id),  
						 size  = 1.5, 
						 fill  = "#9AC4F9", 
						 color = "gray30", 
						 shape = 21
						 )+
	scale_y_continuous(breaks = seq(0.2,0.8,0.2), 
										 labels = seq(0.2,0.8,0.2), 
										 limits = c(0.2,2), 
										 expand = c(0,0)
										 ) +
	scale_x_continuous(breaks = seq(10,50,10), 
										 labels = seq(10,50,10), 
										 limits = c(10,70), 
										 expand = c(0,0)
										 ) +
	coord_cartesian(clip = 'off', xlim = c(10,50), ylim = c(0.2,0.8)) +
	labs(x = "", y = "", fill = "") +
	theme(panel.grid.minor = element_blank(),
				panel.grid.major = element_line(color     = 'gray90', 
																				linetype  = 'dashed', 
																				linewidth = 0.5),
				panel.background = element_rect(fill  = 'white',
																				color = 'gray90'),
				panel.border     = element_rect(colour = "gray50",
																				fill   = NA),
				plot.margin      = margin(c(50,150,50,10)),
				axis.ticks       = element_blank(), 
				legend.position  = 'none') 






caption = ggplot(data.frame(x = 1, y = 1)) + 
							theme_void() +
							labs(title = "Visualisation by DENIAUX Maxime | Data : Transfermarkt | Images : Transfermarkt") +
							theme(panel.background = element_rect(fill  = "white", 
																										color = "white"),
										plot.background  = element_rect(fill  = "white", 
																										color = "white"),
										plot.title       = element_text(hjust  = 0.5, 
																										color  = "gray35", 
																										size   = 7, 
																										margin = margin(0,0,0,0)),
										plot.margin      = margin(t = 0, b = 10)
										)


title = ggplot(data.frame(x = 1, y = 1)) + 
						theme_void() +
						labs(title    = "The most decisive football players of 2022 season in the 5 major leagues",
								 subtitle = "Only the 20 most decisive players from each league of the top 5 (Ligue 1, Serie A, Liga, Bundesliga, Premier League) were selected.  <br> The « Decisive actions by match » variable is just the ratio of the number of matches with at least one goal or assist (a match with one <br> minute of playing time counts as a match). These are league goals and assists only.") +
						theme(panel.background = element_rect(fill  = "white", 
																									color = "white"),
									plot.background  = element_rect(fill  = "white", 
																									color = "white"),
									plot.title       = element_markdown(hjust   = 0.5, 
																											color   = "#976930", 
																											size    = 15, 
																											margin  = margin(20,0,0,0),
																											face    = "bold"),
									plot.subtitle    = element_markdown(hjust      = 0.5, 
																											color      = "gray25", 
																											halign     = 0.5,
																											lineheight = 1.5, 
																											size       = 10, 
																											margin     = margin(10,0,20,0)),
									plot.margin      = margin(t = 10, b = 0)
									)



# Save plot ----

# Values for a local view

# tooltip_css <- "
# div {background-color: #D5D8D6; border-radius: 5px; overflow: hidden;}
# .p_text {float: right;  padding-left: 30px; padding-right: 30px; line-height: 1.27;}
# .span_text {color: #29824E;}
# .span_text_bis {color: #91281F;}
# .image_player {float: left; width: 102px;height: 102px; padding-left:20px; padding-top: 20px; padding-bottom: 20px;}
# "


# Values for my website

tooltip_css <- "
div {background-color: #D5D8D6; border-radius: 5px; overflow: hidden;}
.p_text {float: right;  padding-left: 30px; padding-right: 30px;padding-top: 20px; padding-bottom: 20px; line-height: 1.4;}
.span_text {color: #29824E;}
.span_text_bis {color: #91281F;}
.image_player {float: left; width: 140px;height: 180px; padding-left:20px; padding-top: 30px; padding-bottom: 10px;} 
"


mid <- plot_grid(plot1,plot2,nrow = 1)


x = girafe(ggobj = plot_grid(title,
														 mid,
														 caption,
														 nrow = 3,
														 rel_heights = c(0.16,0.82,0.02)),
					 options = list(opts_tooltip(css = tooltip_css),
					 							  opts_hover(  css = "fill:wheat;stroke:orange;r:3pt;")
					 							 ),
					 width_svg  = 16,
					 height_svg = 6.5)

saveWidget(x, "Final_Plot.html", selfcontained = T)






