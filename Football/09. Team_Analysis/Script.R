
# Packages ----

library(ggplot2)
library(dplyr)
library(cowplot)
library(rvest)
library(janitor)
library(tidyr)
library(lubridate)
library(ggfun)
library(ggpubr)
library(ggtext)




# Directory ----

setwd("C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Git/R-Dataviz/Football/9. Team_Analysis")




# Data ----

# Scrap table

data <-  (read_html("https://www.transfermarkt.co.uk/olympique-de-marseille/kader/verein/244/saison_id/2023/plus/1") %>% 
									     html_nodes('table') %>%
									     .[2] %>%
									     html_table(fill = T))[[1]] %>% 
	          clean_names() %>% 
	          slice(seq(1,nrow((read_html("https://www.transfermarkt.co.uk/olympique-de-marseille/kader/verein/244/saison_id/2023/plus/1") %>% 
						html_nodes('table') %>%
						.[2] %>%
						html_table(fill = T))[[1]]),3)) %>%
	          select(c(4,5,6,8,9,10,12,13))





# Data processing ----


# Column names

colnames(data) <- c('Name','Position','Age','Height','Foot','Joined','End_Contract','Value')


# Replace blank value (or equivalent) with NA value

if(length(which(data == "-", arr.ind=TRUE)) != 0){data[which(data == "-", arr.ind=TRUE)[1], which(data == "-", arr.ind=TRUE)[2]] <- NA}
if(length(which(data == "",  arr.ind=TRUE)) != 0){data[which(data == "",  arr.ind=TRUE)[1], which(data == "",  arr.ind=TRUE)[2]] <- NA}


# Remove lines without information for contract

if(length(which(is.na(data$Joined))) != 0){data <- data[-which(is.na(data$Joined)),]}
if(length(which(is.na(data$End_Contract))) != 0){data <- data[-which(is.na(data$End_Contract)),]}


# Edit Value variable

data$Value <- gsub(x           = data$Value, 
									 pattern     = "€",
									 replacement = ""
									 ) 

data       <- separate(data = data,
											 col  = Value,
											 into = c('Value','Unit'),
											 sep  = "m")

data       <- data %>% mutate(Unit = ifelse(is.na(Unit), 
																						"K",
																						"M"
																						)
															)

data$Value <- gsub(x           = data$Value, 
									 pattern     = "k",
									 replacement = ""
									 ) 

data       <- data %>% mutate(Value = ifelse(Unit == "K",
																						 paste("0.",Value,sep = ""),
																						 Value
																						 )
															)

data       <- data %>% select(-Unit)

data$Value <- as.numeric(data$Value)


# Edit Height variable

data$Height <- gsub(x           = data$Height , 
										pattern     = "m",
										replacement = ""
										) 

data$Height <- gsub(x           = data$Height , 
										pattern     = ",",
										replacement = "."
										) 

data$Height <- as.numeric(data$Height)



# Edit End Contract variable

data$End_Contract <- paste(substr(data$End_Contract,
																	nchar(data$End_Contract) - 3,
																	nchar(data$End_Contract)
																		 ),
													 substr(data$End_Contract,
													 			 1,
													 			 3),
													 substr(data$End_Contract,
													 			 5,
													 			 6),
													 sep = "-"
													 )


data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Jan", 
													replacement = "01"
													)

data$End_Contract <- gsub(x           = data$End_Contract,
													pattern     = "Feb", 
													replacement = "02"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Mar", 
													replacement = "03"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Apr", 
													replacement = "04"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "May", 
													replacement = "05"
													) 

data$End_Contract <- gsub(x           = data$End_Contract,
													pattern     = "Jun",
													replacement = "06"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Jul", 
													replacement = "07"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Aug", 
													replacement = "08"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Sep", 
													replacement = "09"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Oct",
													replacement = "10"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Nov", 
													replacement = "11"
													) 

data$End_Contract <- gsub(x           = data$End_Contract, 
													pattern     = "Dec", 
													replacement = "12"
													) 


data$End_Contract <- ymd(data$End_Contract)



# Edit Joined variable

data$Joined <- paste(substr(data$Joined,
														nchar(data$Joined) - 3,
														nchar(data$Joined)
														),
										 substr(data$Joined,
										 			 1,
										 			 3),
										 substr(data$Joined,
										 			 5,
										 			 6),
										 sep = "-")

data <- data %>% mutate(Joined = ifelse(substr(Joined,11,11) == ",", 
																				paste(substr(Joined,1,9),
																							"0",
																							substr(Joined,10,10),
																							sep = ""
																							),
																				Joined  
																				) 
												)



data$Joined <- gsub(x           = data$Joined, 
										pattern     = "Jan",
										replacement = "01"
										)

data$Joined <- gsub(x           = data$Joined,
										pattern     = "Feb", 
										replacement = "02"
										) 

data$Joined <- gsub(x           = data$Joined, 
										pattern     = "Mar",
										replacement = "03"
										) 

data$Joined <- gsub(x           = data$Joined, 
										pattern     = "Apr", 
										replacement = "04"
										) 

data$Joined <- gsub(x           = data$Joined, 
										pattern     = "May",
										replacement = "05"
										) 

data$Joined <- gsub(x           = data$Joined,
										pattern     = "Jun",
										replacement = "06"
										) 

data$Joined <- gsub(x           = data$Joined,
										pattern     = "Jul", 
										replacement = "07"
										) 

data$Joined <- gsub(x           = data$Joined, 
										pattern     = "Aug", 
										replacement = "08"
										) 

data$Joined <- gsub(x           = data$Joined, 
										pattern     = "Sep", 
										replacement = "09"
										) 

data$Joined <- gsub(x           = data$Joined, 
										pattern     = "Oct",
										replacement = "10"
										) 

data$Joined <- gsub(x           = data$Joined, 
										pattern     = "Nov", 
										replacement = "11"
										) 

data$Joined <- gsub(x           = data$Joined, 
										pattern     = "Dec", 
										replacement = "12"
										) 


data$Joined <- ymd(data$Joined)



# Edit Age variable

for(i in 1:nrow(data)){
	
data$Age[i] = strsplit(x = data$Age[i],
											 split = '(',
											 fixed = T
											 )[[1]][2]
}

data$Age <- gsub(x           = data$Age, 
								 pattern     = ")",
								 replacement = "") 

data$Age <-  as.numeric(data$Age)



# Edit some values of joined date (player arrived with loan)

data$Joined[data$Name == "Pau López"]        <- "2021-07-17"
data$Joined[data$Name == "Rubén Blanco"]     <- "2022-07-20"
data$Joined[data$Name == "Samuel Gigot"]     <- "2022-06-30"
data$Joined[data$Name == "Leonardo Balerdi"] <- "2020-07-21"
data$Joined[data$Name == "Amine Harit"]      <- "2021-08-31"





# Factoring variables

data$Position                    <- factor(x      = data$Position, 
																					 levels = unique(data$Position)
																					 )
data$Name_Value_Order            <- with(data, reorder(Name,Value))
data$End_Contract_Order          <- with(data, reorder(Name,End_Contract))
data$Joined_Order                <- with(data, reorder(Name,Joined))
data$Name_Age_Order              <- with(data, reorder(Name,Age))



# Plots ----



# 1st plot : Contract and age by players

Plot_1 <- ggplot(data) + 
	            # Rectangle transfer window
	            annotate('rect', 
	            				 xmin  = as.Date("2023-06-10"),
	            				 xmax  = as.Date("2023-09-01"),
	            				 ymin  = 0, 
	            				 ymax  = 26.82, 
	            				 fill  = "#F0E362",
	            				 alpha = 0.3
	            				 ) +
	            # Line season start
	            geom_vline(xintercept = as.Date("2023-08-07"), 
	            					 color      = "#F0E362", 
	            					 linetype   = "solid"
	            					 ) +
	            # Text transfer window
	            geom_textbox(x         = as.Date("2020-06-15"), 
												   y         = 26.8, 
												   label     = "Transfer window", 
													 size      = 3, 
													 hjust     = 0, 
													 vjust     = 1, 
													 box.size  = NA, 
													 color     = "#F3EA96",
													 fill      = "gray20", 
													 width     = unit(1.05, "inch")
												 ) +
	            # Text season start
	            annotate('text', 
	            				 label     = "23-24 season start",
	            				 x         = as.Date("2021-06-30"), 
	            				 y         = 25.25,  
	            				 color     = "#F0E362",
	            				 size      = 3
	            				 ) + 
            	scale_y_discrete() +
	            # Segments contract
							geom_segment(aes(x     = Joined, 
															 xend  = End_Contract, 
															 y     = Joined_Order, 
															 yend  = Joined_Order, 
															 color = Age
															 ),
													 alpha     = 0.95,
													 linewidth = 2.75
													 ) + 
	            # Points contract
							geom_point(aes(x = Joined, 
														 y = Joined_Order
														 ), 
												 color = "gray10", 
												 fill  = "gray80", 
												 size  = 3, 
												 shape = 21
												 ) +
							geom_point(aes(x = End_Contract, 
														 y = Joined_Order
														 ), 
												 color = "gray10", 
												 fill  = "gray80", 
												 size  = 3, 
												 shape = 21
												 ) +
	            # Scale x axis
							scale_x_date(date_breaks = "1 year",
													 date_labels = "%Y",
													 limits      = c(as.Date("2019-01-01"),
													 					       as.Date("2029-01-01") 
													 					      )
													 ) +
	            # Segments color gradient
							scale_color_gradient(limits = c(min(data$Age), 
																							max(data$Age)
																						  ), 
																	 breaks = c(min(data$Age), 
																	 					  min(data$Age) + ((max(data$Age) - min(data$Age)) / 2), 
																	 					  max(data$Age)
																	 					 ),
																	 guide  = guide_colourbar(barheight      = 0.4,
																	 												  title.position = 'right',
															    												  title.vjust    = 0.2,
															    												  title          = "  years",
															    												  title.theme    = element_text(size = 9)
															    												  ),
																	 low    = "#FFC8E5",
																	 high   = "#DC0220" 
																	 ) + 
	            # Theme
							theme(panel.background      = element_rect(fill  = "#18202D",
																												 color = "#18202D"
																												 ),
										plot.background       = element_rect(fill  = "#18202D",
																												 color = "#18202D"
																												 ),
										legend.position       = 'none', 
										panel.grid.major.x    = element_line(linetype  = 'dashed', 
																												 color     = "#536178",
																												 linewidth = 0.5
																												 ),
										panel.grid.major.y    = element_blank(),
										panel.grid.minor.x    = element_blank(),
										axis.ticks            = element_blank(),
										axis.text.y           = element_text(color  = "white",
																												 margin = margin(r = 90),
																												 face   = "bold",
																												 size   = 12.5
																												 ),
										axis.text.x           = element_text(color = 'white')
										) +
							labs(y = "",
									 x = ""
									 ) 




# 2nd plot : Value by players

Plot_2 <- ggplot(data) + 
	            # Text player value
	            geom_textbox(x         = -20, 
												   y         = 26.5, 
												   label     = "Player value", 
													 size      = 3, 
													 hjust     = 0, 
													 vjust     = 1, 
													 box.size  = NA, 
													 color     = "black",
													 fill      = "#9DD7CA", 
													 width     = unit(0.82, "inch")
												 ) +
	            # Segments player value
							geom_segment(aes(x    = 0, 
															 xend = Value, 
															 y    = Joined_Order,
															 yend = Joined_Order
															 ),
													 color     = "#53D5B7",
													 alpha     = 0.70,
													 linewidth = 2.75
													 ) + 
	            # Points player value
							geom_point(aes(x = Value, 
														 y = Joined_Order
														 ), 
												 color = "gray10", 
												 fill  = "white", 
												 size  = 3, 
												 shape = 21
												 ) + 
	            # Scale x axis
	            scale_x_reverse(breaks = c(0,5,10,15,20),
	            								labels = paste(c(0,5,10,15,20),
	            															 " M€", 
	            															 sep = ""
	            															 )
	            								) +
	            # Theme
							theme(panel.background      = element_rect(fill  = "#18202D", 
																												 color = "#18202D"
																												 ),
										plot.background       = element_rect(fill  = "#18202D", 
																												 color = "#18202D"
																												 ),
										legend.position       = 'none', 
										panel.grid.major.x    = element_line(linetype  = 'dashed', 
																												 color     = "#536178",
																												 linewidth = 0.5
																												 ),
										panel.grid.major.y    = element_blank(),
										panel.grid.minor.x    = element_blank(),
										axis.ticks            = element_blank(),
										axis.text.y           = element_blank(),
										axis.text.x           = element_text(color = 'white')
										) +
							labs(y = "", 
									 x = ""
									 ) 




# Legend (gradient)

Legend <-  as_ggplot(ggpubr::get_legend(ggplot(data) + 
														            # Segments
																				geom_segment(aes(x     = Joined, 
																												 xend  = End_Contract, 
																												 y     = Joined_Order, 
																												 yend  = Joined_Order, 
																												 color = Age
																												 )
																										 ) + 
														            # Gradient 
																				scale_color_gradient(
																			    limits = c(min(data$Age), 
																			    					 max(data$Age)
																			    					 ), 
																			    breaks = c(min(data$Age), 
																			    					 min(data$Age) + ((max(data$Age) - min(data$Age)) / 2),
																			    					 max(data$Age)
																			    					 ),
																			    guide = guide_colourbar(barheight      = 0.6,
																			    												title.position = 'right',
																			    												title.vjust    = 0.23,
																			    												title          = "  years",
																			    												title.theme    = element_text(size  = 10.5, 
																			    																											color ="white"
																			    																											),
																			    												label.theme    = element_text(size  = 10.5, 
																			    																											color ="white"
																			    																											)
																			    												),
																			    low  = "#FFC8E5",
																			    high = "#DC0220" 
																			   ) + 
														            # Theme legend
																				theme(legend.position        =  c(0.51,0.7), 
																							legend.direction       = "horizontal",
																							legend.background      = element_rect(fill  = "#18202D", 
																																										color = "#18202D"
																																										),
																							legend.key             = element_rect(fill  = "#18202D", 
																																										color = "#18202D"
																																										),
																							legend.text            = element_text(color = "white")
																							)
																				)
									 ) +
	            # Theme plot
							theme(panel.background      = element_rect(fill  = "#18202D", 
																												 color = "#18202D"
																												 ),
										plot.background       = element_rect(fill  = "#18202D", 
																												 color = "#18202D"
																												 ),
										plot.margin           = margin(b = 20)
										)



# Caption 

Caption <-  ggplot(data.frame(x = 1, y = 1)) + 
							theme_void() +
							labs(title = "Visualisation by DENIAUX Maxime | Data : Transfermarkt") +
							theme(panel.background      = element_rect(fill  = "#18202D", 
																												 color = "#18202D"
																												 ),
				            plot.background       = element_rect(fill  = "#18202D", 
				            																		 color = "#18202D"
				            																		 ),
										plot.title            = element_text(hjust  = 0.5, 
																												 color  = "gray80", 
																												 size   = 7, 
																												 margin = margin(0,0,0,0)
																												 ),
										plot.margin           = margin(t = 10, b = 10)
										)

# Title

Title <-  ggplot(data.frame(x = 1, y = 1)) + 
						theme_void() +
						labs(title    = "Marseille, a workforce victim of too much renewal ?",
								 subtitle = "Under the leadership of its multifaceted president, Pablo Longoria, Marseille renewed a large part of its workforce during the last offseason <br> (as in each of them since his arrival at Marseille 3 and a half years ago). A choice criticized by observers explaining that it is more difficult <br> to create a gaming identity with too many new players. Perhaps rightly so given the results of the current season (8th at mid-season). <br> <span style='color:#F0E362'>The transfer window of too many ?</span>") +
						theme(panel.background      = element_rect(fill  = "#18202D", 
																											 color = "#18202D"
																											 ),
				          plot.background       = element_rect(fill  = "#18202D", 
				          																		 color = "#18202D"
				          																		 ),
									plot.title            = element_markdown(hjust   = 0.5, 
																													 color   = "#FFFFFF", 
																													 size    = 25, 
																													 margin  = margin(20,0,10,0),
																													 face    = "bold"
																													 ),
									plot.subtitle         = element_markdown(hjust      = 0.5, 
																													 color      = "#FFFFFF", 
																													 halign     = 0.5,
																													 lineheight = 1.6, 
																													 size       = 15, 
																													 margin     = margin(10,0,0,0)
																													 ),
									plot.margin           = margin(t = 10, b = 0)
									)





# Save final plot ----

ggsave(plot = plot_grid(Title,
												Legend,
												plot_grid(Plot_2,
																	Plot_1,
																	nrow = 1,
																	rel_widths = c(0.37,0.63)
																	),
												Caption,
												nrow = 4,
												rel_heights = c(0.25,0.05,0.65,0.05)
												),
			 filename = "Plot.png",
			 width    = 15,
			 height   = 12,
			 device   = 'png', 
			 bg       = "#18202D"
			 )





