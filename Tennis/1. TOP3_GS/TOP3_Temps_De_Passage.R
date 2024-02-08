
# Packages

library(gt)
library(tidyverse)
library(lubridate)


# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Tennis/1. TOP3_GS")




# Data

Data_GS <- read_delim("Data_GS.txt", 
    									delim = ";", 
											escape_double = FALSE, 
											trim_ws = TRUE)

View(Data_GS)




# Data processing

Birth_Federer  <-  as.Date("1981-08-08")
Birth_Nadal    <-  as.Date("1986-06-03")
Birth_Djokovic <-  as.Date("1987-05-22")


# New variables : age by year

Data_GS <-  Data_GS %>% mutate(Age_Federer  = lubridate::time_length(lubridate::interval(Birth_Federer,as.Date(paste(Year,"-01-01",sep=""))),'years'),
														   Age_Nadal    = lubridate::time_length(lubridate::interval(Birth_Nadal,as.Date(paste(Year,"-01-01",sep=""))),'years'),
														   Age_Djokovic = lubridate::time_length(lubridate::interval(Birth_Djokovic,as.Date(paste(Year,"-01-01",sep=""))),'years')
														   )

Data_GS[,6:8] <-  round(Data_GS[,6:8],digits = 1)
	

# Federer not active in 2022 & 2023

Data_GS$Age_Federer[Data_GS$Year %in% c(2022,2023)] <-  ""


# Replace NA in "" value

Data_GS <- Data_GS %>% mutate_if(is.character, ~replace_na(.,""))




# Build the table



Data_GS  %>%
	
	gt(id = "table") %>%
	
  tab_header(
    title = md("**Men's Singles Grand Slam Winners Since 2004* <br> <br>**")
  ) %>%
	
	tab_source_note(source_note = md(" <br> * Table very largely inspired by that of 'Lev Akabas - Sportico' <br> <br>")) %>%
	
	tab_footnote(footnote = md("Age calculated relative to the first day of each year <br> <br>"),
							 locations = cells_column_labels(
							 	columns = Age_Nadal)
							 ) %>%
    
	
  cols_label(Year = "",
             Australian_Open = "Australian Open",
             French_Open = "French Open",
             Wimbledon = "Wimbledon",
             US_Open = "US Open",
             Age_Federer = "",
             Age_Nadal = "Age",
             Age_Djokovic = "") %>% 
  cols_width(
    Year ~ px(100),
    Australian_Open ~ px(200),
    French_Open ~ px(200),
    Wimbledon ~ px(200),
    US_Open ~ px(200),
    Age_Federer ~ px(75),
    Age_Nadal ~ px(75),
    Age_Djokovic ~ px(75)
  ) %>% 
	
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
	
  tab_style(
    style = cell_fill(color = "#E8F4EE"),
    locations = cells_body(
      columns = Australian_Open,
      rows = Australian_Open == "Federer"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#15964f",weight = "bold"),
    locations = cells_body(
      columns = Australian_Open,
      rows = Australian_Open == "Federer"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#E8F4EE"),
    locations = cells_body(
      columns = French_Open,
      rows = French_Open == "Federer"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#15964f",weight = "bold"),
    locations = cells_body(
      columns = French_Open,
      rows = French_Open == "Federer"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#E8F4EE"),
    locations = cells_body(
      columns = Wimbledon,
      rows = Wimbledon == "Federer"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#15964f",weight = "bold"),
    locations = cells_body(
      columns = Wimbledon,
      rows = Wimbledon == "Federer"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#E8F4EE"),
    locations = cells_body(
      columns = US_Open,
      rows = US_Open == "Federer"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#15964f",weight = "bold"),
    locations = cells_body(
      columns = US_Open,
      rows = US_Open == "Federer"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#FCEDE7"),
    locations = cells_body(
      columns = Australian_Open,
      rows = Australian_Open == "Nadal"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#c41b1b",weight = "bold"),
    locations = cells_body(
      columns = Australian_Open,
      rows = Australian_Open == "Nadal"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#FCEDE7"),
    locations = cells_body(
      columns = French_Open,
      rows = French_Open == "Nadal"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#c41b1b",weight = "bold"),
    locations = cells_body(
      columns = French_Open,
      rows = French_Open == "Nadal"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#FCEDE7"),
    locations = cells_body(
      columns = Wimbledon,
      rows = Wimbledon == "Nadal"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#c41b1b",weight = "bold"),
    locations = cells_body(
      columns = Wimbledon,
      rows = Wimbledon == "Nadal"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#FCEDE7"),
    locations = cells_body(
      columns = US_Open,
      rows = US_Open == "Nadal"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#c41b1b",weight = "bold"),
    locations = cells_body(
      columns = US_Open,
      rows = US_Open == "Nadal"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#E9F3F9"),
    locations = cells_body(
      columns = Australian_Open,
      rows = Australian_Open == "Djokovic"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#1e6996",weight = "bold"),
    locations = cells_body(
      columns = Australian_Open,
      rows = Australian_Open == "Djokovic"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#E9F3F9"),
    locations = cells_body(
      columns = French_Open,
      rows = French_Open == "Djokovic"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#1e6996",weight = "bold"),
    locations = cells_body(
      columns = French_Open,
      rows = French_Open == "Djokovic"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#E9F3F9"),
    locations = cells_body(
      columns = Wimbledon,
      rows = Wimbledon == "Djokovic"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#1e6996",weight = "bold"),
    locations = cells_body(
      columns = Wimbledon,
      rows = Wimbledon == "Djokovic"
    )
  ) %>%
	
  tab_style(
    style = cell_fill(color = "#E9F3F9"),
    locations = cells_body(
      columns = US_Open,
      rows = US_Open == "Djokovic"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#1e6996",weight = "bold"),
    locations = cells_body(
      columns = US_Open,
      rows = US_Open == "Djokovic"
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#15964f",weight = "bold"),
    locations = cells_body(
      columns = Age_Federer
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#c41b1b",weight = "bold"),
    locations = cells_body(
      columns = Age_Nadal
    )
  ) %>%
	
  tab_style(
    style = cell_text(color = "#1e6996",weight = "bold"),
    locations = cells_body(
      columns = Age_Djokovic
    )
  ) %>%
	
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
	
   opt_css(
    css = "

#table .gt_col_headings {
	  border-top-style: hidden;
    border-top-width: 2px;
    border-top-color: #D3D3D3;
    border-bottom-style: solid;
    border-bottom-width: 3px;
    border-bottom-color: #000000;
    border-left-style: none;
    border-left-width: 1px;
    border-left-color: #D3D3D3;
    border-right-style: none;
    border-right-width: 1px;
    border-right-color: #D3D3D3;
}

#table .gt_table {
    display: table;
    border-collapse: collapse;
    margin-left: auto;
    margin-right: auto;
    color: #333333;
    font-size: 16px;
    font-weight: normal;
    font-style: normal;
    background-color: #FFFFFF;
    width: auto;
    border-top-style: hidden;
    border-top-width: 2px;
    border-top-color: #A8A8A8;
    border-right-style: none;
    border-right-width: 2px;
    border-right-color: #D3D3D3;
    border-bottom-style: solid;
    border-bottom-width: 2px;
    border-bottom-color: #A8A8A8;
    border-left-style: none;
    border-left-width: 2px;
    border-left-color: #D3D3D3;
}

"
) %>% gtsave("Table_GS.html")

webshot2::webshot("Table_GS.html",
									"Table_GS.png",
									vwidth = 1500)
	
	
	
	
	
	


















