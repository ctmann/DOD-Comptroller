
# Header ------------------------------------------------------------------
#' Exercise: Downloading and Cleaning most recent year of data
#' from each appropriation .xlsx

# Library -----------------------------------------------------------------

library(tidyverse)
library(feather)
library(janitor)
library(readxl)

# Common Vars and Functions -----------------------------------------------
most_recent_year <- 2019

# Download ------------------------------------------------------------------
#' Create download tibble
x <- read_feather("./Data/Processed/refs.feather") %>% 
  filter(FY %in% most_recent_year,
         file.type %in% 'Excel',
         display.version %in% 'display.version') %>% 
  mutate(download.name = paste0("./Data/Raw/", clean_names(appropriation), ".xlsx" ))

#' download files to local data/raw subfolder 
#' (read_excel only reads local files)
Map(function(url, dest)
  download.file(url, dest), 
    x$file.hyperlink, 
    x$download.name)

# Import ------------------------------------------------------------------

# Add Sheet Names to tibble
x1 <- x %>% 
  mutate(sheets = map(download.name, excel_sheets)) %>% unnest()

  # How many sheets in each workbook?
    x1 %>% 
      group_by(appropriation) %>% 
      count()

# Import: Assume first sheet in each file is most important
x2 <- x1 %>% 
  group_by(appropriation) %>% 
  slice(1) %>% ungroup() %>% 
  mutate(my.data = map(download.name, ~(.x %>% read_excel(col_types="text",skip=1, col_names=TRUE, sheet=1) %>% 
                                          clean_names() ))) 
  
  #How many columns in each main sheet of nested df?
    x2 %>% 
      rowwise() %>% 
      mutate(number.of.cols = ncol(my.data) ) %>% 
      select(appropriation, sheets, number.of.cols)



  