
# Library -----------------------------------------------------------------
library(tidyverse)
library(rvest)
library(feather)

# Refs --------------------------------------------------------------------
my.url <- "https://comptroller.defense.gov/Budget-Materials/Budget2018/"
my.xpath <- '//*[@id="dnn_ctr92093_ContentPane"]'
my.xpath <- '#LiveHTMLWrapper87158 div'

timespan <- as.character(2019:2000)

#                2019    2018     2017     2016     2015     2014     2013    2012     2011
xpath.id <- c("92093", "87158", '77972', '65033', '59922', '58341',  '58365', '58366', "58369", 
              #  2010    2009     2008     2007     2006     2005     2004     2003     2002
              '58371',  '58373', '58375', '58380', '58383', '58385', '58387',  '58388', '58393',
              #  2001    2000
              "58395",  '58396'  )

refs <- tibble(FY = timespan,
               hyperlink = str_c("https://comptroller.defense.gov/Budget-Materials/Budget", timespan, "/"),
               xpath =  str_c('#LiveHTMLWrapper', xpath.id, ' div')) 


# Cleaning Function -------------------------------------------------------

cleaning.function <- function(my.url, my.xpath){
  x <- read_html(my.url) %>% 
          html_node(my.xpath) %>% 
          html_nodes("a") %>% 
          html_text() %>% 
          str_replace( "^$|^ $|^\\\n$", NA_character_) %>% str_trim()

  y <- read_html(my.url) %>%
    html_node(my.xpath) %>%
    html_nodes("a") %>%
    html_attr("href") 
  #%>% ifelse(grepl(pattern = "/Portals/", y),  paste0("https://comptroller.defense.gov",y),y)

  df <- bind_cols(link.title = x, hyperlink =y) %>%
  mutate(document.type = case_when(
    hyperlink %in% NA ~ link.title),
         file.type = case_when(
           str_detect(hyperlink, ".pdf") ~ "PDF",
           str_detect(hyperlink, ".xl")  ~ "Excel",
           str_detect(hyperlink, ".zip") ~ "Zip",
           TRUE ~ "Site")) %>%
  fill(document.type) %>%
  fill(link.title, .direction = "down") %>%
  filter(!is.na(hyperlink) & !(str_detect(document.type, "Links to Budget Materials"))) %>%
  distinct(hyperlink, .keep_all = T) %>%
  mutate( appropriation = case_when(
           str_detect(link.title, "O-1|0-1") ~ "O-1",
           str_detect(link.title, "M-1") ~ "M-1",
           str_detect(link.title, "P-1\\)") ~ "P-1",
           str_detect(link.title, "P-1R") ~ "P-1R",
           str_detect(link.title, "R-1") ~ "R-1",
           str_detect(link.title, "C-1") ~ "C-1",
           str_detect(link.title, "RF-1") ~ "RF-1",
           str_detect(link.title, "Overseas|Deterrence") ~ "OCO.Related",
           str_detect(link.title, "Emergency|Disaster") ~ "Emergency.Related"),
          sorting.id = row_number(),
           display.version = if_else(str_detect(hyperlink, "display"), "display.version", "not.display.version"),
          hyperlink = ifelse(grepl(pattern = "/Portals/", hyperlink),  paste0("https://comptroller.defense.gov",hyperlink),hyperlink)
          )
  return(df)
    }

# Apply Function ----------------------------------------------------------

refs <-  refs %>%  
  mutate(page.data = map2(hyperlink, xpath, cleaning.function)) %>% 
  unnest() %>% 
  rename(main.hyperlink = hyperlink, file.hyperlink = hyperlink1) %>% 
  group_by(FY) %>% 
  mutate(sorting.id = row_number()) %>% 
  mutate(amended.request.or.not = 
           if_else(any(str_detect(document.type, "Amend|Additional"))|
                   any(str_detect(link.title,    "Amend|Additional")), "docs.were.amended.this.FY", "docs.were.not.amended.this.FY")) %>% 
  ungroup()


# Export ------------------------------------------------------------------

#' Export for convenience to 'Intermediary' folder
 write_feather(refs, "./Data/Processed/refs.feather")

#' Export for web to 'Processed" folder
 write_csv(refs, "./Data/Processed/refs.csv")
























