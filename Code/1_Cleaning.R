
# Library -----------------------------------------------------------------
library(tidyverse)
library(rvest)

# Code --------------------------------------------------------------------
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


cleaning.function <- function(my.url, my.x.path){
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
           str_detect(link.title, "O-1") ~ "O-1",
           str_detect(link.title, "M-1") ~ "M-1",
           str_detect(link.title, "P-1\\)") ~ "P-1",
           str_detect(link.title, "P-1R") ~ "P-1R",
           str_detect(link.title, "R-1") ~ "R-1",
           str_detect(link.title, "C-1") ~ "C-1",
           str_detect(link.title, "RF-1") ~ "RF-1",
           str_detect(link.title, "Overseas|Deterrence") ~ "OCO.Related"),
          display.version = if_else(str_detect(hyperlink, "display"), "display.version", "not.display.version"),
          hyperlink = ifelse(grepl(pattern = "/Portals/", hyperlink),  paste0("https://comptroller.defense.gov",hyperlink),hyperlink)
          )

  return(df)
}

cleaning.function <- safely(cleaning.function)


cleaning.function(my.url,  my.xpath)


  map2_dfr(refs$hyperlink[1], refs$xpath[1], cleaning.function)

refs %>% map2_dfr(hyperlink[2], xpath[2], ~cleaning.function)


my.df <- refs %>% 
  mutate(page.data = map2(.$hyperlink, .$xpath, cleaning.function))

my.df %>% unnest() %>% View()

View(refs)





my.url <- "https://comptroller.defense.gov/Budget-Materials/Budget2019/"
my.xpath <- "#LiveHTMLWrapper92093 div"



cleaning.function(my.url, my.xpath)

identical(abrev.refs$hyperlink, my.url)
identical(abrev.refs$xpath, my.xpath)

abrev.refs <- refs[1:2,]

my.df <- abrev.refs %>% 
  mutate(page.data = map2(hyperlink, xpath, cleaning.function) )

cleaning.function(abrev.refs$hyperlink, abrev.refs$xpath)

































