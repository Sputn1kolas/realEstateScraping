#-------------------------------------- Packages--------------------------------------
load_dependencies <- function(){
  dependencies <- c("rvest", "xml2", "stringr", "dplyr", "purrr", "magrittr")
  new.packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(dependencies, require, character.only = TRUE)
}
load_dependencies()


#-------------------------------------- Teach --------------------------------------

require(rvest)
require(magrittr)
url  <- paste("https://www.century21.ca/search/Q-Calgary/51.353166072449156;-114.66533177400947;50.67147808633015;-113.51176732088447/list_dt~DESC/v_Gallery/page1",sep="")
selector <- "#body-wrapper > div > main > div.main-details-wrap > div > div.main-details-section > div.address > h1"
webpage <- read_html(url)
html_nodes(webpage, selector) %>% 
  html_text(.) %>% 
  {.} -> address

print(address)