library('rvest')
library(xml2)
library(stringr)
library(tidyverse)
setwd("../Google Drive/Courses/Predictive Analytics/Real Estate Scraping in Calgary/")


# Helper Functions
remove_list_non_numbers <- function(data_list) {
  # Loops through and removes context
  for(i in 1:length(data_list)) {
    data_list[i] <- remove_non_numbers(data_list[i])
  }
  data_list
}
remove_non_numbers      <- function(x) {
  z <-   as.numeric(gsub("[^\\d]+", "", x, perl=TRUE))
  z
}
remove_whitespace       <- function(x) {
  z <-   gsub(" ", "", x)
  z
}
remove_html_junk        <- function(x, keep_whitespace = FALSE) {
  z <-  gsub("\r\n", "", x)
  p <- gsub("\t", "", z)
  if(keep_whitespace != TRUE ){
    p <- remove_whitespace(p)
  }
  p
}
check_empty             <- function(data) {
  if(is_empty(data)){
    NA
  } else {
    data
  }
}

subpage <- ""
download_image <- function(page, id_code){
  img_pattern <- paste("https://([:print:]*).jpg", sep = "")
  subpage <<- page
  # Find the code for the img
  
  #details-photos-slider > div > div > ul > li.lslide.active > img
  html_nodes(subpage, "#details-photos-slider > ul > li.lslide > img") %>% 
  html_attr("src") %>% 
  {.} -> img
  imgs_directory <- paste("imgs_", Sys.Date(), sep="")
  
  # img <- as.character(str_extract(img, img_pattern))
  print(paste("IMG:",img))
  # downloads the file to the working directory
  download.file( img, destfile = basename(paste("../",imgs_directory,"/",id_code,".jpeg", sep ="")))
  
}

# Primary Function
scrape_point2home <- function(num_pages, city, short_province) {
  
  results <- data.frame(Address = "", Postal_code="",  Description="", Price = "", Square_feet = "", Beds = "", Baths = "", Date_Added ="", Year_built ="", Neighbourhood="", Type_of_home="", Area_of_city = "", Lot_info = "", Id_code = "")
  
  # dir.create(imgs_directory)
  
  for(j in 1:num_pages) {
    # to measure elapsed time
    
    print(paste("Scraping the",j,"page"))
    # short_province <- "AB"
    # city <- "calgary"
    # i <- 1
    url <- paste('https://www.point2homes.com/CA/New-Listings/',short_province,'/',city,'.html?location=', city ,'%2C+',short_province,'&search_mode=location&page=',num_pages,sep="")
    address <- postal_code <-  description <- price <- square_feet <- beds <- baths <- date_added <- year_built <- neighbourhood <- type_of_home <- area_of_city <- "BANANA"
    
    webpage <- read_html(url)
    
    classes <- "#search-results-list > div > ul"
    html_nodes(webpage,classes) %>% 
    {.} -> HTML_list_containing_ids
    id <- "id=\"l_([0-9]{8})"
    id_full <- str_extract_all(as.character(HTML_list_containing_ids[1]), id) 
    code <- "l_([0-9]{8})"
    all_ids_on_page  <- str_extract_all(id_full,code)
    
    #loops through all house ID's on page, and returns information for that ID
    for(i in 1:length(all_ids_on_page[[1]])) {
      start_time <- Sys.time()
      
      # Grabs  ID in the page to be used in namespacing the elements to be found
      id_code <- all_ids_on_page[[1]][i]
      
      
      # Find the code for the img
      html_nodes(webpage, paste("#", id_code," > div.photo-cnt > div > ul > li > a > img", sep="")) %>% 
        html_attr("src") %>% 
        {.} -> img
      
      # beds
      html_nodes(webpage, paste("#", id_code," > div.item-right-cnt > div.item-info-cnt > div.characteristics-cnt > ul > li:nth-child(1)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> beds
      
      # baths
      html_nodes(webpage,paste("#",id_code," > div.item-right-cnt > div.item-info-cnt > div.characteristics-cnt > ul > li:nth-child(2)", sep="")) %>% 
        html_text(.) %>%
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> baths
      
      html_nodes(webpage,paste("#",id_code," > div.item-right-cnt > div.item-info-cnt > div.characteristics-cnt > ul > li:nth-child(3)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> square_feet
      
      html_nodes(webpage,paste("#",id_code," > div.item-right-cnt > div.item-header-cnt > div.price > span.green > span:nth-child(1)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> price
      
      html_nodes(webpage,paste("#",id_code," > div.item-right-cnt > div.item-header-cnt > div.item-address > h2 > a > span > div", sep="")) %>% 
        html_text(.) %>% 
        str_sub(., 11, -11) %>% 
        check_empty(.) %>% 
        {.} -> address
      
      # this pattens matches any printable char [:print:], any number of times * 
      html_pattern <- paste("/CA/Home-For-Sale/",short_province,"/",city,"/([:print:]*).html", sep = "")
      # Link for more information
      html_nodes(webpage, paste("#", id_code, "> div.item-right-cnt > div.item-footer > div > div.inner-right", sep="" )) %>% 
      html_children(.) %>%
      str_extract(., html_pattern) %>% 
      {.} -> pathway
      house_url <- paste("https://www.point2homes.com", pathway[[1]], sep = "") 
      
      #  checks if the pathway is there, if so - follows and dl associated content
      if(is.na(pathway) == FALSE && pathway != "NA" ){
         house_webpage <- read_html(house_url)
         
         download_image(house_webpage, id_code)
          
         list_functions <- list(
          "YearBuilt"     = function(k,l){
              html_nodes(house_webpage,paste("#details_info > div:nth-child(",k,") > div > dl:nth-child(",l,") > dd", sep ="")) %>%   #  
              html_text(.) %>%         
              check_empty(.) %>% 
              {.} ->> year_built
           },
          "PropertyType"  = function(k,l){
              html_nodes(house_webpage,paste("#details_info > div:nth-child(",k,") > div > dl:nth-child(",l,") > dd", sep ="")) %>% 
              html_text(.) %>% 
              remove_html_junk(.) %>% 
              check_empty(.) %>% 
              {.} ->> type_of_home
          },
          "Neighborhood"  = function(k,l){
              html_nodes(house_webpage,paste("#details_info > div:nth-child(",k,") > div > dl:nth-child(",l,") > dd", sep ="")) %>% 
              html_text(.) %>% 
              remove_html_junk(.) %>% 
              check_empty(.) %>% 
              {.[[1]]} ->> neighbourhood
            },
          "Lotinfo"       = function(k,l){
              html_nodes(house_webpage,paste("#details_info > div:nth-child(",k,") > div > dl:nth-child(",l,") > dd", sep ="")) %>% 
              html_text(.) %>% 
              check_empty(.) %>% 
              {.} ->> lot_info
          },
          "PostalCode"    = function(k,l){
              html_nodes(house_webpage,paste("#details_info > div:nth-child(",k,") > div > dl:nth-child(",l,") > dd", sep ="")) %>% 
                html_text(.) %>% 
                remove_html_junk(.) %>% 
                check_empty(.) %>% 
                {.[[2]]} ->> postal_code
            },
          "DateAdded"     = function(k,l){
            html_nodes(house_webpage,paste("#details_info > div:nth-child(",k,") > div > dl:nth-child(",l,") > dd", sep ="")) %>% 
              html_text(.) %>% 
              remove_html_junk(., keep_whitespace = TRUE) %>% 
              check_empty(.) %>% 
              {.} ->> date_added
          }
         )
          
          html_nodes(house_webpage,"#details > div:nth-child(3) > div.description-full-cnt > div") %>% 
            html_text(.) %>% 
            check_empty(.) %>% 
            {.} -> description
          
          html_nodes(house_webpage,"#control_breadcrumbs > ul > li:nth-child(5) > span > a > span") %>% 
            html_text(.) %>% 
            check_empty(.) %>% 
            {.} -> area_of_city

        for(k in 5:6){
            for(l in 1:6){
              html_nodes(house_webpage,paste("#details_info > div:nth-child(",k,") > div > dl:nth-child(",l,") > dt", sep ="")) %>% 
                html_text(.) %>%     
                remove_html_junk(.) %>% 
                check_empty(.) %>% 
                {.} -> title
              if(is.na(title) == FALSE && pathway != "NA" && length(title) == 1 && is_empty(list_functions[[title]]) == FALSE) {
                list_functions[[title]](k,l)
              }
              if(length(title) == 2){
                list_functions$Neighborhood(k,l)
                list_functions$PostalCode(k,l)
              }
            }
          }
        
        houses_df <- data.frame(Address = address, Postal_code=postal_code,  Description=description, Price = price, Square_feet = square_feet, Beds = beds, Baths = baths, Date_Added =date_added, Year_built =year_built, Neighbourhood=neighbourhood, Type_of_home=type_of_home, Area_of_city = area_of_city, Lot_info = lot_info, Id_code = id_code)
        results <- rbind(results, houses_df)
        stop_time <- Sys.time()
        time_elapsed <- stop_time - start_time
        print(paste("adding ", address,"---- This took:", time_elapsed, "seconds"))
        
      } 
    }
  }
  results
}

test <- scrape_point2home(1, "Montreal", "QC")
housing_data <- scrape_point2home(1, "Calgary", "AB")
# View(housing_data)
# save_to_file()
# 
# 
# save_to_file <- function() {
#   write.csv(housing_data, "../housing_data.csv")
# }
# housing_data <- read.csv(file = "Google Drive/Courses/Predictive Analytics/untitled folder/housing_data.csv")


