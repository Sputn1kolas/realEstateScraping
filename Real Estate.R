install_dependencies<- function(){
  install.packages("RCurl")
  install.packages("xml2")
  install.packages("stringr")
  install.packages("tidyverse")
  devtools::install_github("r-lib/crayon")
}

library('rvest')
library(xml2)
library(stringr)
library(tidyverse)
library(crayon) devtools::install_github("r-lib/crayon")


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
find_image <- function(page){
  img_pattern <- paste("https://([:print:]*).jpg", sep = "")

  html_nodes(page, "#details-photos-slider > ul > li.lslide > img") %>% 
  html_attr("src") %>% 
  {.} -> img
  img
}
convert_time <- function(time){
 if(time < 60){
   major_time <- round(time)
   major_units <- "seconds"
   time_remaining_pretty   <-  paste(major_time, major_units)
  } else if(time < 3600 ){
    major_time  <- floor(time / 60)
    minor_time  <- round(time %% 60)
    major_units <- "minutes"
    minor_units <- "seconds"
    time_remaining_pretty <- paste(major_time, major_units,",", minor_time, minor_units)
  } else {
    major_time  <- floor(time / 3600)
    minor_time  <- round((time %% 3600)/60)
    major_units <- "hours"
    minor_units <- "minutes"
    time_remaining_pretty <- paste(major_time, major_units,",", minor_time, minor_units)
  }
  time_remaining_pretty
}

scrape_point2home <- function(start_page, end_page, city, short_province, save = TRUE) {
  
  print(paste("------------------------Finding Housing Data for ",city,"------------------------",sep=""))
  print(" ")
  
  #creates and exmpty dataframe with values to be filled, there may be an easier way to do this
  results <- data.frame(Address = "", Postal_code="",  Description="", Price = "", Square_feet = "", Beds = "", Baths = "", Date_Added ="", Year_built ="", Neighbourhood="", Type_of_home="", Area_of_city = "", Lot_info = "", Id_code = "", Img_uri = "")
  
  # creates a folder to save the file, with the date, province and city
  folder_name <- paste(Sys.Date(),"-",city,"-",short_province, sep = "")
  
  if(save == TRUE){
    dir.create(folder_name)
    print(paste("Created folder: ",folder_name,". Downloaded images and csv will save to this folder", sep = ""))
  }
  
  # this code is for estimating the time to complete
  counter       <- 1
  scrape_times  <- c()
  num_records   <- 0
  calc_progress <- function(time_elapsed){
    scrape_times[counter]   <<-  time_elapsed
    counter                 <<- counter + 1
    percent                 <- round((counter / num_records)*100)
    average_scrape_time  <- mean(scrape_times)
    total_time_elapsed   <- convert_time(sum(scrape_times))
    records_remaining     <- num_records - counter
    time_remaining        <- records_remaining * average_scrape_time
    time_remaining_pretty  <-convert_time(time_remaining)
  
    # will only print every 5 records, %% is the modulus operator in R 
    if(counter %% 5 == 0){
      print(paste(counter,"/",num_records," (",percent,"%) ",time_remaining_pretty," remaining. ","Total elasped time: ", total_time_elapsed, sep = ""))
    }
  }
  
  # loops through the number of pages in the website, to extract content from each page
  for(j in start_page:end_page) {
    print(paste("Scraping page",j))
    
    url <- paste('https://www.point2homes.com/CA/New-Listings/',short_province,'/',city,'.html?location=', city ,'%2C+',short_province,'&search_mode=location&page=',j,sep="")
    address <- postal_code <-  description <- price <- square_feet <- beds <- baths <- date_added <- year_built <- neighbourhood <- type_of_home <- area_of_city <- NA
    
    webpage <- read_html(url)
    
    classes <- "#search-results-list > div > ul"
    html_nodes(webpage,classes) %>% 
    {.} -> HTML_list_containing_ids
    id <- "id=\"l_([0-9]{8})"
    id_full <- str_extract_all(as.character(HTML_list_containing_ids[1]), id) 
    code <- "l_([0-9]{8})"
    all_ids_on_page  <- str_extract_all(id_full,code)
    num_ids         <- length(all_ids_on_page[[1]])
    
    if(num_records == 0){
      num_records <- num_ids * (end_page - start_page + 1)
    }
    #loops through all house ID's on page, and returns information for that ID
    for(i in 1:num_ids) {
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
        
        img_uri <- find_image(house_webpage)
        if(save == TRUE && length(as.character(img_uri)) >= 1){
          download.file(as.character(img_uri), destfile = paste("./",folder_name,"/",id_code,".jpeg", sep =""), method ='curl', quiet = TRUE) 
        }
        if(length(as.character(img_uri)) < 1){
          img_uri <- NA
        }
        
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
              {.} -> postal_code
              postal_code_length <- length(postal_code)
              postal_code <<- postal_code[[postal_code_length]]  
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
       
        houses_df <- data.frame(Address = address, Postal_code=postal_code,  Description=description, Price = price, Square_feet = square_feet, Beds = beds, Baths = baths, Date_Added =date_added, Year_built =year_built, Neighbourhood=neighbourhood, Type_of_home=type_of_home, Area_of_city = area_of_city, Lot_info = lot_info, Id_code = id_code, Img_uri = img_uri)
        results <- rbind(results, houses_df)
        stop_time <- Sys.time()
        time_elapsed <- stop_time - start_time
        calc_progress(time_elapsed)
        # print(paste("adding ", address,"---- This took:", time_elapsed, "seconds"))
        
      } 
    }s
  }
  if(save == TRUE){
    write.csv(results[2:nrow(results),], paste("./",folder_name,"/",folder_name,".csv", sep =""))
  }
  print("")
  print(paste("--------------------------------DONE!--------------------------------"))
  results[2:nrow(results),]
}

housing_data <- scrape_point2home(1,40, "Calgary", "AB", TRUE)
View(housing_data)

# housing_data <- read.csv(file = "Google Drive/Courses/Predictive Analytics/untitled folder/housing_data.csv")