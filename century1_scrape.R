install_dependencies<- function(){
  install.packages("RCurl")
  install.packages("xml2")
  install.packages("stringr")
  install.packages("tidyverse")
  devtools::install_github("r-lib/async")
  
}

library('rvest')
library(xml2)
library(stringr)
library(tidyverse)
library(purrr)


# source http://economicdashboard.alberta.ca/

setwd("../Google Drive/Courses/Predictive Analytics/Real Estate Scraping in Calgary/")

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
  if(purrr::is_empty(data)){
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

scrape_century21 <- function(start_page, end_page, city, short_province, save = TRUE) {
  
  print(paste("------------------------Finding Housing Data for ",city,"------------------------",sep=""))
  print(" ")
  
  #creates and exmpty dataframe with values to be filled, there may be an easier way to do this
  
  # creates a folder to save the file, with the date, province and city
  folder_name <- paste(Sys.Date(),"-",city,"-",short_province, sep = "")
  
  # if(save == TRUE){
  #   dir.create(folder_name)
  #   print(paste("Created folder: ",folder_name,". Downloaded images and csv will save to this folder", sep = ""))
  # }
  
  # this code is for estimating the time to complete
  counter       <- 1
  scrape_times  <- c()
  num_records   <- 0
  calc_progress <- function(time_elapsed){
    scrape_times[counter]   <<-  time_elapsed
    counter                 <<- counter + 1
    percent                 <-  round((counter / num_records)*100)
    average_scrape_time     <-  mean(scrape_times)
    total_time_elapsed      <-  convert_time(sum(scrape_times))
    records_remaining       <-  num_records - counter
    time_remaining          <-  records_remaining * average_scrape_time
    time_remaining_pretty   <-  convert_time(time_remaining)
    
  }
  # loops through the number of pages in the website, to extract content from each page
  for(j in start_page:end_page) {
    print(paste("Scraping page",j))
    
    # creates null versions of the variables, if they are undefined
    address <- postal_code <-  description <- price <- square_feet <- beds <- baths <- date_added <- id_code <- year_built <- neighbourhood <- type_of_home <- area_of_city <- lot_info <- img_uri <- NA

    url <- paste("https://www.century21.ca/search/Q-",city,"/51.353166072449156;-114.66533177400947;50.67147808633015;-113.51176732088447/list_dt~DESC/v_Gallery/page",j,sep="")
  
    webpage <- read_html(url)
    
    classes <- " div.list-actions > a:nth-child(2)"
    html_nodes(webpage, classes) %>% 
      html_attr(., "href") %>% 
      {.} -> pathways
    
    #60 per page
    #loops through all house ID's on page, and returns information for that ID
    for(i in 1:length(pathways)) {
      results <- data.frame(Address = "", Postal_code="",  Description="", Price = "", Square_feet = "", Beds = "", Baths = "", Date_Added ="", Year_built ="", Neighbourhood="", Type_of_home="", Area_of_city = "", Id_code = "", Img_uri = "")
      
      start_time <- Sys.time()
      
      house_url <- paste("https://www.century21.ca", pathways[i], sep = "")
      #  checks if the pathway is there, if so - follows and dl associated content
      # house_webpage <- read_html(house_url)
      
      get_houseUrl <- tryCatch(  house_webpage <<- read_html(house_url), error=function(e){ print(e) })
      get_houseUrl

      html_nodes(house_webpage,"#body-wrapper > div > main > section.main-content > article > section.property-description") %>% 
        html_text(.) %>% 
        check_empty(.) %>% 
        {.} -> description
      
      html_nodes(house_webpage,"#body-wrapper > div > main > div.main-details-wrap > div > div.main-details-section > div.address > h1") %>% 
        html_text(.) %>% 
        check_empty(.) %>% 
        {.} -> address
      
      html_nodes(house_webpage,"#body-wrapper > div > main > div.main-details-wrap > div > div.main-details-section > div.address > h2") %>% 
        html_text(.) %>% 
        check_empty(.) %>% 
        {.} -> postal_code
      
      html_nodes(house_webpage, paste("#body-wrapper > div > main > div.main-details-wrap > div > div.main-details-section > div.price > h4", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> price
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > ul > li:nth-child(1)", sep="")) %>% 
        html_text(.) %>%
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> beds
      
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > ul > li:nth-child(2)", sep="")) %>% 
        html_text(.) %>%
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> baths
      
      # Find the code for the img
      html_nodes(house_webpage, paste("#PhotoViewer > span:nth-child(1) > img", sep="")) %>% 
        html_attr("src") %>% 
        {.} -> img_uri
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > ul > li:nth-child(3)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> square_feet
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > ul > li:nth-child(5)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> year_built
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > span", sep="")) %>%
        html_text(.) %>%
        remove_list_non_numbers(.) %>%
        check_empty(.) %>%
        {.} -> id_code
      
      # for(q in 1:15){
      #   # listing_information
      #   html_nodes(house_webpage, paste("#body-wrapper > div > main > section.main-content > article > section.listing-information > ul.middle > li:nth-child(",q,")")) %>% 
      #     html_text(.) %>% 
      #     remove_html_junk(.) %>% 
      #     check_empty(.) %>% 
      #     {.} -> listing_information 
      #   
      #     title_pattern <- paste("[:alnum:]*", sep = "")
      #     title   <-  str_extract( listing_information, title_pattern) 
      #     content <-  substring( listing_information, nchar(title) + 2, nchar(listing_information) ) 
      #   }
      
      for(q in 1:30){
        html_nodes(house_webpage, paste("#body-wrapper > div > main > section.main-content > article > section.property-features > div:nth-child(",q,") > strong", sep="")) %>% 
          html_text(.) %>% 
          remove_html_junk(.) %>% 
          check_empty(.) %>% 
          {.} -> title 
        html_nodes(house_webpage, paste("#body-wrapper > div > main > section.main-content > article > section.property-features > div:nth-child(",q,") > span", sep="")) %>% 
          html_text(.) %>% 
          remove_html_junk(.) %>% 
          check_empty(.) %>% 
          {.} -> content 
        if(is.na(title) == FALSE){
          if(title == "Community"){
            neighbourhood <<- content
          }
        }
      }
      # 
      # html_nodes(house_webpage,"#control_breadcrumbs > ul > li:nth-child(5) > span > a > span") %>% 
      #   html_text(.) %>% 
      #   check_empty(.) %>% 
      #   {.} -> area_of_city
      #   
      # 
        pick_first <- function(var){
          rows <- nrow(var)
          if(is.null(rows) != TRUE){
            print(var)
            var <<- var[1,]
          }
        }

        vars <- c(address, postal_code, description, price, square_feet, beds, baths, date_added, year_built, id_code, neighbourhood, type_of_home, area_of_city, img_uri)
        lapply(vars, pick_first)
        
        houses_df <- data.frame(Address = address, Postal_code=postal_code,  Description=description, Price = price, Square_feet = square_feet, Beds = beds, Baths = baths, Date_Added =date_added, Year_built =year_built, Id_code =id_code, Neighbourhood=neighbourhood, Type_of_home=type_of_home, Area_of_city = area_of_city, Img_uri = img_uri)
        
        results   <- rbind(results, houses_df)
        stop_time <- Sys.time()
        time_elapsed <- stop_time - start_time
        calc_progress(time_elapsed)
        # print(paste("adding ", address,"---- This took:", time_elapsed, "seconds"))
    }
    write.csv(results[2:nrow(results),], paste("c21_",folder_name,".csv", sep =""))
  }
  print("")
  print(paste("--------------------------------DONE!--------------------------------"))
  results[2:nrow(results),]
}

results <- scrape_century21(1,1,"calgary","ab")
View(results)
