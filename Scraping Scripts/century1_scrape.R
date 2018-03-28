#-------------------------------------- Packages--------------------------------------
load_dependencies <- function(){
  dependencies <- c("rvest", "xml2", "stringr", "dplyr", "purrr", "magrittr")
  new.packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(dependencies, require, character.only = TRUE)
}
load_dependencies()

#-------------------------------------- Dependent Functions --------------------------------------

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

#-------------------------------------- Main Code--------------------------------------

# this takes pages, and location as aguments. It has a feature, where you can pick up and continue to add to 
# a previously saved file. The locaiton of this is specified on the 5th line of the function
scrape_century21 <- function(start_page, end_page, city, short_province, load_from_file = FALSE) {
  print(paste("------------------------Finding Housing Data for ",city,"------------------------",sep=""))
  print(paste("File will be saved in", getwd()))
  
  saved_file_location <- "../../Yelp - Assignment 2/Yelp- R project/century21_2018-03-08-Calgary-ab.csv"
  # empty dataframe for results to be added to.
  if(load_from_file == FALSE){
    results <<- data.frame()
  } else {
    print("Loading saved file")
    results <<- read.csv(saved_file_location)
  }
  
  # creates a folder to save the file, with the date, province and city
  folder_name <- paste(Sys.Date(),"-",city,"-",short_province, sep = "")

  # loops through the number of pages in the website, to extract content from each page
  for(j in start_page:end_page) {
    print(paste("Scraping page",j))
    # saves the file in the current directory, every page
    write.csv(results, paste("century21_",folder_name,".csv", sep =""))
    
    print(results[nrow(results),"Address"])
    
    
    url <- paste("https://www.century21.ca/search/Q-",city,"/51.353166072449156;-114.66533177400947;50.67147808633015;-113.51176732088447/list_dt~DESC/v_Gallery/page",j,sep="")
    webpage <- read_html(url)
    
    if(j == start_page){
     html_nodes(webpage, "div.sort-by-results-counter > span") %>% 
       html_text(.) %>%
       remove_list_non_numbers(.) %>%
       as.numeric(.) %>% 
       {.} -> num_results
       num_page <- floor(num_results/60)
       print(paste(num_results, "total records and", num_page, "pages than can be scraped"))
       print(paste("You are scraping page",start_page,"to page",end_page))
    }
    
    classes <- " div.list-actions > a:nth-child(2)"
    html_nodes(webpage, classes) %>% 
      html_attr(., "href") %>%
      {.} -> pathways
    
    # loops through all subpages collected from the link page, and scrapes it.
    for(i in 1:length(pathways)) {
      new_row <- nrow(results) + 1
      
      house_url <- paste("https://www.century21.ca", pathways[i], sep = "")

      # try catch is used, so the program will not stop if there is an error
      get_houseUrl <- tryCatch( house_webpage <<- read_html(house_url), error=function(e){ })
      get_houseUrl
      
      # --------------- Main Content --------------- 
      html_nodes(house_webpage,"#body-wrapper > div > main > div.main-details-wrap > div > div.main-details-section > div.address > h1") %>% 
        html_text(.) %>% 
        check_empty(.) %>% 
        {.} -> address
        results[new_row, "Address"] <- address

      html_nodes(house_webpage,"#body-wrapper > div > main > div.main-details-wrap > div > div.main-details-section > div.address > h2") %>% 
        html_text(.) %>% 
        check_empty(.) %>% 
        {.} -> postal_code
        results[new_row, "Postal_code"] <- postal_code
        
      if(is.na(postal_code) == TRUE || is.na(address) == TRUE){
        break
      }
      
      html_nodes(house_webpage, paste("#body-wrapper > div > main > div.main-details-wrap > div > div.main-details-section > div.price > h4", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> results[new_row, "Price"]
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > ul > li:nth-child(1)", sep="")) %>% 
        html_text(.) %>%
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> results[new_row, "Beds"]
      
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > ul > li:nth-child(2)", sep="")) %>% 
        html_text(.) %>%
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> results[new_row, "Baths_num"]
      
      html_nodes(house_webpage,"#body-wrapper > div > main > section.main-content > article > section.property-description") %>% 
        html_text(.) %>% 
        check_empty(.) %>% 
        {.} -> results[new_row, "Description"]
      
      # Find the code for the img
      html_nodes(house_webpage, paste("#PhotoViewer > span:nth-child(1) > img", sep="")) %>% 
        html_attr("src") %>%
        check_empty(.) %>% 
        {.} -> results[new_row, "Img_uri"]
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > ul > li:nth-child(3)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> results[new_row, "Square_feet"]
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > ul > li:nth-child(5)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        check_empty(.) %>% 
        {.} -> results[new_row, "Year_built"]
      
      html_nodes(house_webpage,paste("#body-wrapper > div > main > section.main-stats > div > span", sep="")) %>%
        html_text(.) %>%
        remove_list_non_numbers(.) %>%
        check_empty(.) %>%
        {.} -> results[new_row, "Id_code"]
      
      # --------------- Table Content --------------- 
      
      # these two groups content are listed in collumns, but are inconsistantly arranged. As such, you cannot find the content by position. 
      # Its easier to find the title (), and save the content to that data label.
      
      
      # listing_information  (rows and collumns, q controls row, and z collumn)
      for(q in 1:5){
        for(z in 1:3){
          
          collumn <- c("ul.middle", "ul.right", "ul.left")[z]
        
          html_nodes(house_webpage, paste("#body-wrapper > div > main > section.main-content > article > section.listing-information >", collumn ," > li:nth-child(",q,")")) %>%
            html_text(.) %>%
            remove_html_junk(.) %>%
            check_empty(.) %>%
            {.} -> listing_information
            title_pattern <- paste("[:alnum:]*", sep = "")
            title   <-  str_extract( listing_information, title_pattern)
            content <-  substring( listing_information, nchar(title) + 2, nchar(listing_information) )
            
            if(is.na(title) == FALSE){
              results[new_row, title] <- content
            }
          }
      }
      
      # Property features (many rows, no collumns) 
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
            results[new_row, "Neighbourhood"] <- content
          } else {
            results[new_row, title] <- content
          }
        }
      }
    }
  }
  print("")
  print(paste("--------------------------------DONE!--------------------------------"))
  View(results)
  results
}

#-------------------------------------- Wrapper to Start--------------------------------------

# Example call, to find 74 pages for calgary:
 # results <- scrape_century21(2,74,"Calgary","ab", FALSE)

# still gives werid results for montreal, qc...

# a wrapper to give a "ui" to the program
start <- function(){
  print("Welcome to the Century 21 web-scraper!")
  Sys.sleep(3)
  cat ("What province do you want scraped?")
  province <- readline()
  cat ("Great, and what city?")
  city <- readline()
  print(paste("Perfect. I'll find housing data from Century 21 for",city,",",province))
  print("Press cancel, and run start() again, if you made a mistake!")
  Sys.sleep(3)
  housing_data <- scrape_century21(1,10, city, province, FALSE)
}
