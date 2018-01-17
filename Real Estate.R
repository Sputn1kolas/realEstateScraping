library('rvest')
library(xml2)
library(stringr)

# Helper Functions
remove_list_non_numbers <- function(data_list) {
  # Loops through and removes context
  for(i in 1:length(data_list)) {
    data_list[i] <- remove_non_numbers(data_list[i])
  }
  data_list
}
remove_non_numbers <- function(x) {
  z <-   as.numeric(gsub("[^\\d]+", "", x, perl=TRUE))
  z
}



scrape_point2home <- function() {
  results <- data.frame(Address = "", Price = "", Square_feet = "", Beds = "", Baths = "")
  
  for(j in 1:39) {
    print(paste("Scraping the",j,"page"))
    url <- paste('https://www.point2homes.com/CA/New-Listings/AB/Calgary.html?location=Calgary%2C+AB&search_mode=location&page=',1,'&SelectedView=listings&LocationGeoId=437117&location_changed=&ajax=1',sep="")
    
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
      
      # Grabs each ID in the "
      id_code <- all_ids_on_page[[1]][i]
      
      
      # beds
      html_nodes(webpage,paste("#", id_code," > div.item-right-cnt > div.item-info-cnt > div.characteristics-cnt > ul > li:nth-child(1)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        {.} -> beds
      
      # baths
      html_nodes(webpage,paste("#",id_code," > div.item-right-cnt > div.item-info-cnt > div.characteristics-cnt > ul > li:nth-child(2)", sep="")) %>% 
        html_text(.) %>%
        remove_list_non_numbers(.) %>% 
        {.} -> baths
      
      html_nodes(webpage,paste("#",id_code," > div.item-right-cnt > div.item-info-cnt > div.characteristics-cnt > ul > li:nth-child(3)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        {.} -> square_feet
      
      html_nodes(webpage,paste("#",id_code," > div.item-right-cnt > div.item-header-cnt > div.price > span.green > span:nth-child(1)", sep="")) %>% 
        html_text(.) %>% 
        remove_list_non_numbers(.) %>% 
        {.} -> price
      
      
      html_nodes(webpage,paste("#",id_code," > div.item-right-cnt > div.item-header-cnt > div.item-address > h2 > a > span > div", sep="")) %>% 
        html_text(.) %>% 
        str_sub(., 11, -11) %>% 
        {.} -> address
      houses_df <- data.frame(Address = address, Price = price, Square_feet = square_feet, Beds = beds, Baths = baths)
      print(houses_df)
      results <- rbind(results, houses_df)
    }
  }
  results
}

housing_data <- scrape_point2home()
write.csv(housing_data, "housing_data.csv")
housing_data$Price       <- as.numeric(housing_data$Price)
housing_data$Square_feet <- as.numeric(housing_data$Square_feet)
housing_data$Beds        <- as.numeric(housing_data$Beds)
housing_data$Baths       <- as.numeric(housing_data$Baths)


lm(Price ~ . -Address, housing_data)
