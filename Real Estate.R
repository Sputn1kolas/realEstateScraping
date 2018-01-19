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

# Primary Function
scrape_point2home <- function(num_pages) {
  results <- data.frame(Address = "", Postal_code="",  Description="", Price = "", Square_feet = "", Beds = "", Baths = "", Date_Added ="", Year_built ="", Neighbourhood="", Type_of_home="", Area_of_city = "", Lot_info = "")
  
  for(j in 1:num_pages) {
    print(paste("Scraping the",j,"page"))
    url <- paste('https://www.point2homes.com/CA/New-Listings/AB/Calgary.html?location=Calgary%2C+AB&search_mode=location&page=',1,'&SelectedView=listings&LocationGeoId=437117&location_changed=&ajax=1',sep="")
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
      
      # Grabs each ID in the "
      id_code <- all_ids_on_page[[1]][i]
      
      
      # beds
      html_nodes(webpage,paste("#", id_code," > div.item-right-cnt > div.item-info-cnt > div.characteristics-cnt > ul > li:nth-child(1)", sep="")) %>% 
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
      html_pattern <- "/CA/Home-For-Sale/AB/Calgary/([:print:]*).html"
      # Link for more information
      html_nodes(webpage, paste("#", id_code, "> div.item-right-cnt > div.item-footer > div > div.inner-right", sep="" )) %>% 
      html_children(.) %>%
      str_extract(., html_pattern) %>% 
      {.} -> pathway
      house_url <- paste("https://www.point2homes.com", pathway[[1]], sep = "") 
      
      #  checks if the pathway is there, if so - follows and dl associated content
      if(is.na(pathway) == FALSE && pathway != "NA" ){
         house_webpage <- read_html(house_url)
    
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
                print("twoo")
                list_functions$Neighborhood(k,l)
                list_functions$PostalCode(k,l)
              }
            }
          }
        
        houses_df <- data.frame(Address = address, Postal_code=postal_code,  Description=description, Price = price, Square_feet = square_feet, Beds = beds, Baths = baths, Date_Added =date_added, Year_built =year_built, Neighbourhood=neighbourhood, Type_of_home=type_of_home, Area_of_city = area_of_city, Lot_info = lot_info)
        str(houses_df)
      } 
      
      print(paste("adding the house", address))
      results <- rbind(results, houses_df)
    }
  }
  results
}

housing_data <- scrape_point2home(1)
View(housing_data)

write.csv(housing_data, "housing_data.csv")
housing_data <- read.csv(file = "Google Drive/Courses/Predictive Analytics/untitled folder/housing_data.csv")

still_wtrong <- c("postal", "date added")

