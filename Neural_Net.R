# Packages-------------------------------------------------------
install.packages("neuralnet")
install.packages("tidyverse")
library(tidyverse)
library(neuralnet)



housing_data <- read.csv(file = "Google Drive/Courses/Predictive Analytics/Real Estate Scraping in Calgary/housing_data.csv")
View(housing_data)
# Accessory Functions -------------------------------------------------------

  # adds everything NOT in the "drops" list to the merged data, effective dropping the inital collumn
  drop_collumn <- function(dataset, collumn) {
    drop <- c(collumn)
    dataset <- dataset[ , !( names(dataset) %in% drop )]
    dataset
  }
# Creates dummy variables for anything categorical -------------------------------------------------------

  # takes in the collumn and dataframe in question, and returns a dataframe with new variables
  create_dummmy_variables <- function(dataset, collumn) {
    dummy_variable <- factor(dataset[[collumn]])
    dummies        <- model.matrix( ~ dummy_variable)
    for(i in 1:ncol(dummies)) {
      collumn_name          <- colnames(dummies)[i]
      dataset[collumn_name] <- dummies[,i]
    }
    # merged_dataset <- merge(housing_data, dummies)
    dataset <- drop_collumn(dataset, collumn)
    dataset
  }

# Dimensionality reduction (min max) -------------------------------------------------------

  min_max_reduction <- function(dataset, list) {
    for(i in 1:length(list)) {
      collumn <-  list[i]
      data    <-  as.numeric(dataset[[collumn]])
      print(collumn)
      maxs    <-  max(data) 
      mins    <-  min(data)
      range   <-  maxs - mins
        
      scaler <- function(x){ 
        value  <- (x - mins) / range 
        value
      }
      scaled <- scaler(data)
      dataset[[collumn]] <- scaled
    }
    dataset
  }
  
# Dealing with Dates -------------------------------------------------------

  # this will calculate the age ()
  
   housing_data$Age_listing  <- as.numeric(Sys.Date() - as.Date(housing_data$Date_Added, format="%d-%b-%y"))

   housing_data$Age_house    <- as.numeric(format(Sys.Date(), "%Y")) - housing_data$Year_built
  

  # this will break up the date into _day, _month, _year


# Automate minmax reduction + date + dummy creation -------------------------------------------------------
   
   housing_data <- drop_collumn(housing_data,  "Address")
   housing_data <- drop_collumn(housing_data,  "Description")
   housing_data <- drop_collumn(housing_data,  "Postal_code")
   housing_data <-  min_max_reduction(housing_data, c("Price","Square_feet","Beds","Baths", "Date_Added", "Year_built", "Age_house", "Age_listing"))
   housing_data <- create_dummmy_variables(housing_data, "Neighbourhood")
   housing_data <- create_dummmy_variables(housing_data, "Type_of_home")
   housing_data <- create_dummmy_variables(housing_data, "Area_of_city")
   housing_data <- create_dummmy_variables(housing_data, "Lot_info")
    View(housing_data)
   
