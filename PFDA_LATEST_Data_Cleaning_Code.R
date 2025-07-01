# Import Dataset
House_Rent_data <- read.csv("C:\\Users\\User\\OneDrive - Asia Pacific University\\APU Programming for Data Analysis\\Assignment\\House_Rent_Dataset.csv", header = TRUE)


# Rename the headers in the House_Rent_data data frame
colnames(House_Rent_data) <- c("Date_Posted", "Bedroom_Amount", "Rental_Price", "Area", "Flooring", "Area_Type", "Area_Name", "Cities", "Furnishing_Status", "Targetted_Tenants", "Bathroom_Amount", "Contact_Point")
names(House_Rent_data)

View(House_Rent_data)

#-------------------------------------------------------------------------------------------
#### Data Cleaning ####
#-------------------------------------------------------------------------------------------
#Replace Contact Builder to Contact Agent
print(table(House_Rent_data$Contact_Point))
House_Rent_data$Contact_Point[House_Rent_data$Contact_Point == "Contact Builder"] <- "Contact Agent"

# ---- Remove Other Outliers ---- #
House_Rent_data = House_Rent_data [-c(3923,4654), ]

#remove the outliers that has a lot of bathrooms
House_Rent_data = subset (House_Rent_data, Bathroom_Amount < 8) 

#remove the outliers from Rental Price
House_Rent_data = subset (House_Rent_data, Rental_Price < 750000)
House_Rent_data = subset (House_Rent_data, Rental_Price > 1499)

#remove the outliers from Rental Price based on Area
num_rows_with_100000less <- sum(House_Rent_data$Rental_Price == 100000 & House_Rent_data$Area == 6000)
cat("Number of rows with the condition:", num_rows_with_100000less, "\n")
print(House_Rent_data[House_Rent_data$Rental_Price == 100000 & House_Rent_data$Area == 6000,])
House_Rent_data <- subset(House_Rent_data, !(Rental_Price == 100000 & Area == 6000))

#Remove outlier based on area size, 
House_Rent_data <- subset(House_Rent_data, !(Rental_Price == 180000 & Area == 950 & Furnishing_Status == "Carpet Area" & Furnishing_Status == "Unfurnished" & Cities == "Kolkata"))

#remove typo or numerical value
House_Rent_data  <- subset (House_Rent_data, Area_Name != "2 BHK")
House_Rent_data  <- subset (House_Rent_data, Area_Name != "5000")
House_Rent_data  <- subset (House_Rent_data, Area_Name != "700051")

#
num_rows_with_vettuvankeni <- sum(House_Rent_data == "Vettuvankeni")
cat("Number of rows with the condition:", num_rows_with_vettuvankeni, "\n")
print(House_Rent_data[House_Rent_data$Area_Name == "Vettuvankeni",])

House_Rent_data  <- subset (House_Rent_data, Area_Name != "Vettuvankeni")

nrow(House_Rent_data) 

#--------------------------------------------------------------------------------------------------------
#### Data Pre-Processing ####
#--------------------------------------------------------------------------------------------------------

#Data Processing Flooring
library(dplyr)

#CATEGORIZE FLOORS
categorize_floor <- function(floor) {
  front_number <- as.numeric(sub("^([0-9]+).*", "\\1", floor))
  
  if (grepl("^basement", floor, ignore.case = TRUE) || 
      grepl("^ground", floor, ignore.case = TRUE) || 
      grepl("^upper basement", floor, ignore.case = TRUE) ||
      grepl("^lower basement", floor, ignore.case = TRUE) ||
      front_number %in% 1:10) {
    return("Lower Floor")
  } else if (front_number %in% 11:20) {
    return("Medium Floor")
  } else {
    return("Upper Floor")
  }
}

House_Rent_data$Floor_Category <- sapply(House_Rent_data$Flooring, categorize_floor)

View(House_Rent_data)