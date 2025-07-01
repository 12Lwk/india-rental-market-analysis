# Import Dataset
House_Rent_data <- read.csv("C:\\Users\\User\\OneDrive - Asia Pacific University\\APU Programming for Data Analysis\\Assignment\\House_Rent_Dataset.csv", header = TRUE)


# Rename the headers in the House_Rent_data data frame
colnames(House_Rent_data) <- c("Date_Posted", "Bedroom_Amount", "Rental_Price", "Area", "Flooring", "Area_Type", "Area_Name", "Cities", "Furnishing_Status", "Targetted_Tenants", "Bathroom_Amount", "Contact_Point")
names(House_Rent_data)

View(House_Rent_data)

#-------------------------------------------------------------------------------------------
#### Data Cleaning ####
#-------------------------------------------------------------------------------------------
House_Rent_data$Contact_Point[House_Rent_data$Contact_Point == "Contact Builder"] <- "Contact Agent"

# ---- Remove Other Outliers ---- #
House_Rent_data = House_Rent_data [-c(3923,4654), ]
House_Rent_data = subset (House_Rent_data, Bathroom_Amount < 8) #remove the outliers that has a lot of bathrooms
House_Rent_data = subset (House_Rent_data, Rental_Price < 750000)
House_Rent_data = subset (House_Rent_data, Rental_Price > 1499)

unique_count <- unique(House_Rent_data$Flooring)
print(unique_count)

House_Rent_data  <- subset (House_Rent_data, Area_Name != "2 BHK")
House_Rent_data  <- subset (House_Rent_data, Area_Name != "5000")
House_Rent_data  <- subset (House_Rent_data, Area_Name != "700051")

num_rows_with_vettuvankeni <- sum(House_Rent_data == "Vettuvankeni")
cat("Number of rows with the condition:", num_rows_with_vettuvankeni, "\n")
print(House_Rent_data[House_Rent_data$Area_Name == "Vettuvankeni",])

num_rows_with_100000less <- sum(House_Rent_data$Rental_Price == 100000 & House_Rent_data$Area == 6000)
cat("Number of rows with the condition:", num_rows_with_100000less, "\n")
print(House_Rent_data[House_Rent_data$Rental_Price == 100000 & House_Rent_data$Area == 6000,])

House_Rent_data  <- subset (House_Rent_data, Area_Name != "Vettuvankeni")
House_Rent_data <- subset(House_Rent_data, !(Rental_Price == 100000 & Area == 6000))
House_Rent_data <- subset(House_Rent_data, !(Rental_Price == 180000 & Area == 950))

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

#-------------------------------------------------------------------------------------------
#### Data Exploration ####
#-------------------------------------------------------------------------------------------
library(ggplot2)
summary(House_Rent_data$Rental_Price)

summary(House_Rent_data$Area)

#One Continuous and One Continuous 
ggplot(House_Rent_data, aes(x=Area, y=Rental_Price)) + geom_point()

#--------------------------------------------------------------------------------------------------------
#-Analysis 4.2 How does the choice of different types of floors impact the changes in rental prices in Kolkata City?----
#--------------------------------------------------------------------------------------------------------
library(ggplot2)

ggplot(House_Rent_data %>%
         filter(grepl("Kolkata", Cities, ignore.case = TRUE)) %>%
         select(Cities, Rental_Price, Floor_Category) %>%
         filter(Floor_Category %in% c("Lower Floor", "Medium Floor", "Upper Floor")), aes(x = Cities, y = Rental_Price, fill = Floor_Category)) +
  geom_boxplot() +
  ggtitle("Floor Category vs Rental Prices in Kolkata") +
  xlab("Cities") + ylab("Rental Price") +
  labs(fill = "Floor Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(summary(House_Rent_data$Rental_Price[grepl("Kolkata", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Lower Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Kolkata", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Medium Floor"]))

#--------------------------------------------------------------------------------------------------------
#-Analysis 4.3 How does the choice of different types of floors impact the changes in rental prices in Mumbai City?----
#--------------------------------------------------------------------------------------------------------

ggplot(House_Rent_data %>%
         filter(grepl("Mumbai", Cities, ignore.case = TRUE)) %>%
         select(Cities, Rental_Price, Floor_Category) %>%
         filter(Floor_Category %in% c("Lower Floor", "Medium Floor", "Upper Floor")), aes(x = Cities, y = Rental_Price, fill = Floor_Category)) +
  geom_boxplot() +
  ggtitle("Floor Category vs Rental Prices in Mumbai") +
  xlab("Cities") + ylab("Rental Price") +
  labs(fill = "Floor Category") 

Reg_Mumbai_Floor <- lm(Rental_Price ~ Floor_Category, data = subset(House_Rent_data, Cities == "Mumbai"))
summary(Reg_Mumbai_Floor)

print(summary(House_Rent_data$Rental_Price[grepl("Mumbai", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Lower Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Mumbai", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Medium Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Mumbai", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Upper Floor"]))

#--------------------------------------------------------------------------------------------------------
#-Analysis 4.4 How does the choice of different types of floors impact the changes in rental prices in Bangalore City?----
#--------------------------------------------------------------------------------------------------------

ggplot(House_Rent_data %>%
         filter(grepl("Bangalore", Cities, ignore.case = TRUE)) %>%
         select(Cities, Rental_Price, Floor_Category) %>%
         filter(Floor_Category %in% c("Lower Floor", "Medium Floor", "Upper Floor")), aes(x = Cities, y = Rental_Price, fill = Floor_Category)) +
  geom_boxplot() +
  ggtitle("Floor Category vs Rental Prices in Bangalore") +
  xlab("Cities") + ylab("Rental Price") +
  labs(fill = "Floor Category")

Reg_Bangalore_Floor <- lm(Rental_Price ~ Floor_Category, data = subset(House_Rent_data, Cities == "Bangalore"))
summary(Reg_Bangalore_Floor)

print(summary(House_Rent_data$Rental_Price[grepl("Bangalore", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Lower Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Bangalore", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Medium Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Bangalore", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Upper Floor"]))

#--------------------------------------------------------------------------------------------------------
#-Analysis 4.5 How does the choice of different types of floors impact the changes in rental prices in Delhi City?----
#--------------------------------------------------------------------------------------------------------
# *Lower Floor Only #

ggplot(House_Rent_data %>%
         filter(grepl("Delhi", Cities, ignore.case = TRUE)) %>%
         select(Cities, Rental_Price, Floor_Category) %>%
         filter(Floor_Category %in% c("Lower Floor", "Medium Floor", "Upper Floor")), aes(x = Cities, y = Rental_Price, fill = Floor_Category)) +
  geom_boxplot() +
  ggtitle("Floor Category vs Rental Prices in Delhi") +
  xlab("Cities") + ylab("Rental Price") +
  labs(fill = "Floor Category") 

print(summary(House_Rent_data$Rental_Price[grepl("Delhi", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Lower Floor"]))

filtered_Delhi_Floors <- House_Rent_data %>%
  filter(Cities == "Delhi" & 
           Floor_Category == "Lower Floor" & 
           grepl("Ground | Basement | Upper Basement |^[1-10]", Flooring))
ggplot(data = filtered_Delhi_Floors, aes(x = Flooring, fill = ..count..)) +
  geom_bar() +
  scale_fill_gradient(low = "lightblue", high = "purple") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=3) +  
  ggtitle("Frequency of Floors in Delhi") +
  xlab("Flooring Category") + ylab("Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Not Able to Regression

#--------------------------------------------------------------------------------------------------------
#-Analysis 4.6 How does the choice of different types of floors impact the changes rental prices in Chennai City?----
#--------------------------------------------------------------------------------------------------------
ggplot(House_Rent_data %>%
         filter(grepl("Chennai", Cities, ignore.case = TRUE)) %>%
         select(Cities, Rental_Price, Floor_Category) %>%
         filter(Floor_Category %in% c("Lower Floor", "Medium Floor", "Upper Floor")), aes(x = Cities, y = Rental_Price, fill = Floor_Category)) +
  geom_boxplot() +
  ggtitle("Floor Category vs Rental Prices in Chennai") +
  xlab("Cities") + ylab("Rental Price") +
  labs(fill = "Floor Category")

Reg_Chennai_Floor <- lm(Rental_Price ~ Floor_Category, data = subset(House_Rent_data, Cities == "Chennai"))
summary(Reg_Chennai_Floor)

print(summary(House_Rent_data$Rental_Price[grepl("Chennai", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Lower Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Chennai", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Medium Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Chennai", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Upper Floor"]))


#--------------------------------------------------------------------------------------------------------
#-Analysis 4.7 How does the choice of different types of floors impact the changes in rental prices in Hyderabad?----
#--------------------------------------------------------------------------------------------------------

ggplot(House_Rent_data %>%
         filter(grepl("Hyderabad", Cities, ignore.case = TRUE)) %>%
         select(Cities, Rental_Price, Floor_Category) %>%
         filter(Floor_Category %in% c("Lower Floor", "Medium Floor", "Upper Floor")), aes(x = Cities, y = Rental_Price, fill = Floor_Category)) +
  geom_boxplot() +
  ggtitle("Floor Category vs Rental Prices in Hyderabad") +
  xlab("Cities") + ylab("Rental Price") +
  labs(fill = "Floor Category") 

Reg_Hyderabad_Floor <- lm(Rental_Price ~ Floor_Category, data = subset(House_Rent_data, Cities == "Hyderabad"))
summary(Reg_Hyderabad_Floor)

print(summary(House_Rent_data$Rental_Price[grepl("Hyderabad", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Lower Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Hyderabad", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Medium Floor"]))
print(summary(House_Rent_data$Rental_Price[grepl("Hyderabad", House_Rent_data$Cities, ignore.case = TRUE) & House_Rent_data$Floor_Category == "Upper Floor"]))

#---------------------------------------------------------------------------------------------------------------------------------
#-Analysis 4.8 Are there significant differences in rental prices based on different floor categories across different cities?----
#---------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

options(scipen = 999)
ggplot(House_Rent_data, aes(x=Floor_Category, y=Rental_Price, fill=Cities)) +
  geom_boxplot() + 
  ggtitle("Flooring Category vs Rental Price across All Cities") +
  xlab("Flooring Category") + ylab("Rental Price") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_fill_discrete(name="Cities")

Reg_Floor_Cities = lm(Rental_Price ~ Floor_Category + Cities, data = House_Rent_data)
summary(Reg_Floor_Cities)

#--------------------------------------------------------------------------------------------------------------------------------------
#-Analysis 4.10 How does the combination of different flooring categories and furnishing statuses impact rental prices in Kolkata?----
#-------------------------------------------------------------------------------------------------------------------------------------

ggplot(data = House_Rent_data %>% filter(Cities == "Kolkata"), aes(x = Floor_Category, y = Rental_Price, fill = Furnishing_Status)) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Furnishing Status for Each Floor Category in Kolkata") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Furnishing Status") +
  theme_minimal() 

Reg_Kolkata_Furnishing <- lm(Rental_Price ~ Floor_Category + Furnishing_Status, data = subset(House_Rent_data, Cities == "Kolkata"))
summary(Reg_Kolkata_Furnishing)

###No result for Upper Floor, Unfurnished, Kolkata 

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.11 How does the furnishing status of properties interact with different floor categories to affect rental prices in Mumbai?----
#--------------------------------------------------------------------------------------------------------
options(scipen = 999)
ggplot(data = House_Rent_data %>% filter(Cities == "Mumbai"), aes(x = Floor_Category, y = Rental_Price, fill = Furnishing_Status)) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Furnishing Status for Each Floor Category in Mumbai") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Furnishing Status") +
  theme_minimal()

Reg_Mumbai_Furnishing <- lm(Rental_Price ~ Floor_Category + Furnishing_Status, data = subset(House_Rent_data, Cities == "Mumbai"))
summary(Reg_Mumbai_Furnishing)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.12 How do different flooring categories and their corresponding furniture status affect the variation of rental prices in Bangalore?----
#--------------------------------------------------------------------------------------------------------

ggplot(data = House_Rent_data %>% filter(Cities == "Bangalore"), aes(x = Floor_Category, y = Rental_Price, fill = Furnishing_Status)) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Furnishing Status for Each Floor Category in Bangalore") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Furnishing Status") +
  theme_minimal() 

Reg_Bangalore_Furnishing <- lm(Rental_Price ~ Floor_Category + Furnishing_Status, data = subset(House_Rent_data, Cities == "Bangalore"))
summary(Reg_Bangalore_Furnishing)

###No result for Upper Floor, Unfurnished, Bangalore 

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.13 How does the choice of different types of floors and their corresponding furnishing statuses relate to changes in rental prices in Delhi?----
#--------------------------------------------------------------------------------------------------------

ggplot(data = House_Rent_data %>% filter(Cities == "Delhi"), aes(x = Floor_Category, y = Rental_Price, fill = Furnishing_Status)) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Furnishing Status for Each Floor Category in Delhi") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Furnishing Status") +
  theme_minimal() 

###No result for All Medium Floor, Delhi 

###No result for All Upper Floor, Delhi 

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.14 How does the combination of flooring categories and furnishing statuses influence the variation in rental prices in Chennai?----
#--------------------------------------------------------------------------------------------------------

ggplot(data = House_Rent_data %>% filter(Cities == "Chennai"), aes(x = Floor_Category, y = Rental_Price, fill = Furnishing_Status)) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Furnishing Status for Each Floor Category in Chennai") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Furnishing Status") +
  theme_minimal() 

Reg_Chennai_Furnishing <- lm(Rental_Price ~ Floor_Category + Furnishing_Status, data = subset(House_Rent_data, Cities == "Chennai"))
summary(Reg_Chennai_Furnishing)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.15 How do various flooring categories and furnishing statuses interact to affect rental prices in Hyderabad?----
#--------------------------------------------------------------------------------------------------------

ggplot(data = House_Rent_data %>% filter(Cities == "Hyderabad"), aes(x = Floor_Category, y = Rental_Price, fill = Furnishing_Status)) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Furnishing Status for Each Floor Category in Hyderabad") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Furnishing Status") +
  theme_minimal() 

Reg_Hyderabad_Furnishing <- lm(Rental_Price ~ Floor_Category + Furnishing_Status, data = subset(House_Rent_data, Cities == "Hyderabad"))
summary(Reg_Hyderabad_Furnishing)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.18 How does the combination of flooring category and area size impact the rental prices in Kolkata?----
#--------------------------------------------------------------------------------------------------------
categorize_area <- function(area) {
  if (area <= 1000) {
    return("Small Area")
  } else if (area >1000 && area <= 2000) {
    return("Medium Area")
  } else {
    return("Large Area")
  }
}

area_categories <- sapply(House_Rent_data$Area, categorize_area)

options(scipen = 999)
ggplot(data = House_Rent_data %>% filter(Cities == "Kolkata"), aes(x = Floor_Category, y = Rental_Price, fill = ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")))) +
  geom_boxplot() +
  ggtitle(bquote(bold("Comparison of Rental Prices by Area Size for Each Floor Category in Kolkata"))) +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Area Size") +
  theme_minimal() 

Reg_Kolkata_AreaSize <- lm(Rental_Price ~ Floor_Category + ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")), data = subset(House_Rent_data, Cities == "Kolkata"))
summary(Reg_Kolkata_AreaSize)
#--------------------------------------------------------------------------------------------------------
#--Analysis 4.19 How does the interaction between flooring category and area size influence rental prices in Mumbai?----
#--------------------------------------------------------------------------------------------------------
options(scipen = 999)
ggplot(data = House_Rent_data %>% filter(Cities == "Mumbai"), aes(x = Floor_Category, y = Rental_Price, fill = ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")))) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Area Size for Each Floor Category in Mumbai") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Area Size") +
  theme_minimal() 

Reg_Mumbai_AreaSize <- lm(Rental_Price ~ Floor_Category + ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")), data = subset(House_Rent_data, Cities == "Mumbai"))
summary(Reg_Mumbai_AreaSize)                           

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.20 Does the comparison of flooring category and area size have an impact on rental prices in Bangalore?----
#--------------------------------------------------------------------------------------------------------
options(scipen = 999)
ggplot(data = House_Rent_data %>% filter(Cities == "Bangalore"), aes(x = Floor_Category, y = Rental_Price, fill = ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")))) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Area Size for Each Floor Category in Bangalore") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Area Size") +
  theme_minimal() 

Reg_Bangalore_AreaSize <- lm(Rental_Price ~ Floor_Category + ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")), data = subset(House_Rent_data, Cities == "Bangalore"))
summary(Reg_Bangalore_AreaSize) 

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.21 Does the relationship between flooring category and area size have an impact on rental prices in Delhi?----
#--------------------------------------------------------------------------------------------------------
options(scipen = 999)
ggplot(data = House_Rent_data %>% filter(Cities == "Delhi"), aes(x = Floor_Category, y = Rental_Price, fill = ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")))) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Area Size for Each Floor Category in Delhi") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Area Size") +
  theme_minimal() 

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.22 Does the combination of flooring category and area size influence rental prices in Hyderabad?----
#--------------------------------------------------------------------------------------------------------
options(scipen = 999)
ggplot(data = House_Rent_data %>% filter(Cities == "Hyderabad"), aes(x = Floor_Category, y = Rental_Price, fill = ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")))) +
  geom_boxplot() +
  ggtitle("Comparison of Rental Prices by Area Size for Each Floor Category in Hyderabad") +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Area Size") +
  theme_minimal() 

Reg_Hyderabad_AreaSize <- lm(Rental_Price ~ Floor_Category + ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")), data = subset(House_Rent_data, Cities == "Hyderabad"))
summary(Reg_Hyderabad_AreaSize)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.24 How does the combination of these factors reveal patterns or trends in rental price distribution?----
#--------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

options(scipen = 999)
ggplot(data = House_Rent_data, aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Flooring Category", shape = "Furnishing Status") +
  theme_minimal()

Reg_Cluster_All = lm(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = House_Rent_data)
summary(Reg_Cluster_All)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.26 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price in Kolkata----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>% filter(Cities == "Kolkata"), aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status in Kolkata") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Flooring Category", shape = "Furnishing Status") +
  theme_minimal()

Reg_Kolkata_All <- lm(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Kolkata"))
summary(Reg_Kolkata_All)
#--------------------------------------------------------------------------------------------------------
#--Analysis 4.27 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price in Mumbai----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>% filter(Cities == "Mumbai"), aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status in Mumbai") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Flooring Category", shape = "Furnishing Status") +
  theme_minimal() 

Reg_Mumbai_All <- lm(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Mumbai"))
summary(Reg_Mumbai_All)
#--------------------------------------------------------------------------------------------------------
#--Analysis 4.28 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price in Bangalore----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>% filter(Cities == "Bangalore"), aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status in Bangalore") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Flooring Category", shape = "Furnishing Status") +
  theme_minimal()

Reg_Bangalore_All <- lm(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Bangalore"))
summary(Reg_Bangalore_All)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.29 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price in Delhi----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>% filter(Cities == "Delhi"), aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status in Delhi") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Flooring Category", shape = "Furnishing Status") +
  theme_minimal()

#Not able to do regression

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.30 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price in Chennai----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>% filter(Cities == "Chennai"), aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status in Chennai") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Flooring Category", shape = "Furnishing Status") +
  theme_minimal() 

Reg_Chennai_All <- lm(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Chennai"))
summary(Reg_Chennai_All)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.30 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price in Hyderabad----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>% filter(Cities == "Hyderabad"), aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status in Hyderabad") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Flooring Category", shape = "Furnishing Status") +
  theme_minimal() 

Reg_Chennai_All <- lm(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Hyderabad"))
summary(Reg_Chennai_All)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.28 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price all Cities----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data, aes(x = Area, y = Rental_Price, color = Cities,
                                   shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status across Each City") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "City", shape = "Furnishing Status") +
  theme_minimal()

Reg_Cluster_All_Cities = lm(Rental_Price ~ Floor_Category + Area + Furnishing_Status + Cities, data = House_Rent_data)
summary(Reg_Cluster_All_Cities)