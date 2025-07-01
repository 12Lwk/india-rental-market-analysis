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

#-------------------------------------------------------------------------------------------
#### Data Exploration ####
#-------------------------------------------------------------------------------------------

library(ggplot2)
#One Continuous and One Continuous 
ggplot(House_Rent_data, aes(x=Area, y=Rental_Price)) + geom_point()

unique_count <- unique(House_Rent_data$Flooring)
print(unique_count)

#--------------------------------------------------------------------------------------------------------
#-Analysis 4.1 How does the choice of different types of floors impact the changes in rental prices?-----
#--------------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
options(scipen = 999)
ggplot(House_Rent_data, aes(x=Floor_Category, y=Rental_Price, fill=Floor_Category)) +
  geom_boxplot() + 
  ggtitle(bquote(bold("Comparing Floor Categories to Rental Prices"))) +
  xlab("Flooring Category") + ylab("Rental Price") +
  scale_fill_discrete(name="Flooring Category") +
  theme_minimal() +
  labs(caption = "Lower Floor: Basement, Upper basement, Ground, Floors 1 to 10 \nMedium Floor: Floors 10 to 20 floor\nUpper Floor: Floors 21 and above") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, unit = "pt")))

Reg_Floors = lm(Rental_Price ~ Floor_Category, data = House_Rent_data)
summary(Reg_Floors)

#-------------------------------------------------------------------------------------------------------------------------------------------------
#-Analysis 4.9 How do different types of floors affect the rental prices based on whether the properties are furnished or not?--------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data, aes(x = Floor_Category, y = Rental_Price, fill = Furnishing_Status)) +
  geom_boxplot() +
  ggtitle(bquote(bold("Comparing Floor Categories to Rental Prices based on Furnishing Status"))) +
  xlab("Floor Category") +
  ylab("Rental Price") +
  scale_fill_discrete(name = "Furnishing Status") +
  theme_minimal() +
  labs(caption = "Lower Floor: Basement, Upper basement, Ground, Floors 1 to 10 \nMedium Floor: Floors 10 to 20 floor\nUpper Floor: Floors 21 and above") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, unit = "pt")))

Reg_Furnishing <- lm(Rental_Price ~ Floor_Category + Furnishing_Status, data = House_Rent_data)
summary(Reg_Furnishing)

#-------------------------------------------------------------------------------------------------------------------------
#----Analysis 4.17 How does floor categories relate to rental prices based on the area size?-------------------------
#-------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

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
ggplot(data = House_Rent_data, aes(x = Floor_Category, y = Rental_Price, fill = ifelse(Area <= 1000, "Small Area", ifelse(Area <= 2000, "Medium Area", "Large Area")))) +
  geom_boxplot() +
  ggtitle(bquote(bold("Comparison of Rental Prices by Area Size"))) +
  xlab("Floor Category") + ylab("Rental Price") +
  labs(fill = "Area Size") +
  theme_minimal() +
  labs(caption = "Lower Floor: Basement, Upper basement, Ground, Floors 1 to 10 \nMedium Floor: Floors 10 to 20 floor\nUpper Floor: Floors 21 and above") +
  theme(plot.caption = element_text(hjust = 0, margin = margin(t = 10, unit = "pt")))

Reg_AreaSize <- lm(Rental_Price ~ Floor_Category + area_categories, data = House_Rent_data)
summary(Reg_AreaSize)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.34 Exploring the Impact of Clustering Flooring Category, Area Size, and Furnishing Status on Rental Prices----
#--------------------------------------------------------------------------------------------------------
House_Rent_data <- House_Rent_data %>%
  mutate(Cluster = case_when(
    Floor_Category == "Lower Floor" & Furnishing_Status == "Unfurnished" & Area <= 1000 ~ "Cluster 1",
    Floor_Category == "Lower Floor" & Furnishing_Status == "Unfurnished" & Area > 1000 & Area <= 2000 ~ "Cluster 2",
    Floor_Category == "Lower Floor" & Furnishing_Status == "Unfurnished" & Area > 2000 ~ "Cluster 3",
    Floor_Category == "Lower Floor" & Furnishing_Status == "Semi-Furnished" & Area <= 1000 ~ "Cluster 4",
    Floor_Category == "Lower Floor" & Furnishing_Status == "Semi-Furnished" & Area > 1000 & Area <= 2000 ~ "Cluster 5",
    Floor_Category == "Lower Floor" & Furnishing_Status == "Semi-Furnished" & Area > 2000 ~ "Cluster 6",
    Floor_Category == "Lower Floor" & Furnishing_Status == "Furnished" & Area <= 1000 ~ "Cluster 7",
    Floor_Category == "Lower Floor" & Furnishing_Status == "Furnished" & Area > 1000 & Area <= 2000 ~ "Cluster 8",
    Floor_Category == "Lower Floor" & Furnishing_Status == "Furnished" & Area > 2000 ~ "Cluster 9",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Unfurnished" & Area <= 1000 ~ "Cluster 10",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Unfurnished" & Area > 1000 & Area <= 2000 ~ "Cluster 11",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Unfurnished" & Area > 2000 ~ "Cluster 12",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Semi-Furnished" & Area <= 1000 ~ "Cluster 13",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Semi-Furnished" & Area > 1000 & Area <= 2000 ~ "Cluster 14",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Semi-Furnished" & Area > 2000 ~ "Cluster 15",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Furnished" & Area <= 1000 ~ "Cluster 16",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Furnished" & Area > 1000 & Area <= 2000 ~ "Cluster 17",
    Floor_Category == "Medium Floor" & Furnishing_Status == "Furnished" & Area > 2000 ~ "Cluster 18",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Unfurnished" & Area <= 1000 ~ "Cluster 19",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Unfurnished" & Area > 1000 & Area <= 2000 ~ "Cluster 20",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Unfurnished" & Area > 2000 ~ "Cluster 21",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Semi-Furnished" & Area <= 1000 ~ "Cluster 22",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Semi-Furnished" & Area > 1000 & Area <= 2000 ~ "Cluster 23",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Semi-Furnished" & Area > 2000 ~ "Cluster 24",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Furnished" & Area <= 1000 ~ "Cluster 25",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Furnished" & Area > 1000 & Area <= 2000 ~ "Cluster 26",
    Floor_Category == "Upper Floor" & Furnishing_Status == "Furnished" & Area > 2000 ~ "Cluster 27",
    
    TRUE ~ "Other"
  ))

options(scipen = 999)
ggplot(data = House_Rent_data %>%
         filter(Cluster %in% c("Cluster 1", "Cluster 10", "Cluster 11", "Cluster 12", "Cluster 13", "Cluster 14")), 
       aes(x = Area, y = Rental_Price, color = Cluster, shape = Cluster)) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Scatterplot of Rental Prices by Area, Differentiated by Clusters") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Cluster", shape = "Cluster") +
  theme_minimal() +
  facet_wrap(~ Cluster, scales = "free", labeller = labeller(Cluster = c("Cluster 1" = "Lower Floor, Unfurnished, Small Size","Cluster 10" = "Medium Floor, Unfurnished, Small Size", "Cluster 11" = "Medium Floor, Unfurnished, Medium Size", "Cluster 12" = "Medium Floor, Unfurnished, Large Size", "Cluster 13" = "Medium Floor, Semi-Furnished, Small Size", "Cluster 14" = "Medium Floor, Semi-Furnished, Medium Size"
    )
  ))

library(dunn.test)
dunn_output <- dunn.test(House_Rent_data %>%filter(Cluster %in% c("Cluster 1", "Cluster 10", "Cluster 13", "Cluster 14")) %>% pull(Rental_Price),House_Rent_data %>% filter(Cluster %in% c("Cluster 1", "Cluster 10","Cluster 13", "Cluster 14")) %>% pull(Cluster))
print(dunn_output)
p_values <- data.frame(Cluster = dunn_output$comparisons, P_Value = dunn_output$P)
sort_clusters <- p_values[order(p_values$P_Value), ]
print(sort_clusters)

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

Anova_Kolkata_Factors <- aov(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Kolkata"))
summary(Anova_Kolkata_Factors)
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

Anova_Mumbai_Factors <- aov(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Mumbai"))
summary(Anova_Mumbai_Factors)
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

Anova_Bangalore_Factors <- aov(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Bangalore"))
summary(Anova_Bangalore_Factors)
#--------------------------------------------------------------------------------------------------------
#--Analysis 4.29 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price in Delhi----
#--------------------------------------------------------------------------------------------------------
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

ggplot(data = House_Rent_data %>% filter(Cities == "Delhi"), aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status in Delhi") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Flooring Category", shape = "Furnishing Status") +
  theme_minimal() 

Anova_Delhi_Factors <- aov(Rental_Price ~ Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Delhi"))
summary(Anova_Delhi_Factors)

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

Anova_Chennai_Factors <- aov(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Chennai"))
summary(Anova_Chennai_Factors)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.30 Exploring Flooring Category, Area Size, Furnishing Status Impact on Rental Price in Hyderabad----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>% filter(Cities == "Hyderabad"), aes(x = Area, y = Rental_Price, color = factor(Floor_Category), shape = factor(Furnishing_Status))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Rental Prices by Area, Flooring Category, and Furnishing Status in Hyderabad") +
  xlab("Area") + ylab("Rental Price") +
  theme_minimal() 

Anova_Hyderabad_Factors <- aov(Rental_Price ~ Floor_Category + Area + Furnishing_Status, data = subset(House_Rent_data, Cities == "Hyderabad"))
summary(Anova_Hyderabad_Factors) 
