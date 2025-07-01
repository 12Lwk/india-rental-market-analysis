#access data from Excel sheet
install.packages("readxl")
library(readxl)
test_dataset<-read_excel('C:\\Users\\User\\OneDrive - Asia Pacific University\\APU Programming for Data Analysis\\Lab\\testing.xlsx', sheet=1)

View(test_dataset)

# Rename the headers in the House_Rent_data data frame
colnames(test_dataset) <- c("Date_Posted", "Bedroom_Amount", "Rental_Price", "Area", "Flooring", "Area_Type", "Area_Name", "Cities", "Furnishing_Status", "Targetted_Tenants", "Bathroom_Amount", "Contact_Point")
names(test_dataset)

#-------------------------------------------------------------------------------------------
#@@@@@ Misspelling - Targetted Tenant @@@@@

#Use the unique function to filter the Type of Targetted_Tenants (Bachelors, Bachelors/Family & Family)
unique_targetted_tenants <- unique(House_Rent_data$Targetted_Tenants)
print(unique_targetted_tenants)

#Use the table function to count the occurrences of each type of Targeted_Tenants
tenant_counts <- table(House_Rent_data$Targetted_Tenants)
print(tenant_counts)

bachelors_count <- tenant_counts["Bachelors"]
bachelors_family_count <- tenant_counts["Bachelors/Family"]
family_count <- tenant_counts["Family"]

total_count <- bachelors_count + bachelors_family_count + family_count

print(paste("Total of Targeted Tenants count:", total_count))

#Result
#Bachelors = 830, Bachelors/Family = 3444, Family = 472
#Total = 4746
#Conclusion: There are no typos or misspellings in the Targeted_Tenants column
#-------------------------------------------------------------------------------------------
#@@@@@ Misspelling - Contact_Point @@@@@
unique_Contact_Point <- unique(House_Rent_data$Contact_Point)
print(unique_Contact_Point)

Contact_Point_counts <- table(House_Rent_data$Contact_Point)
print(Contact_Point_counts)

Specific_builder_rows <- subset(House_Rent_data, Contact_Point == "Contact Builder")
Specific_builder_rows
#Or can search from the search bar in table 

# Replace "Contact Builder" with "Contact Agent"
House_Rent_data$Contact_Point[House_Rent_data$Contact_Point == "Contact Builder"] <- "Contact Agent"

# Count occurrences of each type of Contact_Point
contact_point_counts <- table(House_Rent_data$Contact_Point)
print(contact_point_counts)

Agent_count <- contact_point_counts["Contact Agent"]
Owner_count <- contact_point_counts["Contact Owner"]

Total_of_Contact <- Agent_count + Owner_count

print(paste("Total of Contact Point count:", Total_of_Contact))

View(House_Rent_data)

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

#=========== Commas ===========

# Check for commas in the Area column
check_commas_rental_price <- grepl(",", House_Rent_data$Rental_Price)

# Check for commas in the Area column
check_commas_area <- grepl(",", House_Rent_data$Area)

# Print rows with commas in the "Rental_Price" column
if (any(check_commas_rental_price)){
  commas_rental_price <- House_Rent_data[has_commas_rental_price, ]
  print("Rows with commas in the 'Rental_Price' column:")
  print(commas_rental_price)
} else{
  print("No rows with commas in the Rental_Price column.")
}

# Print rows with commas in the "Area" column
if(any(check_commas_area)){
  commas_area <- House_Rent_data[has_commas_area, ]
  print("Rows with commas in the 'Area' column:")
  print(commas_area)
} else {
  print("No rows with commas in the Area column.")
}

#-------------------------------------------------------------------------------------------

#=========== Missing data =========== 

# Check which rows have missing values
rows_with_missing <- apply(is.na(House_Rent_data), 1, any)

# Print rows with missing values
if (any(rows_with_missing)) {
  rows_with_missing_data <- House_Rent_data[rows_with_missing, ]
  print("Rows with missing values:")
  print(rows_with_missing_data)
} else {
  print("No rows with missing values.")
}

#-------------------------------------------------------------------------------------------

#=========== Duplicate rows ===========

# Check which rows have duplicates
rows_with_duplicates <- duplicated(House_Rent_data)

# Print rows with duplicates
if (any(rows_with_duplicates)) {
  duplicate_rows_data <- House_Rent_data[rows_with_duplicates, ]
  print("Rows with duplicates:")
  print(duplicate_rows_data)
} else {
  print("No rows with duplicates.")
}

#-------------------------------------------------------------------------------------------

#Error

# Check which rows have duplicates
rows_with_duplicates <- duplicated(House_Rent_data)
rows_with_duplicates

# Display the rows that have duplicates
print(House_Rent_data[rows_with_duplicates, ])

# Avoid Duplication or duplicates
House_Rent_data <- unique(House_Rent_data)

#-------------------------------------------------------------------------------------------

#=========== Numerical Values Stored as text/character ===========

# Check for text/character in "Rental_Price" column
has_text_rental_price <- grepl("[A-Za-z]", House_Rent_data$Rental_Price)

# Check for text/character in "Area" column
has_text_area <- grepl("[A-Za-z]", House_Rent_data$Area)

# Check for text/character in "Date_Posted" column
has_text_date_posted <- grepl("[A-Za-z]", House_Rent_data$Date_Posted)

# Check for text/character in "Bathroom_Amount" column
has_text_bathroom_amount <- grepl("[A-Za-z]", House_Rent_data$Bathroom_Amount)

# Combine all logical vectors to find rows with text/character in any of the specified columns
rows_with_text <- has_text_rental_price | has_text_area | has_text_date_posted | has_text_bathroom_amount

# Print rows with text/character in any of the specified columns
if (any(rows_with_text)) {
  rows_with_text_data <- House_Rent_data[rows_with_text, ]
  print("Rows with text/character values:")
  print(rows_with_text_data)
} else {
  print("No rows with text/character values.")
}
#-------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(House_Rent_data, aes(x=Floor_Category, y=Rental_Price)) + geom_point()
ggplot(House_Rent_data, aes(x=Area, y=Rental_Price)) + geom_point()


small_area_condition <- House_Rent_data %>%
  filter(grepl("Ground | Basement| Upper Basement|^[1-10] ", Flooring),
         Area <= 1000)

medium_area_condition <- House_Rent_data %>%
  filter(grepl("Ground | Basement| Upper Basement|^[1-10] ", Flooring),
         Area > 1000 & Area <= 2000)

large_area_condition <- House_Rent_data %>%
  filter(grepl("Ground | Basement| Upper Basement|^[1-10] ", Flooring),
         Area > 2000)

#-------------------------------------------------------------------------------------------------------------------------
#----Analysis 4.11 What is the frequency of Furnshing Status for Each Floor Category?-----------------
#-------------------------------------------------------------------------------------------------------------------------
#----Frequency of each Furnishing Status----#

library(dplyr)
library(ggplot2)

ggplot(data = House_Rent_data, aes(x = Floor_Category, fill = Furnishing_Status)) +
  geom_bar(position = "dodge") +
  ggtitle("Furnishing Status Count for Each Floor Category") +
  xlab("Floor Category") + ylab("Count") +
  labs(fill = "Furnishing Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------------------------------------------------------------------------------------------
#----------Analysis 4.1 Comparing Floor Categories to Rental Prices-- -----------------------------------
#--------------------------------------------------------------------------------------------------------
library(ggplot2)
#pie(table(House_Rent_data$Floor_Category), labels = unique(House_Rent_data$Floor_Category), main = "Floor Category",
#col = c("red", "yellow", "green")[match(unique(House_Rent_data$Floor_Category), unique(House_Rent_data$Floor_Category))])

ggplot(House_Rent_data, aes(x = "", fill = Floor_Category)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Floor Categories") +
  scale_fill_discrete(name = "Flooring Category") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm")) 

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.3 Comparing Floor Categories to Average Rental Prices--
#--------------------------------------------------------------------------------------------------------

ggplot(average_rental_by_floor <- House_Rent_data %>%
         group_by(Floor_Category) %>%
         summarise(Average_Rental_Price = mean(Rental_Price)), aes(x = Floor_Category, y = Average_Rental_Price)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = paste(round(Average_Rental_Price, 0))), 
            vjust = -0.3, size = 4) +
  ggtitle("Comparing Floor Categories to Average Rental Prices across All Cities") +
  xlab("Floor Category") +
  ylab("Average Rental Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(average_rental_by_floor <- House_Rent_data %>%
         group_by(Cities, Floor_Category) %>%
         summarise(Average_Rental_Price = mean(Rental_Price)), aes(x = Floor_Category, y = Average_Rental_Price, fill = Cities)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Average_Rental_Price, 2))), vjust = -0.3, size = 3) +
  facet_wrap(vars(Cities), ncol = 2) +
  ggtitle("Comparing Floor Categories to Average Rental Prices across All Cities") +
  xlab("Floor Category") +
  ylab("Average Rental Price") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "yellow", "chocolate")) 

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.20 Comparing Floor Categories to Average Rental Prices based on Area Size--
#--------------------------------------------------------------------------------------------------------

ggplot(average_rental_by_floor_area <- House_Rent_data %>%
         mutate(Area_Size = case_when(
           Area <= 1000 ~ "Small Area",
           Area > 1000 & Area <= 2000 ~ "Medium Area",
           Area > 2000 ~ "Large Area"
         )) %>%
         group_by(Floor_Category, Area_Size) %>%
         summarise(Average_Rental_Price = mean(Rental_Price)),
       aes(x = Floor_Category, y = Average_Rental_Price, fill = Area_Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Average_Rental_Price, 0)), 
            position = position_dodge(width = 0.9),
            vjust = -1, size = 2) +
  ggtitle("Comparing Floor Categories to Average Rental Prices based on Area Size") +
  xlab("Floor Category") +
  ylab("Average Rental Price") +
  scale_fill_discrete(name = "Area Size") +
  theme_minimal() 

ggplot(average_rental_by_floor_area <- House_Rent_data %>%
         mutate(Area_Size = case_when(
           Area <= 1000 ~ "Small Area",
           Area > 1000 & Area <= 2000 ~ "Medium Area",
           Area > 2000 ~ "Large Area"
         )) %>%
         group_by(Cities, Floor_Category, Area_Size) %>%
         summarise(Average_Rental_Price = mean(Rental_Price)),
       aes(x = Floor_Category, y = Average_Rental_Price, fill = Area_Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Average_Rental_Price, 0)), 
            position = position_dodge(width = 0.9),
            vjust = -1, size = 2) +
  facet_wrap(vars(Cities), ncol = 2) +
  ggtitle("Comparing Floor Categories to Average Rental Prices based on Area Size across each city") +
  xlab("Floor Category") +
  ylab("Average Rental Price") +
  scale_fill_discrete(name = "Area Size") +
  theme_minimal() 

#--------------------------------------------------------------------------------------------------------------------------------------
#--Analysis 4.16 How does the combination of floor categories and furnishing status influence rental prices across each city?----------
#--------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data, aes(x = Floor_Category, y = Rental_Price, fill = Furnishing_Status)) +
  geom_boxplot() +
  facet_wrap(vars(Cities), ncol = 2) +
  ggtitle("Comparing Floor Categories to Rental Prices based on Furnishing Status across each city") +
  xlab("Floor Category") +
  ylab("Rental Price") +
  scale_fill_discrete(name = "Furnishing Status") +
  theme_minimal()

Reg_Furnishing_Cities <- lm(Rental_Price ~ Floor_Category + Furnishing_Status + Cities, data = House_Rent_data)
summary(Reg_Furnishing_Cities)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.31 Exploring Lower Floor, Area Size, Furnishing Status Impact on Rental Price----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>%
         filter(Floor_Category == "Lower Floor"), aes(x = Area, y = Rental_Price, color = factor(Furnishing_Status), shape = factor(Floor_Category))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Comparing Lower Floor, Area Size, Furnishing Status Impact on Rental Price") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Furnishing Status", shape = "Flooring Category") +
  theme_minimal()

Reg_Lower_All <- lm(Rental_Price ~ Area + Furnishing_Status, data = subset(House_Rent_data, Floor_Category == "Lower Floor"))
summary(Reg_Lower_All)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.32 Exploring Medium Floor, Area Size, Furnishing Status Impact on Rental Price----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>%
         filter(Floor_Category == "Medium Floor"), aes(x = Area, y = Rental_Price, color = factor(Furnishing_Status), shape = factor(Floor_Category))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Comparing Medium Floor, Area Size, Furnishing Status Impact on Rental Price") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Furnishing Status", shape = "Flooring Category") +
  theme_minimal()

Reg_Medium_All <- lm(Rental_Price ~ Area + Furnishing_Status, data = subset(House_Rent_data, Floor_Category == "Medium Floor"))
summary(Reg_Medium_All)

#--------------------------------------------------------------------------------------------------------
#--Analysis 4.33 Exploring Upper Floor, Area Size, Furnishing Status Impact on Rental Price----
#--------------------------------------------------------------------------------------------------------
ggplot(data = House_Rent_data %>%
         filter(Floor_Category == "Upper Floor"), aes(x = Area, y = Rental_Price, color = factor(Furnishing_Status), shape = factor(Floor_Category))) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggtitle("Clustered Scatterplot of Comparing Upper Floor, Area Size, Furnishing Status Impact on Rental Price") +
  xlab("Area") + ylab("Rental Price") +
  labs(color = "Furnishing Status", shape = "Flooring Category") +
  theme_minimal()

Reg_Upper_All <- lm(Rental_Price ~ Area + Furnishing_Status, data = subset(House_Rent_data, Floor_Category == "Upper Floor"))
summary(Reg_Upper_All)
