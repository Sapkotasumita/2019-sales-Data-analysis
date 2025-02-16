library(dplyr)
library(tidyr)
library(lubridate)
library(tidygeocoder)
library(Hmisc)
library(ggplot2)
library(maps)
library(wordcloud)
library(RColorBrewer)

getwd()
setwd("")

files <- list.files(pattern = "Sales_.*\\.csv")

# Read and combine all datasets
data_list <- lapply(files, read.csv, header = TRUE)
data <- do.call(rbind, data_list)


#viewing data 
data


#write.csv(data, "data.csv", row.names = FALSE)


#initial data summary
cat("Number of columns:", ncol(data), "\n")
cat("Number of rows:", nrow(data), "\n")
cat("Column names:", colnames(data), "\n")


#Finding unique values in the data 
unique_count <- sapply(data, function(column) length(unique(column)))
unique_count
unique(data$Product)
unique(data$Quantity.Ordered)
unique(data$Price.Each)
unique(data$Purchase.Address)


head(data, n = 1)


#checking empty rows
empty_rows <- apply(data, 1, function(row) all(is.na(row)| row ==""))
which(empty_rows)
sum(empty_rows)
data_cleaned <- data[!empty_rows, ]
data_cleaned
nrow(data_cleaned)


#finding duplicated rows
new_dataset <- data_cleaned[!duplicated(data_cleaned), ]

# View the new dataset
print(new_dataset)
nrow(new_dataset)
nrow(data)

summary(new_dataset)
sapply(new_dataset, class)

new_dataset$Price.Each <- as.numeric(new_dataset$Price.Each)
new_dataset$Quantity.Ordered <- as.numeric(new_dataset$Quantity.Ordered)
new_dataset$Order.ID <- as.integer(new_dataset$Order.ID)
sum(is.na(new_dataset$Order.ID))
which(is.na(new_dataset$Order.ID))
sum(is.na(new_dataset$Price.Each))
sum(is.na(new_dataset$Quantity.Ordered))
sum(is.na(new_dataset$Order.Date))


new_dataset <- new_dataset[!is.na(new_dataset$Order.ID), ]


nrow(new_dataset)
sapply(new_dataset, class)




new_dataset$Order.Date <- as.POSIXct(new_dataset$Order.Date, format = "%m/%d/%Y %H:%M") 

head(new_dataset$Order.Date)
sum(is.na(new_dataset$Order.Date))


# Extract month and label it (e.g., "January", "February")
new_dataset$Month <- month(new_dataset$Order.Date, label = TRUE, abbr = FALSE)

# Extract the day of the week and label it (e.g., "Sunday", "Monday")
new_dataset$Day <- wday(new_dataset$Order.Date, label = TRUE, abbr = FALSE)

# Extract the hour of the day (24-hour format, 0-23)
new_dataset$Hour <- hour(new_dataset$Order.Date)

new_dataset$Date_Day <- day(new_dataset$Order.Date)
head(new_dataset$Month)
head(new_dataset$Day)
head(new_dataset$Hour)
head(new_dataset$Purchase.Address)

colnames(new_dataset)
new_dataset <- subset(new_dataset, select = -c( Order.Date))
colnames(new_dataset)




# Separate the Purchase.Address into Street, City, State, and Zip

new_dataset <- new_dataset %>%
  mutate(Purchase.Address = as.character(Purchase.Address)) %>%
  separate(Purchase.Address, into = c("Street", "City", "State_Zip"), sep = ", ", remove = FALSE) %>%
  mutate(State = gsub(" [0-9]{5}", "", State_Zip),    # Extract State
         Zip = gsub(".* ", "", State_Zip),          # Extract Zip Code
         State = trimws(State)) %>%                  # Clean up whitespace
  select(-State_Zip)  



new_dataset$Amount <- new_dataset$Quantity.Ordered * new_dataset$Price.Each

# Check the updated dataset
head(new_dataset)
length(unique(new_dataset$Zip))
new_dataset$CityAddress <- paste(new_dataset$City, new_dataset$State, sep = " ")


# Create a data frame with unique addresses
unique_addresses <- data.frame(CityAddress = unique(new_dataset$CityAddress))

# Geocode the unique addresses using Nominatim
geocoded_data <- unique_addresses %>%
  geocode(address = CityAddress, method = 'osm', lat = latitude, long = longitude)

# View the geocoded data
print(geocoded_data)


# Merge geocoded data back to the original dataset
new_dataset <- new_dataset %>%
  left_join(geocoded_data, by = "CityAddress")

# Function to calculate the mode
get_mode <- function(x) {
  uniq_x <- unique(x)  # Get unique values from the input vector
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Find the mode
}

product_mode <- get_mode(new_dataset$Product)
city_mode <- get_mode(new_dataset$City)
state_mode <- get_mode(new_dataset$State)
hour_mode <- get_mode(new_dataset$Hour)
day_mode <- get_mode(new_dataset$Day)
month_mode <- get_mode(new_dataset$Month)
quantity_ordered_mode <- get_mode(new_dataset$Quantity.Ordered)

# Print the results
cat("Mode of Product:", product_mode, "\n")
cat("Mode of City:", city_mode, "\n")
cat("Mode of State:", state_mode, "\n")
cat("Mode of Hour:", hour_mode, "\n")
cat("Mode of Day:", day_mode, "\n")
cat("Mode of Month:", month_mode, "\n")
cat("Mode of Quantity Ordered:", quantity_ordered_mode, "\n")

Amount_mean <- mean(new_dataset$Amount, na.rm = TRUE)
Price_mean <- mean(new_dataset$Price.Each, na.rm =TRUE)
cat("The mean of Amount is:", Amount_mean, "\n")
cat("The mean of Price Each is:", Price_mean, "\n")


Amount_sum <- sum(new_dataset$Amount, na.rm = TRUE)
Quantity_sum <- sum(new_dataset$Quantity.Ordered, na.rm = TRUE)
cat("The total sum of Amount is:", Amount_sum, "\n")
cat("The total sum of Quantity Ordered is:", Quantity_sum, "\n")


# Create the box plot using ggplot2
boxplot_gg <- ggplot(new_dataset, aes(y = Price.Each)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Box Plot of Price Each", y = "Price Each") +
  theme_minimal()

# View the box plot
print(boxplot_gg)

# Save the box plot
ggsave("boxplot_before_outliers.png", plot = boxplot_gg, width = 8, height = 6)


# Remove all rows with Price.Each greater than 150
new_dataset <- new_dataset[new_dataset$Price.Each <= 150, ]


# View the number of remaining rows in the dataset
n_remaining_rows <- nrow(new_dataset)
print(paste("Number of remaining rows:", n_remaining_rows))

# Box plot for Price.Each after removing rows with Price.Each > 150
# Create the box plot using ggplot2
boxplot_after_outliers <- ggplot(new_dataset, aes(y = Price.Each)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Box Plot of Price Each After Removing Outliers", y = "Price Each") +
  theme_minimal()

# View the box plot
print(boxplot_after_outliers)

# Save the box plot
ggsave("boxplot_after_outliers_removal.png", plot = boxplot_after_outliers, width = 8, height = 6)




# Remove Purchased.Address and CityAddress columns from new_dataset
new_dataset <- new_dataset[, !(names(new_dataset) %in% c("Purchase.Address", "CityAddress"))]










##Visualizations
# Calculate the average amount of sales by day of the week
# Calculate the total amount of sales by day of the week
total_amount_by_day <- new_dataset %>%
  group_by(Day) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
  arrange(Day)

# Print the total amounts
print(total_amount_by_day)

# Create the bar plot
p <- ggplot(total_amount_by_day, aes(x = Day, y = Total_Amount, fill = Day)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Amount of Sales by Day of the Week",
       x = "Day of the Week",
       y = "Total Amount") +
  theme_minimal() +
  theme(legend.position = "none")

# Save the plot with specified size (width and height in inches)
ggsave("total_sales_by_day.png", plot = p, width = 12, height = 8) 






avg_amount_by_month <- new_dataset %>%
  group_by(Month) %>%
  summarise(Average_Amount = mean(Amount, na.rm = TRUE)) %>%
  arrange(Month)

# Create the line plot
p_month_line <- ggplot(avg_amount_by_month, aes(x = Month, y = Average_Amount, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Line color and thickness
  geom_point(color = "red", size = 2) +  # Points on the line
  labs(title = "Average Amount of Sales by Month",
       x = "Month",
       y = "Average Amount") +
  theme_minimal()

# Save the plot with specified size
ggsave("average_sales_by_month_line.png", plot = p_month_line, width = 12, height = 8)  # Adjust size as needed

# Display the plot
print(p_month_line)









# Ensure longitude and latitude are present in the dataset
if(!("longitude" %in% colnames(new_dataset)) || !("latitude" %in% colnames(new_dataset))) {
  stop("Longitude and Latitude columns must be present in the dataset.")
}


# Create a base map of the United States
usa_map <- map_data("state")

# Create the spatial visualization
p_sales_map <- ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "#EDE8DC", color = "gray80") +  # Base map with lighter borders
  geom_point(data = new_dataset, aes(x = longitude, y = latitude, size = Amount, color = Amount), alpha = 0.7) +  # Sales data points
  scale_color_gradient(low = "blue", high = "red") +  # Color gradient for Amount
  labs(title = "Sales Amount per Area in the United States",
       x = "Longitude",
       y = "Latitude",
       size = "Sales Amount",
       color = "Sales Amount") +
  theme_minimal() +
  theme(legend.position = "right")

# Save the plot with specified size
ggsave("sales_amount_per_area_usa_map_no_labels.png", plot = p_sales_map, width = 20, height = 12)  # Adjust size as needed

# Display the plot
print(p_sales_map)





# Summarize total amount by state
total_amount_by_state <- new_dataset %>%
  group_by(State) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(Total_Amount))  # Arrange in descending order

# Create a bar plot for total amount by state
p_total_amount_state <- ggplot(total_amount_by_state, aes(x = reorder(State, Total_Amount), y = Total_Amount, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Amount of Sales by State",
       x = "State",
       y = "Total Amount") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()  # Flip coordinates for better readability

# Save the plot with specified size
ggsave("total_amount_sales_by_state.png", plot = p_total_amount_state, width = 20, height = 12)






# Display the plot
print(p_total_amount_state)

top_products <- new_dataset %>%
  group_by(Product) %>%
  summarise(Total_Quantity = sum(Quantity.Ordered, na.rm = TRUE)) %>%
  arrange(desc(Total_Quantity)) %>%
  top_n(5)  # Get top 5 products

# Create a bar plot for top 5 products
p_top_products <- ggplot(top_products, aes(x = reorder(Product, Total_Quantity), y = Total_Quantity, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Products by Quantity Sold",
       x = "Product",
       y = "Total Quantity Sold") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()  # Flip coordinates for better readability

# Save the plot with specified size
ggsave("top_5_products_quantity_sold.png", plot = p_top_products, width = 20, height = 12)

# Display the plot
print(p_top_products)







amount_by_hour <- new_dataset %>%
  group_by(Hour) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE))

# Create a line plot for total amount by hour
p_amount_hour <- ggplot(amount_by_hour, aes(x = Hour, y = Total_Amount)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  geom_point(color = "blue", size = 3) +  # Points on the line
  labs(title = "Total Amount of Sales by Hour",
       x = "Hour of the Day",
       y = "Total Amount") +
  theme_minimal()

# Save the plot with specified size
ggsave("total_amount_by_hour.png", plot = p_amount_hour, width = 20, height = 12)

# Display the plot
print(p_amount_hour)









# Get the top 5 most expensive products
top_expensive_products <- new_dataset %>%
  group_by(Product) %>%
  summarise(Avg_Price = mean(Price.Each, na.rm = TRUE)) %>%
  arrange(desc(Avg_Price)) %>%
  head(5)

# Create a vertical bar plot to compare the top 5 most expensive products
p_top_expensive <- ggplot(top_expensive_products, aes(x = Product, y = Avg_Price, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Most Expensive Products",
       x = "Product",
       y = "Average Price") +
  theme_minimal() +
  theme(legend.position = "none")

# Save the plot with specified size
ggsave("top_5_expensive_products.png", plot = p_top_expensive, width = 20, height = 12)

# Display the plot
print(p_top_expensive)







# Ensure Month is numeric
new_dataset$Month <- as.numeric(new_dataset$Month)

# Re-create the monthly sales summary
monthly_sales <- new_dataset %>%
  group_by(Month) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE))

# Create a time series plot with connected lines
p_monthly_sales <- ggplot(monthly_sales, aes(x = Month, y = Total_Amount)) +
  geom_line(color = "blue", size = 1) +  # Line graph for total sales
  geom_point(color = "red", size = 2) +  # Points at each month
  labs(title = "Total Sales per Month",
       x = "Month",
       y = "Total Sales Amount") +
  theme_minimal()

# Save the plot with specified size
ggsave("total_sales_per_month_line_graph.png", plot = p_monthly_sales, width = 20, height = 12)

# Display the plot
print(p_monthly_sales)
colnames(new_dataset)
nrow(new_dataset)








# Group by State and Product, then summarize total sales
sales_by_state <- new_dataset %>%
  group_by(State, Product) %>%
  summarise(Total_Sales = sum(Amount, na.rm = TRUE)) %>%
  ungroup()

# Identify the most sold item in each state
most_sold_items <- sales_by_state %>%
  group_by(State) %>%
  slice_max(Total_Sales, n = 1) %>%
  ungroup()

# Create a bar plot of the most sold items by state
p<- ggplot(most_sold_items, aes(x = reorder(State, -Total_Sales), y = Total_Sales, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Sold Items by State",
       x = "State",
       y = "Total Sales",
       fill = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("most_sold_items_by_state.png", plot = p, width = 24, height = 16)







# Step 1: Create a Data Frame with All U.S. States and Abbreviations
us_states <- data.frame(
  State_Full = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                 "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
                 "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                 "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  State_Abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                 "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                 "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
                 "WI", "WY")
)


total_sales_by_state_abbr <- new_dataset %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(Amount, na.rm = TRUE))

# Step 3: Merge with All U.S. States
total_sales_all_states <- us_states %>%
  left_join(total_sales_by_state_abbr, by = c("State_Abbr" = "State")) %>%
  mutate(Total_Sales = ifelse(is.na(Total_Sales), 0, Total_Sales))

# Step 4: Plot the Geographical Heatmap
# Get state map data
usa_map <- map_data("state")

# Merge sales data with state map data
sales_geo <- usa_map %>%
  mutate(region = tolower(region)) %>%
  left_join(total_sales_all_states %>% mutate(region = tolower(State_Full)), by = "region")

# Plot the map with sales data
ggplot(sales_geo, aes(long, lat, group = group, fill = Total_Sales)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white", high = "red", name = "Total Sales") +
  labs(title = "Total Sales by State in the United States") +
  coord_fixed(1.3) +
  theme_minimal()

# Save the plot
ggsave("total_sales_by_state_heatmap.png", width = 12, height = 8)






product_frequencies <- new_dataset %>%
  count(Product) %>%
  arrange(desc(n))

# Adjust the product names to make them shorter if needed
product_frequencies$Product <- gsub("USB-C Charging Cable", "USB-C Cable", product_frequencies$Product)
product_frequencies$Product <- gsub("Lightning Charging Cable", "Lightning Cable", product_frequencies$Product)
product_frequencies$Product <- gsub("AAA Batteries \\(4-pack\\)", "AAA Batteries", product_frequencies$Product)

# Create the word cloud
set.seed(123)  # For reproducibility
wordcloud(words = product_frequencies$Product, 
          freq = product_frequencies$n, 
          scale = c(3, 0.8),  # Adjust word size to reduce differences in sizes
          min.freq = 1,  # Minimum frequency to include
          max.words = 100,  # Limit the number of words
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Save the word cloud
png("product_wordcloud.png", width = 2400, height = 000)  # Increase dimensions for better fit
wordcloud(words = product_frequencies$Product, 
          freq = product_frequencies$n, 
          scale = c(3, 0.8), 
          min.freq = 1, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))
dev.off()


#saving the file 
write.csv(new_dataset, "datasets.csv", row.names = FALSE)
