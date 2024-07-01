# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

barcelona_listings <- read.csv("listings.csv")
Hmisc::describe(barcelona_listings)

barcelona_neighbourhoods <- read.csv("neighbourhoods.csv")
Hmisc::describe(barcelona_neighbourhoods)

barcelona_reviews <- read.csv("reviews.csv")
Hmisc::describe(barcelona_reviews)

#------------------------------------------------------------------------------#
barcelona_listings$amenities

# Could not do any amenities analysis as no data is available for any of the properties listed in barcelona
barcelona_listings$has_elevator <- grepl("Elevator", barcelona_listings$amenities)
barcelona_listings$has_wheelchair_access <- grepl("Wheelchair accessible", barcelona_listings$amenities)
barcelona_listings$is_ground_floor <- grepl("Single level home|Ground floor", barcelona_listings$amenities)

# Filter for properties suitable for retired couples
suitable_properties <- barcelona_listings %>%
  filter(has_elevator | has_wheelchair_access | is_ground_floor) %>%
  select(id, neighbourhood_cleansed, price, review_scores_rating, review_scores_location, latitude, longitude)

# Aggregate data by neighbourhood
neighbourhood_summary <- suitable_properties %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    count_of_properties = n(),
    average_price = mean(as.numeric(gsub("[\\$,]", "", price)), na.rm = TRUE),
    average_rating = mean(review_scores_rating, na.rm = TRUE),
    average_location_score = mean(review_scores_location, na.rm = TRUE)
  ) %>%
  arrange(desc(count_of_properties))

# Output the top neighbourhoods for retired couples
print(neighbourhood_summary)


#------------------Neighbourhood Analysis--------------------------------------#

barcelona_listings$price

# Function to clean and convert price from string to numeric
clean_price <- function(price) {
  price_cleaned <- gsub("[^0-9.]", "", price)  # Remove anything that's not a digit or dot
  as.numeric(price_cleaned)  # Convert to numeric
}

# Apply the function to the price column
barcelona_listings$price <- sapply(barcelona_listings$price, clean_price)

barcelona_listings$property_type

# Filter and analyze data based on review scores and property types
analysis_data <- barcelona_listings %>%
  filter(property_type %in% c("Entire cabin", "Entire guest suite", "Entire condo", "Entire rental unit", "Entire guesthouse", "Entire house", "Entire place", "Entire loft", "Entire townhouse", "Entire serviced apartment", "Entire vacation home", "Tiny home", "Private room")) %>%
  select(id, neighbourhood_cleansed, price, review_scores_rating, review_scores_location, review_scores_accuracy, review_scores_cleanliness, review_scores_communication, review_scores_value, latitude, longitude) %>%
  filter(!is.na(review_scores_location), !is.na(review_scores_rating))

# Aggregate data by neighbourhood
neighbourhood_summary <- analysis_data %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    count_of_properties = n(),
    average_price = mean(price, na.rm = TRUE),
    average_rating = mean(review_scores_rating, na.rm = TRUE),
    average_location_score = mean(review_scores_location, na.rm = TRUE),
    average_accuracy_score = mean(review_scores_accuracy, na.rm = TRUE),
    average_cleanliness_score = mean(review_scores_cleanliness, na.rm = TRUE),
    average_communication_score = mean(review_scores_communication, na.rm = TRUE),
    average_value_score = mean(review_scores_value, na.rm = TRUE)
  ) %>%
  arrange(desc(count_of_properties))

# Output the top neighbourhoods for retired couples
print(neighbourhood_summary)

write.csv(neighbourhood_summary, "C:/Coursework/Analytics in Practise/Barcelona/1st quarter/listings/neighbourhood_analysis_summary.csv")

# Create a map visualization if you have latitude and longitude
ggplot(analysis_data, aes(x = longitude, y = latitude, color = review_scores_location)) +
  geom_point(alpha = 0.5) +
  labs(title = "Map of Top Rated Properties for Retired Couples in Barcelona",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Plotting the histogram of average prices by neighborhood
ggplot(neighbourhood_summary, aes(x = average_price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Prices Across Neighborhoods",
       x = "Average Price",
       y = "Count of Neighborhoods") +
  theme_minimal()

# Plotting a bar chart of average prices by neighborhood
ggplot(neighbourhood_summary, aes(x = neighbourhood_cleansed, y = average_price, fill = neighbourhood_cleansed)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(title = "Average Prices Across Neighborhoods",
       x = "Neighborhood",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(neighbourhood_summary, aes(x = count_of_properties, y = average_price)) +
  geom_point(aes(color = neighbourhood_cleansed), size = 4, alpha = 0.6) +  # Color-coded by neighborhood
  geom_hline(yintercept = median(neighbourhood_summary$average_price), linetype = "dashed", color = "red") +
  geom_vline(xintercept = median(neighbourhood_summary$count_of_properties), linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot of Properties vs. Average Price",
       x = "Number of Properties",
       y = "Average Price") +
  theme_minimal() +
  scale_color_discrete(name = "Neighborhood") +
  annotate("text", x = Inf, y = Inf, label = "High Price, High Props", hjust = 1.1, vjust = 2, size = 5) +
  annotate("text", x = 0, y = Inf, label = "High Price, Low Props", hjust = -0.1, vjust = 2, size = 5) +
  annotate("text", x = Inf, y = 0, label = "Low Price, High Props", hjust = 1.1, vjust = -0.2, size = 5) +
  annotate("text", x = 0, y = 0, label = "Low Price, Low Props", hjust = -0.1, vjust = -0.2, size = 5)

# Create a scatter plot with adjusted legend positioning
ggplot(neighbourhood_summary, aes(x = count_of_properties, y = average_price)) +
  geom_point(aes(color = neighbourhood_cleansed), size = 3) +  # Adjust point size as needed
  geom_hline(yintercept = median(neighbourhood_summary$average_price), linetype = "dashed", color = "red") +
  geom_vline(xintercept = median(neighbourhood_summary$count_of_properties), linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot of Properties vs. Average Price",
       x = "Number of Properties",
       y = "Average Price") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Move legend to the bottom
  theme(legend.key.size = unit(0.5, "cm"),  # Adjust size of legend keys
        legend.text = element_text(size = 6),  # Adjust text size in legend
        legend.title = element_text(size = 7)) +  # Adjust legend title size
  guides(color = guide_legend(nrow = 5, override.aes = list(size = 3))) +
  annotate("text", x = Inf, y = Inf, label = "High Price, High Props", hjust = 1.1, vjust = 2, size = 3) +
  annotate("text", x = 0, y = Inf, label = "High Price, Low Props", hjust = 0.5, vjust = 2, size = 3) +
  annotate("text", x = Inf, y = 0, label = "Low Price, High Props", hjust = 1.1, vjust = -0.2, size = 3) +
  annotate("text", x = 0, y = 0, label = "Low Price, Low Props", hjust = 0.5, vjust = -0.2, size = 3)

#---------------------Demand Analysis -----------------------------------------#

library(lubridate)

calendar <- read_csv("C:/Coursework/Analytics in Practise/Barcelona/1st quarter/calendar.csv")

# Ensure date fields are in the correct format
calendar$date <- as.Date(calendar$date)

filtered_calendar <- calendar %>%
  filter(listing_id %in% analysis_data$id)

# Apply the function to the price column
filtered_calendar$price <- sapply(filtered_calendar$price, clean_price)

# Convert 'available' to binary format: 1 for booked (f) and 0 for available (t)
filtered_calendar$available <- ifelse(filtered_calendar$available == "f", 1, 0)

# Create a 'month' column to aggregate data on a monthly basis
filtered_calendar$month <- floor_date(filtered_calendar$date, "month")

# Now let's group by 'listing_id' and 'month' and summarize
demand_analysis <- filtered_calendar %>%
  group_by(listing_id, month) %>%
  summarise(
    days_booked = sum(available, na.rm = TRUE),  # Sum of days not available (booked)
    total_days = n_distinct(date),  # Count of distinct days in the month
    avg_price = mean(price, na.rm = TRUE),
    avg_min_nights = mean(minimum_nights, na.rm = TRUE),
    avg_max_nights = mean(maximum_nights, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    occupancy_rate = days_booked / total_days  # Calculate occupancy rate
  ) %>%
  arrange(listing_id, month)  # Sort by 'listing_id' and 'month'

# We now have a data frame with occupancy rates, average prices, and night restrictions for each property, by month
print(demand_analysis)

# Optional: You can write the resulting dataframe to a CSV for further analysis
write_csv(demand_analysis, "demand_analysis_output.csv")

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Load the necessary libraries
library(dplyr)
library(readr)
library(tidytext)
library(lubridate)
library(ggplot2)
library(textdata)


# Filter reviews for only the properties selected in the neighborhood analysis
filtered_reviews <- reviews %>%
  filter(listing_id %in% analysis_data$id)

# Load sentiments from multiple lexicons
bing_sentiments <- get_sentiments("bing")
afinn_sentiments <- get_sentiments("afinn")
nrc_sentiments <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("negative", "positive"))

# Tokenize the reviews, match to sentiments, and calculate sentiment scores for each lexicon
review_sentiments <- filtered_reviews %>%
  unnest_tokens(word, comments) %>%
  inner_join(bing_sentiments, by = "word") %>%
  mutate(score_bing = ifelse(sentiment == "positive", 1, -1)) %>%
  select(id, score_bing) %>%
  bind_rows(
    filtered_reviews %>%
      unnest_tokens(word, comments) %>%
      inner_join(afinn_sentiments, by = "word") %>%
      select(id, score = value)
  ) %>%
  bind_rows(
    filtered_reviews %>%
      unnest_tokens(word, comments) %>%
      inner_join(nrc_sentiments, by = "word") %>%
      mutate(score_nrc = ifelse(sentiment == "positive", 1, -1)) %>%
      select(id, score_nrc)
  ) %>%
  # Combine scores across lexicons, giving precedence to AFINN where available
  mutate(final_score = coalesce(score, score_bing, score_nrc)/3) %>%
  group_by(id) %>%
  summarise(sentiment_score = sum(final_score, na.rm = TRUE)) %>%
  ungroup()

# Join the sentiment scores back to the filtered reviews
filtered_reviews_with_sentiment <- filtered_reviews %>%
  left_join(review_sentiments, by = "id")

# Output a summary of sentiments by listing
summary_sentiments <- filtered_reviews_with_sentiment %>%
  group_by(listing_id) %>%
  summarise(
    average_sentiment = mean(sentiment_score, na.rm = TRUE),
    total_reviews = n()
  ) %>%
  ungroup()

# Print the summary of sentiments
print(summary_sentiments)
Hmisc::describe(summary_sentiments)

which(summary_sentiments$average_sentiment < 4.089)

# Optional: Save the results to a CSV file
write_csv(summary_sentiments, "summary_sentiments.csv")

ggplot(summary_sentiments, aes(x = average_sentiment)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Sentiment Scores", x = "Average Sentiment Score", y = "Frequency")

summary_sentiments_merged <- merge(x = analysis_data, y = summary_sentiments, by.x = analysis_data$id, by.y = summary_sentiments$listing_id, all.y = TRUE) 

ggplot(summary_sentiments, aes(x = property_type, y = average_sentiment)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sentiment Scores by Property Type", x = "Property Type", y = "Average Sentiment Score")

top_bottom_sentiments <- summary_sentiments %>%
  top_n(10, wt = average_sentiment) %>%
  bind_rows(summary_sentiments %>% top_n(-10, wt = average_sentiment))

ggplot(top_bottom_sentiments, aes(x = reorder(listing_id, average_sentiment), y = average_sentiment, fill = average_sentiment > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top & Bottom Listings by Average Sentiment Score", x = "Listing ID", y = "Average Sentiment Score")


ggplot(summary_sentiments, aes(x = total_reviews, y = average_sentiment)) +
  geom_point(aes(color = average_sentiment), alpha = 0.6) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green") +
  labs(title = "Relationship Between Number of Reviews and Sentiment Score", x = "Total Reviews", y = "Average Sentiment Score")


#------------------------------------------------------------------------------#

filtered_data <- barcelona_listings %>%
                  filter(id %in% analysis_data$id)
str(filtered_data)
filtered_data$host_response_rate <- sapply(filtered_data$host_response_rate, clean_price)
filtered_data$host_acceptance_rate <- sapply(filtered_data$host_acceptance_rate, clean_price)
which(filtered_data$host_response_rate < 50)
which(filtered_data$host_acceptance_rate < 50)

#------------------------------------------------------------------------------#
calendar$listing_id <- as.character(calendar$listing_id)
filtered_data$id <- as.character(filtered_data$id)

# Filter the calendar data to only include listings that are in the filtered listings
calendar_filtered <- calendar %>%
  filter(listing_id %in% filtered_data$id)

# Calculate the average minimum nights booked for each property
average_minimum_nights <- calendar_filtered %>%
  group_by(listing_id) %>%
  summarise(avg_min_nights = mean(minimum_nights, na.rm = TRUE)) %>%
  ungroup()

# Optionally, view the overall distribution of average minimum nights
summary(average_minimum_nights$avg_min_nights)

# Export the results to a CSV for further analysis or reporting
write_csv(average_minimum_nights, "average_minimum_nights.csv")

# Print the average minimum nights data frame
print(average_minimum_nights)

# Example to create a histogram of average minimum nights
library(ggplot2)
ggplot(average_minimum_nights, aes(x = avg_min_nights)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Minimum Nights", x = "Average Minimum Nights", y = "Listings")
