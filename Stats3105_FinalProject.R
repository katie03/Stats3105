## Katie Chen (kc3342)
## Stats Final Project 

library(jsonlite)
library(dplyr)

review_file <- read.csv("/Users/katiechen/Downloads/yelp_dataset/yelp_academic_dataset_review.csv")
nrow(review_file)

businesses <- read.csv("/Users/katiechen/Downloads/yelp_dataset/yelp_academic_dataset_business.csv")
head(businesses)
nrow(businesses)

users <- read.csv("/Users/katiechen/Downloads/yelp_dataset/yelp_academic_dataset_user.csv")
str(users)

# subsetting data  
city_counts <- businesses %>%
  group_by(city) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
head(city_counts)

philadelphia_data <- businesses %>%
  filter(city == "Philadelphia")
nrow(philadelphia_data)

merged_data <- review_file %>%
  inner_join(philadelphia_data, by = "business_id")
nrow(merged_data)

final_data <- merged_data %>%
  inner_join(users, by ="user_id")

nrow(final_data)
str(final_data)
head(final_data)

# filter by cuisines 
cuisine_count <- final_data %>%
  group_by(categories) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cuisine_count
cuisines <- c("Italian", "American (New)", "Mediterranean", "Chinese", "Vietnamese", "Thai", "Spanish", "Japanese")

final_data <- final_data %>%
  filter(!is.na(categories)) %>%
  separate_rows(categories, sep = ", ") %>%
  filter(categories %in% cuisines) 

library(stringr)
library(tidyr)
final_data <- read.csv("/Users/katiechen/Downloads/yelp_dataset/initial_data.csv")
final_data <- final_data %>%
  filter(!is.na(categories)) %>%
  mutate(
    # Split the categories string into a list of categories
    categories_split = str_split(categories, ", ")
  ) %>%
  # Filter rows where at least one category matches the predefined cuisines
  filter(sapply(categories_split, function(x) any(x %in% cuisines))) %>%
  # Unnest the categories into a longer format
  unnest_longer(categories_split) %>%
  # Keep only rows where the split category matches the predefined cuisines
  filter(categories_split %in% cuisines) %>%
  # Classify the restaurant cuisine as the matching category
  mutate(categories = categories_split)

unique(final_data$categories)
nrow(final_data)
write.csv(final_data, "/Users/katiechen/Downloads/yelp_dataset/initial_data.csv", row.names = FALSE)
head(final_data)

final_data <- final_data %>%
  dplyr::select(
    review_id,                # Review unique ID
    user_id,                  # User unique ID
    business_id,              # Business unique ID
    stars.x,                  # Review stars
    stars.y,                  # Business average stars
    latitude, longitude,      # Business geolocation
    date,                     # Review date
    text, 
    city,        # Business name, city, state
    review_count.y,           # Business review count
    attributes, 
    is_open,                  # Business open status
    categories,               # Business categories
    elite                     # User elite years
  ) %>%
  rename(
    review_stars = stars.x,
    business_stars = stars.y,
    business_review_count = review_count.y,
  )

nrow(final_data)
str(final_data)
write.csv(final_data, "/Users/katiechen/Downloads/yelp_dataset/final_data.csv", row.names = FALSE)



Are restaurants in Philadelphia serving Asian cuisines (Chinese, Japanese, Thai, Vietnamese) more likely to close than those serving other cuisines (e.g., Italian, American, Mediterranean, Spanish)? 
  
  What factors (e.g., ratings, location, review frequency, elite_users) best predict restaurant closures in Philadelphia, and do closure rates differ significantly between Asian and non-Asian cuisines?
  
  ```{r}
library(ggplot2)
library(tidyr)
library(dplyr)
library(jsonlite)
library(stringr)
# exploratory analysis 
final_data <- read.csv("/Users/katiechen/Downloads/yelp_dataset/final_data.csv")
str(final_data)
nrow(final_data)
ncol(final_data)

unique(final_data$categories)
cuisine_count <- final_data %>%
  group_by(categories) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cuisine_count

# 0 for closed, 1 for open 
table(final_data$is_open)
missing_is_open <- sum(is.na(final_data$is_open))
cat("Number of missing values in 'is_open':", missing_is_open, "\n")

# number of restaurants 
length(unique(final_data$business_id))

# date range for reviews 
final_data$date <- as.Date(final_data$date, format = "%Y-%m-%d %H:%M:%S")
min(final_data$date)
max(final_data$date)

str(final_data)

# choose subset of features 
closure_rate_by_stars_categories <- final_data %>%
  group_by(business_stars, categories) %>%
  summarize(
    total = n(),  # Total businesses in each group
    closed = sum(is_open == 0, na.rm = TRUE),  # Count closed businesses
    closure_rate = closed / total  # Calculate closure rate
  ) %>%
  filter(total > 10) %>%  # Optional: Filter groups with more than 10 businesses for reliability
  arrange(desc(business_stars))  # Arrange by closure rate

# View the grouped data
print(closure_rate_by_stars_categories)

ggplot(closure_rate_by_stars_categories, aes(x = business_stars, y = closure_rate, group = categories, color = categories)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~categories, scales = "free_y") +
  labs(
    title = "Closure Rates by Star Rating for Different Cuisines",
    x = "Average Star Rating",
    y = "Closure Rate",
    color = "Cuisine",
    caption = "Data Source: Yelp Open Dataset"
  ) +
  theme_minimal()

final_data %>%
  group_by(categories, is_open) %>%
  summarise(avg_review_frequency = mean(business_review_count)) %>%
  ggplot(aes(x = categories, y = avg_review_frequency, fill = as.factor(is_open))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Review Frequency by Cuisine and Status",
       x = "Cuisine Type", y = "Average Review Frequency",
       fill = "Status (Open/Closed)") +  # Adjust legend title
  scale_fill_discrete(
    labels = c("Closed", "Open")    # Explicitly set legend labels
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

library(dplyr)
final_data <- final_data %>%
  mutate(
    # Split the elite string by commas, handle empty strings as no years
    elite_list = str_split(elite, pattern = ",", simplify = FALSE),
    # Count the number of years as elite, treating empty strings as 0
    elite_count = sapply(elite_list, function(x) if (x[1] == "") 0 else length(x)),
    # Binary indicator for whether the user was ever elite
    elite_binary = ifelse(elite_count > 0, 1, 0)
  )

elite_cuisine_summary <- final_data %>%
  group_by(categories, is_open) %>%
  summarise(
    total_elite_reviews = sum(elite_binary, na.rm = TRUE),
    avg_elite_count = mean(elite_count, na.rm = TRUE)
  )

elite_cuisine_summary

# Plot elite user reviews by cuisine and status
ggplot(elite_cuisine_summary, aes(x = categories, y = total_elite_reviews, fill = as.factor(is_open))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Elite User Reviews by Cuisine and Restaurant Status",
    x = "Cuisine",
    y = "Total Elite User Reviews",
    fill = "Restaurant Status"
  ) +
  scale_fill_manual(values = c("red", "darkgreen"), labels = c("Closed", "Open")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

library(ggplot2)
library(ggspatial)
library(sf)

# Load a basemap of Philadelphia from OpenStreetMap
philly_map <- ggplot() +
  annotation_map_tile(zoom = 12, type = "osm") +
  geom_point(data = final_data, aes(x = longitude, y = latitude, color = as.factor(is_open)), alpha = 0.6) +
  scale_color_discrete(labels = c("Closed", "Open")) +
  labs(
    title = "Restaurant Locations and Closure Status in Philadelphia",
    color = "Status"
  ) +
  theme_minimal()

philly_map
ggplot(final_data, aes(x = longitude, y = latitude, color = categories, shape = as.factor(is_open))) +
  annotation_map_tile(zoom = 12, type = "osm") +
  scale_color_brewer(palette = "Set3") +
  geom_point(alpha = 0.4) +
  scale_color_discrete(name = "Cuisine Type") +  # Legend for cuisines
  scale_shape_manual(name = "Status", labels = c("Closed", "Open"), values = c(16, 17)) +  # Legend for closure status
  labs(
    title = "Restaurant Locations, Closure Status, and Cuisine Types in Philadelphia",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


ggplot(final_data, aes(x = longitude, y = latitude, color = categories, shape = as.factor(is_open))) +
  annotation_map_tile(zoom = 12, type = "osm") +
  geom_point(alpha = 0.5, size = 1.5, position = position_jitter(width = 0.002, height = 0.002)) +
  scale_color_brewer(palette = "Set1", name = "Cuisine Type") +
  scale_shape_manual(values = c(16, 17), name = "Status", labels = c("Closed", "Open")) +
  labs(
    title = "Restaurant Locations, Closure Status, and Cuisine Types in Philadelphia",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  facet_wrap(~ categories, ncol = 3)  # Separate panels for each cuisine


closure_rates <- final_data %>%
  group_by(categories) %>%
  summarise(
    total_restaurants = n(),
    closed_restaurants = sum(is_open == 0, na.rm = TRUE),
    closure_rate = closed_restaurants / total_restaurants
  ) %>%
  arrange(desc(closure_rate))  # Sort by closure rate

ggplot(closure_rates, aes(x = reorder(categories, closure_rate), y = closure_rate, fill="lightblue")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Closure Rates by Cuisine Type",
    x = "Cuisine Type",
    y = "Closure Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis to percentage format
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

library(caret)
library(randomForest)
library(e1071)
# Define Asian and Non-Asian cuisines
asian_cuisines <- c("Chinese", "Japanese", "Thai", "Vietnamese")
non_asian_cuisines <- c("Italian", "American (New)", "Mediterranean", "Spanish")

# Filter and preprocess the data
preprocessed_data <- final_data %>%
  filter(!is.na(categories)) %>%
  mutate(
    is_asian_cuisine = ifelse(categories %in% asian_cuisines, 1, 0),  # Binary: 1 = Asian, 0 = Non-Asian
    is_open = as.factor(is_open)  # Target variable
  ) %>%
  select(
    is_open,                    # Target variable
    is_asian_cuisine,           # Cuisine type (Asian or not)
    business_stars,             # Business average stars
    review_stars,               # Review stars
    business_review_count,      # Total number of reviews
    elite_binary,               # Whether the reviewer is elite
    latitude, longitude,         # Location data
    categories,
  ) %>%
  na.omit()

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(preprocessed_data$is_open, p = 0.8, list = FALSE)
train_data <- preprocessed_data[train_index, ]
test_data <- preprocessed_data[-train_index, ]

logistic_model <- glm(
  is_open ~ is_asian_cuisine + business_stars + review_stars + business_review_count + elite_binary + latitude + longitude,
  data = train_data,
  family = binomial
)

# Summary of the logistic regression model
print(summary(logistic_model))

# Predict on the test set
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
logistic_predicted_class <- ifelse(logistic_predictions > 0.5, 1, 0)

# Calculate accuracy for Logistic Regression
logistic_accuracy <- mean(logistic_predicted_class == as.numeric(test_data$is_open) - 1)
cat("Logistic Regression Accuracy:", logistic_accuracy, "\n")

# Confusion Matrix for Logistic Regression
cat("Logistic Regression Confusion Matrix:\n")
print(confusionMatrix(as.factor(logistic_predicted_class), test_data$is_open))

summary(logistic_model)
test_data$logistic_prob <- predict(logistic_model, newdata = test_data, type = "response")
str(test_data)
logistic_summary <- test_data %>%
  group_by(categories) %>%
  summarise(
    avg_predicted_closure_prob = mean(logistic_prob),
    count = n()
  )
cat("Logistic Regression Predicted Probabilities:\n")
print(logistic_summary)

logistic_summary <- test_data %>%
  group_by(is_asian_cuisine) %>%
  summarise(
    avg_predicted_closure_prob = mean(logistic_prob),
    count = n()
  )

cat("Logistic Regression Predicted Probabilities:\n")
print(logistic_summary)

chisq.test(table(test_data$is_open, test_data$is_asian_cuisine))



```{r}
# Train a Random Forest model
rf_model <- randomForest(
  is_open ~ is_asian_cuisine + business_stars + review_stars + business_review_count + elite_binary + latitude + longitude,
  data = train_data,
  ntree = 100,
  mtry = 3,
  importance = TRUE
)

# Predict on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Calculate accuracy for Random Forest
rf_accuracy <- mean(rf_predictions == test_data$is_open)
cat("Random Forest Accuracy:", rf_accuracy, "\n")

# Feature Importance
cat("Random Forest Feature Importance:\n")
print(importance(rf_model))
varImpPlot(rf_model)

# Confusion Matrix for Random Forest
cat("Random Forest Confusion Matrix:\n")
print(confusionMatrix(rf_predictions, test_data$is_open))

# Add predicted probabilities from Random Forest to the test dataset
test_data$rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[,2]  # Probabilities for class 1 (closed)
rf_summary <- test_data %>%
  group_by(is_asian_cuisine) %>%
  summarise(
    avg_predicted_closure_prob = mean(rf_prob),
    count = n()
  )

cat("Random Forest Predicted Probabilities:\n")
print(rf_summary)

test_data$rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[,2]  # Probabilities for class 1 (closed)
rf_summary <- test_data %>%
  group_by(categories) %>%
  summarise(
    avg_predicted_closure_prob = mean(rf_prob),
    count = n()
  )

cat("Random Forest Predicted Probabilities:\n")
print(rf_summary)

chisq.test(table(test_data$is_open, test_data$is_asian_cuisine))

library(pdp)
# PDP for 'latitude'
latitude_pdp <- partial(rf_model, pred.var = "latitude", train = train_data)
plot(latitude_pdp, main = "Partial Dependence of Latitude", xlab = "Latitude", ylab = "Predicted Probability")

# PDP for 'longitude'
longitude_pdp <- partial(rf_model, pred.var = "longitude", train = train_data)
plot(longitude_pdp, main = "Partial Dependence of Longitude", xlab = "Longitude", ylab = "Predicted Probability")

# PDP for 'is_asian_cuisine'
asian_cuisine_pdp <- partial(rf_model, pred.var = "is_asian_cuisine", train = train_data)
plot(asian_cuisine_pdp, main = "Partial Dependence of Is Asian Cuisine", xlab = "Is Asian Cuisine (0 = No, 1 = Yes)", ylab = "Predicted Probability")

# PDP for 'business_stars'
business_stars_pdp <- partial(rf_model, pred.var = "business_stars", train = train_data)
plot(business_stars_pdp, main = "Partial Dependence of Business Stars", xlab = "Business Stars", ylab = "Predicted Probability")

library(pROC)

# Logistic Regression predicted probabilities
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")

# ROC for Logistic Regression
roc_logistic <- roc(test_data$is_open, logistic_predictions)
plot(roc_logistic, col = "blue", main = "ROC Curve for Models")

# Random Forest predicted probabilities for the "closed" class (1)
rf_predictions <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# ROC for Random Forest
roc_rf <- roc(test_data$is_open, rf_predictions)
lines(roc_rf, col = "red")
legend("bottomright", legend = c("Logistic Regression", "Random Forest"),
       col = c("blue", "red"), lwd = 2)

auc_logistic <- auc(roc_logistic)
auc_rf <- auc(roc_rf)
cat("AUC for Logistic Regression:", auc_logistic, "\n")
cat("AUC for Random Forest:", auc_rf, "\n")

# Logistic regression model without spatial correlation (no latitude or longitude)
logistic_model_no_spatial <- glm(
  is_open ~ is_asian_cuisine + business_stars + review_stars + business_review_count + elite_binary,
  data = train_data,
  family = binomial
)

# Summary of the new logistic regression model
summary(logistic_model_no_spatial)

# Predict on the test set
logistic_predictions_no_spatial <- predict(logistic_model_no_spatial, newdata = test_data, type = "response")
logistic_predicted_class_no_spatial <- ifelse(logistic_predictions_no_spatial > 0.5, 1, 0)

# Calculate accuracy
accuracy_no_spatial <- mean(logistic_predicted_class_no_spatial == test_data$is_open)
cat("Accuracy without spatial features:", accuracy_no_spatial, "\n")

# Compare the performance
cat("Change in accuracy:", logistic_accuracy - accuracy_no_spatial, "\n")

library(randomForest)

# Random forest model with different tuning parameters
rf_model_tuned <- randomForest(
  is_open ~ is_asian_cuisine + business_stars + review_stars + business_review_count + elite_binary + latitude + longitude,
  data = train_data,
  ntree = 50,          # Change number of trees
  mtry = 2,             # Change number of variables at each split
  importance = TRUE
)

# Predict on the test set
rf_predictions_tuned <- predict(rf_model_tuned, newdata = test_data, type = "response")

# Calculate accuracy
accuracy_rf_tuned <- mean(rf_predictions_tuned == test_data$is_open)
cat("Accuracy of tuned random forest:", accuracy_rf_tuned, "\n")

