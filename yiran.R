# Libraries ---------------------------------------------------------------
library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(knitr)
library(GGally)
library(reshape2)

# Data Loading --------------------------------------
# Financial Personality data: 
personality <- read.csv("https://raw.githubusercontent.com/karwester/behavioural-finance-task/refs/heads/main/personality.csv")

# Financial Assets data: 
# Set credentials
supabase_url <- "https://pvgaaikztozwlfhyrqlo.supabase.co"
api_key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InB2Z2FhaWt6dG96d2xmaHlycWxvIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NDc4NDE2MjUsImV4cCI6MjA2MzQxNzYyNX0.iAqMXnJ_sJuBMtA6FPNCRcYnKw95YkJvY3OhCIZ77vI"

# Construct the full request URL
request_url <- paste0(supabase_url, "/rest/v1/assets?select=*")

# Make the GET request with the API key in the header
response <- GET(url = request_url, add_headers(apikey = api_key))

# Check the response and parse the JSON data
if (status_code(response) == 200) {
  # Convert the raw JSON content to a data frame
  assets <- fromJSON(content(response, "text", encoding = "UTF-8"))
  # View the first few rows of the data frame
  head(assets_data)
} else {
  # Print an error message if the request failed
  print(paste("Error: Failed to retrieve data. Status code:", status_code(response)))
  print(content(response, "text"))
}

write.csv(personality, "personality.csv", row.names = FALSE)
write.csv(assets, "assets.csv", row.names = FALSE)

# NAs: 
sum(is.na(assets))
sum(is.na(personality))

# Exploring ---------------------------------------------------------------
unique(assets$asset_allocation)
unique(assets$asset_currency)

# Personality: 
# Boxplots: 
personality_long <- personality %>%
  select(confidence, risk_tolerance, composure, impulsivity, impact_desire) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Now, create the plot with facets
ggplot(personality_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free", nrow = 1) +
  labs(title = "Boxplots of Personality Variables",
       x = "", 
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove the x-axis text
        axis.ticks.x = element_blank(), # Remove the x-axis ticks
        legend.position = "none")       # Hide the legend as it's redundant


# Summary Statistics: 
data_for_summary <- personality[, c("confidence", "risk_tolerance", "composure", "impulsivity", "impact_desire")]

# Create table: 
summary_stats <- data.frame(
  Mean = sapply(data_for_summary, mean),
  Median = sapply(data_for_summary, median),
  Min = sapply(data_for_summary, min),
  Max = sapply(data_for_summary, max),
  Std_Dev = sapply(data_for_summary, sd)
) %>% 
  round(3)

# Present with kable()
kable(summary_stats, 
      caption = "Summary Statistics of Personality Traits", 
      format = "html", 
      col.names = c("Mean", "Median", "Minimum", "Maximum", "Standard Deviation")
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), 
    full_width = FALSE, 
    position = "center"
  )

# Correlation Matrix: 
ggpairs(personality[, 2:6])

# Asset: 
asset_table <- dcast(assets, asset_allocation ~ asset_currency, sum, 
                     value.var = "asset_value")

assets_converted <- assets %>%
  mutate(
    asset_value_gbp = case_when(
      asset_currency == "USD" ~ asset_value * 0.74,
      asset_currency == "JPY" ~ asset_value * 0.0051,
      asset_currency == "EUR" ~ asset_value * 0.85,
      asset_currency == "AUD" ~ asset_value * 0.48,
      asset_currency == "GBP" ~ asset_value
    )
  )

assets_summary <- assets_converted %>%
  group_by(asset_allocation) %>%
  summarise(total_gbp = sum(asset_value_gbp, na.rm = TRUE))

ggplot(assets_summary, aes(x = reorder(asset_allocation, -total_gbp), y = total_gbp)) +
  geom_bar(stat = "identity", fill = "#4682B4", color = "black") +
  geom_text(aes(label = round(total_gbp, 2)), vjust = -0.5, size = 3.5) + # Add value labels on top of bars
  labs(
    title = "Total Asset Value in GBP by Allocation Type",
    subtitle = "Aggregated values from multiple currency holdings",
    x = "Asset Allocation",
    y = "Total Value (GBP)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), # Angle the x-axis labels for readability
    panel.grid.major.x = element_blank(), # Remove vertical grid lines
    panel.grid.minor.y = element_blank()
  )

summary(assets_converted$asset_value_gbp)
hist(assets_converted$asset_value_gbp)

# Merged: 
merged_data <- assets_converted %>%
  left_join(personality, by = c("_id" = "X_id"))

correlation_matrix <- cor(merged_data[, 7:12])
correlation_matrix_gbp <- correlation_matrix[2:6, 1]

total_assets_gbp <- merged_data %>% 
  group_by(`_id`) %>% 
  summarise(total_in_gbp = sum(asset_value_gbp))

merged_data_1 <- personality %>% 
  left_join(total_assets_gbp, by = c("X_id" = "_id"))

# Top 5: 
total_rank <- merged_data_1[order(merged_data_1$total_in_gbp,
                                  decreasing = T), 1]
total_rank_top5 <- total_rank[1:5]

merged_data[merged_data$`_id` == total_rank_top5[1], ]
merged_data[merged_data$`_id` == total_rank_top5[2], ]
merged_data[merged_data$`_id` == total_rank_top5[3], ]
merged_data[merged_data$`_id` == total_rank_top5[4], ]
merged_data[merged_data$`_id` == total_rank_top5[5], ]

merged_data_top5 <- rbind(
  merged_data[merged_data$`_id` == total_rank_top5[1], ], 
  merged_data[merged_data$`_id` == total_rank_top5[2], ], 
  merged_data[merged_data$`_id` == total_rank_top5[3], ], 
  merged_data[merged_data$`_id` == total_rank_top5[4], ], 
  merged_data[merged_data$`_id` == total_rank_top5[5], ]
)

write.csv(merged_data_top5, "top5.csv")

View(merged_data_top5[merged_data_top5$asset_allocation == "Crypto", ])

# Identifying -------------------------------------------------------------
gbp_totals <- assets %>%
  filter(asset_currency == "GBP") %>%
  group_by(`_id`) %>%
  summarise(total_gbp_assets = sum(asset_value, na.rm = TRUE))

# Find the person with the highest assets in GBP: 
max_gbp <- max(gbp_totals$total_gbp_assets)
id_gbp <- gbp_totals$`_id`[gbp_totals$total_gbp_assets == max_gbp]

# Find his/her Risk Tolerance Score: 
tolerance_gbp <- personality[personality$X_id == id_gbp, "risk_tolerance"]

## The individual identified as #134 holds the highest total assets, valued at £542.86, and has a corresponding risk tolerance score of 0.555. 
