setwd("C:/Users/AK194688/Downloads/data/data")
getwd()
rm(list = ls())
gc()

library(data.table)
library(tableone)
library(ggplot2)
library(fixest)
library(MatchIt)
library(dplyr)
library(wesanderson)
library(lubridate)

dt <- fread("ENCHANT_Gediz_merged_clean.csv")
dt=setDT(dt)

dt<- dt[, list(
  Period = as.IDate(Period, format = "%Y-%m-%d"),  # Convert to IDate
  mergecol = as.character(mergecol),              # Ensure character type
  `Consumption Point` = as.character(`Consumption Point`),  # Character for IDs
  Customer = as.character(Customer),              # Character for IDs
  District = as.character(District),              # Character for districts
  Region = as.factor(Region),                     # Factor for regions
  `Consumption Amount` = as.numeric(gsub(",", ".", `Consumption Amount`)),  # Convert to numeric
  treat = as.integer(treat),                      # Integer for treatment dummy
  id = as.character(id),                          # Character for combined IDs
  Consumption_log = as.numeric(Consumption_log),  # Ensure numeric for log
  temp = as.integer(temp)                         # Integer for temperature
)]

# Check the structure after assignments
str(dt)

# Define known treatment start dates
start_north <- as.Date("2021-11-01")  # Start date for North
start_south <- as.Date("2021-12-01")  # Start date for South
dt$time=dt$Period

# Add Period variable based on region and date
dt[, Period := ifelse(
  (Region == "North" & Period >= start_north) | 
    (Region == "South" & Period >= start_south) | 
    (Region == "Metropol" & Period >= start_north),  # Post-treatment for Metropol too
  "Post-Treatment", "Pre-Treatment"
)]


# head(dt)
# table(dt$Period, dt$Region)



# Set Theme for Plots
theme_set(theme_minimal())

# Define Panel Data
dt[, Treatment := ifelse(Region %in% c("North", "South"), 1, 0)]
dt[, Post := ifelse((Region == "North" & time >= "2021-11-01") |
                      (Region == "South" & time >= "2021-12-01"), 1, 0)]



dt$consumption=dt$'Consumption Amount'
# Descriptive Statistics (Pre-Outlier)
descriptive_pre <- dt[, .(
  Mean = mean(consumption, na.rm = TRUE),
  SD = sd(consumption, na.rm = TRUE),
  Min = min(consumption, na.rm = TRUE),
  Max = max(consumption, na.rm = TRUE),
  N_Obs = .N,
  N_Households = uniqueN(id)
), by = Region]

# Save Descriptive Table
#fwrite(descriptive_pre, "descriptive_pre.csv")




dt %>% group_by(Period,Region) %>% summarise(start=min(time),end=max(time))


# Boxplot with Outliers
ggplot(dt, aes(x = Region, y = consumption, fill = Region)) +
  geom_boxplot() +
  labs(x = "Region", y = "Consumption (kWh)") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3)) 



ggsave("boxplot_outliers.png", width = 8, height = 6)


# Boxplot shows we have negative values (correction in meter readings... i think we should get rid of ids with corrections to be save)


negative=dt %>% filter(consumption<0)
negative %>% group_by(Region,Post) %>% summarise(n=n()) 
ids=unique(negative$id)#we have 36 ids that we delete


dt=dt %>% filter(!id %in% ids)


# Boxplot with Outliers
ggplot(dt, aes(x = Region, y = consumption, fill = Region)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "Region", y = "Consumption (kWh)") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3)) 

ggsave("boxplot_outliers_non_zero.png", width = 8, height = 6)



# Remove Outliers

# Step 1: Add year to the original dataset
dt[, year := year(time)]

# Step 2: Aggregate to calculate annual consumption per household
dt_annual <- dt[, .(
  Annual_Consumption = sum(`Consumption Amount`, na.rm = TRUE)
), by = .(id, year,Region)]


head(dt_annual)

ggplot(dt_annual, aes(x = Annual_Consumption, fill = Region)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ year, scales = "free") +
  labs(
    title = "Density of Annual Electricity Consumption by Region and Year",
    x = "Annual Consumption (kWh)",
    y = "Density",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3)) 

# Step 3: Detect outliers based on annual consumption
iqr_values_annual <- dt_annual[, .(
  Q1 = quantile(Annual_Consumption, 0.25, na.rm = TRUE),
  Q3 = quantile(Annual_Consumption, 0.75, na.rm = TRUE),
  IQR = IQR(Annual_Consumption, na.rm = TRUE)
)]

iqr_values_annual[, `:=`(
  lower_bound = Q1 - 1.5 * IQR,
  upper_bound = Q3 + 1.5 * IQR
)]

# Step 4: Flag households with annual consumption outside bounds
dt_annual[, Outlier := Annual_Consumption < iqr_values_annual$lower_bound | Annual_Consumption > iqr_values_annual$upper_bound]

# Step 5: Merge back to the main data
dt <- merge(dt, dt_annual[, .(id, year, Outlier)], by = c("id", "year"), all.x = TRUE)

# Step 6: Filter out outliers
dt_clean <- dt[Outlier == FALSE | is.na(Outlier)]  # Include rows without annual data in filtering

# Step 7: Validate results
cat("Rows before cleaning:", nrow(dt), "\n")
cat("Rows after cleaning:", nrow(dt_clean), "\n")




ggplot(dt_clean, aes(x = Region, y = Consumption_log, fill = Region)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "Region", y = "Consumption (log)") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3)) 



ggplot(dt_clean, aes(x = Region, y = , fill = Region)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "Region", y = "Consumption (kWh)") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3))
ggsave("boxplot_outliers_after_outlier.png", width = 8, height = 6)


ggplot(dt_clean[`Consumption Amount` < 100], aes(x = `Consumption Amount`)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Region, scales = "free_y") +
  labs(
    title = "Frequency Plot of Electricity Consumption Below 100 kWh",
    x = "Electricity Consumption (kWh)",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave("Frequency_low_values.png", width = 8, height = 6)


ggplot(dt_clean[`Consumption Amount` == 0], aes(x = time)) +
  geom_bar(fill = "tomato", color = "black", alpha = 0.7) +
  labs(
    title = "Zero Electricity Consumption Over Time",
    x = "Time",
    y = "Frequency of Zero Values"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate monthly percentage of zero values for each region
dt_clean[, `:=`(
  Total_Obs = .N,
  Zero_Obs = sum(`Consumption Amount` == 0, na.rm = TRUE)  # Ensure NA values are handled
), by = .(Region, time)]

# Replace NA values in Total_Obs and Zero_Obs with 0
dt_clean[is.na(Total_Obs), Total_Obs := 0]
dt_clean[is.na(Zero_Obs), Zero_Obs := 0]

dt_clean[, Zero_Percent := (Zero_Obs / Total_Obs) * 100]

# Drop duplicates for unique (Region, time) combinations
dt_plot <- unique(dt_clean[, .(Region, time, Zero_Percent)])

# Create the faceted bar plot
ggplot(dt_plot, aes(x = time, y = Zero_Percent, fill = Region)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  facet_wrap(~ Region, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3)) +  # Apply color scheme
  labs(
    title = "Percentage of Zero Electricity Consumption Over Time",
    x = "Time",
    y = "Percentage of Zero Values"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


ggsave("Frequency_zero_values_time_region.png", width = 8, height = 6)


dt[, Zero_Percentage := sum(`Consumption Amount` == 0, na.rm = TRUE) / .N * 100, by = id]

# Summarize the result to view distribution of percentages
summary(dt$Zero_Percentage)





### What should we do with the zeros??? 
library(effectsize)


dt=dt_clean
outlier=dt %>% filter(Outlier==1)

ggplot(outlier, aes(x = Region, y = consumption, fill = Region)) +
  geom_boxplot() +
  labs(x = "Region", y = "Consumption (kWh)") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3))

ggsave("boxplot_only_outliers.png", width = 8, height = 6)


# Step 1: Calculate the number and percentage of zeros by Region
zero_distribution <- dt_clean[, .(
  Total_Obs = .N,
  Zero_Obs = sum(`Consumption Amount` == 0, na.rm = TRUE),
  Zero_Percent = mean(`Consumption Amount` == 0, na.rm = TRUE) * 100
), by = Region]


# Step 2: Perform Chi-Squared Test
chisq_test <- chisq.test(zero_distribution$Zero_Obs, p = rep(1/3, 3))  # Equal probability for 3 regions
print(chisq_test)

# Step 3: Calculate Cohen's d between regions
# Pairwise Cohen's d for Zero Percentages
cohen_d_pairs <- combn(unique(dt_clean$Region), 2, function(pair) {
  region1 <- pair[1]
  region2 <- pair[2]
  
  d_value <- cohens_d(
    dt_clean[Region == region1, Zero_Percent],
    dt_clean[Region == region2, Zero_Percent]
  )
  
  data.frame(
    Region1 = region1,
    Region2 = region2,
    Cohens_d = d_value$Cohens_d
  )
}, simplify = FALSE)

cohen_d_table <- do.call(rbind, cohen_d_pairs)
print(cohen_d_table)

rm(list = setdiff(ls(), "dt"))
gc()

#The distribtution of zero values is not equal....the effect size is big... 




# Descriptive Statistics (Post-Outlier)
descriptive_post <- dt_clean[, .(
  Mean = mean(consumption, na.rm = TRUE),
  SD = sd(consumption, na.rm = TRUE),
  Min = min(consumption, na.rm = TRUE),
  Max = max(consumption, na.rm = TRUE),
  N_Obs = .N,
  N_Households = uniqueN(id)
), by = Region]

fwrite(descriptive_post, "descriptive_post.csv")

# Boxplot Without Outliers
ggplot(dt_clean, aes(x = Region, y = consumption, fill = Region)) +
  geom_boxplot() +
  labs(x = "Region", y = "Consumption (kWh)") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3)) +
  ggsave("boxplot_no_outliers.png", width = 8, height = 6)




# Time Series Visualization
dt_time <- dt[, .(mean_consumption = mean(consumption, na.rm = TRUE)), by = .(time, Region)]
names(dt)
ggplot(dt_time, aes(x = time, y = mean_consumption, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Time", y = "Mean Consumption (kWh)") +
  scale_color_manual(values = wes_palette("FantasticFox1", 3)) 


ggsave("time_series.png", width = 10, height = 6)



# Calculate mean consumption per time and region
dt_time <- dt[, .(mean_consumption = mean(consumption, na.rm = TRUE)), by = .(time, Region)]

# Calculate first differences
dt_time[, first_diff := mean_consumption - shift(mean_consumption), by = Region]



# Plot first differences
ggplot(dt_time_diff, aes(x = time, y = first_diff, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = wes_palette("FantasticFox1", 3)) +  # Use Wes Anderson palette
  labs(
    title = "",
    x = "Time",
    y = "First Difference in Mean Consumption (kWh)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("time_series_FD.png", width = 10, height = 6)

#frwite(dt,'ENCHANT_Gediz_merged_clean2.csv')
dt=fread('ENCHANT_Gediz_merged_clean2.csv')

rm(list = setdiff(ls(), "dt"))

names(dt)
essential_vars <- c("id", "time", "Period", "Region", 'Treatment','District' ,'Customer', "consumption",'Consumption_log','Consumption Point',"temp")

# Subset the dataset to include only the essential variables
dt <- dt[, ..essential_vars]


head(dt)

dt$Post=ifelse(dt$Period=='Pre-Treatment',0,1)
dt$did=dt$Post*dt$Treatment
head(dt)


dt$consumption_new <- ifelse(dt$consumption == 0, NA, dt$consumption)
# Difference-in-Differences (DiD) Analysis
model_did <- feols(
  log(consumption_new)~ Post*Treatment| id + time,
  data = dt,
  cluster = ~id
)


etable(model_did)


mean(dt_MN$consumption, na.rm = TRUE)

dt[, time_indicator := as.factor(time)]

head(dt$time_indicator)

dt <- dt %>%
  mutate(
    Treatment_Start = case_when(
      Region == "North" ~ as.Date("2021-11-01"),
      Region == "South" ~ as.Date("2021-12-01"),
      Region == "Metropol" ~ as.Date("2021-11-01"),  # Align control group timeline
      TRUE ~ NA_Date_
    ),
    time_to_treatment = as.numeric(difftime(time, Treatment_Start, units = "days")) / 30,
    time_indicator = as.integer(round(time_to_treatment))
  )
# Verify the first few rows of the relevant columns
dt %>%
  select(Region, Treatment_Start, time, time_to_treatment, time_indicator) %>%
  head()

dt_MN=dt %>% filter(Region!='South')
setDT(dt_MN)


rm(model_did)
model_did_MN <- feols(
  consumption_new~ Post*Treatment| id + time,
  data = dt_MN,
  cluster = ~id
)

etable(model_did_MN)


plot(density(dt_MN$consumption_new, na.rm = T))

summary(dt_MN$consumption_new)


table(is.na(dt$consumption_new))

model_event_study <- feols(
  consumption_new ~ i(time_indicator, ref = 0):Treatment + temp | id + time,
  data = dt_MN,
  cluster = ~id
)


  iplot(model_event_study, main = "Event Study Results", xlab = "Months Relative to Treatment", ylab = "Log(Consumption)")
#################################################################

dt_MN=dt %>% filter(Region!='South')
setDT(dt_MN)
mean(dt_MN$consumption_new)

pre_data=dt_MN %>% filter(Post==0)
# Calculate mean consumption and confidence intervals
time_series_data <- pre_data[, .(
  mean_consumption = mean(consumption_new, na.rm = TRUE),
  se_consumption = sd(consumption_new, na.rm = TRUE) / sqrt(.N), # Standard error
  lower_ci = mean(consumption_new, na.rm = TRUE) - 1.96 * (sd(consumption_new, na.rm = TRUE) / sqrt(.N)), # Lower CI
  upper_ci = mean(consumption_new, na.rm = TRUE) + 1.96 * (sd(consumption_new, na.rm = TRUE) / sqrt(.N))  # Upper CI
), by = .(time, Region)]

# Plot the time series with confidence intervals
ggplot(time_series_data, aes(x = time, y = mean_consumption, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Region), alpha = 0.2, color = NA) +
  scale_color_manual(values = wes_palette("FantasticFox1", 3)) +
  scale_fill_manual(values = wes_palette("FantasticFox1", 3)) +
  labs(
    title = "Mean Electricity Consumption Over Time by Region",
    x = "Time",
    y = "Mean Electricity Consumption (kWh)",
    color = "Region",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )



#########################################################################

rm(dt)
library(MatchIt)
pre_data <- pre_data[!is.na(pre_data$consumption_new) & is.finite(pre_data$consumption_new), ]

match_model <- matchit(
  Treatment ~ log(consumption_new), 
  data = pre_data, 
  method = "nearest", 
  distance = "glm"
)

matched_data <- match.data(match_model)

summary(match_model)

trends_data <- matched_data %>%
  group_by(Treatment, time) %>%
  summarize(mean_consumption = mean(consumption_new, na.rm = TRUE), .groups = "drop")

trends_data <- trends_data %>%
  filter(time >= as.Date('2020-07-01'))

ggplot(trends_data, aes(x = time, y = mean_consumption, color = as.factor(Treatment), group = Treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Parallel Trends: Treatment vs. Control",
    x = "Time",
    y = "Mean Consumption (kWh)",
    color = "Group"
  ) +
  scale_color_manual(values = wes_palette("FantasticFox1", 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
matched_ids <- unique(matched_data$id)


  
  # Filter the full dataset to include only matched IDs
filtered_data <- dt_MN %>%
    filter(id %in% matched_ids)
rm(model_event_study)

filtered_data <- filtered_data %>%
  filter(time >= as.Date('2020-05-01'))

model_event_study <- feols(
  log(consumption_new)~ i(time_to_treatment, ref = 0):Treatment |id+ time,
  data = filtered_data,
  cluster = ~id
)

summary(model_event_study)

iplot(model_event_study, main = "Event Study Results", xlab = "Months Relative to Treatment", ylab = "Consumption")

