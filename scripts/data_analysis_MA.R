# Data analysis for MA thesis MEG
# Dani Gargya
# June 24

# workflow ----
# loading all relevant datasets
# calculating means mzp3
# harmonising data sets, combining them
# making overview of all datasets
# exlcuding group 1 from all datasets
# check for normality of data
# check for reliability (internal consistency) through cronbachs alpha
# statistical analysis data
### Kruskal Wallis
### 

# (making pretty graphs)

# loading libraries ----
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggthemes) # for data visualisation
library(treemapify) # for data vis
library(dplyr)
library(purrr)
library(psych) # for cronbach alpha
library(ggrepel)



# importing relevant data ----
mzp1_clean <- read.csv("data/data_collection/data_pauli/angell_pre.csv")
mzp2_clean <- read.csv("data/data_collection/data_pauli/angell_post.csv")
mzp3_clean <- read.csv("data/data_collection/angell_mzp3.csv")


# calculate means mzp3 ----
# Define a function to calculate the mean, excluding -100 and NAs
mean_exclude_negative_100 <- function(row) {
  values <- row[row != -100 & !is.na(row)]
  if (length(values) == 0) {
    return(NA)  # Return NA if all values are excluded
  }
  return(mean(values))
}

# Calculate the mean values for specific groups of columns and add them as new columns
#AT_mean
mzp3_clean$AT_mean <- apply(mzp3_clean[, c("AT01_01", "AT01_02_inverse", "AT01_03", "AT01_04")], 1, mean_exclude_negative_100)

#B_mean
mzp3_clean$B_mean <- apply(mzp3_clean[, c("B001_01", "B001_02", "B001_03_inverse", "B001_04_inverse", "B001_05", "B001_06", "B001_07", "B001_08_inverse", "B001_09_inverse", "B001_10")], 1, mean_exclude_negative_100)

# CS_mean
mzp3_clean$CS_mean <- apply(mzp3_clean[, c("CS01_01", "CS01_02", "CS01_03")], 1, mean_exclude_negative_100)

# IN_mean
mzp3_clean$IN_mean <- apply(mzp3_clean[, c("IN01_01", "IN01_02", "IN01_03")], 1, mean_exclude_negative_100)

# PB_mean
mzp3_clean$PB_mean <- apply(mzp3_clean[, c("PB01_01", "PB01_02")], 1, mean_exclude_negative_100)

# SN_mean
mzp3_clean$SN_mean <- apply(mzp3_clean[, c("SN01_01", "SN01_02", "SN01_03")], 1, mean_exclude_negative_100)

# SW_mean
mzp3_clean$SW_mean <- apply(mzp3_clean[, c("SW01_01", "SW01_02", "SW01_03_inverse", "SW01_04", "SW01_05_inverse", "SW01_06", "SW01_07", "SW01_08")], 1, mean_exclude_negative_100)

# TPB_mean
mzp3_clean$TPB_mean <- apply(mzp3_clean[, c("AT_mean", "SN_mean", "PB_mean", "IN_mean", "B_mean")], 1, mean_exclude_negative_100)

# SW_CS_mean
mzp3_clean$SW_CS_mean <- apply(mzp3_clean[, c("SW_mean", "CS_mean")], 1, mean_exclude_negative_100)


# harmonising and combining dfs ----
# Add a time point indicator to each data frame
mzp1_clean$TP <- "t1"
mzp2_clean$TP <- "t2"
mzp3_clean$TP <- "t3"


# MZP3
# Select only the columns that include '_mean' in their names along with 'TP' and 'Group'
selected_columns_t3 <- mzp3_clean %>%
  select(TP, Group, contains("_mean")) %>%
  rename (Time_Point = TP) 


# Convert to long format
long_df_t3 <- selected_columns_t3 %>%
  pivot_longer(
    cols = contains("_mean"),
    names_to = "Category",
    values_to = "MeanValue")%>%
  mutate(Category = case_when(
    Category == "AT_mean" ~ "AT_Mean",
    Category == "B_mean" ~ "B_Mean",
    Category == "CS_mean" ~ "CS_Mean",
    Category == "IN_mean" ~ "INT_Mean",
    Category == "PB_mean" ~ "PBC_Mean",
    Category == "SN_mean" ~ "SN_Mean",
    Category == "SW_mean" ~ "SW_Mean",
    Category == "TPB_mean" ~ "TPB_Mean",
    Category == "SW_CS_mean" ~ "SW_CS_Mean",
    TRUE ~ Category  # Keep other values unchanged
  ))                  

# mzp2
# Select only the columns that include '_Mean' in their names along with 'TP' and 'Group'
selected_columns_t2 <- mzp2_clean %>%
  select(TP, Gruppe, contains("_Mean")) %>%
  rename(Time_Point = TP,
         Group = Gruppe) %>%
  mutate(Group = case_when(
   Group == 0 ~ "group0",
    Group == 1 ~ "group1",
   Group == 2 ~ "group2",
   TRUE ~ as.character(Group))) %>%
  mutate(Group = as.factor(Group))

# Convert to long format
long_df_t2 <- selected_columns_t2 %>%
  pivot_longer(
    cols = contains("_Mean"),
    names_to = "Category",
    values_to = "MeanValue"
  )


# mzp 1
# Select only the columns that include '_mean' in their names along with 'TP' and 'Group'
selected_columns_t1 <- mzp1_clean %>%
  rename(TPB_Mean = TPB_Mittelwert,
         Group = Gruppe,
         Time_Point = TP) %>%
 select(Time_Point, Group, contains("_Mean")) %>%
 mutate(Group = case_when(
  Group == 0 ~ "group0",
  Group == 1 ~ "group1",
  Group == 2 ~ "group2",
  TRUE ~ as.character(Group))) %>%
 mutate(Group = as.factor(Group))

# Convert to long format
long_df_t1 <- selected_columns_t1 %>%
    pivot_longer(
      cols = contains("_Mean"),
      names_to = "Category",
      values_to = "MeanValue"
    )

# Combine the data frames
combined_df <- rbind(long_df_t1, long_df_t2, long_df_t3) #1617 obs.

# Convert Group, Category and TP to factors
combined_df$Group <- as.factor(combined_df$Group)
combined_df$Time_Point <- as.factor(combined_df$Time_Point)
combined_df$Category <- as.factor(combined_df$Category)


# overview of all datasets with treemap ----
# Summarize the data: count number of answers per group per time point
summary_data <- combined_df %>%
  group_by(Group, Time_Point) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Count = ifelse(Time_Point %in% c("t1", "t2"), Count / 6, Count / 9)) %>% # adjust with calculated means per mzp
  mutate(Alpha = ifelse(Group == "group1", 0.1, 1))  #for displaying group1 that is excluded differently


# Create the treemap
##? add use of different pattern for gorup 1?
(treemap_mzps_groups <- ggplot(summary_data, aes(area = Count, fill = as.factor(Group), label = Count,
                                         subgroup = as.factor(Time_Point), subgroup2 = as.factor(Group))) +
  geom_treemap(aes(alpha = Alpha)) +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  geom_treemap_subgroup2_border(colour = "white", size = 1) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, colour = "black", fontface = "italic", reflow = TRUE) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_identity() +
  #scale_fill_treemap(values = c("group0" = "stripe", "group1" = "dot", "group2" = "solid")) +  # Use pattern fills
  labs(title = "Treemap of Answers per Group per Time Point", fill = "Group") +
  theme_clean()) # Adjust the limits to increase space between timepoints)

ggsave(treemap_mzps_groups, filename = "outputs/treemap_mzps_groups.png",
       height = 5, width = 8)

# excluding group 1 from dataset ----
combined_df_g02 <- combined_df %>%
  filter(Group != "group1")

# check for normality of data ----
### MZP1
# exclude group1
selected_columns_t1 <- selected_columns_t1 %>%
  filter(Group != "group1")

# Find columns with '_Mean' in their name
mean_cols_t1 <- grep("_Mean", names(selected_columns_t1), value = TRUE)

# Function to conduct normality test and return results
normality_test_t1 <- function(col) {
  shapiro_test <- shapiro.test(selected_columns_t1[[col]])
  tibble(Column = col, 
         Test = "Shapiro-Wilk",
         W = shapiro_test$statistic,
         P_Value = shapiro_test$p.value,
         Normal = shapiro_test$p.value >= 0.05)
}

# Map function over columns and combine results
normality_results_t1 <- map_dfr(mean_cols_t1, normality_test_t1)

### MZP2
# exclude group1
selected_columns_t2 <- selected_columns_t2 %>%
  filter(Group != "group1")

# Find columns with '_Mean' in their name
mean_cols_t2 <- grep("_Mean", names(selected_columns_t2), value = TRUE)

# Function to conduct normality test and return results
normality_test_t2 <- function(col) {
  shapiro_test <- shapiro.test(selected_columns_t2[[col]])
  tibble(Column = col, 
         Test = "Shapiro-Wilk",
         W = shapiro_test$statistic,
         P_Value = shapiro_test$p.value,
         Normal = shapiro_test$p.value >= 0.05)
}

# Map function over columns and combine results
normality_results_t2 <- map_dfr(mean_cols_t2, normality_test_t2)

### MZP3
# exclude group1
selected_columns_t3 <- selected_columns_t3 %>%
  filter(Group != "group1")

# Find columns with '_Mean' in their name
mean_cols_t3 <- grep("_mean", names(selected_columns_t3), value = TRUE)

# Function to conduct normality test and return results
normality_test_t3 <- function(col) {
  shapiro_test <- shapiro.test(selected_columns_t3[[col]])
  tibble(Column = col, 
         Test = "Shapiro-Wilk",
         W = shapiro_test$statistic,
         P_Value = shapiro_test$p.value,
         Normal = shapiro_test$p.value >= 0.05)
}

# Map function over columns and combine results
normality_results_t3 <- map_dfr(mean_cols_t3, normality_test_t3)


### calculating internal validity with cronbach alpha MZP3 ----
# stichprobengröße, siehe sample size calculator


# Convert -100 to NA
mzp3_cleaner <- mzp3_clean %>%
  filter(Group != "group1") %>%
  mutate(across(everything(), ~ ifelse(. == -100, NA, .)))

# Get column names with _inverse
columns_with_inverse <- grep("_inverse$", names(mzp3_cleaner), value = TRUE)
# Remove "_inverse" from column names
columns_to_exclude_without_inverse <- gsub("_inverse", "", columns_with_inverse)

filtered_df_cronbach <- mzp3_cleaner %>%
  select(matches("_0[1-9]$|_10$|_inverse"), -matches("WD")) %>%
  select(-one_of(columns_to_exclude_without_inverse)) %>%
  select(-c("B001_03_inverse", "B001_04_inverse")) #excluding these two items to  increase Cronbachs alpha, see MA Pauli S.70


# Handle missing values by dropping rows with NAs
#filtered_df_cronbach <- filtered_df_cronbach %>%
#  na.omit() --> only 14 left!

# handle missing values by using the mean
filtered_df_cronbach <- filtered_df_cronbach %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Group the columns based on the first two letters of the column name
groups <- split.default(filtered_df_cronbach, sub("^(..).*", "\\1", names(filtered_df_cronbach)))


# Calculate Cronbach's alpha for each group
alpha_results <- lapply(groups, function(x) {
  result <- alpha(x)
  result$total$raw_alpha  # Extracting raw alpha from the result
})


# Create a dataframe with scale names and Cronbach's alpha values
df_alpha <- data.frame(
  Scale = names(alpha_results),
  Alpha = unlist(alpha_results)
)

# Statistical analyses ----
#write_csv(x=combined_df, path="data/data_collection/combined_df.csv")
# are the next two useful?
#results_kw <- combined_df_g02 %>%
 # group_by(Category) %>%
  #summarise(
   # Kruskal_Wallis = list(kruskal.test(MeanValue ~ interaction(Group, Time_Point), data = .)))

# Extract and print results for each category
#results_kw <- results_kw %>%
 # mutate(
  #  Statistic = map_dbl(Kruskal_Wallis, "statistic"),
   # p_value = map_dbl(Kruskal_Wallis, "p.value")) %>%
  #select(Category, Statistic, p_value)

# RQ1 Groups and categories Kruskal Wallis test ----
# Perform Kruskal-Wallis test for each competence and time point
# only comparision by time_point? weg?
rq1_kw_group_category <- combined_df_g02 %>%
  group_by(Category, Time_Point) %>%
  summarise(
    kruskal_p = kruskal.test(MeanValue ~ Group)$p.value,
    .groups = 'drop')

# necessary? --> how to include p-values of kruskal test in results?
# Merge the p-values back into the original dataframe for plotting
#combined_df_g02 <- combined_df_g02 %>%
 # left_join(rq1_kw_group_category, by = c("Category", "Time_Point")) 

#combined_df_g02 <- combined_df_g02 %>%
 # mutate(label = paste("p =", round(kruskal_p.x, 3)))

  
# Calculate the means for each Group, Competence, and TimePoint
# adding error bars
# excluding irrelevant groups
# still needed but old?
df_means <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Group, Category, Time_Point) %>%
  summarise(MeanValue2 = mean(MeanValue), 
            LowerCI = MeanValue2 - qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
            UpperCI = MeanValue2 + qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
            .groups = 'drop')


# Plot the data
# with error bars
# old? no signigicance stars?
(rq1_graph_prettier <- ggplot(df_means, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  facet_wrap(~ Category, scales = "free_y") +
  labs(title = "Comparison of Groups by Competences and Time Points",
       x = "Time Point",
       y = "Mean Value",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "bottom"))
  #geom_text(data = combined_df_g02, aes(x = 1, y = Inf, label = label), 
   #           vjust = 1.5, hjust = 0, size = 3, inherit.aes = FALSE, color = "black"))

ggsave(rq1_graph_prettier, file = "outputs/rq1_graph_prettier.png", width = 7, height = 5)

### add kruskal-wallis test results into graph ----
# also comparing within groups between timepoints

# Perform Kruskal-Wallis Test for Group Comparisons
rq1_kw_group_category <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Category, Time_Point) %>%
  summarise(
    kruskal_p_group = kruskal.test(MeanValue ~ Group)$p.value,
    .groups = 'drop')


# Perform Kruskal-Wallis Test for Time Point Comparisons
rq1_kw_time_category <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Category, Group) %>%
  summarise(
    kruskal_p_time = kruskal.test(MeanValue ~ Time_Point)$p.value,
    .groups = 'drop')

#Merge the Results into combined_df_g02
combined_df_g02 <- combined_df_g02 %>%
  left_join(rq1_kw_group_category, by = c("Category", "Time_Point")) %>%
  left_join(rq1_kw_time_category, by = c("Category", "Group"))

# Function to create a combined label for display
create_p_value_label <- function(group_p, time_p) {
  paste0("Group p = ", round(group_p, 3), "\nTime Point p = ", round(time_p, 3))
}


# Create labels for each combination of Category, Group, and Time_Point
combined_df_g02 <- combined_df_g02 %>%
  mutate(p_value_label = create_p_value_label(kruskal_p_group, kruskal_p_time))


# Create the p-value labels
# needed?
combined_df_g02 <- combined_df_g02 %>%
  mutate(
    p_value_label_group = ifelse(!is.na(kruskal_p_group), paste0("Group p = ", round(kruskal_p_group, 3)), NA),
    p_value_label_time = ifelse(!is.na(kruskal_p_time), paste0("Time p = ", round(kruskal_p_time, 3)), NA)
  )


### trying with stars
# Create a new column indicating significant points
# Filter for significant p-values (e.g., p < 0.05)
combined_df_g02 <- combined_df_g02 %>%
  mutate(significance_label = ifelse((!is.na(kruskal_p_group) & kruskal_p_group < 0.05) | 
                                       (!is.na(kruskal_p_time) & kruskal_p_time < 0.05), "*", ""))



### for stars
# Ensure that your original data includes MeanValue2 for plotting
# how come there are sometimes 2 stars?
df_means2 <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Category, Time_Point, Group) %>%
  summarise(
    MeanValue2 = mean(MeanValue, na.rm = TRUE), 
    LowerCI = MeanValue2 - sd(MeanValue, na.rm = TRUE) / sqrt(n()), 
    UpperCI = MeanValue2 + sd(MeanValue, na.rm = TRUE) / sqrt(n()),
    significance_label = first(significance_label),
    .groups = 'drop'
  )

# Plot with p-value annotations and stars
(rq1_graph_prettier <- ggplot(df_means2, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  facet_wrap(~ Category, scales = "free_y") +
  labs(title = "Comparison of Groups by Competences and Time Points",
       x = "Time Point",
       y = "Mean Value",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_text(aes(label = significance_label), vjust = -0.5, size = 6, color = "red"))




### RQ1 use both kruskal and wilcoxon test ----
# Kruskal-Wallis test for time points within each category and group
kw_results <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Category, Group) %>%
  summarise(
    kruskal_p_time = kruskal.test(MeanValue ~ Time_Point)$p.value,
    .groups = 'drop') %>%
  mutate(significance_time = case_when(
      kruskal_p_time < 0.001 ~ "***",
      kruskal_p_time < 0.01 ~ "**",
      kruskal_p_time < 0.05 ~ "*",
      TRUE ~ ""))

# Wilcoxon rank-sum test for groups within each category and time point
wilcox_results <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Category, Time_Point) %>%
  summarise(
    wilcox_p_group = wilcox.test(MeanValue ~ Group)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(
    significance_group = case_when(
      wilcox_p_group < 0.001 ~ "***",
      wilcox_p_group < 0.01 ~ "**",
      wilcox_p_group < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Merge results back into the main dataframe
combined_df_g02 <- combined_df_g02 %>%
  left_join(kw_results, by = c("Category", "Group")) %>%
  left_join(wilcox_results, by = c("Category", "Time_Point"))


# Summarize significance information for each mean data point
# old?
significance_summary <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Category, Time_Point, Group) %>%
  summarise(
    significance_group.y = ifelse(any(significance_group.y != ""), "*", ""),
    significance_time.y = ifelse(any(significance_time.y != ""), "+", "")
  )

# Merge the summarized significance values into df_means
df_means <- df_means %>%
  left_join(significance_summary, by = c("Category", "Time_Point", "Group"))


# Plot with significance symbols
(rq1_graph_prettier <- ggplot(df_means, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  facet_wrap(~ Category, scales = "free_y") +
  labs(title = "Comparison of Groups by Competences and Time Points",
       x = "Time Point",
       y = "Mean Value",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_text_repel(data = df_means %>% filter(significance_group.y.y != ""), 
                    aes(x = Time_Point, y = MeanValue2, label = significance_group.y.y), 
                    vjust = -0.5, color = "black", size = 5, inherit.aes = FALSE) +  # Adjust vjust here
  geom_text_repel(data = df_means %>% filter(significance_time.y.y != ""), 
                    aes(x = Time_Point, y = MeanValue2, label = significance_time.y.y), 
                    vjust = -1.0, color = "blue", size = 3, inherit.aes = FALSE))  # Adjust vjust here

# RQ2 Groups and TPB/ SW at MZP3 ----
# make graph with two lines, one for each group plotting on TPB, SW
# Plot with separate lines for each group
(rq2_groups_tpb_sw <- ggplot(mzp3_cleaner, aes(x = TPB_mean, y = SW_mean, color = Group)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +  # Separate lines for each group
  ylim(0, 3) + 
  theme_clean() +
  annotate("text", size = 2, x = 1, y = 2.5, label = paste("Spearman's rho:", round(spearman_cor, 2), "\n p-value:", round(spearman_pval, 3))) +
  labs(x = "\nMean Sustainability competences", y = "Mean Self-efficacy beliefs\n") +
  theme(legend.position = "bottom"))
# how to interpret?!

#### RQ? mzp3 comparing SW/TPB for groups----
df_tp3 <- combined_df %>%
  filter(Time_Point == "t3" & Category %in% c("SW_Mean", "TPB_Mean"))

# Calculate means for each Group and Competence
df_means3 <- df_tp3 %>%
  group_by(Group, Category) %>%
  summarise(
    MeanValue2 = mean(MeanValue),
    SEM = sd(MeanValue) / sqrt(n()),  # Standard Error of the Mean
    .groups = 'drop'
  )

# Create the plot
(graph_tp3_compare_sw_tpb <- ggplot(df_means3, aes(x = Category, y = MeanValue2, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = MeanValue2 - SEM, ymax = MeanValue2 + SEM), 
                  position = position_dodge(0.7), width = 0.2) +
    labs(title = "Comparison of Mean Values for Competences at TimePoint3",
       x = "Competence",
       y = "Mean Value",
       fill = "Group") +
  theme_clean() +
  theme(legend.position = "bottom"))

ggsave(graph_tp3_compare_sw_tpb, file = "outputs/graph_tp3_compare_sw_tpb.png", width = 7, height = 5)

### prettier version with point plot?
# Create the point plot with error bars
(graph_tp3_compare_sw_tpb_prettier <- ggplot(df_means3, aes(x = Category, y = MeanValue2, color = Group, group = Group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(aes(ymin = MeanValue2 - SEM, ymax = MeanValue2 + SEM), 
                position = position_dodge(0.5), width = 0.2) +
  labs(title = "Comparison of Mean Values for Competences at TimePoint3",
       x = "Competence",
       y = "Mean Value",
       color = "Group") +
  theme_clean() +
  theme(legend.position = "bottom"))

ggsave(graph_tp3_compare_sw_tpb_prettier, file = "outputs/graph_tp3_compare_sw_tpb_prettier.png", width = 7, height = 5)

##### RQ3 SW and TPB ----
# Scatter plot to visualize the relationship
plot(mzp3_clean$TPB_mean, mzp3_clean$SW_mean, main = "Scatter plot of TPB_mean vs SW_mean", xlab = "TPB", ylab = "SW")

# Shapiro-Wilk normality test
shapiro.test(mzp3_clean$TPB_mean) #normally distributed p>0.05
shapiro.test(mzp3_clean$SW_mean) #not normally distributed p<0.05

# Pearson correlation (but not normally distributed data, so not useful?)
correlation_SW <- cor(mzp3_clean$TPB_mean, mzp3_clean$SW_mean)
print(correlation_SW)

# Spearman correlation
spearman_cor <- cor(mzp3_clean$TPB_mean, mzp3_clean$SW_mean, method = "spearman")
print(paste("Spearman correlation:", spearman_cor))
# 0.79 -> strong

spearman_test <- cor.test(mzp3_clean$TPB_mean, mzp3_clean$SW_mean, method = "spearman")
print(spearman_test)
spearman_cor <- spearman_test$estimate
spearman_pval <- spearman_test$p.value
# p value rejects 0 hypothesis of no correlation -> relevant

# Kendall correlation
kendall_cor <- cor(mzp3_clean$TPB_mean, mzp3_clean$SW_mean, method = "kendall")
print(paste("Kendall correlation:", kendall_cor))
# 0.81 -> very strong

# Perform Kendall's tau correlation test
kendall_test <- cor.test(mzp3_clean$TPB_mean, mzp3_clean$SW_mean, method = "kendall")
print(kendall_test)
# p value rejects 0 hypothesis of no correlation -> relevant

# Plot with annotation (SPEARMAN)
(graph_cor_rq3 <-ggplot(mzp3_clean, aes(x = TPB_mean, y = SW_mean)) +
  geom_point(alpha = 0.5, size = 2, color = "#7CFC00") +
  geom_smooth(method = "lm", se = FALSE, color = "#A6761D") +
  ylim(0, 3) + 
  theme_clean() +
  annotate("text", size = 2, x = 1, y = 2.5, label = paste("Spearman's rho:", round(spearman_cor, 2), "\n p-value:", round(spearman_pval, 3))) +
  labs(x = "\nMean Sustainability competences", y = "Mean Self-efficacy beliefs\n"))

ggsave(graph_cor_rq3, file = "outputs/graph_cor_rq3.png", width = 7, height = 5)


#### correlation SW and CS ----
mzp3_cleaner <- mzp3_clean %>%
  filter(!is.na(CS_mean)) %>%
  filter(Group != "group1") 


plot(mzp3_cleaner$SW_mean, mzp3_cleaner$CS_mean,  main = "Scatter plot of SW_mean and CS_mean", xlab = "SW", ylab = "CS")

# Shapiro-Wilk normality test
shapiro.test(mzp3_cleaner$CS_mean) #not normally distributed p<0.05
shapiro.test(mzp3_cleaner$SW_mean) #not normally distributed p<0.05

# Pearson correlation (but not normally distributed data, so not useful?)
correlation_SW_CS <- cor(mzp3_cleaner$SW_mean, mzp3_cleaner$CS_mean)
print(correlation_SW_CS)
# 0.8

# Spearman correlation
spearman_cor_sw_cs <- cor(mzp3_cleaner$SW_mean, mzp3_cleaner$CS_mean, method = "spearman")
print(paste("Spearman correlation:", spearman_cor_sw_cs))
# 0.67

spearman_test_sw_cs <- cor.test(mzp3_cleaner$SW_mean, mzp3_cleaner$CS_mean, method = "spearman")
print(spearman_test_sw_cs)
# p value (<0.05) rejects 0 hypothesis of no correlation -> relevant
# 0.67

spearman_cor_sw_cs <- spearman_test_sw_cs$estimate
spearman_pval_sw_cs <- spearman_test_sw_cs$p.value

# Kendall correlation
kendall_cor_sw_cs <- cor(mzp3_cleaner$SW_mean, mzp3_cleaner$CS_mean, method = "kendall")
print(paste("Kendall correlation:", kendall_cor))
# 0.66 -> strong

# Perform Kendall's tau correlation test
kendall_test_sw_cs <- cor.test(mzp3_cleaner$SW_mean, mzp3_cleaner$CS_mean, method = "kendall")
print(kendall_test_sw_cs)
# p value rejects 0 hypothesis of no correlation -> relevant
# 0.57


# Plot with annotation (SPEARMAN)
(rq3_graph_cor_sw_cs <-ggplot(mzp3_cleaner, aes(x = SW_mean, y = CS_mean)) +
    geom_point(alpha = 0.5, size = 2, color = "#7CFC00") +
    geom_smooth(method = "lm", se = FALSE, color = "#A6761D") +
    ylim(0, 3) + 
    theme_clean() +
    annotate("text", size = 2, x = 1, y = 2.5, label = paste("Spearman's rho:", round(spearman_cor_sw_cs, 2), "\n p-value:", round(spearman_pval_sw_cs, 3))) +
    labs(x = "\nMean Individual Self-efficacy beliefs", y = "Mean Collective Self-efficacy beliefs\n"))

ggsave(rq3_graph_cor_sw_cs, file = "outputs/rq3_graph_cor_sw_cs.png", width = 7, height = 5)


# clean theme ----
theme_clean <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 15, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_text(size = 12, face = "bold"),                              
          legend.position = c(0.2, 0.8))
}



# create colour palette ----
display.brewer.pal(n = 8, name = 'Dark2')
brewer.pal(n = 8, name = "Dark2")
display.brewer.pal(n=4, name = "Set1")
brewer.pal(n=4, name = "Set1")