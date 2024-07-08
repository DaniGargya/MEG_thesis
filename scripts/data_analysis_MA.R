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
### RQ1: Kruskal Wallis and wilcoxon test
### RQ1: Spearman and Eta?
### RQ2: Spearman

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
library(readxl)



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
### RQ1 kruskal and wilcoxon test groups, timepoints and display in graph with stars ----
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


# Calculate the means for each Group, Competence, and TimePoint
# adding error bars
# excluding irrelevant groups
df_means <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Group, Category, Time_Point) %>%
  summarise(MeanValue2 = mean(MeanValue), 
            LowerCI = MeanValue2 - qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
            UpperCI = MeanValue2 + qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
            .groups = 'drop') %>%
  left_join(kw_results, by = c("Category", "Group")) %>%
  left_join(wilcox_results, by = c("Category", "Time_Point"))



# Define a labeller function to rename facets
facet_labeller <- labeller(Category = c(
  TPB_Mean = "Cumulated Sus Competences",
  AT_Mean = "Attitude",
  B_Mean = "Behaviour",
  INT_Mean = "Intention",
  PBC_Mean = "Perceived behaviour control",
  SN_Mean = "Subjective norms"))


# Plot with significance symbols
(rq1_graph_prettier_stars <- ggplot(df_means, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  facet_wrap(~ Category, scales = "fixed", labeller = facet_labeller) +
  labs(title = "Comparison of Groups by Competences and Time Points",
       x = "Time Point",
       y = "Mean Value",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_text_repel(data = df_means %>% filter(significance_group != ""),
                    aes(x = Time_Point, y = MeanValue2, label = significance_group), 
                    vjust = -0.5, color = "black", size = 5, inherit.aes = FALSE) +  # Adjust vjust here
  geom_text_repel(data = df_means %>% filter(significance_time != ""), 
                    aes(x = Time_Point, y = MeanValue2, label = significance_time), 
                    vjust = -1.0, color = "blue", size = 3, inherit.aes = FALSE))  # Adjust vjust here

ggsave(rq1_graph_prettier_stars, file = "outputs/rq1_graph_prettier_stars.png", width = 7, height = 5)

# making CSC bigger
library(patchwork)

# only one star
# Summarize df_means to include only one significance annotation per time point with the correct number of stars
df_means_grouped <- df_means %>%
  group_by(Category, Time_Point) %>%
  summarize(MeanValue2 = mean(MeanValue2, na.rm = TRUE),
            LowerCI = mean(LowerCI, na.rm = TRUE),
            UpperCI = mean(UpperCI, na.rm = TRUE),
            significance_group = paste(unique(significance_group[significance_group != ""]), collapse = ""),
            significance_time = paste(unique(significance_time[significance_time != ""]), collapse = "")) %>%
  ungroup()

# Create a plot for "Cumulated Sustainability Competences" without legend
(cumulated_plot <- ggplot(df_means %>% filter(Category == "TPB_Mean"), aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  labs(title = "Cumulated Sustainability Competences", x = "Time Point", y = "Mean Value") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(1, 2.5)) +  # Set y-axis limits to 1-2.5
  geom_text_repel(data = df_means_grouped %>% filter(Category == "TPB_Mean" & significance_group != ""),
                    aes(x = Time_Point, y = MeanValue2, label = significance_group), 
                    vjust = -0.5, color = "black", size = 5, inherit.aes = FALSE) +  # Adjust vjust here
  geom_text_repel(data = df_means_grouped %>% filter(Category == "TPB_Mean" & significance_time != ""), 
                    aes(x = Time_Point, y = MeanValue2, label = significance_time), 
                    vjust = -1.0, color = "blue", size = 3, inherit.aes = FALSE))


# Create a plot for the other categories with consistent y-axis scales
(other_plots <- ggplot(df_means %>% filter(Category != "TPB_Mean"), aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  facet_wrap(~ Category, scales = "fixed", labeller = facet_labeller) +
  labs(x = "Time Point", y = "Mean Value", color = "Group") +
  theme_minimal() +
  theme(legend.position = "right", plot.title = element_blank()) +
  geom_text_repel(data = df_means_grouped %>% filter(Category != "TPB_Mean" & significance_group != ""),
                    aes(x = Time_Point, y = MeanValue2, label = significance_group), 
                    vjust = -0.5, color = "black", size = 5, inherit.aes = FALSE) +  # Adjust vjust here
  geom_text_repel(data = df_means_grouped %>% filter(Category != "TPB_Mean" & significance_time != ""), 
                    aes(x = Time_Point, y = MeanValue2, label = significance_time), 
                    vjust = -1.0, color = "blue", size = 3, inherit.aes = FALSE))
# Combine the plots
(combined_plot <- (cumulated_plot / other_plots) + 
  plot_layout(heights = c(1, 1))) # Adjust the heights to make the cumulated plot bigger

ggsave(combined_plot, file = "outputs/rq1_combined_plot.png", width = 7, height = 9)

# go back to one plot
# Add a new factor level to control the order and size of the facets
df_means$Category <- factor(df_means$Category, levels = c("TPB_Mean", "AT_Mean", "B_Mean", "INT_Mean", "PBC_Mean", "SN_Mean"))

# Create the plot
(rq1_graph_prettiest <- ggplot(df_means, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  facet_wrap(~ Category, scales = "fixed", labeller = facet_labeller, ncol = 3) +  # 3x2 grid layout
  labs(x = "Time Point",
       y = "Mean Value",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_text_repel(data = df_means_grouped %>% filter(significance_group != ""),
                  aes(x = Time_Point, y = MeanValue2, label = significance_group), 
                  vjust = -0.5, color = "black", size = 5, inherit.aes = FALSE) +  # Adjust vjust here
  geom_text_repel(data = df_means_grouped %>% filter(significance_time != ""), 
                  aes(x = Time_Point, y = MeanValue2, label = significance_time), 
                  vjust = -1.0, color = "blue", size = 3, inherit.aes = FALSE))

ggsave(rq1_graph_prettiest, file = "outputs/rq1_graph_prettiest.png", width = 7, height = 5)
  
### RQ1: Spearman and Eta-Quadrat ----
# Convert Group to numeric
combined_df_g02 <- combined_df_g02 %>%
  mutate(Group_numeric = as.numeric(as.factor(Group)))

# Calculate Spearman's rho for each time point and category
spearman_rho_results <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Category, Time_Point) %>%
  summarise(
    spearman_rho = cor(MeanValue, Group_numeric, method = "spearman"),
    p_value_rho = cor.test(MeanValue, Group_numeric, method = "spearman")$p.value,
    .groups = 'drop'
  )

# Add significance stars based on p-values for Spearman's rho
spearman_rho_results <- spearman_rho_results %>%
  mutate(rho_significance = case_when(
    p_value_rho < 0.001 ~ "***",
    p_value_rho < 0.01 ~ "**",
    p_value_rho < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Merge the Spearman rho results back into df_means
#df_means <- df_means %>%
 # left_join(spearman_rho_results, by = c("Category", "Time_Point"))

# Cohen
# not working yet!!

# Function to compute Cohen's d
compute_cohens_d <- function(data) {
  # Extract means and standard deviations for each group
  group_means <- tapply(data$MeanValue, data$Group, mean)
  group_sds <- tapply(data$MeanValue, data$Group, sd)
  
  # Cohen's d calculation
  diff_means <- group_means[2] - group_means[1]  # Assuming Group 2 minus Group 1
  pooled_sd <- sqrt(((length(data[data$Group == "group0", "MeanValue"]) - 1) * group_sds[1]^2 + (length(data[data$Group == "group2", "MeanValue"]) - 1) * group_sds[2]^2) / (length(data$MeanValue) - 2))
  cohen_d <- diff_means / pooled_sd
  
  return(cohen_d)
}

# Compute Cohen's d for each Category and Time_Point
cohen_d_results <- combined_df_g02 %>%
  group_by(Category, Time_Point) %>%
  summarise(
    cohen_d = compute_cohens_d(.),  # Use "." to refer to the entire data within each group
    .groups = 'drop'
  )



# RQ2 MZP3 comparing SW and TPB ----
# relationship SW and TPB with spearman correlation ----
# checking data
# Scatter plot to visualize the relationship
plot(mzp3_cleaner$TPB_mean, mzp3_cleaner$SW_mean, main = "Scatter plot of TPB_mean vs SW_mean", xlab = "TPB", ylab = "SW")

# Shapiro-Wilk normality test
shapiro.test(mzp3_cleaner$TPB_mean) #normally distributed p>0.05
shapiro.test(mzp3_cleaner$SW_mean) #not normally distributed p<0.05

# Pearson correlation (but not normally distributed data, so not useful?)
# delete?
correlation_SW <- cor(mzp3_cleaner$TPB_mean, mzp3_cleaner$SW_mean)
print(correlation_SW) #0.82

# Spearman correlation
spearman_test <- cor.test(mzp3_cleaner$TPB_mean, mzp3_cleaner$SW_mean, method = "spearman")
print(spearman_test)
spearman_cor <- spearman_test$estimate
spearman_pval <- spearman_test$p.value
# p value rejects 0 hypothesis of no correlation -> relevant
# spearman_correlation 0.794 -> strong relationship

# Kendall correlation
kendall_test <- cor.test(mzp3_cleaner$TPB_mean, mzp3_cleaner$SW_mean, method = "kendall")
print(kendall_test)
# p value rejects 0 hypothesis of no correlation -> relevant
# 0.667 -> strong?


# Plot with annotation (SPEARMAN)
(graph_cor_rq3 <-ggplot(mzp3_cleaner, aes(x = TPB_mean, y = SW_mean)) +
  geom_point(alpha = 0.5, size = 2, color = "#7CFC00") +
  geom_smooth(method = "lm", se = FALSE, color = "#A6761D") +
  ylim(0, 3) + 
  theme_clean() +
  annotate("text", size = 2, x = 1, y = 2.5, label = paste("Spearman's rho:", round(spearman_cor, 2), "\n p-value:", round(spearman_pval, 3))) +
  labs(x = "\nMean Sustainability competences", y = "Mean Self-efficacy beliefs\n"))

ggsave(graph_cor_rq3, file = "outputs/graph_cor_rq3.png", width = 7, height = 5)


# make graph with two lines, one for each group plotting on TPB, SW ----
# how to interpret?! does it even make sense?
# leave out?
(rq2_groups_tpb_sw <- ggplot(mzp3_cleaner, aes(x = TPB_mean, y = SW_mean, color = Group)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", se = FALSE) +  # Separate lines for each group
    ylim(0, 3) + 
    theme_clean() +
    annotate("text", size = 2, x = 1, y = 2.5, label = paste("Spearman's rho:", round(spearman_cor, 2), "\n p-value:", round(spearman_pval, 3))) +
    labs(x = "\nMean Sustainability competences", y = "Mean Self-efficacy beliefs\n") +
    theme(legend.position = "bottom"))


# comparing SW/TPB for groups at MZP3 ----
df_tp3 <- combined_df_g02 %>%
  filter(Time_Point == "t3" & Category %in% c("SW_Mean", "TPB_Mean", "CS_Mean"))


### checking wilcoxon for differences in SW between groups
wilcox_results_sw <- df_tp3 %>%
  group_by(Category, Time_Point) %>%
  summarise(wilcox_p_group = wilcox.test(MeanValue ~ Group)$p.value,
    .groups = 'drop') %>%
  mutate(significance_group = case_when(
      wilcox_p_group < 0.001 ~ "***",
      wilcox_p_group < 0.01 ~ "**",
      wilcox_p_group < 0.05 ~ "*",
      TRUE ~ ""))

### effect size measure
# not working yet!!
# Function to calculate rank-biserial correlation
rank_biserial <- function(U, n1, n2) {
  R <- U - (n1 * (n1 + 1) / 2)
  r <- (2 * R / (n1 * n2)) - 1
  return(r)
}

# Function to calculate common language effect size (CL)
common_language_effect_size <- function(U, n1, n2) {
  cl <- U / (n1 * n2)
  return(cl)
}

# Calculate effect size measures
wilcox_results_sw <- wilcox_results_sw %>%
  mutate(
    n1 = 42,  # Example: Number of observations in Group 1 (adjust as per your data)
    n2 = 7,  # Example: Number of observations in Group 2 (adjust as per your data)
    rank_biserial = rank_biserial(U = qwilcox(wilcox_p_group, n1, n2), n1, n2),
    cl_effect_size = common_language_effect_size(U = qwilcox(wilcox_p_group, n1, n2), n1, n2))


# Calculate effect size measures
wilcox_results_sw <- wilcox_results_sw %>%
  mutate(
    U = qwilcox(wilcox_p_group, n1, n2),  # Replace with the correct method to get U
    rank_biserial = rank_biserial(U, n1, n2),
    cl_effect_size = common_language_effect_size(U, n1, n2)
  )


# Calculate means for each Group and Competence
df_means3 <- df_tp3 %>%
  filter(!is.na(MeanValue)) %>%
  group_by(Group, Category) %>%
  summarise(
    MeanValue2 = mean(MeanValue),
    LowerCI = MeanValue2 - qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
    UpperCI = MeanValue2 + qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
    .groups = 'drop') %>%
  left_join(wilcox_results_sw, by = c("Category"))



# Create the plot
# leave out? not so pretty?
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

### prettier version with point plot
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


#### including CS
# Create the point plot with error bars
(graph_tp3_compare_sw_tpb_prettier_cs <- ggplot(df_means3, aes(x = Category, y = MeanValue2, color = Group, group = Group)) +
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_errorbar(aes(ymin = MeanValue2 - SEM, ymax = MeanValue2 + SEM), 
                  position = position_dodge(0.5), width = 0.2) +
    labs(title = "Comparison of Mean Values for Competences at TimePoint3",
         x = "Competence",
         y = "Mean Value",
         color = "Group") +
    theme_clean() +
    theme(legend.position = "bottom"))

ggsave(graph_tp3_compare_sw_tpb_prettier_cs, file = "outputs/graph_tp3_compare_sw_tpb_prettier.png", width = 7, height = 5)


### making facet wrap with 3 categories and significance values ----
# Plot with significance symbols
(rq2_graph_sw_tpb_cs_stars <- ggplot(df_means3, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
   geom_line() +
   geom_point() +
   geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
   facet_wrap(~ Category, scales = "free_y") +
   labs(title = "Comparison of Groups by Competences",
        x = "Time Point",
        y = "Mean Value",
        color = "Group") +
   theme_minimal() +
   theme(legend.position = "bottom") +
   geom_text_repel(data = df_means3 %>% filter(significance_group != ""), 
                   aes(x = Time_Point, y = MeanValue2, label = significance_group), 
                   vjust = -0.5, color = "black", size = 5, inherit.aes = FALSE)) # Adjust vjust here
   
### use violin plot instead
# Create the violin plot using raw data
# Summarize significance information per category
significance_summary_sw <- df_means3 %>%
  group_by(Category) %>%
  summarise(
    significance_group = paste(unique(significance_group[significance_group != ""]), collapse = " "),
    .groups = 'drop'
  )


(rq2_graph_sw_tpb_cs_stars_violin <- ggplot(df_tp3, aes(x = Group, y = MeanValue, color = Group)) +
    geom_violin(trim = FALSE, alpha = 0.5) +  # Slight transparency to see boxplot
    geom_boxplot(width = 0.1, position = position_dodge(0.9)) +  # Adding boxplot for median and IQR
    geom_jitter(width = 0.2, size = 1, alpha = 0.3) +  # Adding individual data points
  facet_wrap(~ Category, scales = "fixed") +
  labs(title = "Comparison of Groups by Competences at TP3",
       x = "Group",
       y = "Mean Value",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_text(data = significance_summary_sw, 
              aes(x = 1.45, y = 3, label = significance_group),  # Adjust position as needed
              vjust = 1.5, color = "black", size = 5, inherit.aes = FALSE)) # Adjust vjust here

ggsave(rq2_graph_sw_tpb_cs_stars_violin, file = "outputs/rq2_graph_sw_tpb_cs_stars.png", width = 7, height = 5)



#### correlation SW and CS ----
mzp3_cleaner_cs <- mzp3_cleaner %>%
  filter(!is.na(CS_mean))


plot(mzp3_cleaner_cs$SW_mean, mzp3_cleaner_cs$CS_mean,  main = "Scatter plot of SW_mean and CS_mean", xlab = "SW", ylab = "CS")

# Shapiro-Wilk normality test
shapiro.test(mzp3_cleaner_cs$CS_mean) #not normally distributed p<0.05
shapiro.test(mzp3_cleaner_cs$SW_mean) #not normally distributed p<0.05

# Pearson correlation (but not normally distributed data, so not useful?)
# leave out?
correlation_SW_CS <- cor(mzp3_cleaner_cs$SW_mean, mzp3_cleaner_cs$CS_mean)
print(correlation_SW_CS)
# 0.797

# Spearman correlation
spearman_test_sw_cs <- cor.test(mzp3_cleaner_cs$SW_mean, mzp3_cleaner_cs$CS_mean, method = "spearman")
print(spearman_test_sw_cs)
# p value (<0.05) rejects 0 hypothesis of no correlation -> relevant
# 0.653

spearman_cor_sw_cs <- spearman_test_sw_cs$estimate
spearman_pval_sw_cs <- spearman_test_sw_cs$p.value

# Kendall correlation
kendall_test_sw_cs <- cor.test(mzp3_cleaner_cs$SW_mean, mzp3_cleaner_cs$CS_mean, method = "kendall")
print(kendall_test_sw_cs)
# p value rejects 0 hypothesis of no correlation -> relevant
# 0.549


# Plot with annotation (SPEARMAN)
(rq3_graph_cor_sw_cs <-ggplot(mzp3_cleaner_cs, aes(x = SW_mean, y = CS_mean)) +
    geom_point(alpha = 0.5, size = 2, color = "#7CFC00") +
    geom_smooth(method = "lm", se = FALSE, color = "#A6761D") +
    ylim(0, 3) + 
    theme_clean() +
    annotate("text", size = 2, x = 1, y = 2.5, label = paste("Spearman's rho:", round(spearman_cor_sw_cs, 2), "\n p-value:", round(spearman_pval_sw_cs, 3))) +
    labs(x = "\nMean Individual Self-efficacy beliefs", y = "Mean Collective Self-efficacy beliefs\n"))

ggsave(rq3_graph_cor_sw_cs, file = "outputs/rq3_graph_cor_sw_cs.png", width = 7, height = 5)




#### RQ2: checking components of Self-efficacy----
# Read the Excel file
codebook_sw_cs <- read_excel("data/data_collection/codebook_sw_cs.xlsx")

# Specify the questions of interest explicitly
questions_sw <- c("CS01_01", "CS01_02", "CS01_03", "SW01_01", "SW01_02", "SW01_03_inverse", "SW01_04", "SW01_05_inverse", "SW01_06", "SW01_07", "SW01_08")

# Calculate the mean scores for the specified questions
mean_scores_sw <- colMeans(mzp3_cleaner[, questions_sw], na.rm = TRUE)

# Convert the mean scores to a data frame for plotting
mean_scores_sw_df <- data.frame(
  Question = names(mean_scores_sw),
  Mean_Score = mean_scores_sw
)

# Join the dataframes by the column 'Question'
merged_sw_cs <- left_join(mean_scores_sw_df, codebook_sw_cs, by = "Question")


# check distribution of data
# Plot histograms
for (question in questions_sw) {
  p <- ggplot(mzp3_cleaner, aes_string(x = question)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of", question), x = question, y = "Frequency") +
    theme_minimal()
  print(p)
}

# Plot QQ-plots
for (question in questions_sw) {
  qqnorm(mzp3_cleaner[[question]], main = paste("QQ-Plot of", question))
  qqline(mzp3_cleaner[[question]], col = "red")
}

# Perform Shapiro-Wilk tests
shapiro_results_sw_cs <- data.frame(
  Question = questions_sw,
  p_value = sapply(questions_sw, function(question) shapiro.test(mzp3_cleaner[[question]])$p.value)
)
## none is normally distributed


# compare questions SW and CS right next to each other----
# Filter and arrange the data to compare the specified pairs
comparison_df <- merged_sw_cs %>%
  filter(Question %in% Question) %>%
  mutate(Pair = case_when(
    Question == "CS01_01" | Question == "SW01_01" ~ "CS01_01 and SW01_01",
    Question == "CS01_02" | Question == "SW01_06" ~ "CS01_02 and SW01_06",
    Question == "CS01_03" | Question == "SW01_04" ~ "CS01_03 and SW01_04"
  ))


# test statistically their relationship using spearman ----
# Calculate Spearman correlations for the specified pairs
spearman_results_sw_cs <- data.frame(
  Pair = c("CS01_01 and SW01_01", "CS01_02 and SW01_06", "CS01_03 and SW01_04"),
  Correlation = c(
    cor(mzp3_cleaner$CS01_01, mzp3_cleaner$SW01_01, method = "spearman", use = "complete.obs"),
    cor(mzp3_cleaner$CS01_02, mzp3_cleaner$SW01_06, method = "spearman", use = "complete.obs"),
    cor(mzp3_cleaner$CS01_03, mzp3_cleaner$SW01_04, method = "spearman", use = "complete.obs")
  ),
  P_Value = c(
    cor.test(mzp3_cleaner$CS01_01, mzp3_cleaner$SW01_01, method = "spearman", exact = FALSE)$p.value,
    cor.test(mzp3_cleaner$CS01_02, mzp3_cleaner$SW01_06, method = "spearman", exact = FALSE)$p.value,
    cor.test(mzp3_cleaner$CS01_03, mzp3_cleaner$SW01_04, method = "spearman", exact = FALSE)$p.value
  )
)

### plotting results spearman
# Specify the pairs
pairs <- list(
  c("CS01_01", "SW01_01"),
  c("CS01_02", "SW01_06"),
  c("CS01_03", "SW01_04")
)


pair_names <- c("CS01_01 & SW01_01", "CS01_02 & SW01_06", "CS01_03 & SW01_04")

# Calculate mean scores and correlations for each pair
results <- lapply(seq_along(pairs), function(i) {
  pair <- pairs[[i]]
  data <- na.omit(mzp3_cleaner[, pair])
  means <- colMeans(data, na.rm = TRUE)
  correlation_test <- cor.test(data[[1]], data[[2]], method = "spearman", exact = FALSE)
  
  data.frame(
    Question = pair,
    Mean = means,
    Pair = rep(pair_names[i], 2),
    Correlation = correlation_test$estimate,
    P_Value = correlation_test$p.value,
    Significance = ifelse(correlation_test$p.value < 0.05, "*", "")
  )
})

# Combine results into a single dataframe
plot_data <- do.call(rbind, results)
plot_data$Pair <- rep(pair_names, each = 2)


# Summarize for annotations
annotations <- plot_data %>%
  group_by(Pair) %>%
  summarize(
    Correlation = first(Correlation),
    P_Value = first(P_Value),
    Significance = first(Significance),
    MaxMean = max(Mean) * 1.1
  )

# Plotting
# not working yet
(comparision_sw_cs_spearman <- ggplot(plot_data, aes(x = Question, y = Mean, fill = Question)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~ Pair, scales = "free") +
    #geom_text(data = annotations, aes(x = 1, y = MaxMean, label = ifelse(Significance == "*", paste("★ ρ=", round(Correlation, 2), "p=", round(P_Value, 3)), paste("ρ=", round(Correlation, 2), "p=", round(P_Value, 3)))), vjust = 0) +
    labs(title = "Comparison of Means and Spearman Correlation", x = "Question", y = "Mean Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text.x = element_text(size = 14, face = "bold")))


ggsave(comparision_sw_cs_spearman, file = "outputs/comparision_sw_cs_spearman.png", width = 7, height = 9)

# Perform Wilcoxon signed-rank tests ----
# add between groups and categories

# Calculate the mean scores for each group and question
group_mean_scores <- mzp3_cleaner %>%
  select(Group, all_of(questions_sw)) %>%
  pivot_longer(cols = -Group, names_to = "Question", values_to = "Score") %>%
  group_by(Group, Question) %>%
  summarize(Mean_Score = mean(Score, na.rm = TRUE), .groups = 'drop')

# Join the group mean scores with the theoretical classifications
merged_group_scores <- left_join(group_mean_scores, codebook_sw_cs, by = "Question")

# If you want to view or compare within theoretical classifications:
final_analysis <- merged_group_scores %>%
  group_by(Theoretical_classification, Group) %>%
  summarize(Avg_Mean_Score = mean(Mean_Score, na.rm = TRUE), .groups = 'drop')

# Assuming 'merged_group_scores' contains the appropriate data from previous steps
ggplot(merged_group_scores, aes(x = Group, y = Mean_Score, fill = Group)) +
  geom_boxplot() +
  facet_wrap(~ Theoretical_classification, scales = "fixed") +
  labs(title = "Distribution of Mean Scores by Group and Classification",
       x = "Group",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Assuming there are only two groups to compare within each theoretical classification
wilcoxon_results_sw_cs <- merged_group_scores %>%
  group_by(Theoretical_classification) %>%
  summarize(
    W_test = list(wilcox.test(Mean_Score ~ Group, data = cur_data())),
    .groups = 'drop'
  )

# Extract p-values and test statistics
wilcoxon_results_sw_cs$P_Value <- sapply(wilcoxon_results_sw_cs$W_test, function(x) x$p.value)
wilcoxon_results_sw_cs$Statistic <- sapply(wilcoxon_results_sw_cs$W_test, function(x) x$statistic)



# Assuming there are only two groups to compare within general classification
wilcoxon_results_sw_cs2 <- merged_group_scores %>%
  group_by(General_classification) %>%
  summarize(
    W_test = list(wilcox.test(Mean_Score ~ Group, data = cur_data())),
    .groups = 'drop'
  )

# Extract p-values and test statistics
wilcoxon_results_sw_cs2$P_Value <- sapply(wilcoxon_results_sw_cs2$W_test, function(x) x$p.value)
wilcoxon_results_sw_cs2$Statistic <- sapply(wilcoxon_results_sw_cs2$W_test, function(x) x$statistic)
# only agent-action-aim statistically significantly different
# show in violin plot?


### comparing behaviour and SW ----


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