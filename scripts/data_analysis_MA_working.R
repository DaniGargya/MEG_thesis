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
library(knitr)



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
mzp1_clean$TP <- "Measurement point 1"
mzp2_clean$TP <- "Measurement point 2"
mzp3_clean$TP <- "Measurement point 3"


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
  filter(Group != "group1") %>%
  mutate(Group = recode(Group, "group0" = "control group", "group2" = "involved group")) %>%
  group_by(Group, Time_Point) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Count = ifelse(Time_Point %in% c("Measurement point 1", "Measurement point 2"), Count / 6, Count / 9)) %>% # adjust with calculated means per mzp
  mutate(Alpha = ifelse(Group == "group1", 0.1, 1)) 

# Create the treemap
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

# new
(treemap_mzps_groups <- ggplot(summary_data, aes(area = Count, fill = as.factor(Group), label = Count,
                                                subgroup = as.factor(Time_Point))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre") +
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE, colour = "black", fontface = "italic", size = 5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Treemap of Answers per Group per Time Point", fill = "Group") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)))


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


# calculating internal validity with cronbach alpha MZP3 ----
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
    P_value = kruskal.test(MeanValue ~ Time_Point)$p.value,
    Kruskal_Wallis_H = kruskal.test(MeanValue ~ Time_Point)$statistic,  # Extract Kruskal-Wallis H statistic
    .groups = 'drop') #%>%
  mutate(significance_time = case_when(
      kruskal_p_time < 0.001 ~ "***",
      kruskal_p_time < 0.01 ~ "**",
      kruskal_p_time < 0.05 ~ "*",
      TRUE ~ ""))
  
kw_results <- kw_results %>%
  mutate(Group = as.character(Group)) %>%
    mutate(Group = case_when(
      Group == "group0" ~ "Control group",
      Group == "group2" ~ "Involved group",
      TRUE ~ Group)) %>%
    rename(Scale = Category)


# Create the table with kable
markdown_table_kruskal <- kable(kw_results, format = "markdown", 
                        col.names = c("Scale", "Group", "P-Value", "Kruskal-Wallis H"))

# Save the Markdown table to a text file
writeLines(markdown_table_kruskal, "outputs/markdown_table_kruskal.md")



# Wilcoxon rank-sum test for groups within each category and time point
wilcox_results_rq1 <- combined_df_g02 %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Category, Time_Point) %>%
  summarise(
    wilcox_p_group = wilcox.test(MeanValue ~ Group)$p.value,
    W_Statistic = wilcox.test(MeanValue ~ Group)$statistic,
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

wilcox_results_rq1 <- wilcox_results_rq1 %>%
  rename(Scale = Category)


# Create the table with kable
markdown_table_wilcox_rq1 <- kable(wilcox_results_rq1, format = "markdown", 
                                col.names = c("Scale", "Measurement Point", "P-Value", "W Statistic", "Significance symbols"))

# Save the Markdown table to a text file
writeLines(markdown_table_wilcox_rq1, "outputs/markdown_table_wilcox_rq1.md")

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







# Plot with significance symbols
# old?
(rq1_graph_prettier_stars <- ggplot(df_means1, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
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
  mutate(Time_Point = recode(Time_Point, "t1" = "MP1", "t2" = "MP2", "t3" = "MP3")) %>%
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

# Define a labeller function to rename facets
facet_labeller <- labeller(Category = c(
  AT_Mean = "Sustainability Attitudes\n(Attitude)",
  INT_Mean = "Sustainability Attitudes\n(Intention)",
  PBC_Mean = "Sustainability Attitudes\n(PBC)",
  SN_Mean = "Sustainability Attitudes\n(Subjective norms)",
  B_Mean = "Sustainability Behaviours\n(Behaviour)",
  TPB_Mean = "Cumulative\nSA and SB"))

df_means1 <- df_means %>%
  mutate(Group = recode(Group, "group0" = "Control group", "group2" = "Involved group")) %>%
  mutate(Time_Point = recode(Time_Point, "t1" = "MP1", "t2" = "MP2", "t3" = "MP3"))


# Custom colors for the groups
custom_colors <- c("#E7298A", "#1B9E77")

# Add a new factor level to control the order and size of the facets
df_means1$Category <- factor(df_means1$Category, levels = c("AT_Mean", "INT_Mean", "B_Mean", "PBC_Mean", "SN_Mean", "TPB_Mean"))

# theme clean 2
theme_clean2 <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), # Corrected the unit specification
          plot.title = element_text(size = 15, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_text(size = 12, face = "bold"),                              
          legend.position = "bottom") # Changed legend.position to a descriptive keyword for consistency
}


# Create the plot
(rq1_graph_prettiest <- ggplot(df_means1, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  facet_wrap(~ Category, scales = "fixed", labeller = facet_labeller, ncol = 3) +  # 3x2 grid layout
  labs(x = "\nMeasurement Points",
       y = "Mean Value",
       color = "Group") +
  #theme_clean2() +  
  scale_color_manual(values = custom_colors) +  # Custom colors for the groups
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   # Remove major grid lines
          panel.grid.minor = element_blank(),   # Remove minor grid lines
          legend.position = "bottom",
        legend.text = element_text(size = 12, face = "italic"),          
        legend.title = element_text(size = 12, face = "bold")) +
    
  #theme(legend.position = "bottom") +
  geom_text_repel(data = df_means_grouped %>% filter(significance_group != ""),
                  aes(x = Time_Point, y = MeanValue2, label = significance_group), 
                  vjust = -1.5, color = "black", size = 6, inherit.aes = FALSE, segment.color = NA) +  # Adjust vjust here
  geom_text_repel(data = df_means_grouped %>% filter(significance_time != ""), 
                  aes(x = Time_Point, y = MeanValue2, label = significance_time), 
                  vjust = -1.0, color = "blue", size = 3, inherit.aes = FALSE)+
  guides(fill = guide_legend(title = NULL)))

ggsave(rq1_graph_prettiest, file = "outputs/rq1_graph_prettiest2.png", width = 7, height = 5)


# even perfect
(rq1_graph_perfect <- ggplot(df_means1, aes(x = Time_Point, y = MeanValue2, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.1) +  # Adding error bars
  facet_wrap(~ Category, scales = "fixed", labeller = facet_labeller, ncol = 3) +  # 3x2 grid layout
  labs(x = "\nMeasurement Points", y = "\nMean Value") +
  scale_color_manual(values = custom_colors) +  # Custom colors for the groups
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines
        panel.border = element_rect(fill = NA, color = "black"), # Add border around each facet
        legend.position = "bottom",
        legend.text = element_text(size = 12, face = "italic"),          
        legend.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "plain"),             
        axis.title.y = element_text(size = 14, face = "plain")) +
  geom_text_repel(data = df_means_grouped %>% filter(significance_group != ""),
                  aes(x = Time_Point, y = MeanValue2, label = significance_group), 
                  vjust = -1.5, color = "black", size = 6, inherit.aes = FALSE, segment.color = NA) +  # Adjust vjust here
  geom_text_repel(data = df_means_grouped %>% filter(significance_time != ""), 
                  aes(x = Time_Point, y = MeanValue2, label = significance_time), 
                  vjust = -1.0, color = "blue", size = 3, inherit.aes = FALSE) +
  guides(color = guide_legend(title = NULL)))  # Remove the legend title

ggsave(rq1_graph_perfect, file = "outputs/rq1_graph_perfect.png", width = 7, height = 5)


### RQ2 relationship SW and TPB with spearman correlation at MZP3 ----
# checking data
# Scatter plot to visualize the relationship
plot(mzp3_cleaner$TPB_mean, mzp3_cleaner$SW_mean, main = "Scatter plot of TPB_mean vs SW_mean", xlab = "TPB", ylab = "SW")

# Shapiro-Wilk normality test
shapiro.test(mzp3_cleaner$TPB_mean) #normally distributed p>0.05
shapiro.test(mzp3_cleaner$SW_mean) #not normally distributed p<0.05
shapiro.test(mzp3_cleaner_cs$CS_mean) #not normally distributed p<0.05


# Spearman correlation
spearman_test <- cor.test(mzp3_cleaner$TPB_mean, mzp3_cleaner$SW_mean, method = "spearman")
print(spearman_test)
spearman_cor <- spearman_test$estimate
spearman_pval <- spearman_test$p.value
# p value rejects 0 hypothesis of no correlation -> relevant
# spearman_correlation 0.794 -> strong relationship

# Plot with annotation (SPEARMAN)
(rq2a_graph_cor_tpb_sw <-ggplot(mzp3_cleaner, aes(x = TPB_mean, y = SW_mean)) +
  geom_point(alpha = 0.5, size = 2, color = "#7CFC00") +
  geom_smooth(method = "lm", se = FALSE, color = "#A6761D") +
  ylim(0, 3) + 
  theme_clean() +
    annotate("text", size = 3, x = 1, y = 2.5, label = paste("Spearman's rho:", round(spearman_cor, 2), "\np-value < .001")) +
  labs(x = "\nMean Sustainability Attitudes and Behaviours (TPB)", y = "Mean Efficacy beliefs\n"))

ggsave(rq2a_graph_cor_tpb_sw, file = "outputs/rq2a_graph_cor_tpb_sw.png", width = 7, height = 5)



### RQ3: self-efficacy and groups ----
### comparing between groups SW/CS ----
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

# filter out relevant categories
df_tp3 <- combined_df_g02 %>%
  filter(Time_Point == "t3" & Category %in% c("SW_Mean", "CS_Mean"))


### checking wilcoxon for differences in SW/ CS between groups
wilcox_rq3_sw_cs_between <- df_tp3 %>%
  group_by(Category) %>%
  rename(Scale = Category) %>%
  summarise(wilcox_p_group = wilcox.test(MeanValue ~ Group)$p.value,
            W_Statistic = wilcox.test(MeanValue ~ Group)$statistic,
    .groups = 'drop') #%>%
  mutate(significance_group = case_when(
      wilcox_p_group < 0.001 ~ "***",
      wilcox_p_group < 0.01 ~ "**",
      wilcox_p_group < 0.05 ~ "*",
      TRUE ~ ""))
# no significant differences between the groups


# Create the table with kable
markdown_table_wilcox_rq3_sw_cs_between <- kable(wilcox_rq3_sw_cs_between, format = "markdown", 
                                   col.names = c("Scale", "P-Value", "W Statistic")) 
                                   

# Save the Markdown table to a text file
writeLines(markdown_table_wilcox_rq3_sw_cs_between, "outputs/markdown_table_wilcox_rq3_sw_cs_between.md")




# Calculate means for each Group and Competence
df_means3 <- df_tp3 %>%
  filter(!is.na(MeanValue)) %>%
  group_by(Group, Category) %>%
  summarise(
    MeanValue2 = mean(MeanValue),
    LowerCI = MeanValue2 - qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
    UpperCI = MeanValue2 + qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
    .groups = 'drop') %>%
  left_join(wilcox_results_sw_cs, by = c("Category"))


# comparing sw vs cs within groups ----
# Aggregate scores for CS and SW questions within each group
aggregate_scores <- mzp3_cleaner %>%
  pivot_longer(cols = c(starts_with("CS"), starts_with("SW")), names_to = "Question", values_to = "Score") %>%
  mutate(Type = ifelse(str_detect(Question, "CS"), "CS", "SW")) %>%
  drop_na() %>%
  group_by(Group, Type) %>% 
  summarize(Aggregate_Score = mean(Score, na.rm = TRUE), .groups = 'drop')

# Reshape data to wide format to ensure paired observations
wide_data <- aggregate_scores %>%
  pivot_wider(names_from = Type, values_from = Aggregate_Score)

# Function to perform Wilcoxon signed-rank test for aggregated scores
wilcoxon_test_aggregated <- function(data, group) {
  filtered_data <- data %>% filter(Group == group)
  
  cs_scores <- filtered_data$CS
  sw_scores <- filtered_data$SW
  
  test <- wilcox.test(cs_scores, sw_scores, paired = TRUE)
  return(data.frame(
    Group = group,
    Statistic = test$statistic,
    P_Value = test$p.value
  ))
}

# Perform Wilcoxon tests for overall comparison within each group
results_group0_overall <- wilcoxon_test_aggregated(wide_data, "group0")
results_group2_overall <- wilcoxon_test_aggregated(wide_data, "group2")

# Combine results into a dataframe
wilcoxon_results_overall <- rbind(results_group0_overall, results_group2_overall)

# Add Significance column for stars
wilcoxon_results_overall$Significance <- ifelse(wilcoxon_results_overall$P_Value < 0.05, "*", "")

# create table
wilcoxon_rq3_sw_cs_within <- wilcoxon_results_overall %>%
  mutate(Group = as.character(Group)) %>%
  mutate(Group = case_when(
    Group == "group0" ~ "Control group",
    Group == "group2" ~ "Involved group",
    TRUE ~ Group))


# Create the table with kable
markdown_table_wilcoxon_rq3_sw_cs_within <- kable(wilcoxon_rq3_sw_cs_within, format = "markdown", 
                                                 col.names = c("Group", "W Statistic", "P-Value")) 
                                                 

# Save the Markdown table to a text file
writeLines(markdown_table_wilcoxon_rq3_sw_cs_within, "outputs/markdown_table_wilcox_rq3_sw_cs_within.md")





# Prepare data for visualization
# Function to calculate confidence intervals
calculate_ci <- function(data, conf_level = 0.95) {
  mean_val <- mean(data, na.rm = TRUE)
  stderr <- sd(data, na.rm = TRUE) / sqrt(length(data))
  error_margin <- qnorm((1 + conf_level) / 2) * stderr
  lower_ci <- mean_val - error_margin
  upper_ci <- mean_val + error_margin
  return(data.frame(Mean = mean_val, Lower_CI = lower_ci, Upper_CI = upper_ci))
}

# Apply CI calculation to the mean scores
mean_scores_with_ci <- mzp3_cleaner %>%
  pivot_longer(cols = c(starts_with("CS"), starts_with("SW")), names_to = "Question", values_to = "Score") %>%
  mutate(Type = ifelse(str_detect(Question, "CS"), "CS", "SW")) %>%
  drop_na() %>%
  group_by(Group, Type) %>%
  summarize(Mean_Score = mean(Score, na.rm = TRUE),
            Lower_CI = mean(Score, na.rm = TRUE) - qnorm(0.975) * (sd(Score, na.rm = TRUE) / sqrt(n())),
            Upper_CI = mean(Score, na.rm = TRUE) + qnorm(0.975) * (sd(Score, na.rm = TRUE) / sqrt(n())), .groups = 'drop')



mean_scores_with_ci1 <- mean_scores_with_ci %>%
  mutate(Group = recode(Group, "group0" = "Control group", "group2" = "Involved group")) %>%
  mutate(Type = recode(Type, "CS" = "Collective efficacy beliefs", "SW" = "Personal efficacy beliefs")) #%>%
  #rename(`Types of Efficacy` = Type)

# Define custom colors2
custom_colors2 <- c("#D95F02", "#E6AB02")

# Plotting the results with annotation and error bars
(rq2b_boxplot_sw_cs_overall <- ggplot(mean_scores_with_ci1, aes(x = Type, y = Mean_Score, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(0.7)) +
  facet_wrap(~ Group, scales = "fixed") +
  labs(y = "\n\nMean Value") +
  theme_clean() +
  scale_fill_manual(values = custom_colors2) + 
  theme(axis.title.x = element_blank(),            # Remove x-axis title
          axis.text.x = element_blank(),             # Remove x-axis text
          axis.ticks.x = element_blank(),            # Remove x-axis ticks
          legend.position = "bottom")+
    guides(fill = guide_legend(title = NULL)))    # Remove the legend title)


ggsave(rq2b_boxplot_sw_cs_overall, file = "outputs/rq2b_boxplot_sw_cs_overall.png", width = 7, height = 5)


# checking level of individual questions sw vs cs ----
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


# test Wilcoxon comparing questions pairs and within groups ----
# Specify the pairs
pairs <- list(
  c("CS01_01", "SW01_01"),
  c("CS01_02", "SW01_06"),
  c("CS01_03", "SW01_04")
)

pair_names <- c("CS01_01 & SW01_01", "CS01_02 & SW01_06", "CS01_03 & SW01_04")


# Filter data for each group
group0_data <- mzp3_cleaner %>% filter(Group == "group0")
group2_data <- mzp3_cleaner %>% filter(Group == "group2")


# Function to perform Wilcoxon signed-rank test for a given group and pair of questions
wilcoxon_test <- function(data, pair) {
  # Filter out rows with missing values for the current pair
  filtered_data <- data %>% select(all_of(pair)) %>% na.omit()
  
  # Check if there are enough observations for the test
  if (nrow(filtered_data) < 2) {
    return(data.frame(
      Pair = paste(pair[1], "and", pair[2]),
      Statistic = NA,
      P_Value = NA
    ))
  }
  
  test <- wilcox.test(filtered_data[[pair[1]]], filtered_data[[pair[2]]], paired = TRUE)
  return(data.frame(
    Pair = paste(pair[1], "and", pair[2]),
    Statistic = test$statistic,
    P_Value = test$p.value
  ))
}

# Perform Wilcoxon tests for each pair within each group
results_group0 <- lapply(pairs, wilcoxon_test, data = group0_data)
results_group2 <- lapply(pairs, wilcoxon_test, data = group2_data)

# Combine results into dataframes
wilcoxon_results_group0 <- do.call(rbind, results_group0)
wilcoxon_results_group2 <- do.call(rbind, results_group2)

# Add Group column to the results
wilcoxon_results_group0$Group <- "group0"
wilcoxon_results_group2$Group <- "group2"

# Combine results for both groups
wilcoxon_results_pairs_within_groups <- rbind(wilcoxon_results_group0, wilcoxon_results_group2)

# Add Significance column for stars
wilcoxon_results_pairs_within_groups$Significance <- ifelse(wilcoxon_results$P_Value < 0.05, "*", "")


# viz
# Combine data for both groups
combined_data <- mzp3_cleaner %>%
  select(Group, all_of(unlist(pairs))) %>%
  pivot_longer(cols = -Group, names_to = "Question", values_to = "Score")

# Calculate mean scores for each group and question
mean_scores <- combined_data %>%
  group_by(Group, Question) %>%
  summarize(Mean_Score = mean(Score, na.rm = TRUE), .groups = 'drop')

# Create a Pair column for facet wrapping
mean_scores <- mean_scores %>%
  mutate(Pair = case_when(
    Question %in% pairs[[1]] ~ "CS01_01 and SW01_01",
    Question %in% pairs[[2]] ~ "CS01_02 and SW01_06",
    Question %in% pairs[[3]] ~ "CS01_03 and SW01_04"
  ))

# Merge the Wilcoxon test results with the mean scores for annotation
mean_scores <- mean_scores %>%
  left_join(wilcoxon_results_pairs_within_groups, by = c("Pair", "Group"))

# Plotting the results with annotation
(rq2b_comparision_sw_cs_wilcox <- ggplot(mean_scores, aes(x = Question, y = Mean_Score, fill = as.factor(Group))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ Pair, scales = "free") +
  geom_text(aes(label = Significance), position = position_dodge(width = 0.8), vjust = -0.5) +
  labs(title = "Mean Scores for Each Group by Question Pairs",
       x = "Question",
       y = "Mean Score",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(rq2b_comparision_sw_cs_wilcox, file = "outputs/rq2b_comparision_sw_cs_wilcox.png", width = 7, height = 5)



### comparing groups Links AAA  ----
# add between groups and categories

# Calculate the mean scores for each group and question
group_mean_scores <- mzp3_cleaner %>%
  select(Group, all_of(questions_sw)) %>%
  pivot_longer(cols = -Group, names_to = "Question", values_to = "Score") #%>%
  group_by(Group, Question) #%>%
  summarize(Mean_Score = mean(Score, na.rm = TRUE), .groups = 'drop')

# copied from above
mean_scores_with_ci <- mzp3_cleaner %>%
  pivot_longer(cols = c(starts_with("CS"), starts_with("SW")), names_to = "Question", values_to = "Score") %>%
  mutate(Type = ifelse(str_detect(Question, "CS"), "CS", "SW")) #%>%
  drop_na() %>%
  group_by(Group, Type) %>%
  summarize(Mean_Score = mean(Score, na.rm = TRUE),
            Lower_CI = mean(Score, na.rm = TRUE) - qnorm(0.975) * (sd(Score, na.rm = TRUE) / sqrt(n())),
            Upper_CI = mean(Score, na.rm = TRUE) + qnorm(0.975) * (sd(Score, na.rm = TRUE) / sqrt(n())), .groups = 'drop')


# Join the group mean scores with the theoretical classifications
merged_group_scores <- left_join(group_mean_scores, codebook_sw_cs, by = "Question")



# If you want to view or compare within theoretical classifications:
final_analysis <- merged_group_scores %>%
  group_by(Theoretical_classification, Group) %>%
  summarize(Avg_Mean_Score = mean(Mean_Score, na.rm = TRUE), .groups = 'drop')

# Assuming 'merged_group_scores' contains the appropriate data from previous steps
(rq2b_boxplot_compare_groups_linksAAA <- ggplot(merged_group_scores, aes(x = Group, y = Mean_Score, fill = Group)) +
  geom_boxplot() +
  facet_wrap(~ Theoretical_classification, scales = "fixed") +
  labs(title = "Distribution of Mean Scores by Group and Classification",
       x = "Group",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(rq2b_boxplot_compare_groups_linksAAA, file = "outputs/rq2b_boxplot_compare_groups_linksAAA.png", width = 7, height = 5)

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
# only agent-action-aim statistically significantly different
# show in violin plot?


# comparing between groups aim vs action focussed ----
# add new classification only based on aim vs action
merged_group_scores <- merged_group_scores %>%
  mutate(Theoretical_classification2 = case_when(
    grepl("aim", Theoretical_classification, ignore.case = TRUE) ~ "aim",
    TRUE ~ "action"
  ))




# If you want to view or compare within theoretical classifications:
final_analysis2 <- merged_group_scores %>%
  group_by(Theoretical_classification2, Group) %>%
  summarize(Avg_Mean_Score2 = mean(Mean_Score, na.rm = TRUE), .groups = 'drop')


# Assuming there are only two groups to compare within each theoretical classification
wilcoxon_results_aim_action <- merged_group_scores %>%
  group_by(Theoretical_classification2) %>%
  summarize(
    W_test = list(wilcox.test(Mean_Score ~ Group, data = cur_data())),
    .groups = 'drop'
  )

# Extract p-values and test statistics
wilcoxon_results_aim_action$P_Value <- sapply(wilcoxon_results_aim_action$W_test, function(x) x$p.value)
wilcoxon_results_aim_action$Statistic <- sapply(wilcoxon_results_aim_action$W_test, function(x) x$statistic)

# Add significance stars
wilcoxon_results_aim_action$Significance <- ifelse(wilcoxon_results_aim_action$P_Value < 0.05, "*", "")

# create table
wilcox_rq3_aim_action_between <- wilcoxon_results_aim_action %>%
  select(-W_test)


markdown_table_wilcox_rq3_aim_action_between <- kable(wilcox_rq3_aim_action_between, format = "markdown", 
                                                 col.names = c("Theoretical classification", "P-Value", "W Statistic", "Significance symbol")) 


# Save the Markdown table to a text file
writeLines(markdown_table_wilcox_rq3_aim_action_between, "outputs/markdown_table_wilcox_rq3_aim_action_between.md")



# Merge Wilcoxon test results back into the main dataframe for plotting
merged_group_scores <- merged_group_scores %>%
  left_join(wilcoxon_results_aim_action %>% select(Theoretical_classification2, Significance), by = "Theoretical_classification2")


### compare action vs aim within groups ----
# Aggregate scores for aim and action questions within each group
aggregate_scores <- merged_group_scores %>%
  group_by(Group, Theoretical_classification2) %>% 
  summarize(Aggregate_Score = mean(Mean_Score, na.rm = TRUE), .groups = 'drop')


# Reshape data to wide format to ensure paired observations
wide_data2 <- aggregate_scores %>%
  pivot_wider(names_from = Theoretical_classification2, values_from = Aggregate_Score)


# Function to perform Wilcoxon signed-rank test for aggregated scores
wilcoxon_test_aggregated2 <- function(data, group) {
  filtered_data <- data %>% filter(Group == !!group)
  
  aim_scores <- filtered_data$aim
  action_scores <- filtered_data$action
  
  test <- wilcox.test(aim_scores, action_scores, paired = TRUE)
  return(data.frame(
    Group = group,
    Statistic = test$statistic,
    P_Value = test$p.value
  ))
}

# Perform Wilcoxon tests for overall comparison within each group
results_group0_aa <- wilcoxon_test_aggregated2(wide_data2, "group0")
results_group2_aa <- wilcoxon_test_aggregated2(wide_data2, "group2")

# Combine results into a dataframe
wilcoxon_results_aim_action2 <- rbind(results_group0_aa, results_group2_aa)

# Add Significance column for stars
wilcoxon_results_aim_action2$Significance <- ifelse(wilcoxon_results_aim_action2$P_Value < 0.05, "*", "")


# create table
wilcoxon_rq3_aim_action_within <- wilcoxon_results_aim_action2 %>%
  mutate(Group = as.character(Group)) %>%
  mutate(Group = case_when(
    Group == "group0" ~ "Control group",
    Group == "group2" ~ "Involved group",
    TRUE ~ Group))


# Create the table with kable
markdown_table_wilcoxon_rq3_aim_action_within <- kable(wilcoxon_rq3_aim_action_within, format = "markdown", 
                                                  col.names = c("Group", "W Statistic", "P-Value")) 

# Save the Markdown table to a text file
writeLines(markdown_table_wilcoxon_rq3_aim_action_within, "outputs/markdown_table_wilcoxon_rq3_aim_action_within.md")



# Apply CI calculation to the mean scores
# working!!
mean_scores_with_ci2 <- merged_group_scores %>%
  group_by(Group, Theoretical_classification2) %>%
  drop_na() %>%
  summarize(
    Mean_Score = mean(Score, na.rm = TRUE),
    Lower_CI = mean(Score, na.rm = TRUE) - qnorm(0.975) * (sd(Score, na.rm = TRUE) / sqrt(n())),
    Upper_CI = mean(Score, na.rm = TRUE) + qnorm(0.975) * (sd(Score, na.rm = TRUE) / sqrt(n())),
    .groups = 'drop')

mean_scores_with_ci2 <- mean_scores_with_ci2 %>%
  mutate(Group = recode(Group, "group0" = "Control group", "group2" = "Involved group")) %>%
  mutate(Theoretical_classification2 = recode(Theoretical_classification2, "action" = "Action-focused efficacy beliefs", "aim" = "Aim-focused efficacy beliefs")) #%>%
#rename(`Types of Efficacy` = Type)

# Define custom colors2
custom_colors3 <- c("#7570B3", "#666666")


# Plotting the results with annotation and error bars
(rq2b_bars_compare_groups_aim_action <- ggplot(mean_scores_with_ci, aes(x = Theoretical_classification2, y = Mean_Score, fill = Theoretical_classification2)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(0.7)) +
  facet_wrap(~ Group, scales = "fixed") +
  #geom_text(data = wilcoxon_results_aim_action,
   #         aes(x = 1.5, y = max(mean_scores_with_ci$Upper_CI) * 1.05, label = Significance), vjust = -0.5, size = 5) +
  labs(title = "Comparison of Mean Scores for Aim and Action Questions by Group",
       x = "Type",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(rq2b_bars_compare_groups_aim_action, file = "outputs/rq2b_bars_compare_groups_aim_action.png", width = 7, height = 5)


# plotting (same as above)
(rq2b_boxplot_aim_action <- ggplot(mean_scores_with_ci2, aes(x = Theoretical_classification2, y = Mean_Score, fill = Theoretical_classification2)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(0.7)) +
    facet_wrap(~ Group, scales = "fixed") +
    labs(y = "\n\nMean Value") +
    theme_clean() +
    scale_fill_manual(values = custom_colors3) + 
    theme(axis.title.x = element_blank(),            # Remove x-axis title
          axis.text.x = element_blank(),             # Remove x-axis text
          axis.ticks.x = element_blank(),            # Remove x-axis ticks
          legend.position = "bottom")+
    guides(fill = guide_legend(title = NULL)))    # Remove the legend title)

ggsave(rq2b_boxplot_aim_action, file = "outputs/rq2b_boxplot_aim_action.png", width = 7, height = 5)


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