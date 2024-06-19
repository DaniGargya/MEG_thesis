# Data analysis for MA thesis MEG
# Dani Gargya
# June 24

# workflow ----
# loading all relevant datasets
# calculating means mzp3
# check for normality of data
# harmonising data sets, combining them
# statistical analysis data
# making pretty graphs

# loading libraries ----
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggthemes) # for data visualisation



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
# based on means or original data??
mzp3_clean$TPB_mean <- apply(mzp3_clean[, c("AT_mean", "SN_mean", "PB_mean", "IN_mean", "B_mean")], 1, mean_exclude_negative_100)

# SW_CS_mean
# based on means or original data??
mzp3_clean$SW_CS_mean <- apply(mzp3_clean[, c("SW_mean", "CS_mean")], 1, mean_exclude_negative_100)



# check for normality of data ----
# Apply the Shapiro-Wilk test to each time point
shapiro_test_time1 <- shapiro.test(mzp1_clean$TPB_Mittelwert)
shapiro_test_time2 <- shapiro.test(mzp2_clean$TPB_Mean)
shapiro_test_time3 <- shapiro.test(mzp3_clean$TPB_mean)

# Print results
print(shapiro_test_time1) # not normally distributed?!
print(shapiro_test_time2) #not normally distributed?!
print(shapiro_test_time3) #normally distributed?




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


# testing for normality ----
# Assuming relevant columns are those ending in '_Mean'
relevant_columns <- combined_df %>%
  select(contains("_Mean"))

# Perform Shapiro-Wilk normality test for each relevant column and group
# does not work
normality_results <- relevant_columns %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  mutate(Group = df$Group) %>%
  group_by(Category, Group) %>%
  summarise(p_value = shapiro.test(Value)$p.value)



### calculating internal validity with cronbach alpha ----
# + stichprobengröße? siehe Seite 69 MA
library(psych)
# Convert -100 to NA
mzp3_cleaner[mzp3_cleaner == -100] <- NA

# Get column names with _inverse
columns_with_inverse <- grep("_inverse$", names(mzp3_cleaner), value = TRUE)
# Remove "_inverse" from column names
columns_to_exclude_without_inverse <- gsub("_inverse", "", columns_with_inverse)

filtered_df_cronbach <- mzp3_cleaner %>%
  select(matches("_0[1-9]$|_10$|_inverse"), -matches("WD")) %>%
  select(-one_of(columns_to_exclude_without_inverse))



# Handle missing values by dropping rows with NAs
#filtered_df_cronbach <- filtered_df_cronbach %>%
 # na.omit()

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




# Conduct Kruskal-Wallis test for each Category ----
#write_csv(x=combined_df, path="data/data_collection/combined_df.csv")

results_kw <- combined_df %>%
  group_by(Category) %>%
  summarise(
    Kruskal_Wallis = list(kruskal.test(MeanValue ~ interaction(Group, Time_Point), data = .))
  )

# Extract and print results for each category
results_kw <- results_kw %>%
  mutate(
    Statistic = map_dbl(Kruskal_Wallis, "statistic"),
    p_value = map_dbl(Kruskal_Wallis, "p.value")
  ) %>%
  select(Category, Statistic, p_value)



# RQ1 Groups and categories Kruskal Wallis test ----
# Perform Kruskal-Wallis test for each competence and time point
rq1_kw_group_category <- combined_df %>%
  filter(Group != "group1") %>% #exclude group 1 from analysis
  group_by(Category, Time_Point) %>%
  summarise(
    kruskal_p = kruskal.test(MeanValue ~ Group)$p.value,
    .groups = 'drop')

results_rq1_kw_test <- print(rq1_kw_group_category)

# Merge the p-values back into the original dataframe for plotting
combined_df <- combined_df %>%
  filter(Group != "group1") %>% #exclude group 1 from analysis
  left_join(rq1_kw_group_category, by = c("Category", "Time_Point"))

# Plot the data - violin plot ----
ggplot(combined_df, aes(x = Group, y = MeanValue, fill = Group)) +
  geom_boxplot() +
  geom_violin(alpha = 0.3) +
  facet_grid(Time_Point ~ Category) +
  labs(title = "Comparison of Groups by Competences and Time Points",
       x = "Group",
       y = "MeanValue") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(data = rq1_kw_group_category, aes(x = 1.5, y = max(combined_df$MeanValue), label = paste("p =", round(kruskal_p, 3))), inherit.aes = FALSE)



#

# Plot the data
(rq1_graph_categories_groups <- ggplot(combined_df, aes(x = Time_Point, y = MeanValue, group = Group, color = Group)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Category, scales = "free_y") +
  labs(title = "Comparison of Groups by Competences and Time Points",
       x = "Time Point",
       y = "Value",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "bottom"))

ggsave(rq1_graph_categories_groups, file = "outputs/rq1_graph_categories_groups.png", width = 7, height = 5)

#### prettier graph?
# Calculate the means for each Group, Competence, and TimePoint
# adding error bars
# excluding irrelevant groups
df_means <- combined_df %>%
  filter(!Category %in% c("SW_Mean", "CS_Mean", "SW_CS_Mean")) %>% #exclude irrelevant categories for this analysis
  group_by(Group, Category, Time_Point) %>%
  summarise(MeanValue2 = mean(MeanValue), 
            LowerCI = MeanValue2 - qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
            UpperCI = MeanValue2 + qt(0.975, length(MeanValue) - 1) * sd(MeanValue) / sqrt(length(MeanValue)),
            .groups = 'drop')


# Plot the data
# with error bars
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

ggsave(rq1_graph_prettier, file = "outputs/rq1_graph_prettier.png", width = 7, height = 5)

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