# Load necessary libraries
install.packages("emmeans")
install.packages("multcomp")
library(emmeans)
library(multcomp)


#### ANOVA ####
##### EDA_AR #####
# Load the data from CSV
edaar <- read.csv("edaar.csv")

# Display the first few rows to inspect the data
head(edaar)

# Remove rows where EventRene is 'CG'
edaar <- subset(edaar, EventRene != 'CG')


# Fit ANOVA model to compare the EDAAR values in different packaging types
anova_edaar <- aov(EDA_AR ~ EventRene, data = edaar)

# Print the ANOVA table for EDAAR
summary(anova_edaar)


# ##### Tukey's HSD test for EDAAR 
# # Calculate Estimated Marginal Means
# em_means_edaar <- emmeans(anova_edaar, specs = "EventRene")
# 
# # Perform Tukey's HSD test for pairwise comparisons
# tukey_edaar <- pairs(em_means_edaar, adjust = "tukey")
# 
# # Print the results
# summary(tukey_edaar)





##### RMSSD #####
# Load the data from CSV
rmssd <- read.csv("rmssd.csv")

# Display the first few rows to inspect the data
head(rmssd)

# Remove rows where EventRene is 'CG'
rmssd <- subset(rmssd, EventRene != 'CG')


# Fit ANOVA model to compare the RMSSD values in different packaging types
anova_rmssd <- aov(RMSSD ~ EventRene, data = rmssd)

# Print the ANOVA table for RMSSD
summary(anova_rmssd)



# ##### Tukey's HSD test for RMSSD 
# # Calculate Estimated Marginal Means
# em_means_rmssd <- emmeans(anova_rmssd, specs = "EventRene")
# 
# # Perform Tukey's HSD test for pairwise comparisons
# tukey_rmssd <- pairs(em_means_rmssd, adjust = "tukey")
# 
# # Print the results
# summary(tukey_rmssd)






##### PFD #####
# Load the data from CSV
pfd <- read.csv("pfd.csv")

# Display the first few rows to inspect the data
head(pfd)

# Remove rows where EventRene is 'CG'
pfd <- subset(pfd, EventRene != 'CG')


# Fit ANOVA model to compare the PFD values in different packaging types
anova_pfd <- aov(PFD ~ EventRene, data = pfd)

# Print the ANOVA table for PFD
summary(anova_pfd)


# ##### Tukey's HSD test for PFD 
# # Calculate Estimated Marginal Means
# em_means_pfd <- emmeans(anova_pfd, specs = "EventRene")
# 
# # Perform Tukey's HSD test for pairwise comparisons
# tukey_pfd <- pairs(em_means_pfd, adjust = "tukey")
# 
# # Print the results
# summary(tukey_pfd)




##### EMOTIONS #####
# Load the data from CSV
emotions <- read.csv("emotions.csv")

# Display the first few rows to inspect the data
head(emotions)

# Remove rows where EventRene is 'CG'
emotions <- subset(emotions, EventRene != 'CG')

# List of emotional expressions
emotions_list <- c("Ave_Surprised", "Ave_Angry", "Ave_Disgusted", "Ave_Happy", "Ave_Sad", "Ave_Scared")

# Loop through each emotional expression
for (emotion in emotions_list) {
  cat("\n##### ANOVA and Tukey's HSD for", emotion, "#####\n")
  
  # Fit ANOVA model
  formula <- as.formula(paste(emotion, "~ EventRene"))
  anova_model <- aov(formula, data = emotions)
  
  # Print the ANOVA table
  cat("\nANOVA results for", emotion, ":\n")
  print(summary(anova_model))
  
  # # Calculate Estimated Marginal Means
  # em_means <- emmeans(anova_model, specs = "EventRene")
  # 
  # # Perform Tukey's HSD test for pairwise comparisons
  # tukey_test <- pairs(em_means, adjust = "tukey")
  
  # # Print the results
  # cat("\nTukey's HSD test results for", emotion, ":\n")
  # print(summary(tukey_test))
}





#### Chi-square test ####
# Load the dataset
preference <- read.csv("preference.csv")

# Display the first few rows to inspect the data
head(preference)

# Create a frequency table of the observed preferences
observed_frequencies <- table(preference$Preference)

# 'FT' is included with a frequency of 0
observed_frequencies <- c(observed_frequencies, FT = 0)

# Print the observed frequencies
print(observed_frequencies)

# Define the expected frequencies
total_observations <- sum(observed_frequencies)
hypothesized_proportions <- c(BS = 0.2, HF = 0.2, FT = 0.2, All = 0.2, CG = 0.2)

# Calculate expected frequencies
expected_frequencies <- hypothesized_proportions * total_observations

# Print the expected frequencies
print(expected_frequencies)

# Perform the Chi-square test
chi_square_test <- chisq.test(observed_frequencies, p = hypothesized_proportions)

# Print the results
print(chi_square_test)







#### Paired t-test ####

##### EDA_AR #####
# Load the dataset
edaar_pairedt <- read.csv("edaar_pairedt.csv")

# Ensure the data is correctly paired
# Filter for Popularity cue and Non-Popularity cue
popularity_cue_edaar <- edaar_pairedt[edaar_pairedt$EventRene_phases == 'Popularity cue', "EDA_AR_pairedt"]
non_popularity_cue_edaar <- edaar_pairedt[edaar_pairedt$EventRene_phases == 'Non_Popularity cue', "EDA_AR_pairedt"]

# Check if lengths match
length(popularity_cue_edaar)
length(non_popularity_cue_edaar)

# Combine into a data frame to ensure pairing is maintained
edaar_pop <- merge(
  edaar_pairedt[edaar_pairedt$EventRene_phases == 'Popularity cue', c("Session", "EDA_AR_pairedt")],
  edaar_pairedt[edaar_pairedt$EventRene_phases == 'Non_Popularity cue', c("Session", "EDA_AR_pairedt")],
  by = "Session",
  suffixes = c("_popularity", "_non_popularity")
)

# Perform the paired t-test
edaar_pop_t <- t.test(edaar_pop$EDA_AR_pairedt_popularity, edaar_pop$EDA_AR_pairedt_non_popularity, paired = TRUE)
print(edaar_pop_t)



# Filter for Health cue and Non-Health cue
health_cue_edaar <- edaar_pairedt[edaar_pairedt$EventRene_phases == 'Health cue', "EDA_AR_pairedt"]
non_health_cue_edaar <- edaar_pairedt[edaar_pairedt$EventRene_phases == 'Non_Health cue', "EDA_AR_pairedt"]

# Check if lengths match
length(health_cue_edaar)
length(non_health_cue_edaar)

# Combine into a data frame to ensure pairing is maintained
edaar_health <- merge(
  edaar_pairedt[edaar_pairedt$EventRene_phases == 'Health cue', c("Session", "EDA_AR_pairedt")],
  edaar_pairedt[edaar_pairedt$EventRene_phases == 'Non_Health cue', c("Session", "EDA_AR_pairedt")],
  by = "Session",
  suffixes = c("_health", "_non_health")
)

# Perform the paired t-test
edaar_health_t <- t.test(edaar_health$EDA_AR_pairedt_health, edaar_health$EDA_AR_pairedt_non_health, paired = TRUE)
print(edaar_health_t)




##### RMSSD #####
# Load the dataset
rmssd_pairedt <- read.csv("rmssd_pairedt.csv")

# Ensure the data is correctly paired
# Filter for Popularity cue and Non-Popularity cue
popularity_cue_rmssd <- rmssd_pairedt[rmssd_pairedt$EventRene_phases == 'Popularity cue', "RMSSD_pairedt"]
non_popularity_cue_rmssd <- rmssd_pairedt[rmssd_pairedt$EventRene_phases == 'Non_Popularity cue', "RMSSD_pairedt"]

# Check if lengths match
length(popularity_cue_rmssd)
length(non_popularity_cue_rmssd)

# Combine into a data frame to ensure pairing is maintained
rmssd_pop <- merge(
  rmssd_pairedt[rmssd_pairedt$EventRene_phases == 'Popularity cue', c("Session", "RMSSD_pairedt")],
  rmssd_pairedt[rmssd_pairedt$EventRene_phases == 'Non_Popularity cue', c("Session", "RMSSD_pairedt")],
  by = "Session",
  suffixes = c("_popularity", "_non_popularity")
)

# Perform the paired t-test
rmssd_pop_t <- t.test(rmssd_pop$RMSSD_pairedt_popularity, rmssd_pop$RMSSD_pairedt_non_popularity, paired = TRUE)
print(rmssd_pop_t)

# Filter for Health cue and Non-Health cue
health_cue_rmssd <- rmssd_pairedt[rmssd_pairedt$EventRene_phases == 'Health cue', "RMSSD_pairedt"]
non_health_cue_rmssd <- rmssd_pairedt[rmssd_pairedt$EventRene_phases == 'Non_Health cue', "RMSSD_pairedt"]

# Check if lengths match
length(health_cue_rmssd)
length(non_health_cue_rmssd)

# Combine into a data frame to ensure pairing is maintained
rmssd_health <- merge(
  rmssd_pairedt[rmssd_pairedt$EventRene_phases == 'Health cue', c("Session", "RMSSD_pairedt")],
  rmssd_pairedt[rmssd_pairedt$EventRene_phases == 'Non_Health cue', c("Session", "RMSSD_pairedt")],
  by = "Session",
  suffixes = c("_health", "_non_health")
)

# Perform the paired t-test
rmssd_health_t <- t.test(rmssd_health$RMSSD_pairedt_health, rmssd_health$RMSSD_pairedt_non_health, paired = TRUE)
print(rmssd_health_t)





##### PFD #####
# Load the dataset
pfd_pairedt <- read.csv("pfd_pairedt.csv")

# Ensure the data is correctly paired
# Filter for Popularity cue and Non-Popularity cue
popularity_cue_pfd <- pfd_pairedt[pfd_pairedt$EventRene_phases == 'Popularity cue', "PFD_pairedt"]
non_popularity_cue_pfd <- pfd_pairedt[pfd_pairedt$EventRene_phases == 'Non_Popularity cue', "PFD_pairedt"]

# Check if lengths match
length(popularity_cue_pfd)
length(non_popularity_cue_pfd)

# Combine into a data frame to ensure pairing is maintained
pfd_pop <- merge(
  pfd_pairedt[pfd_pairedt$EventRene_phases == 'Popularity cue', c("Session", "PFD_pairedt")],
  pfd_pairedt[pfd_pairedt$EventRene_phases == 'Non_Popularity cue', c("Session", "PFD_pairedt")],
  by = "Session",
  suffixes = c("_popularity", "_non_popularity")
)

# Perform the paired t-test
pfd_pop_t <- t.test(pfd_pop$PFD_pairedt_popularity, pfd_pop$PFD_pairedt_non_popularity, paired = TRUE)
print(pfd_pop_t)

# Filter for Health cue and Non-Health cue
health_cue_pfd <- pfd_pairedt[pfd_pairedt$EventRene_phases == 'Health cue', "PFD_pairedt"]
non_health_cue_pfd <- pfd_pairedt[pfd_pairedt$EventRene_phases == 'Non_Health cue', "PFD_pairedt"]

# Check if lengths match
length(health_cue_pfd)
length(non_health_cue_pfd)

# Combine into a data frame to ensure pairing is maintained
pfd_health <- merge(
  pfd_pairedt[pfd_pairedt$EventRene_phases == 'Health cue', c("Session", "PFD_pairedt")],
  pfd_pairedt[pfd_pairedt$EventRene_phases == 'Non_Health cue', c("Session", "PFD_pairedt")],
  by = "Session",
  suffixes = c("_health", "_non_health")
)

# Perform the paired t-test
pfd_health_t <- t.test(pfd_health$PFD_pairedt_health, pfd_health$PFD_pairedt_non_health, paired = TRUE)
print(pfd_health_t)




##### EMOTIONS #####
# Load the dataset
emotions_pairedt <- read.csv("emotions_pairedt.csv")

# List of emotional expressions
emotional_expressions <- c("Pair_Ave_Surprised", "Pair_Ave_Angry", "Pair_Ave_Disgusted", "Pair_Ave_Happy", "Pair_Ave_Sad", "Pair_Ave_Scared")

# Function to perform paired t-test
perform_paired_t_test <- function(expression) {
  # Filter for Popularity cue and Non-Popularity cue
  popularity_cue <- emotions_pairedt[emotions_pairedt$EventRene_phases == 'Popularity cue', expression]
  non_popularity_cue <- emotions_pairedt[emotions_pairedt$EventRene_phases == 'Non_Popularity cue', expression]
  
  # Combine into a data frame to ensure pairing is maintained
  pop_data <- merge(
    emotions_pairedt[emotions_pairedt$EventRene_phases == 'Popularity cue', c("Session", expression)],
    emotions_pairedt[emotions_pairedt$EventRene_phases == 'Non_Popularity cue', c("Session", expression)],
    by = "Session",
    suffixes = c("_popularity", "_non_popularity")
  )
  
  # Perform the paired t-test for Popularity vs. Non-Popularity
  pop_t_test <- t.test(pop_data[, paste0(expression, "_popularity")], pop_data[, paste0(expression, "_non_popularity")], paired = TRUE)
  print(paste("Popularity vs Non-Popularity for", expression))
  print(pop_t_test)
  
  # Filter for Health cue and Non-Health cue
  health_cue <- emotions_pairedt[emotions_pairedt$EventRene_phases == 'Health cue', expression]
  non_health_cue <- emotions_pairedt[emotions_pairedt$EventRene_phases == 'Non_Health cue', expression]
  
  # Combine into a data frame to ensure pairing is maintained
  health_data <- merge(
    emotions_pairedt[emotions_pairedt$EventRene_phases == 'Health cue', c("Session", expression)],
    emotions_pairedt[emotions_pairedt$EventRene_phases == 'Non_Health cue', c("Session", expression)],
    by = "Session",
    suffixes = c("_health", "_non_health")
  )
  
  # Perform the paired t-test for Health vs. Non-Health
  health_t_test <- t.test(health_data[, paste0(expression, "_health")], health_data[, paste0(expression, "_non_health")], paired = TRUE)
  print(paste("Health vs Non-Health for", expression))
  print(health_t_test)
}

# Loop through each emotional expression and perform the paired t-tests
for (expression in emotional_expressions) {
  perform_paired_t_test(expression)
}