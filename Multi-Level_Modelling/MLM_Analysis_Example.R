### Install necessary packages
install.packages(c("interactions", "ggeffects", "cowplot", "readr", "lme4", "ggplot2", "dplyr", "performance", "effects", "broom.mixed", "knitr", "lmerTest"))

### Load required packages
library(readr)       # For reading data
library(lme4)        # For multilevel modeling
library(ggplot2)     # For plotting
library(dplyr)       # For data manipulation
library(performance) # For checking model assumptions
library(effects)     # For visualizing effects
library(interactions)# For visualizing interactions
library(ggeffects)   # For visualizing predicted effects
library(cowplot)     # For combining plots
library(broom.mixed) # For tidying model output
library(knitr)       # For tables
library(lmerTest)    # For p-values in mixed models

### Set seed for reproducibility
set.seed(123)

### Read the CSV file
drummers_data <- read.csv("/Users/evangelia_karakoliou/Downloads/drummers_panic_attacks.csv")

### Data preparation
# Convert categorical variables to factors
categorical_vars <- c("Gender", "Trigger", "Sweating", "Shortness_of_Breath",
                      "Dizziness", "Chest_Pain", "Trembling", "Medical_History",
                      "Medication", "Smoking", "Therapy", "Experience_Level",
                      "Performance_Anxiety", "Recent_Performance", "Tour_Status",
                      "Teaching_Status", "Performance_Venue_Size", "Band_ID", "Genre_ID")

drummers_data <- drummers_data %>%
  mutate(across(all_of(categorical_vars), as.factor))

### Initial data exploration
# Correlation between numeric variables
numeric_vars <- drummers_data %>%
  select(Age, Panic_Attack_Frequency, Duration_Minutes, Heart_Rate,
         Caffeine_Intake, Exercise_Frequency, Sleep_Hours, Alcohol_Consumption,
         Panic_Score, Weekly_Practice_Hours, Years_Playing)

correlation_matrix <- cor(numeric_vars)

# Create correlation heatmap and save to file
png("correlation_heatmap.png", width = 800, height = 700)
correlation_heatmap <- function(cormat) {
  melted_corr <- reshape2::melt(cormat)

  ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "white", mid = "lightblue",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "right") +
    coord_fixed() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 2.6) +
    labs(title = "Correlation Heatmap of Numeric Variables")
}

heatmap_plot <- correlation_heatmap(correlation_matrix)
print(heatmap_plot)
dev.off()

### Create boxplots for key variables

# 1. Panic attacks by experience level
plot1 <- ggplot(drummers_data, aes(x = Experience_Level, y = Panic_Attack_Frequency, fill = Experience_Level)) +
  geom_boxplot(outlier.shape = 1, outlier.colour = "darkgrey", width = 0.6) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 10) +
  labs(title = "Panic Attacks by Experience Level",
       x = "Experience Level",
       y = "Monthly Panic Attack Frequency") +
  theme(plot.title = element_text(size = 12),
        legend.position = "bottom")

# 2. Panic attacks by performance anxiety level
plot2 <- ggplot(drummers_data, aes(x = Performance_Anxiety, y = Panic_Attack_Frequency, fill = Performance_Anxiety)) +
  geom_boxplot(outlier.shape = 1, outlier.colour = "darkgrey", width = 0.6) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal(base_size = 10) +
  labs(title = "Panic Attacks by Performance Anxiety",
       x = "Performance Anxiety",
       y = "Monthly Panic Attack Frequency") +
  theme(plot.title = element_text(size = 12),
        legend.position = "bottom")

# Save combined boxplots
png("boxplots_combined.png", width = 900, height = 500)
combined_plots <- plot_grid(plot1, plot2, ncol = 2)
print(combined_plots)
dev.off()

# 3. Relationship between practice hours and panic attacks
practice_plot <- ggplot(drummers_data, aes(x = Weekly_Practice_Hours, y = Panic_Attack_Frequency, color = Experience_Level)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Practice Hours and Panic Attacks",
       x = "Weekly Practice Hours",
       y = "Monthly Panic Attack Frequency")

# Save practice hours plot
png("practice_hours_plot.png", width = 800, height = 500)
print(practice_plot)
dev.off()

# --------------------
# Multi-level modeling
# --------------------

# Model 1: Basic random intercept model with Band_ID
model1 <- lmer(Panic_Score ~ Experience_Level + Performance_Anxiety + Weekly_Practice_Hours +
                 (1 | Band_ID),
               data = drummers_data,
               REML = FALSE) # Use ML for AIC/BIC comparison

# Model 2: Crossed random effects with Band_ID and Genre_ID
model2 <- lmer(Panic_Score ~ Experience_Level + Performance_Anxiety + Weekly_Practice_Hours +
                 (1 | Band_ID) + (1 | Genre_ID),
               data = drummers_data,
               REML = FALSE)

# Model 3: Full model with more predictors
model3 <- lmer(Panic_Score ~ Experience_Level + Performance_Anxiety + Weekly_Practice_Hours +
                 Age + Sleep_Hours + Caffeine_Intake + Tour_Status + Performance_Venue_Size +
                 Medical_History + Therapy +
                 (1 | Band_ID) + (1 | Genre_ID),
               data = drummers_data,
               REML = FALSE)

# Model 4: Interaction model
model4 <- lmer(Panic_Score ~ Experience_Level * Tour_Status + Performance_Anxiety +
                 Weekly_Practice_Hours + Age + Sleep_Hours +
                 (1 | Band_ID) + (1 | Genre_ID),
               data = drummers_data,
               REML = FALSE)

# Compare models using AIC and BIC
model_comparison <- data.frame(
  Model = c("Model 1: Basic", "Model 2: Crossed RE", "Model 3: Full", "Model 4: Interaction"),
  AIC = AIC(model1, model2, model3, model4)$AIC,
  BIC = BIC(model1, model2, model3, model4)$BIC,
  LogLik = c(logLik(model1), logLik(model2), logLik(model3), logLik(model4))
)

# Save model comparison table
write.csv(model_comparison, "model_comparison.csv", row.names = FALSE)

# Create summary function for model output
model_summary_table <- function(model, title) {
  fixed_effects <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      significant = ifelse(p.value < 0.05, "*", ""),
      p.value = ifelse(p.value < 0.001, "< 0.001", round(p.value, 3))
    ) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value, significant)

  # Save as CSV
  write.csv(fixed_effects, paste0(title, "_fixed_effects.csv"), row.names = FALSE)

  # Return for further use
  return(fixed_effects)
}

# Generate summaries for all models
model3_summary <- model_summary_table(model3, "model3")
model4_summary <- model_summary_table(model4, "model4")

# Plot fixed effects of the best model (assuming model3 based on your code)
png("fixed_effects_plot.png", width = 800, height = 600)
fixed_effects_plot <- ggplot(model3_summary,
                             aes(y = reorder(term, estimate),
                                 x = estimate,
                                 xmin = conf.low,
                                 xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Fixed Effects of Final Model",
       x = "Estimate",
       y = NULL) +
  theme_minimal()
print(fixed_effects_plot)
dev.off()

# Visualize interaction from model4
png("interaction_plot.png", width = 800, height = 500)
interaction_plot <- interact_plot(model4,
                                  pred = Experience_Level,
                                  modx = Tour_Status,
                                  plot.points = TRUE,
                                  point.alpha = 0.3) +
  labs(title = "Interaction: Experience Level and Tour Status",
       y = "Panic Score") +
  theme_minimal()
print(interaction_plot)
dev.off()

# Visualize predicted effects
png("predicted_effects.png", width = 900, height = 700)
predicted_effects <- ggpredict(model3, terms = c("Weekly_Practice_Hours", "Experience_Level"))
predicted_plot <- plot(predicted_effects) +
  labs(title = "Predicted Panic Score by Practice Hours and Experience",
       x = "Weekly Practice Hours",
       y = "Predicted Panic Score") +
  theme_minimal()
print(predicted_plot)
dev.off()

# -------------------------
# Additional outcome models
# -------------------------

# Model for panic attack frequency
frequency_model <- lmer(Panic_Attack_Frequency ~ Experience_Level + Performance_Anxiety +
                          Weekly_Practice_Hours + Recent_Performance + Tour_Status +
                          Caffeine_Intake + Sleep_Hours + Therapy +
                          (1 | Band_ID) + (1 | Genre_ID),
                        data = drummers_data,
                        REML = TRUE) # Switch to REML for final model

# Model for panic attack duration
duration_model <- lmer(Duration_Minutes ~ Experience_Level + Performance_Anxiety +
                         Weekly_Practice_Hours + Years_Playing + Medical_History +
                         Therapy + Heart_Rate +
                         (1 | Band_ID) + (1 | Genre_ID),
                       data = drummers_data,
                       REML = TRUE)

# Generate summaries
freq_summary <- model_summary_table(frequency_model, "frequency_model")
duration_summary <- model_summary_table(duration_model, "duration_model")

# Calculate ICCs and save
icc_values <- data.frame(
  Model = c("Panic Score Model", "Frequency Model", "Duration Model"),
  ICC = c(
    performance::icc(model3)$ICC_conditional,
    performance::icc(frequency_model)$ICC_conditional,
    performance::icc(duration_model)$ICC_conditional
  )
)
write.csv(icc_values, "icc_values.csv", row.names = FALSE)

# Check model assumptions for final models and save diagnostics
png("model3_diagnostics.png", width = 800, height = 800)
check_model(model3, panel = TRUE)
dev.off()

# Visualize key predicted relationships for frequency model
png("frequency_by_anxiety.png", width = 800, height = 500)
predicted_freq <- ggpredict(frequency_model, terms = c("Weekly_Practice_Hours", "Performance_Anxiety"))
freq_plot <- plot(predicted_freq) +
  labs(title = "Predicted Panic Attack Frequency by Practice Hours and Anxiety") +
  theme_minimal()
print(freq_plot)
dev.off()

# Create a comprehensive report table with key findings
report_table <- data.frame(
  Outcome = c(rep("Panic Score", 3), rep("Attack Frequency", 3), rep("Attack Duration", 3)),
  TopPredictors = c(
    model3_summary$term[1], model3_summary$term[2], model3_summary$term[3],
    freq_summary$term[1], freq_summary$term[2], freq_summary$term[3],
    duration_summary$term[1], duration_summary$term[2], duration_summary$term[3]
  ),
  Effect = c(
    round(model3_summary$estimate[1], 2), round(model3_summary$estimate[2], 2), round(model3_summary$estimate[3], 2),
    round(freq_summary$estimate[1], 2), round(freq_summary$estimate[2], 2), round(freq_summary$estimate[3], 2),
    round(duration_summary$estimate[1], 2), round(duration_summary$estimate[2], 2), round(duration_summary$estimate[3], 2)
  ),
  Significant = c(
    model3_summary$significant[1], model3_summary$significant[2], model3_summary$significant[3],
    freq_summary$significant[1], freq_summary$significant[2], freq_summary$significant[3],
    duration_summary$significant[1], duration_summary$significant[2], duration_summary$significant[3]
  )
)

# Save report table
write.csv(report_table, "key_findings_report.csv", row.names = FALSE)

### The End
