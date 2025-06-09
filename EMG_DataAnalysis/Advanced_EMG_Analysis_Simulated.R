# Advanced EMG Analysis of Drummer Muscle Activity
# Description: Simulating and analysing EMG data from torso, arm, and wrist muscles during drumming

# install packages
install.packages(c("ggplot2", "dplyr", "tidyr", "signal", "pracma", "wavelets", "ggridges", "viridis", "patchwork", "lme4", "car"))

# load libraries
# efficient loading: lapply(c("ggplot2", "dplyr", "tidyr", "signal", "pracma", "wavelets", "ggridges", "viridis", "patchwork", "lme4", "car"), library, character.only = TRUE)
library(ggplot2)
library(dplyr)
library(tidyr)
library(signal)
library(pracma)
library(wavelets)
library(ggridges)
library(viridis)
library(patchwork)
library(lme4)
library(car)
set.seed(8473)

# simulation parameters
sampling_rate <- 2000 # measured in Hz -> standard for high-quality EMG
duration <- if(rnorm(1) < 0) 5 else 5 # seconds per trial
n_trials <- 20 # no. of trials
n_subjects <- 15 # sample size
drumming_patterns <- c("single_stroke", "double_stroke", "paradiddle") # strokes & rudiments
tempo <- c("slow", "medium", "fast") # drumming tempo

# muscles monitored that are relevant to drummers
muscles <- c(
  "biceps_brachii", # primary elbow flexor
  "triceps_brachii", # primary elbow extensor
  "deltoid_anterior", # shoulder flexion/abduction
  "deltoid_posterior", # shoulder extension/abduction
  "flexor_carpi_radialis", # wrist flexion
  "extensor_carpi_radialis", # wrist extension
  "flexor_digitorum_superficialis", # finger flexion
  "erector_spinae", # spinal extension
  "latissimus_dorsi", # torso stability
  "rectus_abdominis" # core stabilization
)

# function to simulate a realistic EMG signal
generate_emg_signal <- function(duration, fs, muscle, pattern, temp) {
  # no. of samples
  n_samples <- duration * fs
  
  # base parameters which vary by muscle, pattern and tempo
  if(temp == "slow") {
    stroke_freq <- 2 # Hz
    intensity_factor <- 0.7
  } else if(temp == "medium") {
    stroke_freq <- 4 # Hz
    intensity_factor <- 1.0
  } else { # fast
    stroke_freq <- 7 # Hz
    intensity_factor <- 1.3
  }
  
  # muscle-specific parameters for realistic MVC % during drumming
  # MVC = maximal voluntary contraction under a concentric contraction
  muscle_params <- list(
    biceps_brachii = list(mean_amplitude = 0.4, noise_level = 0.05, fatigue_rate = 0.02),
    triceps_brachii = list(mean_amplitude = 0.6, noise_level = 0.06, fatigue_rate = 0.03),
    deltoid_anterior = list(mean_amplitude = 0.5, noise_level = 0.04, fatigue_rate = 0.02),
    deltoid_posterior = list(mean_amplitude = 0.45, noise_level = 0.04, fatigue_rate = 0.02),
    flexor_carpi_radialis = list(mean_amplitude = 0.7, noise_level = 0.07, fatigue_rate = 0.04),
    extensor_carpi_radialis = list(mean_amplitude = 0.8, noise_level = 0.08, fatigue_rate = 0.05),
    flexor_digitorum_superficialis = list(mean_amplitude = 0.75, noise_level = 0.07, fatigue_rate = 0.04),
    erector_spinae = list(mean_amplitude = 0.3, noise_level = 0.03, fatigue_rate = 0.01),
    latissimus_dorsi = list(mean_amplitude = 0.35, noise_level = 0.04, fatigue_rate = 0.02),
    rectus_abdominis = list(mean_amplitude = 0.25, noise_level = 0.03, fatigue_rate = 0.01)
  )
  
  # pattern-specific adjustments (modulation of muscles based on drumming technique)
  pattern_factor <- switch(pattern,
                           "single_stroke" = ifelse(muscle %in% c("flexor_carpi_radialis", "extensor_carpi_radialis", 
                                                                  "flexor_digitorum_superficialis"), 1.2, 0.9),
                           "double_stroke" = ifelse(muscle %in% c("flexor_digitorum_superficialis", "extensor_carpi_radialis"), 
                                                    1.3, ifelse(muscle %in% c("biceps_brachii", "triceps_brachii"), 0.8, 1.0)),
                           "paradiddle" = ifelse(muscle %in% c("deltoid_anterior", "deltoid_posterior", "latissimus_dorsi"), 
                                                 1.1, ifelse(muscle %in% c("erector_spinae", "rectus_abdominis"), 1.2, 0.95))
  )
  
  # combine all factors
  base_amplitude <- muscle_params[[muscle]]$mean_amplitude * intensity_factor * pattern_factor
  
  # generate a time vector
  t <- seq(0, duration, 1/fs)
  
  # generate the EMG baseline - coloured noise to simulate biological signal
  noise <- rnorm(n_samples, 0, muscle_params[[muscle]]$noise_level)
  for(i in 2:length(noise)) {
    noise[i] <- 0.95 * noise[i-1] + noise[i] # AR(1) process for coloured noise
  }
  
  # generate stroke events
  if(pattern == "single_stroke") {
    # regular, evenly spaced strokes
    stroke_times <- seq(0, duration, 1/stroke_freq)
  } else if(pattern == "double_stroke") {
    # paired strokes with spacing
    pairs <- seq(0, duration, 2/stroke_freq)
    stroke_times <- c()
    for(p in pairs) {
      stroke_times <- c(stroke_times, p, p + 0.05) # 50ms between doubles
    }
    stroke_times <- stroke_times[stroke_times <= duration]
  } else { # paradiddle (RLRR LRLL pattern)
    base_times <- seq(0, duration, 4/stroke_freq) # each paradiddle takes 4 beats
    stroke_times <- c()
    for(p in base_times) {
      # RLRR pattern with timing adjustments
      if(p + 0.75/stroke_freq <= duration) {
        stroke_times <- c(stroke_times, 
                          p, 
                          p + 0.25/stroke_freq, 
                          p + 0.5/stroke_freq, 
                          p + 0.75/stroke_freq)
      }
    }
  }
  
  # generate impulse response template for EMG burst (using Gaussian function)
  impulse_len <- 0.1 * fs # 100ms impulse response
  impulse_t <- seq(0, 0.1, 1/fs)
  impulse_response <- exp(-(impulse_t - 0.02)^2 / 0.001) # Gaussian centered at 20ms
  impulse_response <- impulse_response / max(impulse_response) # normalise
  
  # initialise EMG signal with noise
  emg <- noise
  
  # add EMG bursts at stroke times
  for(st in stroke_times) {
    start_idx <- max(1, round(st * fs))
    end_idx <- min(n_samples, start_idx + impulse_len - 1)
    if(start_idx < n_samples) {
      burst_length <- min(impulse_len, n_samples - start_idx + 1)
      emg[start_idx:end_idx] <- emg[start_idx:end_idx] + 
        base_amplitude * impulse_response[1:burst_length]
    }
  }
  
  # add fatigue effect (decreasing amplitude over time)
  fatigue_factor <- 1 - muscle_params[[muscle]]$fatigue_rate * cumsum(rep(1/fs, n_samples))
  fatigue_factor[fatigue_factor < 0.6] <- 0.6 # limit fatigue effect
  
  emg <- emg * fatigue_factor
  
  # add some low-frequency drift
  drift <- sin(2 * pi * 0.1 * t) * 0.05 * base_amplitude
  
  # add simulated power line noise (50 Hz)
  line_noise <- sin(2 * pi * 50 * t) * 0.08 * base_amplitude
  
  # add simulated ECG artifact (primarily affecting torso muscles)
  ecg_artifact <- rep(0, n_samples)
  if(muscle %in% c("erector_spinae", "latissimus_dorsi", "rectus_abdominis")) {
    heart_rate <- 70/60 # beats per second (70 BPM)
    ecg_times <- seq(0.5, duration, 1/heart_rate) # start at 0.5s
    for(et in ecg_times) {
      et_idx <- round(et * fs)
      if(et_idx > 0 && et_idx < n_samples - 100) {
        # simplified ECG waveform (QRS complex)
        qrs_len <- 0.1 * fs # 100ms
        qrs_t <- seq(0, 0.1, 1/fs)
        qrs_wave <- 0.5 * base_amplitude * c(
          rep(0, 10),
          -0.2 * sin(pi * (1:20)/20), 
          sin(pi * (1:50)/25), 
          -0.3 * sin(pi * (1:20)/10),
          rep(0, 100 - 10 - 20 - 50 - 20)
        )[1:length(qrs_t)]
        
        ecg_artifact[et_idx:(et_idx + qrs_len - 1)] <- qrs_wave
      }
    }
  }
  
  # occasional motion artifacts
  motion_artifact <- rep(0, n_samples)
  if(runif(1) < 0.3) { # 30% chance of motion artifact
    artifact_start <- sample(1:(n_samples - 0.5*fs), 1) # random starting point
    artifact_duration <- round(runif(1, 0.1, 0.3) * fs) # 100-300ms
    artifact_end <- min(n_samples, artifact_start + artifact_duration)
    artifact_amp <- runif(1, 1.5, 3) * base_amplitude # large spike
    
    # create motion artifact shape (sudden spike and return)
    motion_shape <- c(
      seq(0, artifact_amp, length.out = round(artifact_duration/4)),
      seq(artifact_amp, -artifact_amp/2, length.out = round(artifact_duration/2)),
      seq(-artifact_amp/2, 0, length.out = round(artifact_duration/4))
    )
    
    if(length(motion_shape) > (artifact_end - artifact_start + 1)) {
      motion_shape <- motion_shape[1:(artifact_end - artifact_start + 1)]
    }
    
    motion_artifact[artifact_start:artifact_end] <- motion_shape
  }
  
  # final EMG signal with all components
  emg <- emg + drift + line_noise + ecg_artifact + motion_artifact
  emg <- abs(emg) # taking absolute value to simulate rectified EMG
  
  return(emg)
}

# generate complete dataset
cat("Generating simulated EMG data...\n")
emg_data <- list()
for(subj in 1:3) { # reducing to 3 subjects for demonstration
  for(pat in drumming_patterns) {
    for(tmp in tempo) {
      for(trial in 1:3) { # reducing to 3 trials for demonstration
        trial_data <- data.frame(
          time = seq(0, duration, 1/sampling_rate),
          subject = subj,
          pattern = pat,
          tempo = tmp,
          trial = trial
        )
        
        # generate EMG for each muscle
        for(mus in muscles) {
          trial_data[[mus]] <- generate_emg_signal(duration, sampling_rate, mus, pat, tmp)
        }
        
        emg_data <- c(emg_data, list(trial_data))
      }
    }
  }
}

# combine all trials into one dataframe
all_data <- bind_rows(emg_data)
cat("Data generation complete.\n")

##### Signal Processing ####
# function for EMG preprocessing with artifacts saved for visualisation
preprocess_emg <- function(signal, fs) {
  # store original signal
  original <- signal
  
  # Butterworth bandpass filter (20-450 Hz - standard for surface EMG)
  bf <- butter(4, c(20, 450)/(fs/2), "pass")
  filtered <- filtfilt(bf$b, bf$a, signal)
  after_bandpass <- filtered
  
  # notch filter for power line interference (50 Hz)
  notch <- butter(2, c(49, 51)/(fs/2), "stop")
  filtered <- filtfilt(notch$b, notch$a, filtered)
  after_notch <- filtered
  
  # full-wave rectification
  rectified <- abs(filtered)
  
  # smoothing (moving average - 100ms window)
  window_size <- round(0.1 * fs)
  smoothed <- pracma::movavg(rectified, window_size, type="s")
  
  # detect outliers (potential motion artifacts)
  mean_signal <- mean(smoothed)
  sd_signal <- sd(smoothed)
  threshold <- mean_signal + 3 * sd_signal
  
  # identify artifacts
  artifact_indices <- which(smoothed > threshold)
  
  # remove/replace artifacts with interpolation if they exist
  if(length(artifact_indices) > 0) {
    # group consecutive indices
    breaks <- which(diff(artifact_indices) > 1)
    groups <- c(0, breaks, length(artifact_indices))
    
    for(i in 1:(length(groups)-1)) {
      start_idx <- artifact_indices[groups[i] + 1]
      end_idx <- artifact_indices[groups[i+1]]
      
      # get values before and after artifact
      if(start_idx > 1 && end_idx < length(smoothed)) {
        before_val <- smoothed[start_idx - 1]
        after_val <- smoothed[end_idx + 1]
        
        # linear interpolation
        smoothed[start_idx:end_idx] <- seq(
          before_val, after_val, length.out = end_idx - start_idx + 1
        )
      }
    }
  }
  
  after_artifact_removal <- smoothed
  
  return(list(
    original = original,
    after_bandpass = after_bandpass,
    after_notch = after_notch,
    rectified = rectified,
    smoothed = smoothed,
    after_artifact_removal = after_artifact_removal,
    artifact_indices = artifact_indices
  ))
}

# process each muscle signal - store intermediate results for visualisation
cat("Processing EMG signals...\n")
# create dataframe to store one example of each stage for visualisation
viz_data <- all_data %>% 
  filter(subject == 1, pattern == "paradiddle", tempo == "fast", trial == 1) %>%
  select(time, flexor_carpi_radialis, extensor_carpi_radialis, biceps_brachii, erector_spinae)

#### Signal Processing and Artifact Removal Visualisation ###
# process a single example for visualisation 
cat("Generating processing visualization...\n")
viz_muscle <- "flexor_carpi_radialis"
viz_subject <- 1
viz_pattern <- "paradiddle"
viz_tempo <- "fast"
viz_trial <- 1

# extract data for visualisation
viz_data <- all_data %>% 
  filter(subject == viz_subject, pattern == viz_pattern, tempo == viz_tempo, trial == viz_trial)
viz_signal <- viz_data[[viz_muscle]]
viz_time <- viz_data$time

# apply processing steps with visualisation
viz_processing <- preprocess_emg(viz_signal, sampling_rate)

# create visualisation dataframe
viz_df <- data.frame(
  time = viz_time,
  original = viz_processing$original,
  after_bandpass = viz_processing$after_bandpass,
  after_notch = viz_processing$after_notch,
  rectified = viz_processing$rectified,
  smoothed = viz_processing$smoothed,
  artifact_removed = viz_processing$after_artifact_removal
)

# plot EMG signal processing stages
processing_plot <- viz_df %>%
  pivot_longer(cols = -time, names_to = "stage", values_to = "amplitude") %>%
  mutate(stage = factor(stage, levels = c("original", "after_bandpass", "after_notch", 
                                          "rectified", "smoothed", "artifact_removed"))) %>%
  ggplot(aes(x = time, y = amplitude)) +
  geom_line() +
  facet_wrap(~stage, scales = "free_y", ncol = 1) +
  labs(title = paste("EMG Signal Processing Stages:", viz_muscle),
       subtitle = paste("Subject:", viz_subject, "Pattern:", viz_pattern, "Tempo:", viz_tempo),
       x = "Time (s)", y = "Amplitude (mV)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# highlight artifacts on original signal
artifact_plot <- ggplot(viz_df, aes(x = time)) +
  geom_line(aes(y = original), color = "black") +
  geom_point(data = viz_df[viz_processing$artifact_indices, ], 
             aes(y = original), color = "red", size = 1) +
  geom_line(aes(y = artifact_removed), color = "blue", alpha = 0.7) +
  labs(title = "Artifact Detection and Removal",
       subtitle = paste("Subject:", viz_subject, "Pattern:", viz_pattern, "Tempo:", viz_tempo),
       x = "Time (s)", y = "Amplitude (mV)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# save plots
pdf("emg_processing_visualization.pdf", width = 10, height = 12)
print(processing_plot)
print(artifact_plot)
dev.off()

### Multi-muscle EMG Visualization ###
# compare multiple muscles during different drumming patterns
key_muscles <- c("flexor_carpi_radialis", "extensor_carpi_radialis", 
                 "biceps_brachii", "triceps_brachii", "erector_spinae")

# create processed data for key muscles
multi_muscle_df <- all_data %>% 
  filter(subject == 1, trial == 1, tempo == "medium") %>%
  select(time, pattern, all_of(key_muscles))

# apply processing to each muscle
for(mus in key_muscles) {
  for(pat in unique(multi_muscle_df$pattern)) {
    idx <- which(multi_muscle_df$pattern == pat)
    sig <- multi_muscle_df[[mus]][idx]
    multi_muscle_df[[paste0(mus, "_processed")]][idx] <- preprocess_emg(sig, sampling_rate)$smoothed
  }
}

# create long format for plotting
multi_muscle_long <- multi_muscle_df %>%
  select(time, pattern, contains("_processed")) %>%
  pivot_longer(cols = contains("_processed"), 
               names_to = "muscle", 
               values_to = "amplitude") %>%
  mutate(muscle = gsub("_processed", "", muscle))

# plot multiple muscles across different patterns
multi_muscle_plot <- ggplot(multi_muscle_long, aes(x = time, y = amplitude, color = muscle)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~pattern, ncol = 1) +
  scale_color_viridis_d() +
  labs(title = "EMG Activity Across Multiple Muscles During Different Drumming Patterns",
       subtitle = "Subject 1, Medium Tempo",
       x = "Time (s)", y = "Amplitude (mV)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# save plot
pdf("multi_muscle_comparison.pdf", width = 10, height = 12)
print(multi_muscle_plot)
dev.off()

### Tempo Effect Visualisation ###
# create data for tempo comparison
tempo_comparison <- all_data %>%
  filter(subject == 1, pattern == "single_stroke", trial == 1) %>%
  select(time, tempo, extensor_carpi_radialis)

# process signals
for(tmp in unique(tempo_comparison$tempo)) {
  idx <- which(tempo_comparison$tempo == tmp)
  sig <- tempo_comparison$extensor_carpi_radialis[idx]
  tempo_comparison$processed[idx] <- preprocess_emg(sig, sampling_rate)$smoothed
}

# plot tempo effect
tempo_plot <- ggplot(tempo_comparison, aes(x = time, y = processed, color = tempo)) +
  geom_line() +
  scale_color_manual(values = c("slow" = "blue", "medium" = "purple", "fast" = "red")) +
  labs(title = "Effect of Tempo on Extensor Carpi Radialis Activity",
       subtitle = "Single Stroke Pattern, Subject 1",
       x = "Time (s)", y = "Amplitude (mV)") +
  theme_minimal()

# save plot
pdf("tempo_effect.pdf", width = 10, height = 6)
print(tempo_plot)
dev.off()

### Spectral Analysis ###
# function for spectral analysis
analyze_spectrum <- function(signal, fs) {
  # calculate power spectral density
  spec <- spectrum(signal, plot = FALSE)
  
  # create dataframe for plotting
  spec_df <- data.frame(
    frequency = spec$freq * fs,
    power = spec$spec
  )
  
  return(spec_df)
}

# analyse one sample from each drumming pattern
spec_analysis <- data.frame()
for(pat in drumming_patterns) {
  sample_data <- all_data %>%
    filter(subject == 1, pattern == pat, tempo == "medium", trial == 1)
  
  # process flexor carpi radialis
  signal <- sample_data$flexor_carpi_radialis
  processed <- preprocess_emg(signal, sampling_rate)$after_notch
  
  # get spectrum
  spec_df <- analyze_spectrum(processed, sampling_rate)
  spec_df$pattern <- pat
  
  spec_analysis <- rbind(spec_analysis, spec_df)
}

# plot spectral comparison
spectrum_plot <- ggplot(spec_analysis, aes(x = frequency, y = power, color = pattern)) +
  geom_line(alpha = 0.7) +
  scale_x_continuous(limits = c(0, 500)) +
  labs(title = "Power Spectral Density of Flexor Carpi Radialis EMG",
       subtitle = "Comparison Between Drumming Patterns (Medium Tempo)",
       x = "Frequency (Hz)", y = "Power") +
  theme_minimal()

# save plot
pdf("spectral_analysis.pdf", width = 10, height = 6)
print(spectrum_plot)
dev.off()

### Feature Extraction and Statistical Analysis ###
# function to extract features from a processed EMG signal
extract_features <- function(signal) {
  # Mean Absolute Value
  mav <- mean(abs(signal))
  
  # Root Mean Square
  rms <- sqrt(mean(signal^2))
  
  # integrated EMG
  iemg <- sum(abs(signal))
  
  # waveform Length
  wl <- sum(abs(diff(signal)))
  
  # variance
  var_emg <- var(signal)
  
  return(c(MAV = mav, RMS = rms, IEMG = iemg, WL = wl, VAR = var_emg))
}

# extract features for all conditions
feature_results <- data.frame()
for(subj in unique(all_data$subject)) {
  for(pat in drumming_patterns) {
    for(tmp in tempo) {
      for(trl in unique(all_data$trial)) {
        # get segment data
        seg_data <- all_data %>% 
          filter(subject == subj, pattern == pat, tempo == tmp, trial == trl)
        
        if(nrow(seg_data) > 0) {
          for(mus in key_muscles) {
            # process signal
            signal <- seg_data[[mus]]
            processed <- preprocess_emg(signal, sampling_rate)$smoothed
            
            # extract features
            features <- extract_features(processed)
            
            # create result row
            result_row <- data.frame(
              subject = subj,
              pattern = pat,
              tempo = tmp,
              trial = trl,
              muscle = mus,
              t(features)
            )
            
            feature_results <- rbind(feature_results, result_row)
          }
        }
      }
    }
  }
}

# calculate summary statistics
feature_summary <- feature_results %>%
  group_by(pattern, tempo, muscle) %>%
  summarize(
    MAV_mean = mean(MAV),
    RMS_mean = mean(RMS),
    IEMG_mean = mean(IEMG),
    MAV_sd = sd(MAV),
    RMS_sd = sd(RMS),
    IEMG_sd = sd(IEMG),
    .groups = "drop"
  )

# plot mean RMS by muscle, pattern and tempo
rms_plot <- ggplot(feature_results, aes(x = muscle, y = RMS, fill = pattern)) +
  geom_boxplot() +
  facet_wrap(~tempo) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Root Mean Square (RMS) Values by Muscle, Pattern and Tempo",
       x = "Muscle", y = "RMS Value")

# save plot
pdf("feature_analysis.pdf", width = 12, height = 8)
print(rms_plot)
dev.off()

### Time-Frequency Analysis (wavelet transform) ###
# wavelet analysis on one example signal
wavelet_signal <- all_data %>%
  filter(subject == 1, pattern = "paradiddle", tempo == "fast", trial == 1) %>%
  pull(flexor_carpi_radialis)

processed_signal <- preprocess_emg(wavelet_signal, sampling_rate)$after_notch

# apply continuous wavelet transform
wavelet_result <- cwt(processed_signal, dj = 1/12, n.scale = 50, 
                      powerscales = TRUE, mother = "morlet", param = 6)

# create time-frequency heatmap
time_freq_df <- expand.grid(
  time = seq(0, duration, length.out = length(processed_signal)),
  scale = wavelet_result$scale
)
time_freq_df$power <- as.vector(abs(wavelet_result$wave)^2)
time_freq_df$period <- wavelet_result$period

# convert scale to pseudo-frequency
time_freq_df$frequency <- sampling_rate / time_freq_df$period

# filter to relevant frequency range
time_freq_df <- time_freq_df %>% filter(frequency <= 500 & frequency >= 10)

# plot time-frequency analysis
wavelet_plot <- ggplot(time_freq_df, aes(x = time, y = frequency, fill = power)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "magma") +
  scale_y_log10() +
  labs(title = "Wavelet Time-Frequency Analysis of Flexor Carpi Radialis EMG",
       subtitle = "Paradiddle Pattern at Fast Tempo",
       x = "Time (s)", y = "Frequency (Hz)") +
  theme_minimal()

# save plot
pdf("wavelet_analysis.pdf", width = 10, height = 8)
print(wavelet_plot)
dev.off()

### Muscle Co-Activation Analysis ###
# calculate co-activation indices between antagonist pairs
co_activation <- data.frame()
for(subj in unique(all_data$subject)) {
  for(pat in drumming_patterns) {
    for(tmp in tempo) {
      for(trl in unique(all_data$trial)) {
        # get segment data
        seg_data <- all_data %>% 
          filter(subject == subj, pattern == pat, tempo == tmp, trial == trl)
        
        if(nrow(seg_data) > 0) {
          # process antagonist pair signals
          biceps <- preprocess_emg(seg_data$biceps_brachii, sampling_rate)$smoothed
          triceps <- preprocess_emg(seg_data$triceps_brachii, sampling_rate)$smoothed
          
          flexor <- preprocess_emg(seg_data$flexor_carpi_radialis, sampling_rate)$smoothed
          extensor <- preprocess_emg(seg_data$extensor_carpi_radialis, sampling_rate)$smoothed
          
          # calculate co-activation index (minimum of each pair divided by maximum)
          arm_coact <- mean(pmin(biceps, triceps) / pmax(biceps, triceps), na.rm = TRUE)
          wrist_coact <- mean(pmin(flexor, extensor) / pmax(flexor, extensor), na.rm = TRUE)
          
          # create result row
          result_row <- data.frame(
            subject = subj,
            pattern = pat,
            tempo = tmp,
            trial = trl,
            arm_coactivation = arm_coact,
            wrist_coactivation = wrist_coact
          )
          
          co_activation <- rbind(co_activation, result_row)
        }
      }
    }
  }
}

# plot co-activation by pattern and tempo
coact_plot <- co_activation %>%
  pivot_longer(cols = c(arm_coactivation, wrist_coactivation),
               names_to = "muscle_group", values_to = "coactivation") %>%
  ggplot(aes(x = pattern, y = coactivation, fill = tempo)) +
  geom_boxplot() +
  facet_wrap(~muscle_group) +
  scale_fill_viridis_d() +
  labs(title = "Muscle Co-Activation Indices by Drumming Pattern and Tempo",
       x = "Drumming Pattern", y = "Co-Activation Index") +
  theme_minimal()

# save plot
pdf("coactivation_analysis.pdf", width = 10, height = 6)
print(coact_plot)
dev.off()

### Summary Report Generation ###
# Print summary statistics to console
cat("\n---- EMG Analysis Summary ----\n")
cat("Number of subjects analyzed:", length(unique(all_data$subject)), "\n")
cat("Drumming patterns:", paste(drumming_patterns, collapse=",
