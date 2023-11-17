# Part 1: Generic Start

# Function to simulate the game with increased randomness
simulate_game_random <- function() {
  # Stage 1 parameters with 4% randomness (use own cr, but make sure there's a gap of 0.04 for uncertainty)
  stage1_catch_prob <- runif(1, 0.71, 0.75)
  stage1_words_per_catch <- 940
  stage1_words_required <- 4000
  
  # Stage 2 parameters with 4% randomness (use own cr, but make sure there's a gap of 0.04 for uncertainty)
  stage2_a_encounter_prob <- runif(1, 0.58, 0.62)
  stage2_a_catch_prob <- runif(1, 0.71, 0.75)
  stage2_a_words_per_catch <- 940
  
  stage2_b_catch_prob <- runif(1, 0.65, 0.69)
  stage2_b_words_per_catch <- 3750
  stage2_b_additional_turns <- 2
  
  # Simulation
  total_words <- 0
  turns_left <- 25
  total_turns <- 25
  
  # Stage 1
  while (total_words < stage1_words_required && turns_left > 0) {
    if (runif(1) < stage1_catch_prob) {
      total_words <- total_words + stage1_words_per_catch
    }
    turns_left <- turns_left - 1
  }
  
  # Stage 2
  while (turns_left > 0) {
    if (runif(1) < stage2_a_encounter_prob) {
      turns_left <- turns_left - 1  # Loss of turn due to encounter
      if (runif(1) < stage2_a_catch_prob) {
        total_words <- total_words + stage2_a_words_per_catch
      }
    } else {
      turns_left <- turns_left - 1  # Loss of turn due to encounter
      if (runif(1) < stage2_b_catch_prob) {
        total_words <- total_words + stage2_b_words_per_catch
        total_turns <- total_turns + stage2_b_additional_turns
        turns_left  <- turns_left + stage2_b_additional_turns
      }
    }
  }
  
  return(c(volume = total_words / 4000, gnawbel_prize = (total_words / 4000) * 2*54, CC = (total_words / 4000) + 4, total_turns = total_turns))
} #remove the 2 if toc4 not upgraded, change to 4 if event

# Simulate the game 50,000 times with increased randomness
simulations_random <- replicate(50000, simulate_game_random())

# Extract parameters from simulations
volume_random <- simulations_random[1,]
gnawbel_prize_random <- simulations_random[2,]
CC_random <- simulations_random[3,]
total_turns_random <- simulations_random[4,]

# Calculate the median values
median_volume_random <- median(volume_random)
median_gnawbel_prize_random <- median(gnawbel_prize_random)
median_CC_random <- median(CC_random)
median_turns_random <- median(total_turns_random)

# Calculate the 25th to 75th percentiles
percentiles_volume_random <- quantile(volume_random, c(0.25, 0.75))
percentiles_gnawbel_prize_random <- quantile(gnawbel_prize_random, c(0.25, 0.75))
percentiles_CC_random <- quantile(CC_random, c(0.25, 0.75))
percentiles_turns_random <- quantile(total_turns_random, c(0.25, 0.75))

# Print the results with increased randomness
cat("Median Volume (with increased randomness):", median_volume_random, "\n")
cat("25th Percentile of Volume:", percentiles_volume_random[1], "\n")
cat("75th Percentile of Volume:", percentiles_volume_random[2], "\n")
cat("\n")
cat("Median Gnawbel Prize:", median_gnawbel_prize_random, "\n")
cat("25th Percentile of Gnawbel Prize:", percentiles_gnawbel_prize_random[1], "\n")
cat("75th Percentile of Gnawbel Prize:", percentiles_gnawbel_prize_random[2], "\n")
cat("\n")
cat("Median CC:", median_CC_random, "\n")
cat("25th Percentile of CC:", percentiles_CC_random[1], "\n")
cat("75th Percentile of CC:", percentiles_CC_random[2], "\n")
cat("\n")
cat("Median Total Turns:", median_turns_random, "\n")
cat("25th Percentile of Total Turns:", percentiles_turns_random[1], "\n")
cat("75th Percentile of Total Turns:", percentiles_turns_random[2], "\n")
cat("\n")

# Part 2: Startpoint

# Function to simulate the game with increased randomness and starting from specific words and turns
simulate_game_random_from_start <- function(starting_words, starting_turns) {
 # Stage 1 parameters with 4% randomness 
  stage1_catch_prob <- runif(1, 0.71, 0.75)
  stage1_words_per_catch <- 940
  stage1_words_required <- 4000
  
  # Stage 2 parameters with 4% randomness
  stage2_a_encounter_prob <- runif(1, 0.58, 0.62)
  stage2_a_catch_prob <- runif(1, 0.71, 0.75)
  stage2_a_words_per_catch <- 940
  
  stage2_b_catch_prob <- runif(1, 0.65, 0.69)
  stage2_b_words_per_catch <- 3750
  stage2_b_additional_turns <- 2
  
  # Simulation
  total_words <- starting_words
  turns_left <- starting_turns
  total_turns <- 25
  
  # Stage 1
  while (total_words < stage1_words_required && turns_left > 0) {
    turns_left <- turns_left - 1  # Loss of turn due to encounter
    if (runif(1) < stage1_catch_prob) {
      total_words <- total_words + stage1_words_per_catch
    }
  }
  
  # Stage 2
  while (turns_left > 0) {
    if (runif(1) < stage2_a_encounter_prob) {
      turns_left <- turns_left - 1  # Loss of turn due to encounter
      if (runif(1) < stage2_a_catch_prob) {
        total_words <- total_words + stage2_a_words_per_catch
      }
    } else {
      turns_left <- turns_left - 1  # Loss of turn due to encounter
      if (runif(1) < stage2_b_catch_prob) {
        total_words <- total_words + stage2_b_words_per_catch
        total_turns <- total_turns + stage2_b_additional_turns
        turns_left  <- turns_left + stage2_b_additional_turns
      }
    }
  }
  
  return(c(volume = total_words / 4000, gnawbel_prize = (total_words / 4000) * 2*54, CC = (total_words / 4000) + 4, total_turns = total_turns))
} #same instructions as before

# Simulate the game 5000 times starting from given point
simulations_random_start <- replicate(50000, simulate_game_random_from_start(44100,9)) #replace with (points, turns left)

# Extract parameters from simulations
volume_random_start <- simulations_random_start[1,]
gnawbel_prize_random_start <- simulations_random_start[2,]
CC_random_start <- simulations_random_start[3,]
total_turns_random_start <- simulations_random_start[4,]

# Calculate the median values
median_volume_random_start <- median(volume_random_start)
median_gnawbel_prize_random_start <- median(gnawbel_prize_random_start)
median_CC_random_start <- median(CC_random_start)
median_turns_random_start <- median(total_turns_random_start)

# Calculate the 25th to 75th percentiles
percentiles_volume_random_start <- quantile(volume_random_start, c(0.25, 0.75))
percentiles_gnawbel_prize_random_start <- quantile(gnawbel_prize_random_start, c(0.25, 0.75))
percentiles_CC_random_start <- quantile(CC_random_start, c(0.25, 0.75))
percentiles_turns_random_start <- quantile(total_turns_random_start, c(0.25, 0.75))

# Print the results with increased randomness and starting from specific words and turns
cat("Median Volume (starting from specific words and turns):", median_volume_random_start, "\n")
cat("25th Percentile of Volume:", percentiles_volume_random_start[1], "\n")
cat("75th Percentile of Volume:", percentiles_volume_random_start[2], "\n")
cat("\n")
cat("Median Gnawbel Prize (starting from specific words and turns):", median_gnawbel_prize_random_start, "\n")
cat("25th Percentile of Gnawbel Prize:", percentiles_gnawbel_prize_random_start[1], "\n")
cat("75th Percentile of Gnawbel Prize:", percentiles_gnawbel_prize_random_start[2], "\n")
cat("\n")
cat("Median CC (starting from specific words and turns):", median_CC_random_start, "\n")
cat("25th Percentile of CC:", percentiles_CC_random_start[1], "\n")
cat("75th Percentile of CC:", percentiles_CC_random_start[2], "\n")
cat("\n")

# Simulate the game starting from a clean slate 5000 times
clean_slate_runs <- simulations_random
# Extract words from clean slate runs
words_clean_slate <- clean_slate_runs[1, ] * 4000

# Calculate median and percentiles for clean slate runs
median_words_clean_slate <- median(words_clean_slate)
percentiles_words_clean_slate <- quantile(words_clean_slate, c(0.25, 0.75))

# Simulate the specific run with parameters from 5000 runs
specific_run <- c(
  volume = median_volume_random_start,
  gnawbel_prize = median_gnawbel_prize_random_start,
  CC = median_CC_random_start,
  total_turns = median_turns_random_start
)

# Compare specific run to clean slate distribution
# Calculate specific run percentile
percentile_specific_run <- mean(words_clean_slate < specific_run["volume"] * 4000) * 100

# Print the percentile message

if (percentile_specific_run > 50) {
  cat(sprintf("Your specific run is performing above median clean slate runs, at %.2f percentile, and you will reach %.2f words.\n", percentile_specific_run, specific_run["volume"] * 4000))
} else {
  cat(sprintf("Your specific run is performing below median clean slate runs, at %.2f percentile, and you will reach %.2f words.\n", percentile_specific_run, specific_run["volume"] * 4000))
}
# Plot histograms

par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 1))

# Plot Volume histogram
hist(volume_random, main = "Volume", xlab = "Volume", col = "lightblue", border = "black")
abline(v = specific_run["volume"], col = "red", lty = 1)
#text(x = specific_run["volume"], y = max(table(volume_random)) + 0.1, "Specific Run", pos = 2, col = "red")

# Plot Gnawbel Prize histogram
hist(gnawbel_prize_random, main = "Gnawbel Prize", xlab = "Gnawbel Prize", col = "lightgreen", border = "black")
abline(v = specific_run["gnawbel_prize"], col = "red", lty = 1)
#text(x = specific_run["gnawbel_prize"], y = max(table(gnawbel_prize_random)) + 0.1, "Specific Run", pos = 2, col = "red")

# Plot CC histogram
hist(CC_random, main = "CC", xlab = "CC", col = "lightcoral", border = "black")
abline(v = specific_run["CC"], col = "red", lty = 1)
#text(x = specific_run["CC"], y = max(table(CC_random)) + 0.1, "Specific Run", pos = 2, col = "red")

# Plot Total Turns histogram
hist(total_turns_random, main = "Total Turns", xlab = "Total Turns", col = "lightgoldenrodyellow", border = "black")
#abline(v = specific_run["total_turns"], col = "red", lty = 1)
#text(x = specific_run["total_turns"], y = max(table(total_turns_random)) + 0.1, "Specific Run", pos = 2, col = "red")

# Reset plot layout
par(mfrow = c(1, 1))

