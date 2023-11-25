#Part 1: Param gathering
cat("Enter grammarian cr (e.g., 0.73): ");
cr1 <- as.numeric(readLines("stdin",n=1));
cat("Enter mythweaver cr (e.g., 0.67): ");
cr2 <- as.numeric(readLines("stdin",n=1));

cat("Enter starting words (e.g., 44100): ");
a <- as.numeric(readLines("stdin",n=1));
cat("Enter starting turns (e.g., 9): ");
b <- as.numeric(readLines("stdin",n=1));
cat("Simulating games...", "\n")


#count change to find total turns
find_unique_way_to_make_change <- function(amount, denom1, denom2) {
  # Check if the amount is divisible by denom1
  if (amount %% denom1 == 0) {
    return(c(amount / denom1, 0))  # Return the number of denom1 coins and 0 denom2 coins
  }
  
  # Check if the amount is divisible by denom2
  if (amount %% denom2 == 0) {
    return(c(0, amount / denom2))  # Return 0 denom1 coins and the number of denom2 coins
  }
  
  # Iterate through the possible number of denom1 coins
  for (num_denom1 in 0:(amount %/% denom1)) {
    # Calculate the remaining amount after using denom1 coins
    remaining_amount <- amount - num_denom1 * denom1
    
    # Check if the remaining amount is divisible by denom2
    if (remaining_amount %% denom2 == 0) {
      # Return the combination of denom1 and denom2 coins
      return(c(num_denom1, remaining_amount / denom2))
    }
  }
  
  # If no combination is found, return NULL
  return(NULL)
}

unique_way <- find_unique_way_to_make_change(a, 940, 3750)
mythweavers <- unique_way[2]
cat("Mythweavers so far:", mythweavers, "\n")
cat("\n")
#Part 2: Simulations
# Function to simulate the game with increased randomness and starting from specific words and turns. If input is (0,25), simulate from start
simulate_game_random_from_start <- function(starting_words, starting_turns) {
  # Stage 1 parameters with 4% randomness
  stage1_catch_prob <- runif(1, cr1-0.02, cr1+0.02)
  stage1_words_per_catch <- 940
  stage1_words_required <- 4000
  
  # Stage 2 parameters with 4% randomness
  stage2_a_encounter_prob <- runif(1, 0.58, 0.62)
  stage2_a_catch_prob <- runif(1, cr1-0.02, cr1+0.02)
  stage2_a_words_per_catch <- 940
  
  stage2_b_catch_prob <- runif(1, cr2-0.02, cr2+0.02)
  stage2_b_words_per_catch <- 3750
  stage2_b_additional_turns <- 2
  
  # Simulation
  total_words <- starting_words
  turns_left <- starting_turns
   if (!is.null(mythweavers)) {
    total_turns <- 25 + mythweavers * 2
  } else {
    total_turns <- 25
  }
  
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
  
  return(c(volume = total_words / 4000, gnawbel_prize = (total_words / 4000) * 4*54, CC = (total_words / 4000) + 4, total_turns = total_turns))
} #prizes replace 4 with 2 if normal condition, 1 if no TOC4
 
# Simulate the game 50000 times from start with increased randomness
simulations_random <- replicate(50000, simulate_game_random_from_start(0,25))

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

# Print the results for clean slate
cat("Clean Slate results:", "\n")
cat("\n")
cat("Median Volume :", median_volume_random, "\n")
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

# Simulate the game 50000 times starting from given point
simulations_random_start <- replicate(50000, simulate_game_random_from_start(a,b))

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

# Calculate mythweavers from the final total turns
final_mythweavers <- (total_turns_random_start - 25) / 2

# Calculate the median, 25th, and 75th percentiles of final mythweavers
median_final_mythweavers <- median(final_mythweavers)
percentiles_final_mythweavers <- quantile(final_mythweavers, c(0.25, 0.75))

# Print the results with increased randomness and starting from specific words and turns
cat("Current Run Results:", "\n")
cat("\n")
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
cat("Median total turns (starting from specific words and turns):", median_turns_random_start, "\n")
cat("25th Percentile of turns:", percentiles_turns_random_start[1], "\n")
cat("75th Percentile of total turns:", percentiles_turns_random_start[2], "\n")
cat("\n")
cat("Median Final Mythweavers:", median_final_mythweavers, "\n")
cat("25th Percentile of Final Mythweavers:", percentiles_final_mythweavers[1], "\n")
cat("75th Percentile of Final Mythweavers:", percentiles_final_mythweavers[2], "\n")
# Extract words from clean slate runs
words_clean_slate <- simulations_random[1, ] * 4000

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
abline(v = specific_run["total_turns"], col = "red", lty = 1)
#text(x = specific_run["total_turns"], y = max(table(total_turns_random)) + 0.1, "Specific Run", pos = 2, col = "red")
