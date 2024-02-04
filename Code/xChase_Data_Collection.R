# Load Libraries

library(baseballr)
library(tidyverse)
library(mgcv)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
library(ggthemes)

# Attain game codes for all games 2008-2022
# Combine every pitch from the last 250 games of 2022 into one dataset

game_info <- get_game_info_sup_petti()
combinedData <- mlb_pbp(game_pk = 718700)
for(i in 1:250) {
  print(i)
  combinedData <- rbind(combinedData, mlb_pbp(game_pk = game_info$game_pk[i]), fill = TRUE)
}

# Filter data to only include pitches
# Create new variables
    # same_hand (whether the matchup was L/L or R/R), 1 for same hand 0 for different
    # Swing (0 for non-swing, 1 for swing)
# Select necessary variables for model

combinedData <- combinedData |>
  filter(isPitch == "TRUE") |>
  mutate(same_hand = if_else(matchup.batSide.code == matchup.pitchHand.code, 1, 0)) |>
  mutate(Swing =  if_else(details.description == "Ball", 0, 1)) |>
  select(Swing, count.balls.start, count.strikes.start, pitchData.startSpeed, pitchData.extension,
         pitchData.coordinates.pX, pitchData.coordinates.pZ, pitchData.breaks.spinRate,
         pitchData.breaks.spinDirection, pitchData.breaks.breakVertical,
         pitchData.breaks.breakHorizontal, same_hand, matchup.batSide.code, details.type.code, pitchData.strikeZoneBottom,
         pitchData.strikeZoneTop)

# Create data frame of only Four Seam Fastballs
# Filter to only include pitches that are not in the "theoretical strike zone"
# Create height_above_zone to standardize vertical pitch position to player height

four_seam_Data <- combinedData |>
  filter(
    details.type.code == "FF",
    pitchData.coordinates.pX < -.83 | pitchData.coordinates.pX > .83 | pitchData.coordinates.pZ > pitchData.strikeZoneTop | pitchData.coordinates.pZ < pitchData.strikeZoneBottom,
    count.balls.start < 4, count.strikes.start < 3) |>
  mutate(height_above_zone = pitchData.coordinates.pZ - pitchData.strikeZoneTop)

# Flip all X coordinates for lefty hitters

for (n in seq_along(four_seam_Data$matchup.batSide.code)) {
  if (four_seam_Data$matchup.batSide.code[n] == "L") {
    four_seam_Data$pitchData.coordinates.pX[n] <- -1 * four_seam_Data$pitchData.coordinates.pX[n]
  }
}

# Create train and test data
# Split data into 70% train and 30% test

set.seed(188)

number_rows <- nrow(four_seam_Data)
order_rows <- sample(number_rows)
four_seam_Data <- four_seam_Data[order_rows,]

train_FF_data <- four_seam_Data[1:3370] |>
  select(count.balls.start, count.strikes.start,
         pitchData.startSpeed,
         pitchData.extension, pitchData.coordinates.pX,
         pitchData.breaks.spinRate, height_above_zone,
         pitchData.breaks.spinDirection, pitchData.breaks.breakVertical,
         pitchData.breaks.breakHorizontal, same_hand)
train_FF_labels <- four_seam_Data[1:3370] |>
  select(Swing)

test_FF_data <- four_seam_Data[3370:4821] |>
  select(count.balls.start, count.strikes.start,
         pitchData.startSpeed,
         pitchData.extension, pitchData.coordinates.pX,
         pitchData.breaks.spinRate, height_above_zone,
         pitchData.breaks.spinDirection, pitchData.breaks.breakVertical,
         pitchData.breaks.breakHorizontal, same_hand)

test_FF_labels <- four_seam_Data[3370:4821] |>
  select(Swing)

# Create random forest model composed of 250 trees and a minimum node size of 50

rfm = randomForest(as.factor(train_FF_labels$Swing) ~ ., data = train_FF_data, na.action = na.roughfix, ntree = 250,
min.node.size = 50)
rfm
plot(rfm)
varImpPlot(rfm)

# Testing accuracy

test_FF_data$swing_pred_prob <- predict(rfm, test_FF_data, type = "prob")[,2]

mean(test_FF_data$swing_pred_prob, na.rm = TRUE)
mean(test_FF_labels$Swing, na.rm = TRUE)

# Create a plot of the strikezone with xChase of different zones

ggplot(
  data = test_FF_data,
  mapping = aes(x = test_FF_data$pitchData.coordinates.pX, y = test_FF_data$height_above_zone)
) +
  geom_point(mapping = aes(color = test_FF_data$swing_pred_prob)) +
  scale_colour_gradientn(colours=c("lightblue", "blue")) +
  labs(
    title = "Expected Swing Rate based on location",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )