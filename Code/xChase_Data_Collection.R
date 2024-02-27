# Load Libraries

library(baseballr)
library(tidyverse)
library(mgcv)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
library(ggthemes)
library(scoring)
# Attain game codes for all games 2008-2022
# Combine every pitch from the last 500 games of 2022-2021 into one dataset

game_info <- get_game_info_sup_petti()
combinedData <- mlb_pbp(game_pk = 718700)
for(i in 1:1000) {
  print(i)
  combinedData <- rbind(combinedData, mlb_pbp(game_pk = game_info$game_pk[i]), fill = TRUE)
}

# Filter data to only include pitches
# Create new variables
    # same_hand (whether the matchup was L/L or R/R), 1 for same hand 0 for different
    # Swing (0 for non-swing, 1 for swing)
# Select necessary variables for model


filtered_Data <- combinedData |>
  filter(isPitch == "TRUE") |>
  mutate(same_hand = if_else(matchup.batSide.code == matchup.pitchHand.code, 1, 0)) |>
  mutate(Swing =  if_else(details.description == "Ball", 0, 1)) |>
  mutate(count.balls.start = if_else(details.description %in% c('Ball', "Ball In Dirt", "Hit By Pitch"), count.balls.start - 1, count.balls.start)) |>
  mutate(count.strikes.start = if_else(details.description %in% c('Swinging Strike', "Foul", "Called Strike", "Swinging Strike (Blocked)", "Foul Tip"),
                                       count.strikes.start - 1, count.strikes.start)) |>
  mutate(count = ifelse(count.balls.start == 0 & count.strikes.start == 0, 1,
                        ifelse(count.balls.start == 0 & count.strikes.start == 1, 2,
                               ifelse(count.balls.start == 0 & count.strikes.start == 2, 3,
                                      ifelse(count.balls.start == 1 & count.strikes.start == 0, 4,
                                             ifelse(count.balls.start == 1 & count.strikes.start == 1, 5,
                                                    ifelse(count.balls.start == 1 & count.strikes.start == 2, 6,
                                                           ifelse(count.balls.start == 2 & count.strikes.start == 0, 7,
                                                                  ifelse(count.balls.start == 2 & count.strikes.start == 1, 8,
                                                                         ifelse(count.balls.start == 2 & count.strikes.start == 2, 9,
                                                                                ifelse(count.balls.start == 3 & count.strikes.start == 0, 10,
                                                                                       ifelse(count.balls.start == 3 & count.strikes.start == 1, 11,
                                                                                              ifelse(count.balls.start == 3 & count.strikes.start == 2, 12, NA))))))))))))) |>
  select(Swing, count, pitchData.startSpeed, pitchData.extension,
         pitchData.coordinates.pX, pitchData.coordinates.pZ, pitchData.breaks.spinRate,
         pitchData.breaks.spinDirection, pitchData.breaks.breakVertical,
         pitchData.breaks.breakHorizontal, same_hand, matchup.batSide.code, details.type.code, pitchData.strikeZoneBottom,
         pitchData.strikeZoneTop) 

# Create data frame of only Four Seam Fastballs
# Filter to only include pitches that are not in the "theoretical strike zone"
# Create height_above_zone to standardize vertical pitch position to player height

four_seam_Data <- filtered_Data |>
  filter(
    details.type.code == "FF",
    pitchData.coordinates.pX < -.83 | pitchData.coordinates.pX > .83 | pitchData.coordinates.pZ > pitchData.strikeZoneTop | pitchData.coordinates.pZ < pitchData.strikeZoneBottom) |>
  mutate(height_above_zone = pitchData.coordinates.pZ - pitchData.strikeZoneTop)

# Flip all X coordinates for lefty hitters

four_seam_Data <- four_seam_Data |>
  mutate(pitchData.coordinates.pX = if_else(matchup.batSide.code == "L", pitchData.coordinates.pX * -1, pitchData.coordinates.pX))

# Compute average chase rate by count

# Create train and test data
# Split data into 70% train and 30% test

set.seed(188)

number_rows <- nrow(four_seam_Data)
order_rows <- sample(number_rows)
four_seam_Data <- four_seam_Data[order_rows,]

train_FF_data <- four_seam_Data[1:33800] |>
  select(count,
         pitchData.startSpeed,
         pitchData.extension, pitchData.coordinates.pX,
         pitchData.breaks.spinRate, height_above_zone,
         pitchData.breaks.spinDirection, pitchData.breaks.breakVertical,
         pitchData.breaks.breakHorizontal, same_hand)
train_FF_labels <- four_seam_Data[1:33800] |>
  select(Swing)

test_FF_data <- four_seam_Data[33800:48286] |>
  select(count,
         pitchData.startSpeed,
         pitchData.extension, pitchData.coordinates.pX,
         pitchData.breaks.spinRate, height_above_zone,
         pitchData.breaks.spinDirection, pitchData.breaks.breakVertical,
         pitchData.breaks.breakHorizontal, same_hand, Swing)

# Create random forest model composed of 250 trees and a minimum node size of 50

FF_rfm = randomForest(as.factor(train_FF_labels$Swing) ~ ., data = train_FF_data, na.action = na.roughfix, ntree = 200,
min.node.size = 50)
FF_rfm
plot(FF_rfm)
varImpPlot(FF_rfm)
# Testing accuracy

test_FF_data$swing_pred_prob <- predict(FF_rfm, test_FF_data, type = "prob")[,2]

mean(test_FF_data$swing_pred_prob, na.rm = TRUE)
mean(test_FF_labels$Swing, na.rm = TRUE)


brier_score <- brierscore(test_FF_data$Swing ~ test_FF_data$swing_pred_prob, data = test_FF_data)
mean(brier_score, na.rm = TRUE)












# Create a plot of the strikezone with xChase of different zones / different counts

ggplot(
  data = test_FF_data,
  mapping = aes(x = test_FF_data$pitchData.coordinates.pX, y = test_FF_data$height_above_zone)
) +
  geom_point(mapping = aes(color = test_FF_data$swing_pred_prob)) +
  scale_colour_gradientn(colours=c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 1),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (0-0 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 2),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (0-1 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 3),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (0-2 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 4),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (1-0 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 5),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (1-1 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 6),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (1-2 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 7),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (2-0 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 8),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (2-1 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 9),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (2-2 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 10),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (3-0 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 11),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (3-1 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )

ggplot(
  data = subset(test_FF_data, count == 12),
  mapping = aes(x = pitchData.coordinates.pX, y = height_above_zone, )
) +
  geom_point(mapping = aes(color = swing_pred_prob)) +
  scale_colour_gradientn(colours =c("lightblue", "red")) +
  labs(
    title = "Expected Swing Rate based on location (3-2 count)",
    x = "Horizontal deviation from center (feet)",
    y = "Feet above zone",
    color = "Expected Swing Rate"
  )
# Count/Horizontal location Visualizations

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -.5 &
                test_FF_data$count.balls.start == 0 & test_FF_data$count.strikes.start == 0),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "0-0 count, pitch is less than .5 feet above or below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  ) +
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 1 & test_FF_data$count.strikes.start == 0),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "1-0 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 2 & test_FF_data$count.strikes.start == 0),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "2-0 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  count == 0),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "3-0 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 3 & test_FF_data$count.strikes.start == 1),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "3-1 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 3 & test_FF_data$count.strikes.start == 2),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "3-2 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 0 & test_FF_data$count.strikes.start == 1),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "0-1 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 0 & test_FF_data$count.strikes.start == 2),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "0-2 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 1 & test_FF_data$count.strikes.start == 1),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "1-1 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 1 & test_FF_data$count.strikes.start == 2),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "1-2 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 2 & test_FF_data$count.strikes.start == 2),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "2-2 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 2 & test_FF_data$count.strikes.start == 1),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "2-1 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)

ggplot(
  data = subset(test_FF_data, test_FF_data$height_above_zone < .5 & test_FF_data$height_above_zone > -1.875 &
                  test_FF_data$count.balls.start == 2 & test_FF_data$count.strikes.start == 2),
  mapping = aes(x = pitchData.coordinates.pX, y = swing_pred_prob)
) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Expected swing rate based on Horizontal location",
    subtitle = "2-2 count, pitch is less than .5 feet above or 1.875 feet below the zone",
    x = "Horizontal deviation from center (feet)",
    y = "Expected Swing Rate"
  )+
  ylim(0,1)