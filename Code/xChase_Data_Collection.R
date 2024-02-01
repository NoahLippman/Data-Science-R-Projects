library(baseballr)
library(tidyverse)

# Data Collection 
game_packs <- get_game_pks_mlb(date = '2023-09-01', level_ids = 1) |>
  select(game_pk)

game_info <- get_game_info_sup_petti()
combinedData <- mlb_pbp(game_pk = 718700)
for(i in 1:100) {
  print(i)
  combinedData <- rbind(combinedData, mlb_pbp(game_pk = game_info$game_pk[i]), fill = TRUE)
}

combinedData <- combinedData |>
  filter(isPitch == "TRUE") |>
  select(game_pk, details.event, count.balls.start, count.strikes.start, pitchData.strikeZoneTop,
         pitchData.strikeZoneBottom, matchup.batSide.code, matchup.pitchHand.code,
         details.type.code, pitchData.startSpeed, pitchData.extension,
         pitchData.coordinates.pX, pitchData.coordinates.pZ, pitchData.breaks.spinRate,
         pitchData.breaks.spinDirection, pitchData.breaks.breakVertical,
         pitchData.breaks.breakHorizontal)


four_seam_Data <- combinedData |>
  filter(
    details.type.code == "FF")