require(magrittr)
#loading data
observation.data = read.csv("~/sso-physical-disorder-analysis/data/2015-real-ya-utf.csv", row.names = NULL, header = TRUE, sep = ";")

#cleaning data

#change classes
observation.data$pair.number %<>% as.factor
observation.data$floors.count %<>% as.character %>% as.numeric #impossible to factor -> numeric
observation.data$is.ya %<>% as.factor

#releveling
levels(observation.data$building.type) = c(NA, levels(observation.data$building.type)[16], levels(observation.data$building.type)[16], rep(levels(observation.data$building.type)[17], 2), rep(levels(observation.data$building.type)[13], 5), levels(observation.data$building.type)[15], levels(observation.data$building.type)[14], levels(observation.data$building.type)[13:17])
levels(observation.data$windows.scenery) = c(NA, NA, 0,0,0,1,1,0,1)
levels(observation.data$building.state) = c(NA, NA, rep(levels(observation.data$building.state)[18],2), rep(levels(observation.data$building.state)[19],4), rep(levels(observation.data$building.state)[17],3), rep(levels(observation.data$building.state)[16],1), rep(levels(observation.data$building.state)[15],2), levels(observation.data$building.state)[15:19], NA, rep(levels(observation.data$building.state)[18],2), rep(levels(observation.data$building.state)[19],2))
levels(observation.data$garbage.bins) = c(NA, NA, levels(observation.data$garbage.bins)[9], levels(observation.data$garbage.bins)[9], levels(observation.data$garbage.bins)[8], levels(observation.data$garbage.bins)[7], levels(observation.data$garbage.bins)[7:9], NA, NA, NA, NA)
levels(observation.data$graffiti) = c(NA, rep(levels(observation.data$graffiti)[12], 4), levels(observation.data$graffiti)[10], levels(observation.data$graffiti)[11], levels(observation.data$graffiti)[9], levels(observation.data$graffiti)[9:12], NA, NA, NA, NA)

#dealing with fucked up security
