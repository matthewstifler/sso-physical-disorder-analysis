require(magrittr)
#loading data
observation.data = read.csv("~/sso-physical-disorder-analysis/data/2015-real-ya-utf.csv", row.names = NULL, header = TRUE, sep = ";")
#cleaning data
observation.data$pair.number %<>% as.factor
observation.data$floors.count %<>% as.character %>% as.numeric #impossible to factor -> numeric
levels(observation.data$building.type) = c(NA, levels(observation.data$building.type)[16], levels(observation.data$building.type)[16], rep(levels(observation.data$building.type)[17], 2), rep(levels(observation.data$building.type)[13], 5), levels(observation.data$building.type)[15], levels(observation.data$building.type)[14], levels(observation.data$building.type)[13:17])
levels(observation.data$windows.scenery) = c(NA, NA, 0,0,0,1,1,0,1)
