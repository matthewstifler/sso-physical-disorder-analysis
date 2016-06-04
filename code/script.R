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
observation.data$security %<>% as.character
observation.data$security[observation.data$security == "01.май"] = "1;5"
observation.data$security[observation.data$security == "02.май"] = "2;5"
sec.no = ifelse((str_count(observation.data$security, "u'no'") > 0) | (str_count(observation.data$security, "0") > 0), 1, 0)# no or 0
sec.guard = ifelse((str_count(observation.data$security, "u'guards'") > 0) | (str_count(observation.data$security, "1") > 0), 1, 0)
sec.fence = ifelse((str_count(observation.data$security, "u'fence'") > 0) | (str_count(observation.data$security, "2") > 0) | (str_count(observation.data$security, "3") > 0), 1, 0)
sec.cctv = ifelse((str_count(observation.data$security, "u'surveillance_facade'") > 0) | (str_count(observation.data$security, "4") > 0), 1, 0)

observation.data = cbind(observation.data, sec.no = as.factor(sec.no), sec.guard = sec.guard %>% as.factor, sec.fence = sec.fence %>% as.factor, sec.cctv = sec.cctv %>% as.factor)

#numeric vars
levels(observation.data$windows.broken.share)[c(1:2, 59, 65:81, 84:85, 88:89)] = NA
levels(observation.data$windows.broken.share)[c(21, 35:37, 43, 61, 64:65)] = NA
levels(observation.data$windows.broken.share)[c(2,8,11,15,18)] = c(0, 0.05, 0.1, 0.1,0.2)
levels(observation.data$windows.broken.share)[c(22:52)] = c(NA, 1, 0.1, 1, NA, NA, 0.15, NA, NA, 0.2, NA, NA, NA, 0.03, 0.33, NA, 0.4, NA, 0.05, NA, NA, 0.6, NA, 0.07, NA, NA, 0.9, 1, 1, 0.25, 0)
levels(observation.data$windows.broken.share)[c(17,19)] = c(0.5, 0.9)
observation.data$windows.broken.share %<>% as.character %>% as.numeric

levels(observation.data$windows.bars.share) = levels(observation.data$windows.bars.share) %>% str_replace_all(",", ".")
levels(observation.data$windows.bars.share)[c(1:7, 15, 31, 38:41, 46, 50, 60, 62, 68, 70:77, 78:88)] = c(NA, NA, rep(0,5), 0.05, 0.14, rep(NA, 4), NA, 0.2, 0.5, NA, NA, NA, 1, 0.1, 1, 1, 0.1, NA, 0.5, rep(NA, 6), 0.15, NA, NA, 0.33)
levels(observation.data$windows.bars.share)[c(53:62, 64, 66, 68:70, 72:79, 81:84, 86:96, 99, 102, 103, 105:107, 111, 114:126, 128:153, 155: 163, 166:170)] = NA
levels(observation.data$windows.bars.share)[c(53:72)] = c(0.2, 1, 0.22, 0.25, 0.3, 0.33, 0.05, 0.05, 0.5, 0.5, 0.6, 0.8, 0.5, 1, 0.9, 0.95, 1, 0, 0.017, 0.063)
observation.data$windows.bars.share %<>% as.character %>% as.numeric
