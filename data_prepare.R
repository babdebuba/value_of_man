library(lubridate)

# prepare the data
# ludvigson
ludvigson <- read.table(file = "ludvigson.csv", skip = 18)
ludvigson_rows_number <- dim(ludvigson)[1]
consumption <- ludvigson[-ludvigson_rows_number, 2]
asset_wealth <- ludvigson[-ludvigson_rows_number, 3]
cay <- ludvigson[-ludvigson_rows_number, 5]
ludvigson_time <- ymd("1953-01-01") %m+% months(0:182 * 3)
ludvigson <- data.frame(time = ludvigson_time, 
  consumption = consumption, asset_wealth = asset_wealth, 
  cay = cay)
# macro_quarterly
# names
macro_quarterly_names <- read.csv2(
  file = "hendry_quarterly.csv", stringsAsFactors = F)
macro_quarterly_names <- as.matrix(
  macro_quarterly_names[2, -1])
macro_quarterly_subset <- c(1, 21, 2, 6, 43, 42)
macro_quarterly_names <-
  macro_quarterly_names[macro_quarterly_subset]
# variables
macro_quarterly <- read.csv2(file = "hendry_quarterly.csv",
  stringsAsFactors = F, skip = 9, header = F)
macro_quarterly <- as.matrix(macro_quarterly[, -1])
macro_quarterly <- macro_quarterly[, macro_quarterly_subset]
macro_quarterly_time <- ymd("1959-01-01") %m+% 
  months(0:191 * 3)
colnames(macro_quarterly) <- macro_quarterly_names
# total
macro_quarterly <- data.frame(time = macro_quarterly_time, macro_quarterly)
macro_quarterly_oos <- apply(macro_quarterly[157:160, -1], 2,
  mean)
# macro_monthly
# names
macro_monthly_names <- read.csv2(
  file = "hendry_monthly.csv", stringsAsFactors = F)
macro_monthly_names <- as.matrix(
  macro_monthly_names[2, -1])
macro_monthly_subset <- 54
macro_monthly_names <-
  macro_monthly_names[macro_monthly_subset]
# variables
macro_monthly <- read.csv2(file = "hendry_monthly.csv",
  stringsAsFactors = F, skip = 10, header = F)
macro_monthly <- as.matrix(macro_monthly[, -1])
macro_monthly <- macro_monthly[, macro_monthly_subset]
macro_monthly_time <- ymd("1959-01-01") %m+% 
  months(0:575)
macro_monthly_oos <- mean(macro_monthly[469:480])
# total
macro_monthly <- data.frame(time = macro_monthly_time, macro_monthly)
colnames(macro_monthly)[2] <- macro_monthly_names

# average the data
# ludvigson
time_start <- grep(macro_monthly$time[1], 
  ludvigson$time) - 1
time_end_quarterly <- grep(ludvigson$time[dim(ludvigson)[1]], 
  macro_quarterly$time)
time_end_monthly <- grep(ludvigson$time[dim(ludvigson)[1]], 
  macro_monthly$time)
ludvigson <- ludvigson[-(1:time_start), ]
ludvigson_index <- NA * ludvigson[, -1]
for (i in 1:(dim(ludvigson)[1] / 4)) {
  ludvigson_index[(i - 1) * 4 + 1, ] <- apply(
    ludvigson[((i - 1) * 4 + 1):(i * 4), -1], 2, mean)
}
time <- ludvigson$time[!is.na(ludvigson_index[, 1])]
ludvigson_oos <- ludvigson[, -1]
ludvigson_oos <- apply(ludvigson_oos[157:159, ], 2, mean)
ludvigson <- ludvigson_index[!is.na(ludvigson_index[, 1]), ]
# macro_quarterly
macro_quarterly <- macro_quarterly[1:time_end_quarterly,]
macro_quarterly_index <- NA * macro_quarterly[, -1]
for (i in 1:(dim(macro_quarterly)[1] / 4)) {
  macro_quarterly_index[(i - 1) * 4 + 1, ] <- apply(
    macro_quarterly[((i - 1) * 4 + 1):(i * 4), -1], 2, mean)
}
macro_quarterly <- macro_quarterly_index[!is.na(
  macro_quarterly_index[, 1]), ]
# macro_monthly
macro_monthly <- macro_monthly[1:time_end_monthly,]
macro_monthly_index <- NA * macro_monthly[, -1]
for (i in 1:(length(macro_monthly_index) / 12)) {
  macro_monthly_index[(i - 1) * 12 + 1] <- mean(
    macro_monthly[((i - 1) * 12 + 1):(i * 12), -1])
}
macro_monthly <- macro_monthly_index[!is.na(
  macro_monthly_index)]
# total
ludvigson_monthly_quarterly <- data.frame(time = time,
  consumption = ludvigson[, 1], asset_wealth = ludvigson[, 2],
  cay = ludvigson[, 3], fedfunds = macro_monthly, 
  rgdp = macro_quarterly[, 1], pgdp = macro_quarterly[, 2],
  gdpinv = macro_quarterly[, 4], 
  hours_worked = macro_quarterly[, 5], 
  compens_hour = macro_quarterly[, 6],
  consumption_macro_set = macro_quarterly[, 3]
  )
save(ludvigson_monthly_quarterly, file = "ludvigson_monthly_quarterly.RData")
oos = c(ludvigson_oos, fedfunds = macro_monthly_oos, 
  macro_quarterly_oos[-3], macro_quarterly_oos[3])
save(oos, file = "oos.RData")
