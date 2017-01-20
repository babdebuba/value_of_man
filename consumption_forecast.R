library(BMR)
library(bvarsv)

set.seed(34)

load("ludvigson_monthly_quarterly.RData")
load("oos.RData")

model_bvarm <- BVARM(ludvigson_monthly_quarterly[, -c(1, 11)], 
  irf.periods = 1)
density_forecast_bvarm <- forecast(model_bvarm,periods=1,
  shocks=F,plot=F, percentiles=c(.05,.50,.95),backdata=1,
  save=F)$Forecasts

i <- 1
plot(density(density_forecast_bvarm[1, i, ]))
abline(v = oos[i])

save(density_forecast_bvarm, 
  file = "density_forecast_bvarm.RData")

write.csv2(data.frame(
  consumption = density_forecast_bvarm[1, 1, ],
  asset_wealth = density_forecast_bvarm[1, 2, ],
  cay = density_forecast_bvarm[1, 3, ]),
  file = "density_forecast_bvarm.csv")
write.csv2(ludvigson_monthly_quarterly, 
  file = "ludvigson_monthly_quarterly.csv")
write.csv2(oos, file = "oos.csv")
