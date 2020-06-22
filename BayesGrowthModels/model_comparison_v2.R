# Load function
Rcpp::sourceCpp('core_github.cpp')
library(Metrics);
library(ggplot2);
library(reshape2)

# Input data
data_raw = read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"), header = T)

data_tx = data_raw[data_raw$Province_State == "Texas", ]
counties = c("Collin","Dallas","Denton","Ellis","Hood","Hunt","Johnson","Kaufman","Parker","Rockwall","Somervell","Tarrant","Wise")
data_dfw = data_tx[which(data_tx$Admin2 %in% counties), -1:-11]
row.names(data_dfw) = counties
days = dim(data_dfw)[2]


# DFW counties estimated population
N_dfw = data.frame(Collin = 944350, Dallas = 2586552, Denton = 807047, Ellis = 168838, Hood = 56901, 
                   Hunt = 92152, Johnson = 163475, Kaufman = 118910, Parker = 129802, Rockwall = 93642, 
                   Somervell = 8743, Tarrant = 2019977, Wise = 64639)

# ================================================================================
# ================================================================================
# ================================================================================

# Input settings
# region: "US", "DFW"
# "Collin","Dallas","Denton","Ellis","Hood",
# "Hunt","Johnson","Kaufman","Parker","Rockwall",
# "Somervell","Tarrant","Wise"

region = "Denton"
days_initial = 130; # Incremental days intervals
days_predict = 7; # Number of forecasting days
file_name = "Denton.png"

# ================================================================================
# ================================================================================
# ================================================================================

if (region == "US") {
  cumulative_cases = as.numeric(colSums(data_raw[,-1:-11]))
  N = 328200000
} else if (region == "DFW") {
  cumulative_cases = as.numeric(colSums(data_dfw))
  N = sum(N_dfw)
} else {
  cumulative_cases = as.numeric(data_dfw[county, ])
  N = as.numeric(N_dfw[county])
}

names(cumulative_cases) <- as.Date("2020-01-22") + 0:(days - 1);

save <- FALSE; # Save detailed MCMC outputs

error_metric = function(data_true, data_predict) {
  #mse(data_true, data_predict)
  rmse(data_true, data_predict)
}

# ================================================================================

error_df = data.frame(row.names = c("logistic_growth", "generalized_logistic_growth", "Richards_growth", "generalized_Richards_growth", "Bertalanffy_growth", "Gompertz_growth"))



for (i in days_initial : (days - days_predict)) {
  error = rep(NA, 6)
  
  data_train = cumulative_cases[1 : i]
  data_true = cumulative_cases[(i + 1) : (i + days_predict)]
  last_day = names(cumulative_cases[i + days_predict])
  
  # logistic growth model
  res = growth_model_grm(data_train, 1.0, 1.0, N, days_predict, save);
  data_predict = res$C_pred_mean
  #print(data_predict)
  error[1] = error_metric(data_true, data_predict)
  
  # generalized logistic growth model
  res = growth_model_grm(data_train, -1.0, 1.0, N, days_predict, save);
  data_predict = res$C_pred_mean
  #print(data_predict)
  error[2] = error_metric(data_true, data_predict)
  
  # Richards growth model
  res = growth_model_grm(data_train, 1.0, -1.0, N, days_predict, save);
  data_predict = res$C_pred_mean
  #print(data_predict)
  error[3] = error_metric(data_true, data_predict)
  
  # generalized Richards growth model
  res = growth_model_grm(data_train, -1.0, -1.0, N, days_predict, save);
  data_predict = res$C_pred_mean
  #print(data_predict)
  error[4] = error_metric(data_true, data_predict)
  
  # Bertalanffy growth model
  res = growth_model_grm(data_train, 2/3, 1/3, N, days_predict, save);
  data_predict = res$C_pred_mean
  #print(data_predict)
  error[5] = error_metric(data_true, data_predict)
  
  # Gompertz growth model
  res = growth_model_2p(data_train, N, days_predict, 3, save);
  data_predict = res$C_pred_mean
  #print(data_predict)
  error[6] = error_metric(data_true, data_predict)
  
  print(error)
  
  error_df$col = error
  names(error_df)[i - days_initial + 1] = last_day
}

print(error_df)

df = as.data.frame(t(error_df))
df$date = row.names(df)
df_long <- melt(df, id="date", value.name = "rmse")

p = ggplot(data = df_long, aes(x = date, y = rmse, group = variable, colour = variable)) +
  geom_line() + geom_point()
ggsave(filename = file_name, plot = p, width = 12, height = 6)




