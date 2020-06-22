# Load function
Rcpp::sourceCpp('core_github.cpp')
library(Metrics);
library(ggplot2);


# Input data
# US cumulative confirmed cases between 01/22 and 05/18, 2020
cumulative_cases <- c(1, 1, 2, 2, 5, 5, 5, 5, 5, 7, 8, 8, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 15, 15, 15, 51, 51, 57, 58, 60, 68, 74, 98, 118, 149, 217, 262, 402, 518, 583, 959, 1281, 1663, 2179, 2727, 3499, 4632, 6421, 7783, 13747, 19273, 25600, 33276, 43843, 53736, 65778, 83836, 101657, 121465, 140909, 161831, 188172, 213242, 243622, 275367, 308650, 336802, 366317, 397121, 428654, 462780, 496535, 526396, 555313, 580619, 607670, 636350, 667592, 699706, 732197, 758809, 784326, 811865, 840351, 869170, 905358, 938154, 965785, 988197, 1012582, 1039909, 1069424, 1103461, 1132539, 1158040, 1180375, 1204351, 1229331, 1257023, 1283929, 1309550, 1329260, 1347881, 1369376, 1390406, 1417774, 1442824, 1467820, 1486757, 1508308);
days = length(cumulative_cases)
names(cumulative_cases) <- as.Date("2020-01-22") + 0:(days - 1);

# US estimated population
N <- 328200000;

# Input settings
days_interval <- 30; # Incremental days intervals
days_predict <- 7; # Number of forecasting days
save <- FALSE; # Save detailed MCMC outputs

error_metric = function(data_true, data_predict) {
  mse(data_true, data_predict)
}
# ================================================================================
# ================================================================================
# ================================================================================



error_mat = data.frame(row.names = c("logistic_growth", "generalized_logistic_growth", "Richards_growth", "generalized_Richards_growth", "Bertalanffy_growth", "Gompertz_growth"))

for (i in 1: ((days - days_predict) %/%  days_interval)) {
  error = rep(NA, 6)
  data_train = cumulative_cases[1 : (i * days_interval)]
  #print(data_train)
  data_true = cumulative_cases[(i * days_interval + 1) : (i * days_interval + days_predict)]
  #print(data_true)
  last_day = names(cumulative_cases[i * days_interval + days_predict])
  #print(last_day)
  
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
  
  error_mat$col = error
  names(error_mat)[i] = last_day
}


print(error_mat)

  











