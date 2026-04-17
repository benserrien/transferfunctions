# evaluate-transferfunction.R

# plot predicties (met interval) & coverage en RMSE van de transferfunctie
evaluatie_tf <- function(data_testx) {
  summary <- data_testx %>%
    summarise(
      coverage = mean(between(pHH2O, lwr, upr)),
      RMSE = sqrt(mean((pHH2O - fit)^2))
    )
  data_testx %>%
    ggplot(aes(pHCa_obs, pHH2O)) +
    geom_point(color = "red") +
    geom_pointrange(aes(pHCa_obs, fit, ymin = lwr, ymax = upr), 
                    color = "blue", alpha = .1) +
    labs(x = "Meting met niet-referentiemethode (W)", 
         y = "Y (rood) &\nYhat [95% CI] (blauw)") +
    annotate("text",
             x = quantile(data_testx$pHCa_obs, .01),
             y = quantile(data_testx$pHH2O, .99),
             label = paste0("coverage = ",
                            sprintf("%.2f",summary$coverage))) +
    annotate("text",
             x = quantile(data_testx$pHCa_obs, .01),
             y = quantile(data_testx$pHH2O, .97),
             label = paste0("RMSE = ",
                            sprintf("%.3f",summary$RMSE)))
}