##Economic Factors Determining Hog Litter Rates##
#################################################
library(readxl)
library(writexl)
library(urca)
library(vars)
library(mFilter)
library(forecast)
library(tidyverse)
library(tseries)
library(plm)
library(AER)


lr_df = read_xlsx("lr_df.xlsx")

#Exploratory Analysis#
######################

#Summary Statistics
summary(lr_df)

lr_df %>%
  summarise(sd_lr = sd(litter_rate),
            sd_pwm = sd(pwm),
            sd_wean_price = sd(wean_price),
            sd_fpr = sd(feed_price_ratio),
            sd_UE = sd(Unemployment))

#Litter Rate Over Time
fmt = "%Y-Q%q"
lr_df$Date = as.yearqtr(lr_df$Date, format = fmt)

lr_df %>%
  ggplot(aes(Date, litter_rate)) + geom_line(color="red") +
  xlab("Year") + ylab("Litter Rate") +
  scale_x_yearqtr(breaks = seq(from = min(lr_df$Date), to = max(lr_df$Date)+1, by = 4), format = fmt, ) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"),
        axis.text.x = element_text(size=10, face="bold", color = "black"),
        axis.text.y = element_text(size=10, face="bold", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank())

#Mortality Rate Over Time
lr_df %>%
  ggplot(aes(Date, pwm)) + geom_line(color="red") +
  xlab("Year") + ylab("Mortality Rate") +
  scale_x_yearqtr(breaks = seq(from = min(lr_df$Date), to = max(lr_df$Date)+1, by = 4), format = fmt, ) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"),
        axis.text.x = element_text(size=10, face="bold", color = "black"),
        axis.text.y = element_text(size=10, face="bold", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank())

#Wean Price Over Time
lr_df %>%
  ggplot(aes(Date, wean_price)) + geom_line(color="red") +
  xlab("Year") + ylab("Wean Price") +
  scale_x_yearqtr(breaks = seq(from = min(lr_df$Date), to = max(lr_df$Date)+1, by = 4), format = fmt, ) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"),
        axis.text.x = element_text(size=10, face="bold", color = "black"),
        axis.text.y = element_text(size=10, face="bold", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank())

#Feed Price Ratio
lr_df %>%
  ggplot(aes(Date, feed_price_ratio)) + geom_line(color="red") +
  xlab("Year") + ylab("Feed Price Ratio") +
  scale_x_yearqtr(breaks = seq(from = min(lr_df$Date), to = max(lr_df$Date)+1, by = 4), format = fmt, ) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"),
        axis.text.x = element_text(size=10, face="bold", color = "black"),
        axis.text.y = element_text(size=10, face="bold", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank())

#Averages by Quarter
lr_df %>% 
  select(Quarter,litter_rate,pwm, wean_price, feed_price_ratio, Unemployment) %>%
  group_by(Quarter) %>%
  summarise(lr_mean = format(round(mean(litter_rate),3),3), 
            pwm_mean = format(round(mean(pwm),4),4), 
            wp_mean = format(round(mean(wean_price),4),4), 
            fpr_mean = format(round(mean(feed_price_ratio),4),4),
            ue_mean = format(round(mean(Unemployment),4),4))

#Testing#
#########

#ADF Test for Quarterly Variables
adf.test(lr_df$litter_rate)
kpss.test(lr_df$litter_rate, null="Trend")
plot(lr_df$Trend[2:80],diff(lr_df$litter_rate, lag = 1, differences = 1))

adf.test(lr_df$pwm)
adf.test(lr_df$wean_price)
adf.test(lr_df$feed_price_ratio)
adf.test(lr_df$Unemployment)
adf.test(diff(lr_df$Unemployment, lag= 1, differences = 1))

#ADF Test for Residuals 
formula = litter_rate ~ lagged_litter_rate + pwm + wean_price + feed_price_ratio + Unemployment + Trend + Quarter
model = lm(formula, lr_df)
summary(model)

adf.test(model$residuals, k = 1)
adf.test(model$residuals, k = 2)
adf.test(model$residuals, k = 3)
adf.test(model$residuals, k = 4)

#Hausman Specification Test Temp/Drought Index (ABS(MAX-AVG, MIN-AVG))
iv_model = ivreg(litter_rate ~ lagged_litter_rate + pwm + wean_price + feed_price_ratio + Unemployment + Trend + Quarter|lagged_litter_rate + feed_price_ratio + Unemployment + Trend + Quarter + exports + hot_cold,
                 data = lr_df)

summary(iv_model, diagnostics = TRUE)

#Regression Analysis#
#####################
formula1 = log(litter_rate) ~ log(lagged_litter_rate) + pwm + wean_price + log(feed_price_ratio) + Unemployment + Trend + Quarter
model1 = lm(formula1, lr_df)
summary(model1)


iv_model = ivreg(log(litter_rate) ~ log(lagged_litter_rate) + pwm + wean_price + log(feed_price_ratio) + Unemployment + Trend + Quarter|log(lagged_litter_rate) + log(feed_price_ratio) + Unemployment + Trend + Quarter + log(exports) + hc_percent,
                 data = lr_df)

summary(iv_model, diagnostics = TRUE)

#Percent of growth due to trend
(80*0.002)/(log(11.670000)-log(8.870000))
(80*0.0027)/(log(11.670000)-log(8.870000))

#Auto Regression#
#################

#Vector Auto Regression#
########################

#Persistence
acf(lr_df$litter_rate)
pacf(lr_df$litter_rate)

acf(lr_df$pwm)
pacf(lr_df$pwm)

acf(lr_df$wean_price)
pacf(lr_df$wean_price)

acf(lr_df$feed_price_ratio)
pacf(lr_df$feed_price_ratio)

acf(lr_df$Unemployment)
pacf(lr_df$Unemployment)

#Finding Optimal Lags
vars = subset(lr_df,select = c(5,7:10))

lagselect = VARselect(vars, lag.max = 4, type = "both")
lagselect$selection

#Building VAR model
var_model = VAR(vars, p = 4, type = "both", season = 4)
summary(var_model$varresult$litter_rate)

#VAR Diagnostics

#Serial Correlation (serial correlation if p-value < 0.05)
serial1  = serial.test(var_model, lags.pt = 8, type = "PT.asymptotic")
serial1

#Heteroskedasticity (>0.05 no heteroskedasticity)
arch1 = arch.test(var_model, lags.multi = 8, multivariate.only = TRUE)
arch1

#Structural Breaks in Residuals
stability1 = stability(var_model, type = "OLS-CUSUM")
plot(stability1)

#VAR Forecasting
forecast = predict(var_model, n.ahead = 20, ci = .95)
fanchart(forecast)

forecast_df = lr_df %>%
  select(as.Date(Date), litter_rate)

forecast_df = forecast_df %>%
  mutate(fitted_vals = c(var_model$varresult$litter_rate$fitted.values,NA,NA,NA,NA),
         lower_bound = NA,
         upper_bound = NA)

forecast_df[nrow(forecast_df)+1:16,] = NA

forecast_df[77:96,3] = forecast$fcst$litter_rate[,1]
forecast_df[77:96,4] = forecast$fcst$litter_rate[,2]
forecast_df[77:96,5] = forecast$fcst$litter_rate[,3]

forecast_df = read_xlsx("forecast_df.xlsx")

forecast_df$Date = as.yearqtr(forecast_df$Date, format = fmt)

forecast_df %>%
  ggplot(aes(x = Date)) + 
  geom_line(aes(y = litter_rate, color="Actual")) +
  geom_line(aes(y = fitted_vals, color="Predicted")) +
  geom_ribbon(aes(ymin = lower_bound,ymax = upper_bound, fill="95% Confidence Interval"), alpha=0.2) +
  scale_color_manual(values=c("red", "blue")) +
  labs(color = "") +
  guides(fill = guide_legend(title = "")) +
  xlab("Year") + ylab("Litter Rate") +
  scale_x_yearqtr(breaks = seq(from = min(forecast_df$Date), to = max(forecast_df$Date), by = 2), format = fmt, ) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=12, face="bold", color = "black"),
        axis.title.y = element_text(size=12, face="bold", color = "black"),
        axis.text.x = element_text(size=10, face="bold", color = "black", angle = 90),
        axis.text.y = element_text(size=10, face="bold", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position='bottom',
        panel.background = element_blank())