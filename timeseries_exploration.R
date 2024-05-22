library(tidyverse)
library(ggplot2)
library(forecast)
library(stats)


main_filled_df <- read.csv('C:/Users/boypa/OneDrive - The University of Auckland/Project/Data/main_filled_df.csv')

#splitting
main_filled_df$Date <- as.Date(main_filled_df$Date)
main_filled_df$Date <- as_datetime(main_filled_df$datetime)
main_filled_train_df <- main_filled_df %>% filter(Date< as.Date("2023-01-01"))
main_filled_test_df <- main_filled_df %>% filter(Date>= as.Date("2023-01-01"))

#plot-daily scale
tail(main_filled_train_df, 500) %>% ggplot(aes(x=Index,y=Load)) + geom_line() + 
  labs(title = 'Demand Chart', x = 'Index', y = 'Load-MW')
#plot-weekly scale
tail(main_filled_train_df, 5000) %>% ggplot(aes(x=Index,y=Load)) + geom_line() + 
  labs(title = 'Demand Chart', x = 'Index', y = 'Load-MW')


#nonseasonal
a_acf <- Acf(main_filled_train_df$Load, lag.max = 3360, plot = FALSE)
a_pacf <- Pacf(main_filled_train_df$Load, lag.max = 336, plot = FALSE)
a_acf_df <- data.frame(Lag = 1:(length(a_acf$acf)-1), ACF = a_acf$acf[2:length(a_acf$acf)])
a_pacf_df <- data.frame(Lag = 1:(length(a_pacf$acf)-1), ACF = a_pacf$acf[2:length(a_pacf$acf)])

#plot acf
ggplot(a_acf_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Autocorrelation Function (ACF)",
       x = "Lag",
       y = "Autocorrelation") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

#plot pacf
ggplot(a_pacf_df, aes(x = Lag, y = ACF, ylim(-0.25,0.25))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Partial Autocorrelation Function (PACF)",
       x = "Lag",
       y = "Partial Autocorrelation") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")




#----differencing 1, lag =0
diff1 <- main_filled_train_df$Load %>% diff(differences = 1)
diff1_df <- data.frame(Index=1:length(diff1), Values = diff1)
ggplot(diff1_df, aes(x=Index, y=Values)) + geom_line() + labs(title = 'Differencing = 1, Lags = 0')


#----differencing 1, lag =48
diff1_l48 <- main_filled_train_df$Load %>% diff(differences = 1, lag=48)
diff1_l48_df <- data.frame(Index=1:length(diff1_l48), Values = diff1_l48)
ggplot(diff1_l48_df, aes(x=Index, y=Values)) + geom_line() + labs(title = 'Differencing = 1, Lags = 48')

#----differencing 2, lag =48
diff2_l48 <- main_filled_train_df$Load %>% diff(differences = 2, lag=48)
diff2_l48_df <- data.frame(Index=1:length(diff2_l48), Values = diff2_l48)
ggplot(diff2_l48_df, aes(x=Index, y=Values)) + geom_line() + labs(title = 'Differencing = 1, Lags = 48')


acf_d1_s48 <- Acf(diff1_l48, lag.max = 3360, plot = FALSE)
pacf_d1_s48 <- Pacf(diff1_l48, lag.max = 336, plot = FALSE)
acf_d1_s48_df <- data.frame(Lag = 1:(length(acf_d1_s48$acf)-1), ACF = acf_d1_s48$acf[2:length(acf_d1_s48$acf)])
pacf_d1_s48_df <- data.frame(Lag = 1:(length(pacf_d1_s48$acf)-1), ACF = pacf_d1_s48$acf[2:length(pacf_d1_s48$acf)])


#plot pacf
ggplot(pacf_d1_s48_df, aes(x = Lag, y = ACF, ylim(-0.25,0.25))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Partial Autocorrelation Function (PACF) D=1, S=48",
       x = "Lag",
       y = "Partial Autocorrelation") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

#plot pacf
ggplot(acf_d1_s48_df, aes(x = Lag, y = ACF, ylim(1,-0.5))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = " Autocorrelation Function (ACF) D=1, S=48",
       x = "Lag",
       y = " Autocorrelation") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")


