rm(list=ls())
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

# Install SwedishPolls package if you don't have it!
# devtools::install_github("MansMeg/SwedishPolls", subdir = "RPackage")

# load necessary packages
my_packages <- c("dplyr", "ggplot2", "lubridate", "tidyr",
                 "SwedishPolls", "forecast")

for (i in 1:length(my_packages)){
  if(!require(my_packages[i], character.only = TRUE)){
    install.packages(my_packages[i])
  }
}

# get polling data
dirty_data <- SwedishPolls::get_polls()

# get mean dates
df <- dirty_data %>%
  mutate_each(funs(as.Date(., "%m-%d-%Y")), contains("collectPeriod")) %>%
  rowwise %>%
  mutate(date = mean.Date(c(collectPeriodFrom, collectPeriodTo)))

# drop columns i don't care about
cols_i_want <- c(seq(3,10),13,19)
df <- df[,cols_i_want]

# drop missing values
df <- na.omit(df)

# convert from percentages to decimal fractions
party_cols <- seq(1,8)
df[,party_cols] <- 0.01*df[,party_cols]

# convert from fractions to actual numbers of people
for (party in party_cols){
  df[,party] <- round(df[,party]*df["n"], digits = 0)
}

# sum over months
df$date <- floor_date(df$date, "month")

df <- aggregate(cbind(M,L,C,KD,S,V,MP,SD,n)~date,
                data=df,FUN=sum)

# convert back to percentages
for (party in seq(2,9)){
  df[,party] <- (df[,party]/df[,10])*100
}
df <- df[, !(names(df) %in% "n")]

# election dates
elect_dates <- data.frame(date =c("2006-09-06",
                                  "2010-09-19",
                                  "2014-09-14",
                                  "2018-09-09",
                                  "2022-09-11"))

# plotting
monthly_plot <- ggplot(data = df, aes(date)) +
  geom_line(aes(y = M, color = "M")) +
  geom_line(aes(y = L, color = "L")) +
  geom_line(aes(y = C, color = "C")) +
  geom_line(aes(y = KD, color = "KD")) +
  geom_line(aes(y = S, color = "S")) +
  geom_line(aes(y = V, color = "V")) +
  geom_line(aes(y = MP, color = "MP")) +
  geom_line(aes(y = SD, color = "SD")) +
  xlab("Date") + ylab("% Support") +
  scale_color_manual(name = "Parties",
                     values = c("M" = "#52BDEC",
                                "L" = "#006AB3",
                                "C" = "#009933",
                                "KD" = "#000077",
                                "S" = "#E8112d",
                                "V"= "#DA291C",
                                "MP" = "#83CF39",
                                "SD" = "#DDDD00")) +
  geom_vline(data = elect_dates, aes(xintercept = as.Date(date)),linetype = 3)

# ------- MOVING AVERAGES -------
window_size <- 3
df_MA <- df %>%
  mutate(M = rollmean(M, k = window_size, fill = NA, align = "right")) %>%
  mutate(L = rollmean(L, k = window_size, fill = NA, align = "right")) %>%
  mutate(C = rollmean(C, k = window_size, fill = NA, align = "right")) %>%
  mutate(KD = rollmean(KD, k = window_size, fill = NA, align = "right")) %>%
  mutate(S = rollmean(S, k = window_size, fill = NA, align = "right")) %>%
  mutate(V = rollmean(V, k = window_size, fill = NA, align = "right")) %>%
  mutate(MP = rollmean(MP, k = window_size, fill = NA, align = "right")) %>%
  mutate(SD = rollmean(SD, k = window_size, fill = NA, align = "right")) %>%
  drop_na()

MA_plot <- ggplot(data = df_MA, aes(date)) +
  geom_line(aes(y = M, color = "M")) +
  geom_line(aes(y = L, color = "L")) +
  geom_line(aes(y = C, color = "C")) +
  geom_line(aes(y = KD, color = "KD")) +
  geom_line(aes(y = S, color = "S")) +
  geom_line(aes(y = V, color = "V")) +
  geom_line(aes(y = MP, color = "MP")) +
  geom_line(aes(y = SD, color = "SD")) +
  xlab("Date") + ylab("% Support") +
  scale_color_manual(name = "Parties",
                     values = c("M" = "#52BDEC",
                                "L" = "#006AB3",
                                "C" = "#009933",
                                "KD" = "#000077",
                                "S" = "#E8112d",
                                "V"= "#DA291C",
                                "MP" = "#83CF39",
                                "SD" = "#DDDD00"))

# ------- TIME SERIES FORECASTING --------
# ----
# ----
# create new data frame to combine actual and forecast polling numbers
df2 <- df
df2[(nrow(df2)+1):(nrow(df2)+10),] <- NA
df2[(nrow(df)+1):nrow(df2),1] <- seq(as.Date(df[nrow(df),1]), # adding dates
                                     length=11, by='1 month')[2:11]

# using automatic arima selection from forecast package
for (i in seq(2,9)){
  df2[(nrow(df)+1):nrow(df2),i] <- as.numeric(forecast(auto.arima(df[,i]))$mean)
}

# ------- ALLOCATING SEATS --------
n_voters <- 6535271 # assume same as 2018
quotient <- 1.2 # St. Laguë quotient
n_seats <- 349 # no. of seats in parliament

# using most recent polling numbers
results <- df[nrow(df),2:9] # vector of forecast results
results <- results[colMeans(results) >= 4] # drop parties not beating threshold
n_parties <- length(results) # number of parties beating threshold

results[2,] <- round(results[1,]*n_voters/100, digits = 0) # no. of votes/party

results[3,] <- 0 # this is where we'll count number of seats

rownames(results) <- c("Percent", "Votes", "Seats")

comp_index <- data.frame(matrix(NA, 349, n_parties+1))
colnames(comp_index) <- c(colnames(results), "Winner")
comp_index[1,1:n_parties] <- results[2,1:n_parties]/quotient

winner <- ""

for (i in seq(n_seats)){
  comp_index[i,n_parties+1] <-
    colnames(comp_index)[max.col(comp_index[i,1:n_parties],
                                 ties.method = "first")] # determine winner
  comp_index[i+1,1:n_parties] <- comp_index[i,1:n_parties] # update next row
  winner <- comp_index[i,n_parties+1] # get name of winner
  results[3,winner] <- results[3,winner] + 1 # allocate new seat
  comp_index[i+1,winner] <- results[2,winner]/
    (2*results[3,winner]+1) # update comparative index
}

# using ARIMA forecast election results
fc_results <- df2 %>%
  filter(date == "2022-09-01") # get vector of forecast election results

fc_results <- fc_results[,2:9] # vector of forecast results
fc_results <- fc_results[colMeans(fc_results) >= 4] # drop parties under 4%
fc_n_parties <- length(fc_results) # number of parties beating threshold

fc_results[2,] <- round(fc_results[1,]*n_voters/100, digits = 0) # votes/party

fc_results[3,] <- 0 # this is where we'll count number of seats

rownames(fc_results) <- c("Percent", "Votes", "Seats")

fc_comp_index <- data.frame(matrix(NA, 349, fc_n_parties+1))
colnames(fc_comp_index) <- c(colnames(fc_results), "Winner")
fc_comp_index[1,1:n_parties] <- fc_results[2,1:fc_n_parties]/quotient

fc_winner <- ""

for (i in seq(n_seats)){
  fc_comp_index[i,n_parties+1] <-
    colnames(fc_comp_index)[max.col(fc_comp_index[i,1:fc_n_parties],
                                    ties.method = "first")] # determine winner
  fc_comp_index[i+1,1:fc_n_parties] <- fc_comp_index[i,1:fc_n_parties] # update next row
  fc_winner <- fc_comp_index[i,fc_n_parties+1] # get name of winner
  fc_results[3,fc_winner] <- fc_results[3,fc_winner] + 1 # allocate new seat
  fc_comp_index[i+1,fc_winner] <- fc_results[2,fc_winner]/
    (2*fc_results[3,fc_winner]+1) # update comparative index
}