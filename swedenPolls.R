rm(list=ls())
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

# Install SwedishPolls package if you don't have it!
# devtools::install_github("MansMeg/SwedishPolls", subdir = "RPackage")

# load necessary packages
my_packages <- c("dplyr", "ggplot2", "lubridate", "tidyr",
                 "SwedishPolls", "forecast", "zoo", "reshape2")

for (i in 1:length(my_packages)){
  if(!require(my_packages[i], character.only = TRUE)){
    install.packages(my_packages[i])
  }
}

# set graphics theme
th <- theme_light()

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

# I want to get weighted averages for each month
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
elect_dates <- data.frame(date = c("2006-09-06",
                                  "2010-09-19",
                                  "2014-09-14",
                                  "2018-09-09",
                                  "2022-09-11"))

# plotting: monthly numbers
monthly_plot <- ggplot(df, aes(date)) +
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
  geom_vline(data = elect_dates, aes(xintercept = as.Date(date)),linetype = 3) +
  theme_light() +
  ggtitle("Party support over time")

# plotting: bar chart of this month's status 
bar_chart <- df[nrow(df),] %>%
  melt(id.vars = "date") %>%
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  xlab("Parties") + ylab("% Support") +
  scale_fill_manual(values = c("#52BDEC",
                               "#006AB3",
                               "#009933",
                               "#000077",
                               "#E8112d",
                               "#DA291C",
                               "#83CF39",
                               "#DDDD00")) +
  geom_hline(yintercept=4, linetype=3) +
  theme(legend.position="none")  +
  theme_light() +
  ggtitle("Public opinion today")

# ------- MOVING AVERAGES PLOT -------
window_size <- 3

MA_plot <- df %>%
  mutate(M = rollmean(M, k = window_size, fill = NA, align = "right")) %>%
  mutate(L = rollmean(L, k = window_size, fill = NA, align = "right")) %>%
  mutate(C = rollmean(C, k = window_size, fill = NA, align = "right")) %>%
  mutate(KD = rollmean(KD, k = window_size, fill = NA, align = "right")) %>%
  mutate(S = rollmean(S, k = window_size, fill = NA, align = "right")) %>%
  mutate(V = rollmean(V, k = window_size, fill = NA, align = "right")) %>%
  mutate(MP = rollmean(MP, k = window_size, fill = NA, align = "right")) %>%
  mutate(SD = rollmean(SD, k = window_size, fill = NA, align = "right")) %>%
  drop_na() %>%
  ggplot(aes(date)) +
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
  theme_light() +
  ggtitle("Moving average of party support")

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

# ------- TIME SERIES FORECASTING --------

# number of periods ahead to forecast
h = 5

# create new data frame to combine actual and forecast polling numbers
df2 <- df # copy original df
df2[(nrow(df2)+1):(nrow(df2)+h),] <- NA # add new rows for forecast numbers
df2[(nrow(df)+1):nrow(df2),1] <- seq(as.Date(df[nrow(df),1]), # adding dates
                                     length=h+1, by='1 month')[2:(h+1)]

# using automatic ARIMA selection from forecast package
for (i in seq(2,9)){
  df2[(nrow(df)+1):nrow(df2),i] <- as.numeric(forecast(auto.arima(df[,i]), h)$mean)
}

# using ARIMA forecast election results to forecast seat allocation
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

# plot forecasts (with the default 10 years ahead)
fcplot_M <- autoplot(forecast(auto.arima(df$M)))
fcplot_L <- autoplot(forecast(auto.arima(df$L)))
fcplot_C <- autoplot(forecast(auto.arima(df$C)))
fcplot_KD <- autoplot(forecast(auto.arima(df$KD)))
fcplot_S <- autoplot(forecast(auto.arima(df$S)))
fcplot_V <- autoplot(forecast(auto.arima(df$V)))
fcplot_MP <- autoplot(forecast(auto.arima(df$MP)))
fcplot_SD <- autoplot(forecast(auto.arima(df$SD)))

# ------- ALLIANCE V. RED-GREENS V. SD --------
df3 <- data.frame(matrix(NA, nrow(df), 4))
colnames(df3) <- c("date", "Alliance", "Red-greens", "SD")
df3$date <- df$date
df3$Alliance <- df$M + df$L + df$C + df$KD
df3$`Red-greens` <- df$S + df$V + df$MP
df3$SD <- df$SD

coalition_plot <- ggplot(df3, aes(date)) +
  geom_line(aes(y = Alliance, color = "Alliance")) +
  geom_line(aes(y = `Red-greens`, color = "Red-greens")) +
  geom_line(aes(y = SD, color = "SD")) +
  xlab("Date") + ylab("% Support") +
  scale_color_manual(name = "Coalition",
                     values = c("Alliance" = "#52BDEC",
                                "Red-greens" = "#E8112d",
                                "SD" = "#DDDD00")) +
  geom_vline(data = elect_dates, aes(xintercept = as.Date(date)),linetype = 3) +
  theme_light() +
  ggtitle("Support for traditional coalitions")

# ------- RIGHT-WING VS. LEFT-WING --------
df4 <- data.frame(matrix(NA, nrow(df), 4))
colnames(df4) <- c("date", "Right", "Left")
df4$date <- df$date
df4$Right <- df$M + df$L + df$SD + df$KD
df4$Left <- df$S + df$V + df$MP + df$C

new_landscape_plot <- ggplot(df4, aes(date)) +
  geom_line(aes(y = Right, color = "Right")) +
  geom_line(aes(y = Left, color = "Left")) +
  xlab("Date") + ylab("% Support") +
  scale_color_manual(name = "Parties",
                     values = c("Right" = "#000077",
                                "Left" = "#E8112d")) +
  geom_vline(data = elect_dates, aes(xintercept = as.Date(date)),linetype = 3) +
  theme_light() +
  ggtitle("Support for hypothetical new coalitions")

# How many seats would these coalitions get if the election were today?
right_seats <- 0
left_seats <- 0

right_parties <- c("M", "L", "KD", "SD")
left_parties <- c("C", "S", "V", "MP")

for (party in right_parties){
  if (party %in% colnames(results)){
    right_seats <- right_seats + results[3,party]
  }
}

for (party in left_parties){
  if (party %in% colnames(results)){
    left_seats <- left_seats + results[3,party]
  }
}

results[3,]
