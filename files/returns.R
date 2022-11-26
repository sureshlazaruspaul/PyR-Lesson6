library("data.table")
library("tidyverse")
library("lubridate")
library("sqldf")
library("broom")
library("tidyquant")



#-------------------------------------------------------------------------------
# path to files ...
#-------------------------------------------------------------------------------

# Github path to data
gitpath <- "https://raw.githubusercontent.com/sureshlazaruspaul/"

# Local path to files
dirpath <- "C:/data/stock_data/"







#-------------------------------------------------------------------------------
# stock data header file
# - contains all firms
# - contains industry codes, firm name, and identifiers
# - contains beginning and ending date of trading
# - create a column of ones(used for merging later on)
#-------------------------------------------------------------------------------

msfhdr <- fread(paste0(dirpath, "msfhdr.csv")) %>%
  select(c("permno", "hsiccd",
           "hcomnam", "begret", "endret")) %>%
  mutate(
    begret = as.Date(begret, "%d%b%Y"),
    endret = as.Date(endret, "%d%b%Y"),
    dummy = as.integer(1)
  )

head(msfhdr)
glimpse(msfhdr)







#-------------------------------------------------------------------------------
# monthly market return file ...
# - contains returns of S&P 500 firms (equally-weighted, value-weighted)
#-------------------------------------------------------------------------------

msp500 <- fread(paste0(dirpath, "msp500.csv")) %>%
  mutate(
    date = as.Date(caldt, "%d%b%Y")
  )

glimpse(msp500)

# get all trading dates from market return files... 
dates <- msp500 %>% select(date) %>%
  mutate(
    dummy = as.integer(1)
  )

glimpse(dates)








#-------------------------------------------------------------------------------
# get s&p 500 list ...
# - identifes which firms are in S&P
#-------------------------------------------------------------------------------

msp500list <- fread(paste0(dirpath, "msp500list.csv")) %>%
  mutate(
    begdt = as.Date(start, "%d%b%Y"),
    enddt = as.Date(ending, "%d%b%Y")
  ) %>%
  select(
    -start, -ending
  )

glimpse(msp500list)









#-------------------------------------------------------------------------------
# get monthly stock returns ...
# - gives stock returns (monthly) of all firms
#-------------------------------------------------------------------------------

msf <- fread(paste0(dirpath, "msf.csv")) %>%
  mutate(
    date = as.Date(date, "%d%b%Y")
  ) %>%
  select(
    permno, date, ret, retx
  )

summary(msf) # diagnose return file
glimpse(msf)

msf <- msf %>%
  filter(
    !is.na(ret) & ret > -0.99 &
      !is.na(retx) & retx > -0.99
  )

summary(msf) # diagnose return file
glimpse(msf)









#-------------------------------------------------------------------------------
# for the full list of firms, get all possible trading dates ...
#-------------------------------------------------------------------------------

stock_data1 <- msfhdr %>%
  # get unique firms
  distinct() %>%
  # get trading dates for each firm 
  left_join( # merge dates ...
    dates,
    by = "dummy"
  ) %>%
  select(-dummy)

summary(stock_data1) # diagnose file
glimpse(stock_data1)








#-------------------------------------------------------------------------------
# for all possible trading dates, see if there is returns ...
#-------------------------------------------------------------------------------

stock_data2 <- stock_data1 %>%
  # get return data
  left_join(
    msf,
    by = c("permno", "date")
  ) %>%
  # limit to trading trades
  filter(
    date >= begret & date <= endret
  ) %>%
  select(-begret, -endret)

summary(stock_data2) # diagnose file

#-------------------------------------------------------------------------------
# Question? 
# Return should be there, but missing. What to do with missing returns?
#-------------------------------------------------------------------------------

glimpse(stock_data2)









#-------------------------------------------------------------------------------
# restrict firms to S&P 500 firms only ...
# get s&p list data ...
#-------------------------------------------------------------------------------

stock_data3 <- stock_data2 %>%
  # when was the firm in the s&p list?
  left_join(
    msp500list,
    by = c("permno")
  )  %>%
  # restrict sample
  filter(
    # drop unmerged data (S&P 500 firms only)
    !is.na(begdt) &
      # keep only firms within date range
      (date >= begdt & date <= enddt) &
        # define a sample period, if any
        between(date, as.Date("1925-01-01"), 
                      as.Date("2020-12-31"))
  ) %>%
  select(-begdt, -enddt) %>%
  # what to do with missing returns? 
  # drop missing values (or)
  # filter(
  #  !is.na(ret) & ret >= -0.99 &
  #     !is.na(retx) & retx >= -0.99
  # ) %>%
  # what to do with missing returns? 
  # replace with ZERO
  mutate(
    ret = ifelse(is.na(ret), 0, ret),
    retx = ifelse(is.na(retx), 0, retx),
    month = as.integer(month(date)),
    year = as.integer(year(date))
  ) 

summary(stock_data3) # diagnose file
glimpse(stock_data3)










#-------------------------------------------------------------------------------
# DO ALL THE PREVIOUS STEPS IN ONE SHOT
#-------------------------------------------------------------------------------

stock_data <- msfhdr %>%
  # get unique firms
  distinct() %>%
  # get trading dates for each firm 
  left_join( # merge dates ...
    dates,
    by = "dummy"
  ) %>%
  select(-dummy) %>%
  # get return data
  left_join(
    msf,
    by = c("permno", "date")
  ) %>%
  # limit to trading trades
  filter(
    date >= begret & date <= endret
  ) %>%
  select(-begret, -endret) %>%
  # when was the firm in the s&p list?
  left_join(
    msp500list,
    by = c("permno")
  )  %>%
  # restrict sample
  filter(
    # drop unmerged data (S&P 500 firms only)
    !is.na(begdt) &
      # keep only firms within date range
      (date >= begdt & date <= enddt) &
      # define a sample period, if any
      between(date, as.Date("1925-01-01"), 
              as.Date("2020-12-31"))
  ) %>%
  select(-begdt, -enddt) %>%
  # what to do with missing returns? 
  # drop missing values (or)
  # filter(
  #  !is.na(ret) & ret >= -0.99 &
  #     !is.na(retx) & retx >= -0.99
  # ) %>%
  # what to do with missing returns? 
  # replace with ZERO
  mutate(
    ret = ifelse(is.na(ret), 0, ret),
    retx = ifelse(is.na(retx), 0, retx),
    month = as.integer(month(date)),
    year = as.integer(year(date))
  )

rm("dates", "msfhdr", "msp500", "msf", "msp500list")

summary(stock_data) # diagnose file
glimpse(stock_data)










#-------------------------------------------------------------------------------
# Industry classification
#-------------------------------------------------------------------------------
# http://mba.tuck.dartmouth.edu/pages/
#   faculty/ken.french/Data_Library
#-------------------------------------------------------------------------------

ffind <- fread(paste0(gitpath,
                      "fama-french-ind-class/main/ffind.csv")) %>%
  filter(ind_def == 5) %>%
  rename(ffclass5 = class) %>%
  select(ffclass5, sic_start, sic_end)

glimpse(ffind)










#-------------------------------------------------------------------------------
# get three-factors for the 3-factor-model 
#-------------------------------------------------------------------------------

three_factor <-
  fread(paste0(gitpath, 
               "fama-french-ind-class/main/3factors.csv")) %>%
  mutate(
    year = as.integer(round(date/100, digits = 0)),
    month = as.integer(date - (year*100)),
    mktpre = mktpre/100,
    smb = smb/100,
    hml = hml/100,
    rf = rf/100
  ) %>% select(-date)

glimpse(three_factor)










#-------------------------------------------------------------------------------
# SQL merge - merge by range ...
# - merge if firm's sic is between sic_start and sic_end values
#-------------------------------------------------------------------------------

final_data <- 
  sqldf("select * 
        from stock_data LEFT JOIN ffind on 
        (stock_data.hsiccd >= ffind.sic_start and 
            stock_data.hsiccd <= ffind.sic_end)",
        stringsAsFactors = FALSE) %>%
  # replace missing with ....
  mutate(
    ffclass5 = as.factor(ifelse(is.na(ffclass5), 5, ffclass5)),
    sic_start = ifelse(is.na(sic_start), -9999, sic_start),
    sic_end = ifelse(is.na(sic_end), -9999, sic_end)
  ) %>%
  # merge 3-factors to stock_data
  left_join(
    three_factor,
    by = c("year", "month")
  ) %>%
  select(-hsiccd, -sic_start, -sic_end)

rm("ffind", "three_factor")












#-------------------------------------------------------------------------------
# multiple linear regression
#-------------------------------------------------------------------------------

# sample: industry 1
# Consumer Durables, Nondurables, Wholesale, 
#   Retail, and Some Services (Laundries, Repair Shops)

fit1 <- lm(ret ~ mktpre + smb + hml,
           final_data[which(final_data$ffclass5 == 1),])

# show results
summary(fit1)

coefficients(fit1)           # model coefficients
confint(fit1, level=0.95)    # CIs for model parameters
vcov(fit1)                   # covariance matrix for model parameters




# sample: industry 2
# Manufacturing, Energy, and Utilities

fit2 <- lm(ret ~ mktpre + smb + hml,
           final_data[which(final_data$ffclass5 == 2),])

# show results
summary(fit2)

coefficients(fit2)           # model coefficients
confint(fit2, level=0.95)    # CIs for model parameters
vcov(fit2)                   # covariance matrix for model parameters




# sample: industry 3
# Business Equipment, Telephone and Television Transmission

fit3 <- lm(ret ~ mktpre + smb + hml,
           final_data[which(final_data$ffclass5 == 3),])

# show results
summary(fit3)

coefficients(fit3)           # model coefficients
confint(fit3, level=0.95)    # CIs for model parameters
vcov(fit3)                   # covariance matrix for model parameters




# sample: industry 4
# Healthcare, Medical Equipment, and Drugs

fit4 <- lm(ret ~ mktpre + smb + hml,
           final_data[which(final_data$ffclass5 == 4),])

# show results
summary(fit4)

coefficients(fit4)           # model coefficients
confint(fit4, level=0.95)    # CIs for model parameters
vcov(fit4)                   # covariance matrix for model parameters




# sample: industry 5
# All Other industries

fit5 <- lm(ret ~ mktpre + smb + hml,
           final_data[which(final_data$ffclass5 == 5),])

# show results
summary(fit5)

coefficients(fit5)           # model coefficients
confint(fit5, level=0.95)    # CIs for model parameters
vcov(fit5)                   # covariance matrix for model parameters





# full sample

fit <- lm(ret ~ mktpre + smb + hml, final_data)

# show results
summary(fit)

coefficients(fit)           # model coefficients
confint(fit, level=0.95)    # CIs for model parameters
vcov(fit)                   # covariance matrix for model parameters









#-------------------------------------------------------------------------------
# ggplot plots: histogram 
#-------------------------------------------------------------------------------

# canvas
canvas <- final_data %>%
  ggplot(
    aes(x=ret)
  ) +
  theme_classic() +
  labs(
    title = "Distribution of Monthly Returns",
    subtitle = "Sample: S&P 500 firms only",
    caption = paste("From", min(final_data$date), 
                    "to", max(final_data$date)),
    x = "Monthly Returns",
    y = "Frequency"
  )

canvas + geom_histogram(bins = 1000)

# Clear the current plot
# dev.off()









# canvas
canvas <- final_data %>%
  ggplot() +
  theme_classic() +
  xlim(-1,1) +
  labs(
    title = "Distribution of Monthly Returns",
    subtitle = "Sample: S&P 500 firms only",
    caption = paste("From", min(final_data$date), 
                    "to", max(final_data$date)),
    x = "Monthly Returns",
    y = "Frequency"
  )

h1 <- canvas + geom_histogram(aes(x=ret),
                              bins = 100,
                              fill = "red")

h2 <- h1 + geom_histogram(aes(x=ret),
                          bins = 250,
                          fill = "green")

h3 <- h2 + geom_histogram(aes(x=ret),
                          bins = 1000,
                          fill = "blue"); h3

# Clear the current plot
# dev.off()









# what happens when you increase bins?

# Put graphs in 3 rows and 1 column
par(mfrow = c(3, 1))

# Histograms for each bin size
hist(final_data$ret, 
     breaks = 100,
     xlim = c(-1, 1),
     col = "red",
     main = "Bins/Breaks = 100",
     xlab = "")

hist(final_data$ret, 
     breaks = 250,
     xlim = c(-1, 1),
     col = "red",
     main = "Bins/Breaks = 250",
     xlab = "")

hist(final_data$ret, 
     breaks = 1000,
     xlim = c(-1, 1),
     col = "red",
     main = "Bins/Breaks = 1000",
     xlab = "")

# Restore graphic parameter
par(mfrow=c(1, 1))

# Clear the current plot
# dev.off()









# show bins effect on same plot using base R

# Histograms for each bin size
h1 <- hist(final_data$ret, 
     breaks = 100)

h2 <- hist(final_data$ret, 
     breaks = 250)

h3 <- hist(final_data$ret, 
     breaks = 1000)

plot(h1, col = "lightblue",
     xlim = c(-1, 1),
     main = "Distribution of monthly returns",
     xlab = "Monthly Returns",
     ylab = "Frequency"
     ) # Plot the 1st histogram

plot(h2, col = "lightgreen", add = TRUE,
     xlim = c(-1, 1)) # Add 2nd histogram

plot(h3, col = "red", add = TRUE,
     xlim = c(-1, 1)
     ) # Add 3rd histogram

# Clear the current plot
# dev.off()