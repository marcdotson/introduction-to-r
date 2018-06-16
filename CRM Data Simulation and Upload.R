# Simulation methods adapted from Chapman and Feit's "R for Marketing Research and Analytics."
library(tidyverse)
set.seed(42)

# Set the number of customers in the database.
ncust <- 1500

# STORE DATA
store_data <- data_frame(id=8001:(8000+ncust))
store_data$age <- rnorm(n=ncust, mean=38, sd=7)
store_data$gender <- factor(rbinom(n=ncust, size=1, prob=0.7), labels=c("Male", "Female"))
store_data$credit_score <- rnorm(n=ncust, mean=3*store_data$age+620, sd=50)
store_data$store_trans <- as.integer(rnbinom(ncust, size=0.2, mu=12 + 0.5 * store_data$age))
store_data$store_spend <- exp(rnorm(ncust, mean=3.5, sd=0.4)) * store_data$store_trans
summary(store_data); str(store_data)

write.csv(store_data,file="store_data.csv",row.names=FALSE)

# ONLINE DATA
online_data <- NULL
for (cust in 1:ncust) {
  for (year in 1:2) {
    for (month in 1:12) {
      week1_visit <- rnbinom(1, size=0.3, mu = 25 - 0.7 * (store_data$age[cust] - median(store_data$age)))
      week2_visit <- rnbinom(1, size=0.3, mu = 25 - 0.7 * (store_data$age[cust] - median(store_data$age)))
      week3_visit <- rnbinom(1, size=0.3, mu = 25 - 0.7 * (store_data$age[cust] - median(store_data$age)))
      week4_visit <- rnbinom(1, size=0.3, mu = 25 - 0.7 * (store_data$age[cust] - median(store_data$age)))
      online_trans <- rbinom(1, size=sum(week1_visit,week2_visit,week3_visit,week4_visit), prob=0.3)
      online_spend <- exp(rnorm(1, mean=3, sd=0.1)) * online_trans
      online_data <- rbind(online_data,c(store_data$id[cust],week1_visit,week2_visit,week3_visit,week4_visit,
                                         online_trans,online_spend))
    }
  }
}
online_data <- data.frame(online_data)
year_mo <- rep(c(c("2014_jan","2014_feb","2014_mar","2014_apr","2014_may","2014_jun",
                   "2014_jul","2014_aug","2014_sep","2014_oct","2014_nov","2014_dec"),
                 c("2015_jan","2015_feb","2015_mar","2015_apr","2015_may","2015_jun",
                   "2015_jul","2015_aug","2015_sep","2015_oct","2015_nov","2015_dec")),
               ncust)
online_data$year_mo <- year_mo
names(online_data) <- c("id","week1_visit","week2_visit","week3_visit",
                        "week4_visit","online_trans","online_spend","year_mo")
online_data$week4_visit[sample(1:nrow(online_data),size=10)] <- NA
summary(online_data); str(online_data)

write.csv(online_data,file="online_data.csv",row.names=FALSE)

# SATISFACTION DATA
sat_data <- data_frame(id=store_data$id)
sat_data$country <- c(rep("US",round(ncust/2)),rep("CA",ncust-round(ncust/2)))
sat_data$sat_overall <- rnorm(ncust, mean=ifelse(store_data$store_trans > mean(store_data$store_trans), 4, 3.1), sd=0.7)
sat_data$sat_overall[sat_data$sat_overall > 5] <- 5; sat_data$sat_overall[sat_data$sat_overall < 1] <- 1
sat_data$sat_service <- floor(sat_data$sat_overall + rnorm(ncust, mean=ifelse(sat_data$country=="US",0.2,0.5), sd=0.4))
sat_data$sat_selection <- floor(sat_data$sat_overall + rnorm(ncust, mean=ifelse(sat_data$country=="US",-0.5,-0.2), sd=0.6))
summary(sat_data)

write.csv(sat_data,file="sat_data.csv",row.names=FALSE)

