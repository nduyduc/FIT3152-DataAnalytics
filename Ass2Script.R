setwd("D:/MonashUni/C2000-SD/2nd Year/Sem2/FIT3152")
rm(list = ls())
WAUS <- read.csv("WAUS2019.csv")
L <- as.data.frame(c(1:49))
set.seed(28378210) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows
head(WAUS)

# question 1 - preprocessing data

WAUS.rain = WAUS[(WAUS$RainTomorrow == "Yes"),]
WAUS.norain = WAUS[(WAUS$RainTomorrow == "No"),]

str(WAUS)
attach(WAUS)
contrasts(WindGustDir) = contr.treatment(16)
contrasts(WindDir9am) = contr.treatment(16)
contrasts(WindDir3pm) = contr.treatment(16)
contrasts(RainToday) = contr.treatment(2)


# question 2 - split train test data

train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

# question 3 - classification models

