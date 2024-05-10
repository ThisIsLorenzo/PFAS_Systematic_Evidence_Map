library(readr)
library(here)
alldata <- read_csv(here("data","alldata.csv"), skip = 0)
dim(alldata) #[1] 6122 43
