getwd()
setwd( "C:/Users/aname/OneDrive/Documents/KU Leuven/Magisterij/SEMESTER 2/ALIM/Assignment")

install.packages("tidyverse")
library(tidyverse)
library(data.table)

# uploading the data
Slovenia_unisex = as.data.table(read.table("life_table.txt",dec=".",header=TRUE))
Slovenia_males = as.data.table(read.table("males_life_table.txt",dec=".",header=TRUE))
Slovenia_females = as.data.table(read.table("females_life_table.txt",dec=".",header=TRUE))

# making all the age variables numeric
Slovenia_unisex$Age <- as.numeric(Slovenia_unisex$Age)
setnafill(Slovenia_unisex,fill=110)
Slovenia_males$Age <- as.numeric(Slovenia_males$Age)
setnafill(Slovenia_males,fill=110)
Slovenia_females$Age <- as.numeric(Slovenia_females$Age)
setnafill(Slovenia_females,fill=110)


# extracting only data from the year 2019
Slovenia_2019 = as.data.frame(Slovenia_unisex[Year == "2019",])
Slovenia_2019_males = as.data.frame(Slovenia_males[Year == "2019",])
Slovenia_2019_females = Slovenia_females[Year == "2019",]

# visualizing qx
library(ggplot2)
KULbg <- "#116E8A"

g_unisex <- ggplot(Slovenia_2019, aes(Age, log(qx))) + 
  geom_point(col = KULbg) + 
  geom_line(stat = "identity", col = KULbg) + 
  theme_bw() +
  ggtitle("Slovenia - unisex, 2019") + 
  labs(y = bquote(ln(q[x])))

g_unisex

g_male <- ggplot(Slovenia_2019_males, aes(Age, log(qx))) + 
  geom_point(col = KULbg) + 
  geom_line(stat = "identity", col = KULbg) + 
  theme_bw() +
  ggtitle("Slovenia - males, 2019") + 
  labs(y = bquote(ln(q[x])))

g_male

g_females <- 
  ggplot(Slovenia_2019_females, aes(Age, log(qx))) + 
  geom_point(col = KULbg) +
  geom_line(stat = "identity", col = KULbg, group=TRUE) + 
  theme_bw() +
  ggtitle("Slovenia - females, 2019") + 
  labs(y = bquote(ln(q[x])))

g_females


