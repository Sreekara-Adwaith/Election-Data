#Importing all the libraries I think we need to have

library(tidyr)
library(dplyr)
library(readr)


#Importing the data

data_ge <- read_csv("Election Data/ge.all.20190729.csv")  # (Why is as_tibble not working?)


#Trying to look at the data a bit

View(data_ge)
summary(data_ge)


#CLEANING THE DATA
#Selecting the useful columns

clean_ge <- select(data_ge, State_Name, Assembly_No, Constituency_No, Votes, Margin)
View(clean_ge)

#Creating data frames for every assembly



assembly <- unique(clean_ge[,2])


for (i in assembly) {
  write.csv(filter(clean_ge, Assembly_No == i), paste0("C:/Users/Percy/Desktop/Research Internship/Clean Data/Assembly Wise/", i, ".csv"))
}


for (i in unique(data_ge[,2])) {
  filter(clean_ge, Assembly_No == i)
}

unique(data_ge[,2])


data_3 <- filter(clean_ge, Assembly_No == 3)
View(data_3)
write.csv(data_3, "C:/Users/Percy/Desktop/Research Internship/Clean Data/Assembly_3.csv")

data_4 <- filter(clean_ge, Assembly_No == 4)
View(data_4)
write.csv(data_4, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_4.csv")

data_5 <- filter(clean_ge, Assembly_No == 5)
View(data_5)
write.csv(data_5, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_5.csv")

data_6 <- filter(clean_ge, Assembly_No == 6)
View(data_6)
write.csv(data_6, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_7.csv")

data_7 <- filter(clean_ge, Assembly_No == 7)
View(data_7)
write.csv(data_7, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_7.csv")

data_8 <- filter(clean_ge, Assembly_No == 8)
View(data_8)
write.csv(data_8, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_8.csv")

data_9 <- filter(clean_ge, Assembly_No == 9)
View(data_9)
write.csv(data_9, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_9.csv")

data_10 <- filter(clean_ge, Assembly_No == 10)
View(data_10)
write.csv(data_10, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_10.csv")

data_11 <- filter(clean_ge, Assembly_No == 11)
View(data_11)
write.csv(data_11, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_11.csv")

data_12 <- filter(clean_ge, Assembly_No == 12)
View(data_12)
write.csv(data_12, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_12.csv")

data_13 <- filter(clean_ge, Assembly_No == 13)
View(data_13)
write.csv(data_13, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_13.csv")

data_14 <- filter(clean_ge, Assembly_No == 14)
View(data_14)
write.csv(data_14, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_14.csv")

data_15 <- filter(clean_ge, Assembly_No == 15)
View(data_15)
write.csv(data_15, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_15.csv")

data_16 <- filter(clean_ge, Assembly_No == 16)
View(data_16)
write.csv(data_16, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_16.csv")

data_17 <- filter(clean_ge, Assembly_No == 17)
View(data_17)
write.csv(data_17, "C:/Users/Percy/Desktop/Research Internship/Clean Data//Assembly_17.csv")


#Trying to separate

data_3.1 <- select(data_3, State_Name, Constituency_No, Margin)
View(data_3.1)

long <- pivot_wider(data_3.1, id_cols = State_Name, names_from = Constituency_No, values_from = Margin)
View(long)

ap <- long %>%
  subset(State_Name == "Andhra_Pradesh")

View(ap)


write.csv(ap, "C:/Users/Percy/Desktop/Research Internship/Clean Data//AP.csv")



apk <- mutate(ap, grouped_id = row_number())
thing <- pivot_wider(apk, names_from = Constituency_No, values_from = Margin)

new_data = as.data.frame(lapply(thing, na.omit, nrow = 1))

na.omit(thing)

View(thing)




"""



mydata <- filter(data_3.1, Margin != 0, )

View(mydata)

group_by(mydata, Margin) 

View(mydata)

mydata1 <- mutate(mydata, grouped_id = row_number())

View(mydata1)

mydata2 <- mydata1 %>%
  spread(State_Name, Margin) %>%
  select(-grouped_id)
  
View(mydata2)

new_data = as.data.frame(lapply(mydata2, na.omit))

View(new_data)
"""




"""

## csv
for (i in seq_along(users)) {
  write.csv(flw[[i]], file = paste0(users[i], "-followers.csv"), row.names = FALSE)
  cat("*")
}

## better version of csv
if (!"readr" %in% installed.packages()) {
  install.packages("readr")
}
for (i in seq_along(users)) {
  readr::write_csv(flw[[i]], path = paste0(users[i], "-followers.csv"))
  cat("*")
}

## as R data object (this is usual my preference)
for (i in seq_along(users)) {
  saveRDS(flw[[i]], file = paste0(users[i], "-followers.csv"))
  cat("*")
}

"""


