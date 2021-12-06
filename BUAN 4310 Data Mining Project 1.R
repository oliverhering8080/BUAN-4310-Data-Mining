# BUAN 4310 Project 1 

library(caret)
library(dplyr)

# Reading in Data and looking at first few rows--------------------
data <- read.csv('patents_3.csv', header = TRUE)

head(data)

str(data)



# Basic grouping-------------------------

# patents per year
patents_per_year <- table(data$grantyear)

barplot(patents_per_year, main = 'Patents per Year',
        ylab = 'Number of Patents')


# grouping by state ranked highest to lowest

patents_by_state <- table(data$ee_state)

patents_by_state %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

# grouping by city

patents_by_city <- table(data$ee_city)

patents_by_city %>%
  as.data.frame() %>%
  arrange(desc(Freq))

# grouping by company

patents_by_company <- table(data$ee_name)

top10_companies <- head(sort(patents_by_company, decreasing = TRUE), 10)

top10_df <- top10_companies %>% as.data.frame()
top10_df  




