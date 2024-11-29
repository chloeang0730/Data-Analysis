install.packages("ggplot2")
install.packages("skimr")
install.packages("dplyr")
install.packages("treemapify")
library(ggplot2)
library(skimr)
library(dplyr)
library(cluster)
library(treemapify)

#-------------------------------------------
# Assignment 1 
#-------------------------------------------
rm(list = ls())
set.seed(32581343) # XXXXXXXX = your student ID
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase = cvbase[sample(nrow(cvbase), 40000), ] # 40000 rows
View(cvbase)
attach(cvbase)

#-------------------------------------------
# Question 1a : Description of data
#-------------------------------------------
# Check the datatypes of the columns
str(cvbase)

# Check occurrences of NA values in all columns
colSums(is.na(cvbase))

# Get a summary of the data
skim(cvbase)

# Get the unique values and number of unique values of the data
unique_value = function(col){
  uniq = unique(col)
  num = length(uniq)
  return(list(UniqueValues = uniq, NumberOfUniqueValues = num))
}
col_uniq_value = lapply(cvbase, unique_value)
col_uniq_value


#-------------------------------------------
# Question 1b : Pre-processing & cleaning data
#-------------------------------------------
# Step 1 : Replace employStatus and coronaClose with 0 
columns_to_replace = c("employstatus_1", "employstatus_2", "employstatus_3","employstatus_4",
                       "employstatus_5", "employstatus_6","employstatus_7","employstatus_8",
                       "employstatus_9","employstatus_10","coronaClose_1","coronaClose_2",
                       "coronaClose_3","coronaClose_4","coronaClose_5","coronaClose_6")

for (col in columns_to_replace) {
  cvbase[is.na(cvbase[[col]]), col] = 0
}

# Step 2 : Replace "" in coded_country as NA value
coded_country[coded_country == ""] = NA
unique(cvbase$coded_country)

# Step 3 : Drop rows with NA values
cvbase = na.omit(cvbase)

# Make sure no more NA values 
colSums(is.na(cvbase))

# Take out character type attribute 
cvbase_subset = cvbase %>% select(-rankOrdLife_1,-rankOrdLife_2,-rankOrdLife_3,-rankOrdLife_4,
                                  -rankOrdLife_5,-rankOrdLife_6,-coded_country)
matrix = cor(cvbase_subset)

# Create heat map of numerical attribute
heatmap(matrix)


# Check Occurrence of country
country_occurrence = as.data.frame(table(cvbase$coded_country))
View(country_occurrence)

# Factorize rankOrdLife 
# levels_description = c("A" = "Beauty", "B" = "Achievement", "C" = "Victory", "D" = "Friendship", "E" = "Love", "F" = "Empathy")
cvbase$rankOrdLife_1 = factor(cvbase$rankOrdLife_1)
cvbase$rankOrdLife_2 = factor(cvbase$rankOrdLife_2)
cvbase$rankOrdLife_3 = factor(cvbase$rankOrdLife_3)
cvbase$rankOrdLife_4 = factor(cvbase$rankOrdLife_4)
cvbase$rankOrdLife_5 = factor(cvbase$rankOrdLife_5)
cvbase$rankOrdLife_6 = factor(cvbase$rankOrdLife_6)

contrasts(cvbase$rankOrdLife_1) = contr.treatment(levels(cvbase$rankOrdLife_1))
contrasts(cvbase$rankOrdLife_2) = contr.treatment(levels(cvbase$rankOrdLife_2))
contrasts(cvbase$rankOrdLife_3) = contr.treatment(levels(cvbase$rankOrdLife_3))
contrasts(cvbase$rankOrdLife_4) = contr.treatment(levels(cvbase$rankOrdLife_4))
contrasts(cvbase$rankOrdLife_5) = contr.treatment(levels(cvbase$rankOrdLife_5))
contrasts(cvbase$rankOrdLife_6) = contr.treatment(levels(cvbase$rankOrdLife_6))

# Rank Order Life
rankOrdLife = c("rankOrdLife_1","rankOrdLife_2","rankOrdLife_3",
                "rankOrdLife_4","rankOrdLife_5","rankOrdLife_6")
? barplot
? table
for(i in rankOrdLife){
  occurrence = table(cvbase[[i]])
  print(occurrence)
  barplot(occurrence, xlab = "Categories", ylab = "Frequency",main = "'rankOrdLife' Distribution",sub = i, col= 'skyblue')
}

# Employment Status
Count = c(sum(cvbase$employstatus_1), sum(cvbase$employstatus_2),sum(cvbase$employstatus_3), sum(cvbase$employstatus_4),
          sum(cvbase$employstatus_5), sum(cvbase$employstatus_6),sum(cvbase$employstatus_7), sum(cvbase$employstatus_8),
          sum(cvbase$employstatus_9), sum(cvbase$employstatus_10)
)
employmentStatus = c("1-24 hours per week", "25-39 hours per week", "40+ hours per week",
                     "Not employed, looking", "Not employed, not looking", "Homemaker",
                     "Retired", "Disabled", "Student", "Volunteering")

employment_status_summary = data.frame(employmentStatus,Count)

ggplot(employment_status_summary, aes(x = employmentStatus, y = Count)) +
  geom_bar(stat = "identity",color = 'black', fill = 'skyblue') +
  labs(title = "Distribution of Employment Status", y = "Count", x= "Employment Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Education level
ggplot(cvbase, aes(x = edu)) + geom_bar(fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Education Level", x = "Education Level", y = "Frequency")

# Gender
ggplot(cvbase, aes(x = gender)) + geom_bar(fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Gender", x = "Gender", y = "Frequency")

# Age 
ggplot(cvbase, aes(x = age)) + geom_bar(fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Age", x = "ageGroup", y = "Frequency")


# Isolation online & offline
iso_df = cvbase[,c("isoFriends_inPerson","isoOthPpl_inPerson", 
                   "isoFriends_online", "isoOthPpl_online")]
iso_long = reshape2::melt(iso_df)
ggplot(iso_long, aes(x = value)) + 
  geom_histogram(bins = 8, fill = 'skyblue', color = "black") + 
  facet_wrap(~ variable, scales = "free_x") + 
  labs(title = "Distribution of Social Isolation Measures",x = "Score",y = "Count")

# Loneliness
lone_df = cvbase[,c("lone01","lone02","lone03")]
lone_long = reshape2::melt(lone_df)
ggplot(lone_long, aes(x = value)) + 
  geom_histogram(bins = 5, fill = 'skyblue', color = "black") + 
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Distribution of Social Loneliness Measures",x = "Score",y = "Count")

# Boredomness
bor_df = cvbase[,c("bor01","bor02","bor03")]
bor_long = reshape2::melt(bor_df)
ggplot(bor_long, aes(x = value)) + 
  geom_histogram(bins = 5, fill = 'skyblue', color = "black") + 
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Distribution of Social Boredomness Measures",x = "Score",y = "Count")

# Corona Personal Behavior
c19perBeh_df = cvbase[,c("c19perBeh01","c19perBeh02","c19perBeh03")]
c19perBeh_long = reshape2::melt(c19perBeh_df)
ggplot(c19perBeh_long, aes(x = value)) + 
  geom_histogram(bins = 5, fill = 'skyblue', color = "black") + 
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Distribution of Corona Personal Behavior",x = "Score",y = "Count")

# Corona Radical Action
c19RCA_df = cvbase[,c("c19RCA01","c19RCA02","c19RCA03")]
c19RCA_long = reshape2::melt(c19RCA_df)
ggplot(c19RCA_long, aes(x = value)) + 
  geom_histogram(bins = 5, fill = 'skyblue', color = "black") + 
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Distribution of Corona Radical Action",x = "Score",y = "Count")

# Corona ProSocial Behavior
c19ProSo_df = cvbase[,c("c19ProSo01","c19ProSo02","c19ProSo03")]
c19ProSo_long = reshape2::melt(c19ProSo_df)
ggplot(c19ProSo_long, aes(x = value)) + 
  geom_histogram(bins = 5, fill = 'skyblue', color = "black") + 
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Distribution of Corona ProSocial Behavior",x = "Score",y = "Count")

# Comprehensive distribution of numerical attributes
cvbase_subset = cvbase %>%
  select(-employstatus_1, -employstatus_2, -employstatus_3, -employstatus_4,
         -employstatus_5, -employstatus_6, -employstatus_7, -employstatus_8,
         -employstatus_9,-employstatus_10, -coronaClose_1,-coronaClose_2,
         -coronaClose_3,-coronaClose_4,-coronaClose_5,-coronaClose_6,
         -rankOrdLife_1,-rankOrdLife_2,-rankOrdLife_3,-rankOrdLife_4,
         -rankOrdLife_5,-rankOrdLife_6,-coded_country)
skim_df = skim(cvbase_subset)
skim_num_df = yank(skim_df, "numeric")
View(skim_num_df)

#---------------------------------------------
# Question 2 : Ukraine vs Other Countries
#--------------------------------------------- 
# Create other countries data set 
other = cvbase %>% filter(coded_country != "Ukraine")
other_subset = other %>% select(-employstatus_1, -employstatus_2, -employstatus_3, -employstatus_4,
                                    -employstatus_5, -employstatus_6, -employstatus_7, -employstatus_8, 
                                    -employstatus_9,-employstatus_10, -coronaClose_1,-coronaClose_2,
                                    -coronaClose_3,-coronaClose_4,-coronaClose_5,-coronaClose_6, 
                                    -rankOrdLife_1,-rankOrdLife_2,-rankOrdLife_3,-rankOrdLife_4,
                                    -rankOrdLife_5,-rankOrdLife_6,-coded_country)
other_df = skim(other_subset)
other_df = yank(other_df, "numeric")
View(other_df)

# Create Ukraine data set 
ukraine = cvbase %>% filter(coded_country == "Ukraine")
ukraine_subset = ukraine %>% select(-employstatus_1, -employstatus_2, -employstatus_3, -employstatus_4,
                               -employstatus_5, -employstatus_6, -employstatus_7, -employstatus_8, 
                               -employstatus_9,-employstatus_10, -coronaClose_1,-coronaClose_2,
                               -coronaClose_3,-coronaClose_4,-coronaClose_5,-coronaClose_6, 
                               -rankOrdLife_1,-rankOrdLife_2,-rankOrdLife_3,-rankOrdLife_4,
                               -rankOrdLife_5,-rankOrdLife_6,-coded_country)
ukraine_df = skim(ukraine_subset)
ukraine_df = yank(ukraine_df, "numeric")
View(ukraine_df)

# Retrieve numeric data only from both skim data frames
subset_other = other_df[, c("skim_variable", "p0","p25","p50","p75","p100")]
subset_other = subset_other %>% mutate(country = "Other")

subset_Ukraine = ukraine_df[, c("skim_variable", "p0","p25","p50","p75","p100")]
subset_Ukraine = subset_Ukraine %>% mutate(country = "Ukraine")

# Combine both Ukraine and other countries data
whole = rbind(subset_other,subset_Ukraine)
View(whole)

# plot a box plot to do comparison
ggplot(whole, aes(x = skim_variable, lower = p25, upper = p75, middle = p50, ymin = p0, ymax = p100, fill = country)) +
  geom_boxplot(stat = "identity") +
  theme_minimal() +
  labs(y = "Value", title = "Comparison of Attributes between Ukraine and Other Countries") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#---------------------------------------------
# Question 2b : Focus Group Model 
#----------------------------------------------
model = function(target,data) {
  formula_str <- paste(target, "~ isoFriends_inPerson + isoOthPpl_inPerson + isoFriends_online + isoOthPpl_online + 
            lone01 + lone02 + lone03 + happy + lifeSat + MLQ + bor01 + bor02 + bor03 + consp01 + consp02 + consp03 + 
           coronaClose_1 + coronaClose_2 + coronaClose_3 + coronaClose_4 + coronaClose_5 + coronaClose_6 + 
           gender + age + edu + c19perBeh01 + c19perBeh02 + c19perBeh03 + c19RCA01 + c19RCA02 + c19RCA03 + 
           rankOrdLife_1 + rankOrdLife_2 + rankOrdLife_3 + rankOrdLife_4 + rankOrdLife_5 + rankOrdLife_6 + 
           employstatus_1 + employstatus_2 + employstatus_3 +employstatus_4 +employstatus_5 + employstatus_6 + 
           employstatus_7 + employstatus_8 + employstatus_9 + employstatus_10")
                      
  formula = as.formula(formula_str)
  fit = lm(formula, data = data)
  return(summary(fit))
}
summary_model_c19ProSo01 = model("c19ProSo01", ukraine)
summary_model_c19ProSo01
summary_model_c19ProSo02 = model("c19ProSo02", ukraine)
summary_model_c19ProSo02
summary_model_c19ProSo03 = model("c19ProSo03", ukraine)
summary_model_c19ProSo03
summary_model_c19ProSo04 = model("c19ProSo04", ukraine)
summary_model_c19ProSo04


#---------------------------------------------
# Question 2c : Other group model
#---------------------------------------------
# Create linear regression model for other countries as a group, added coded_country as an attribute
model = function(target,data) {
  formula_str = paste(target, "~ isoFriends_inPerson + isoOthPpl_inPerson + isoFriends_online + isoOthPpl_online + 
           lone01 + lone02 + lone03 + happy + lifeSat + MLQ + bor01 + bor02 + bor03 + consp01 + consp02 + consp03 + 
           coronaClose_1 + coronaClose_2 + coronaClose_3 + coronaClose_4 + coronaClose_5 + coronaClose_6 + 
           gender + age + edu + c19perBeh01 + c19perBeh02 + c19perBeh03 + c19RCA01 + c19RCA02 + c19RCA03 + 
           rankOrdLife_1 + rankOrdLife_2 + rankOrdLife_3 + rankOrdLife_4 + rankOrdLife_5 + rankOrdLife_6 +
           employstatus_1 + employstatus_2 + employstatus_3 +employstatus_4 +employstatus_5 + employstatus_6 + employstatus_7 +
           employstatus_8 +employstatus_9 + employstatus_10 + coded_country")

  formula = as.formula(formula_str)
  data$coded_country = as.numeric(as.factor(data$coded_country))
  fit = lm(formula, data = data)
  return(summary(fit))
}

summary_model_c19ProSo01 = model("c19ProSo01",other)
summary_model_c19ProSo01
summary_model_c19ProSo02 = model("c19ProSo02",other)
summary_model_c19ProSo02
summary_model_c19ProSo03 = model("c19ProSo03",other)
summary_model_c19ProSo03
summary_model_c19ProSo04 = model("c19ProSo04",other)
summary_model_c19ProSo04



#---------------------------------------------
# Question 3 : Other group model
#---------------------------------------------
data = read.csv('data1.csv')
data = subset(data, select = -Year)
View(data)
str(data)

# Make gdp as numeric
data[,14] = as.numeric(as.character(data[,14]))

# Check NA data
colSums(is.na(data))

# Make NA as mean
for(i in 2:ncol(data)) {
    column_mean = mean(data[[i]], na.rm = TRUE)
    data[[i]][is.na(data[[i]])] = column_mean
  
}

# Factorise country and scale the data
data$Country = factor(data$Country)
data_scaled = data
data_scaled[,2:14]  = scale(data_scaled[,2:14])
View(data_scaled)

?kmeans
# K- means
dkfit = kmeans(data_scaled[,2:14],8, nstart = 50)
dkfit$cluster
T1 = table(actual = data$Country, fitted = dkfit$cluster)
T1 = as.data.frame.matrix(T1)
View(T1)

# Find similar countries 
focus_country_index = which(data$Country == "Ukraine") #
focus_cluster = dkfit$cluster[focus_country_index]
similar_countries = row.names(T1)[T1[, focus_cluster] == 1]
similar_countries

# Check how many clusters is suitable
silhouette_score = function(k){
  km = kmeans(data_scaled[,2:14], k, nstart = 50)
  # calculate the silhouette score
  ss = silhouette(km$cluster, dist(data_scaled[,2:14]))
  mean(ss[,3])
}

k = 2:15

# Run silhouette function for all value of k and plot a graph
avg_sil = sapply(k,silhouette_score)
plot(k, type = 'b', avg_sil, xlab  = "Number of Clusters",ylab  = "Average Silhouette Score")

#-------------------------------------
# Question 3b : 
#-------------------------------------
# Five countries
specific_countries = c("Algeria", "Belarus", "China", "Thailand", "Greece")

# Retrieve data of 5 countries
df_kmeans = cvbase[cvbase$coded_country %in% specific_countries,]
View(df_kmeans)

# Fit linear model of the 5 countries as a group
model = function(target,data) {
  formula_str = paste(target, "~ isoFriends_inPerson + isoOthPpl_inPerson + isoFriends_online + isoOthPpl_online + 
           lone01 + lone02 + lone03 + happy + lifeSat + MLQ + bor01 + bor02 + bor03 + consp01 + consp02 + consp03 + 
           coronaClose_1 + coronaClose_2 + coronaClose_3 + coronaClose_4 + coronaClose_5 + coronaClose_6 + 
           gender + age + edu + c19perBeh01 + c19perBeh02 + c19perBeh03 + c19RCA01 + c19RCA02 + c19RCA03 + 
           rankOrdLife_1 + rankOrdLife_2 + rankOrdLife_3 + rankOrdLife_4 + rankOrdLife_5 + rankOrdLife_6 +
           employstatus_1 + employstatus_2 + employstatus_3 +employstatus_4 +employstatus_5 + employstatus_6 + employstatus_7 +
           employstatus_8 +employstatus_9 + employstatus_10 + coded_country")
  
  formula = as.formula(formula_str)
  data$coded_country = as.numeric(as.factor(data$coded_country))
  fit = lm(formula, data = data)
  return(summary(fit))
}
summary_kmodel_c19ProSo01 = model("c19ProSo01",df_kmeans)
summary_kmodel_c19ProSo01
summary_kmodel_c19ProSo02 = model("c19ProSo02",df_kmeans)
summary_kmodel_c19ProSo02 
summary_kmodel_c19ProSo03 = model("c19ProSo03",df_kmeans)
summary_kmodel_c19ProSo03
summary_kmodel_c19ProSo04 = model("c19ProSo04",df_kmeans)
summary_kmodel_c19ProSo04

