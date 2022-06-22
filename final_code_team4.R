library(readxl)
library(ggplot2)
library(fastDummies)
library(caTools)
library(party)
library(dplyr)
library(magrittr)
library(tidyr)
library(rpart)
library(rpart.plot)
diabetic_data <- read_excel("C:/Users/Siddharth Varada/OneDrive - Hult Students/Hackathon/diabetic_data.xlsx")
View(diabetic_data)
cor.test(diabetic_data$num_lab_procedures, diabetic_data$num_medications, method = c("pearson", "kendall", "spearman"))
cor.test(diabetic_data$num_lab_procedures, diabetic_data$num_diagnoses, method = c("pearson", "kendall", "spearman"))
diabetic_data$gender_binary <-  gsub("Female", "0", diabetic_data$gender)
diabetic_data$gender_binary <- gsub("Male", "1", diabetic_data$gender_binary)
diabetic_data$gender_binary <- gsub("Unknown/Invalid", "", diabetic_data$gender_binary)
diabetic_data$gender_binary <- as.numeric(diabetic_data$gender_binary)
diabetic_data <- diabetic_data[!diabetic_data$gender == "Unknown/Invalid",]

diabetic_data$diabetesMed_binary <- gsub("No", "0", diabetic_data$diabetesMed)
diabetic_data$diabetesMed_binary <- gsub("Yes", "1", diabetic_data$diabetesMed_binary)
diabetic_data$diabetesMed_binary <- as.numeric(diabetic_data$diabetesMed_binary)


diabetic_data$race_fixed <- gsub("[?]", "Unknown", diabetic_data$race)

clean_diabetic_data <- diabetic_data %>%
  select(-c(weight, payer_code, medical_specialty, diag_1, diag_2, diag_3)) 

glimpse(clean_diabetic_data)


my_logit <- glm(diabetesMed_binary~num_lab_procedures+num_procedures+num_medications+number_outpatient+number_emergency+number_inpatient+number_diagnoses, data=diabetic_data, family="binomial")
summary(my_logit)

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row <- nrow(data)
  total_row <-  size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

# creating train and test set
data_train <- create_train_test(clean_diabetic_data, 0.8, train = TRUE)
data_test <- create_train_test(clean_diabetic_data, 0.8, train = FALSE)
dim(data_train)

# distribution of values within readmitted col
prop.table(table(data_train$readmitted))

# fitting a model
fit <- rpart(readmitted~., data = data_train, method = 'class')


# plot decision tree
rpart.plot(fit, extra = 106)


exp(-0.0029565) - 1
exp(-0.1305959) - 1
exp(0.0852041) - 1
exp(0.0052773) - 1
exp(0.0770207) - 1
exp(-0.0014806) - 1
exp(-0.0517359) - 1

diabetic_data$race_binary <- c()
diabetic_data$payer_code_binary <- c()

for(i in 1:nrow(diabetic_data))
{
  if(diabetic_data$race[i] == "Caucasian") 
  {
    diabetic_data$race_binary[i] <- 1
  }
  else if(diabetic_data$race[i] == "AfricanAmerican") 
  {
    diabetic_data$race_binary[i] <- 2
  }
  else if(diabetic_data$race[i] == "Other") 
  {
    diabetic_data$race_binary[i] <- 3
  }
  else if(diabetic_data$race[i] == "Asian") 
  {
    diabetic_data$race_binary[i] <- 4
  }
  else if(diabetic_data$race[i] == "Hispanic")
  {
    diabetic_data$race_binary[i] <- 5
  }
  else
  {
    diabetic_data$race_binary[i] <- 0
  }
}

for(i in 1:nrow(diabetic_data))
{
  if(diabetic_data$payer_code[i] == "?") 
  {
    diabetic_data$payer_code_binary[i] <- 0
  }
  else
  {
    diabetic_data$payer_code_binary[i] <- 1
  }
}

Caucasian_count <- 0
AfricanAmerican_count <- 0
Other_count <- 0
Asian_count <- 0
Hispanic_count <- 0
Unknown_count <- 0

Caucasian_ins <- 0
AfricanAmerican_ins <- 0
Other_ins <- 0
Asian_ins <- 0
Hispanic_ins <- 0
Unknown_ins <- 0

for(i in 1:nrow(diabetic_data))
{
  if(diabetic_data$race_binary[i] == 1)
  {
    Caucasian_count <- Caucasian_count+1
    Caucasian_ins <- Caucasian_ins + diabetic_data$payer_code_binary[i]
  }
  else if(diabetic_data$race_binary[i] == 2) 
  {
    AfricanAmerican_count <- AfricanAmerican_count+1
    AfricanAmerican_ins <- AfricanAmerican_ins + diabetic_data$payer_code_binary[i]
  }
  else if(diabetic_data$race_binary[i] == 3)
  {
    Other_count <- Other_count+1
    Other_ins <- Other_ins + diabetic_data$payer_code_binary[i]
  }
  else if(diabetic_data$race_binary[i] == 4)
  {
    Asian_count <- Asian_count+1
    Asian_ins <- Asian_ins + diabetic_data$payer_code_binary[i]
  }
  else if(diabetic_data$race_binary[i] == 5) 
  {  
    Hispanic_count <- Hispanic_count+1
    Hispanic_ins <- Hispanic_ins + diabetic_data$payer_code_binary[i]
  }
}

AfricanAmerican_ins/AfricanAmerican_count
Caucasian_ins/Caucasian_count
Asian_ins/Asian_count
Hispanic_ins/Hispanic_count
Unknown_ins/Unknown_count

my_linear <- lm(payer_code_binary~race, data=diabetic_data)
summary(my_linear)

exp(-0.24213) - 1
exp(-0.17387) - 1
exp(-0.13538) - 1
exp(-0.24794) - 1
exp(-0.20498) - 1
