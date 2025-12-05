library(plumber)
library(tidyverse)
library(tidymodels)

#* @apiTitle Final Project API
#* @apiDescription Three endpoints on diabetes data
# read in data on diabetes
diabetes_data <- read_csv("../diabetes_binary_health_indicators_BRFSS2015.csv")

# convert columns with binary variables to factors and change the names of values
diabetes_data <- diabetes_data |> 
  mutate(
    Diabetes_binary = as.factor(Diabetes_binary), 
    HighBP = as.factor(HighBP), 
    HighChol = as.factor(HighChol), 
    CholCheck = as.factor(CholCheck), 
    Smoker = as.factor(Smoker), 
    Stroke = as.factor(Stroke), 
    HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack), 
    PhysActivity = as.factor(PhysActivity), 
    Fruits = as.factor(Fruits), 
    Veggies = as.factor(Veggies), 
    HvyAlcoholConsump = as.factor(HvyAlcoholConsump), 
    AnyHealthcare = as.factor(AnyHealthcare), 
    NoDocbcCost = as.factor(NoDocbcCost), 
    GenHlth = as.factor(GenHlth), 
    DiffWalk = as.factor(DiffWalk), 
    Sex = as.factor(Sex), 
    Age = as.factor(Age), 
    Education = as.factor(Education), 
    Income = as.factor(Income)
  )


# change level names of factors
levels(diabetes_data$Diabetes_binary) <- list("No Diabetes" = "0", "Prediabetes/Diabetes" = "1")

levels(diabetes_data$HighBP) <- list("No High Blood Pressue" = "0", "High Blood Pressure" = "1")

levels(diabetes_data$HighChol) <- list("No High Cholesterol" = "0", "High Cholesterol" = "1")

levels(diabetes_data$CholCheck) <- list("No Chol. check in last 5 years" = "0", "Chol. check in last 5 years" = "1")

levels(diabetes_data$Smoker) <- list("Non-Smoker" = "0", "Smoker" = "1")

levels(diabetes_data$Stroke) <- list("Never had strok" = "0", "Had stroke" = "1")

levels(diabetes_data$HeartDiseaseorAttack) <- list("Never had heart disease/attack" = "0", "Had heart disease/attack" = "1")

levels(diabetes_data$PhysActivity) <- list("No physical activity in last 30 days" = "0", "Physical activity in last 30 days" = "1")

levels(diabetes_data$Fruits) <- list("Doesn't eat fruit" = "0", "Eats fruit" = "1")

levels(diabetes_data$Veggies) <- list("Doesn't eat veggies" = "0", "Eats veggies" = "1")

levels(diabetes_data$HvyAlcoholConsump) <- list("Not heavy drinker" = "0", "Heavy drinker" = "1")

levels(diabetes_data$AnyHealthcare) <- list("No healthcare" = "0", "Healthcare" = "1")

levels(diabetes_data$NoDocbcCost) <- list("Can't afford doctor" = "0", "Can afford doctor" = "1")

levels(diabetes_data$GenHlth) <- list("Excellent" = "1", "Very Good" = "2", "Good" = "3", "Fair" = "4", "Poor" = "5")

levels(diabetes_data$DiffWalk) <- list("No difficulty walking" = "0", "Difficulty walking" = "1")

levels(diabetes_data$Sex) <- list("Female" = "0", "Male" = "1")

levels(diabetes_data$Age) <- list("18-24" = "1", "25-29" = "2", "30-34" = "3", "35-39" = "4", "40-44" = "5", "45-49" = "6", "50-54" = "7", "55-59" = "8", "60-64" = "9", "65-69" = "10", "70-74" = "11", "75-79" = "12", "80+" = "13")

levels(diabetes_data$Education) <- list("No school/kindergarten" = "1", "Elementary" = "2", "Some high school" = "3", "High school graduate" = "4", "Some college" = "5", "College graduate" = "6")

levels(diabetes_data$Income) <- list("< $10,000" = "1", "$10,000-$15,000" = "2", "$15,000-$20,000" = "3", "$20,000-$25,000" = "4", "$25,000-$35,000" = "5", "$35,000-$50,000" = "6", "$50,000-$75,000" = "7", "$75,000+" = "8")

# read in our best overall model workflow
workflow <- readRDS("../final_wkf.RDS")
model <- readRDS("../final_model.RDS")


# extract the final model
extract_model<- extract_workflow(model)
extract_model

# find the prediction of each observation for the final model
preds <- predict(extract_model, new_data = diabetes_data)

# create a data set with both the predictions and results that can be used in the confusion matrix in the third endpoint
preds_vs_outcome <- bind_cols(diabetes_data$Diabetes_binary, preds)

# assign this data set better names
names(preds_vs_outcome) <- c("outcomes", "predictions")


# save the mean BMI of the data set as an object
mean_BMI <- mean(diabetes_data$BMI)

#* Predict Diabetes Based on Input Values
#* @param highBP Levels: 'High Blood Pressure' and 'No High Blood Pressure'
#* @param highChol Levels: 'High Cholesterol and 'No High Cholesterol'
#* @param veggies Levels: 'Doesn't eat veggies' and 'Eats veggies' 
#* @param genHlth Levels: 'Poor', 'Fair', 'Good', 'Very Good', and 'Excellent'
#* @param diffWlk Levels: 'No difficulty walking' and 'Difficult walking'
#* @param anyHlthcare Levels: 'No healthcare' and 'Healthcare'
#* @param BMI Numeric variable
#* @get /pred
function(highBP = "No High Blood Pressure",
         highChol = "No High Cholesterol",
         veggies = "Eats veggies",
         genHlth = "Very Good",
         diffWlk = "No difficulty walking",
         anyHlthcare = "Healthcare",
         BMI = mean_BMI
         ) {
    input_values <- tibble(
      HighBP = factor(highBP, levels = levels(diabetes_data$HighBP)),
      HighChol = factor(highChol, levels = levels(diabetes_data$HighChol)),
      Veggies = factor(veggies, levels = levels(diabetes_data$Veggies)),
      GenHlth = factor(genHlth, levels = levels(diabetes_data$GenHlth)),
      DiffWalk = factor(diffWlk, levels = levels(diabetes_data$DiffWalk)),
      AnyHealthcare = factor(anyHlthcare, levels = levels(diabetes_data$AnyHealthcare)),
      BMI = as.numeric(BMI)
        )
    
    predict(extract_model, new_data = input_values)
}

# Query one (default values): 
# http://127.0.0.1:8000/pred?highBP=No%20High%20Blood%20Pressure&highChol=No%20High%20Cholesterol&veggies=Eats%20veggies&genHlth=Very%20Good&diffWlk=No%20difficulty%20walking&anyHlthcare=Healthcare&BMI=28.3824

# Query two:
# http://127.0.0.1:8000/pred?highBP=High%20Blood%20Pressure&highChol=High%20Cholesterol&veggies=Eats%20veggies&genHlth=Poor&diffWlk=Difficulty%20walking&anyHlthcare=Healthcare&BMI=50

# Query three:
# http://127.0.0.1:8000/pred?highBP=High%20Blood%20Pressure&highChol=High%20Cholesterol&veggies=Doesn%27t%20eat%20veggies&genHlth=Good&diffWlk=Difficulty%20walking&anyHlthcare=No%20healthcare&BMI=38


#* Provide Name and Github Pages url
#* @get /info
function(){
list(Name = "Nathan Honea",
Github_pages_url = "https://nhonea12.github.io/FinalProject/EDA.html")
}

#* Provide a Confusion Matrix of Predictions vs Outcomes
#* @serializer png
#* @get /confusion
function(){
# create the confusion matrix of predictions vs. outcomes
cm <- preds_vs_outcome |> 
  conf_mat(outcomes, predictions)

# plot the matrix
print(autoplot(cm, type = "heatmap"))
}
