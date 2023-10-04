#----importing data
read.csv("C:\Users\shrad\Downloads.csv")
data <- read.csv("cancer.csv")
setwd("C:/Users/shrad/Downloads")
data
print(data)



#---------exploring data
nrow(data) #no.of rows
ncol(data) #no.of columns
column_names <- colnames(data)
column_names

#------------drop first & last column
new_data <- subset(data, select = -c(id, X))
new_data
head(new_data)


#------------null value check
is.na(new_data)
sum(is.na(new_data))

#convert character to categorical variable 
new_data$diagnosis <- ifelse(new_data$diagnosis == "M", 1, 0)#data manipualtion
new_data$diagnosis <- as.numeric(new_data$diagnosis)
class(new_data$diagnosis)
names(new_data)

#-------------duplicate check
duplicates <- duplicated(new_data) 
sum(duplicates)
str(new_data)

#Count unique values in the "Category" column
table(data$diagnosis)

#Using the caret package for one-hot encoding
#install.packages("lattice")
#library(caret)

# ------------------------******ENCODING METHODS*******-------------------------

#converting into categorical columns(object/string/character character type)
#one-hot encoding - creates dummy variables/columns ex: There are 3 classes like sunny/cloudy/overcast in weather column
#label encoding - ex: sunny/cloudy/overcast - 0,1,2

#-------------------------------------------------------------------------------

#Perform one-hot encoding
#encoded_data <- dummyVars("", data = data)
#encoded_data <- data.frame(predict(encoded_data, newdata = data)

#logistic regression because in the dataset there's one dependent variable and 
#multiple independent variables 

install.packages("ggplot2")
library(stats)


logistic_model <- glm(diagnosis ~ ., data = new_data, family = binomial)
summary(logistic_model)

#estimate indicate relationship bw outcome variable with predictor variable keeping all the other columns constant---------*
#Extract the coefficients and their associated p-values
coefficients <- coef(logistic_model)
p_values <- summary(logistic_model)$coefficients[, "Pr(>|z|)"]
p_values

#Create a data frame to store coefficients and p-values
coefficients_df <- data.frame(Coefficient = coefficients, P_Value = p_values)

#Sort by absolute coefficient magnitude in descending order
coefficients_df <- coefficients_df[order(-abs(coefficients_df$Coefficient)), ]
coefficients_df
#View the sorted coefficients
print(coefficients_df)

library(ggplot2)

#Assuming you have the coefficients_df data frame from the previous code
#Create a bar plot of coefficients sorted by their absolute magnitude
coefficients_df <- coefficients_df[order(-abs(coefficients_df$Coefficient)), ]

#Create the bar plot #----logistic regression coefficient magnitude--------#
ggplot(coefficients_df, aes(x = reorder(rownames(coefficients_df), -abs(Coefficient)), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Logistic Regression Coefficient Magnitude",
       x = "Features",
       y = "Coefficient") +
  coord_flip() +
  theme_minimal()



# Create a scatter plot
ggplot(new_data, aes(x = radius_mean, y = texture_mean, color = diagnosis)) +
  geom_point() +
  labs(title = "Scatter Plot of Radius Mean vs. Texture Mean",
       x = "Radius Mean",
       y = "Texture Mean")

#create a correlation plot
library(corrplot)

correlation_matrix <- cor(new_data)

corrplot(correlation_matrix, method = "circle", type = "full",
         title = "Correlation Plot", tl.col = "black")


#Predict probabilities for the new data
predicted_probabilities <- predict(logistic_model, new_data, type = "response")
predicted_probabilities
#### Threshold for binary classification (typically 0.5)
threshold <- 0.5

#### Predict probabilities for the new data


#### Convert predicted probabilities to class labels
predicted_classes <- ifelse(predicted_probabilities >= threshold, 1, 0)
predicted_classes
#### View the predicted class labels
print(predicted_classes[1:3])



#Data Visualization
x<- c(357,212)
labels<- c("Benign","Malignant")
piepercent <- round(100 * x /sum(x), 1)
png(file="cancer_title_colours.jpg")
pie(x, labels = piepercent, main = "Frequency of tumor", col = rainbow(length(x)))
legend("topright",c("Benign","Malignant"),cex = 0.8,
       fill = rainbow(length(x)))
counts<- table(data$diagnosis)
percentages<- prop.table(counts) * 100

pie(counts, main = "Frequency of cancer", labels = paste0(names(counts),"\n",sprintf("%.1f%%", percentages)), col=rainbow(length(counts)))


#visualizing distribution of data via histograms
library(ggplot2)
ggplot(data, aes(x = radius_se, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = radius_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = texture_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = perimeter_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = area_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = smoothness_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = compactness_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = concavity_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = concave.points_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = symmetry_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

ggplot(data, aes(x = fractal_dimension_mean, fill = diagnosis )) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of numerical variable by diagnosis",
       x = "Numerical Variable", y = "Frequency") + 
  scale_fill_manual(values = c("M" = "blue","B" = "green"))

# Step 1: Extract coefficients and p-values
coef_summary <- summary(logistic_model)$coef
coef_summary
# Step 2: Filter variables based on p-value and coefficient magnitude
significant_vars <- coef_summary[coef_summary[, "Pr(>|z|)"] < 0.05, ]
significant_vars <- significant_vars[abs(significant_vars[, "Estimate"]) > 1, ] # Assuming "large coefficient" means absolute value > 1
significant_vars

# Step 3: Extract the names of the significant variables
significant_var_names <- rownames(significant_vars)
significant_var_names


nrow(significant_vars)
ncol(significant_vars)




significant_var_names <- significant_var_names[-1] #eliminating intercept 
significant_var_names

# Step 4: Create a new data frame with only the significant variables
#significant_data <- subset(new_data, select = c(significant_var_names))

#names(new_data)
# Step 5: Fit a logistic regression model with the selected variables
#new_logistic_model <- glm(diagnosis ~ ., data = significant_data, family = binomial)

# Step 6: Print summary of the new model
#summary(new_logistic_model)

# Create a boxplot for selected variables
boxplot(new_data$diagnosis ~ new_data$fractal_dimension_se)
ggplot(new_data, aes(x = diagnosis, y = fractal_dimension_se))+ 
  geom_boxplot(fill = "lightblue", color = "blue") + 
  labs(title = "Boxplot", x = "Diagnosis", y = "fractal_dimension_se")
#Plot the same changing other independent variables and we can know the significant variables#
