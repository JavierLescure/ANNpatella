#set seed
set.seed(7716916)	# seeds: 	

#load the data
library(readxl)
dataset <- read_excel("~/Universidad/TFM/SPSS/Test_Patella/Test_Dataset_SinOutliers.xlsx", sheet = "Hoja2")
View(dataset)

#split data in male & female datasets
data_m <- subset(dataset, dataset$SEX_NUM==1)
data_f <- subset(dataset, dataset$SEX_NUM==2)

smp_size_m <- floor(0.21 * nrow(data_m))
smp_size_f <- floor(0.24 * nrow(data_f))

train_ind_m <- sample(seq_len(nrow(data_m)), size = smp_size_m)
train_ind_f <- sample(seq_len(nrow(data_f)), size = smp_size_f)

valid_m <- data_m[train_ind_m, ]
train_m <- data_m[-train_ind_m, ]
valid_f <- data_f[train_ind_f, ]
train_f <- data_f[-train_ind_f, ]

#merge both sex dataset in train & valid datasets
train_data <- rbind(train_m, train_f)
valid_data <- rbind(valid_m, valid_f)

# load the library
library(caret)

# define training control
train_control <- trainControl(method="LOOCV")

# train the model
model <- train(SEX_NUM~MH_N+MB_N+MAH_N, data=train_data, trControl=train_control, method="neuralnet")

# summarize results
print(model)

# set the cut point
require(cutpointr)
nn_train=predict(model,train_data)
data_cp=cbind(train_data, nn_train)
opt_cut <- cutpointr(data_cp, data_cp$nn_train, data_cp$SEX_NUM)
cp <- opt_cut$optimal_cutpoint

#validation
nn_valid=predict(model,valid_data)
nn_valid_classifier <- ifelse(nn_valid >= cp, 2, 1)
valid_data=cbind(valid_data, nn_valid, nn_valid_classifier)

write.table(valid_data,"~/Universidad/TFM/SPSS/test_Patella/validation.txt")