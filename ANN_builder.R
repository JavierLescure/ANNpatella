#set seed
	set.seed(5918589)	

#load the data and save into a var called "dataset"

#split data in male and female datasets and create the partition train - test
	data_m <- subset(dataset, dataset$SEX_NUM==1)
	data_f <- subset(dataset, dataset$SEX_NUM==2)
	
	smp_size_m <- floor(0.21 * nrow(data_m))
	smp_size_f <- floor(0.24 * nrow(data_f))
	
	train_ind_m <- sample(seq_len(nrow(data_m)), size = smp_size_m)
	train_ind_f <- sample(seq_len(nrow(data_f)), size = smp_size_f)
	
	test_m <- data_m[train_ind_m, ]
	train_m <- data_m[-train_ind_m, ]
	test_f <- data_f[train_ind_f, ]
	train_f <- data_f[-train_ind_f, ]
	
#merge both sex dataset in train & test datasets
	train_data <- rbind(train_m, train_f)
	test_data <- rbind(test_m, test_f)
	
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

#test
	nn_test=predict(model,test_data)
	nn_test_classifier <- ifelse(nn_test >= cp, 2, 1)
	test_data=cbind(test_data, nn_test, nn_test_classifier)