# load the library
	library(caret)

# load the ANN model
	model <- readRDS("ANN_patella.rds")

# load your data and save it into a var called "test"

# set the cut point
	cp <- 1.371355

# classify the data

	nn_test=predict(model,test)
	nn_test_classifier <- ifelse(nn_test >= cp, 2, 1)
	test=cbind(test nn_test, nn_test_classifier)