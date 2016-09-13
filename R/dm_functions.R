#Below functions are individual DM algorithms and other supplementary functions used in uslfin package

#' @title K-Means Learn
#'
#' @description
#' Implements a specialized version of K-Means algorithm on the data set. When creating the clusters (in-sample) the function uses pricing_error, moneyness and maturity. But when predicting, it uses only moneyness and maturity covariates. All covariates are scaled between 0-100.
#'
#' @param raw_data The option data set given in the format of \link{uslfin_ds_1}.
#' @param CallPut It denotes whether to use the call or put options.
#' @param randseed To set the randomness seed to a known value. Good for reproducibility.
#' @param moneyness_interval Minimum and maximum of moneyness values. Required for rescaling.
#' @param maturity_interval Minimum and maximum of maturity values. Required for rescaling. In trading days (one year \= 252 days)
#' @param n_cluster Number of clusters. If \code{0}, then number of clusers is determined by the contract set size. Exact calculation is \code{min(round(nrow(training_matrix)/100),200)}
#' @export
kmeans_learn<-function(raw_data,CallPut="call",randseed=0,moneyness_interval=c(0.9,1.1),maturity_interval=c(4,252),n_cluster=0){
	scaled_data <-
    raw_data %>%
    #Use only appropriate option type (call or put)
		dplyr::filter(type == CallPut) %>%
    #Rescale moneyness and maturity to a 0-100 scale each.
  	dplyr::mutate_(scaled_moneyness = lazyeval::interp(~((moneyness-min(moneyness_interval))/(max(moneyness_interval)-min(moneyness_interval))*100)),
				           scaled_maturity  = lazyeval::interp(~((maturity-min(maturity_interval))/(max(maturity_interval)-min(maturity_interval))*100)))

  #Prepare the training data
	training_data <-
    scaled_data %>%
    #Filter only training data
		dplyr::filter(t_or_p == "Training") %>%
		dplyr::mutate_(scaled_error=lazyeval::interp(~(rescale_error(pricing_error,mederror=NA)*100)))

  #Create the error + moneyness + maturity matrix for kmeans
	training_matrix<-
    training_data %>%
		dplyr::select(scaled_error,scaled_moneyness,scaled_maturity) %>%
		as.matrix

  #Number of clusters are determined by the number of contracts (n_contracts/100). Max 200 clusters
  if(n_cluster == 0){
    n_cluster <- min(round(nrow(training_matrix)/100),200)
  }

  #Set the seed if there is a predetermined one
	if(randseed>0){
		set.seed(randseed)
	}

  #Train K-Means
	kmeans_training <- kmeans(training_matrix,centers=n_cluster,nstart=round(n_cluster/5),iter.max=10^6)
  #Get the cluster centers
	kmeans_centers <- kmeans_training$centers[,c("scaled_moneyness","scaled_maturity")]
  #Get the average pricing error of each cluster
	kmeans_center_errors <-
    training_data %>%
		dplyr::mutate(clusters=kmeans_training$cluster) %>%
		dplyr::group_by(clusters) %>%
		dplyr::summarise_(model_estimate=lazyeval::interp(~mean(pricing_error,na.rm=TRUE)))

  #Get both training and prediction contracts' prediction error on pricing errors.
	result_data <-
    scaled_data %>%
		dplyr::rowwise() %>%
		dplyr::mutate(clusters=get_the_cluster(scaled_moneyness,scaled_maturity,kmeans_centers)) %>%
		dplyr::ungroup() %>%
		dplyr::left_join(.,kmeans_center_errors,by="clusters") %>%
		dplyr::mutate_(prediction_error=lazyeval::interp(~(abs(pricing_error-model_estimate)/pmax(abs(pricing_error),0.01))))

	# summary<-
  #   result_data %>%
  #   dplyr::group_by(t_or_p) %>%
  #   dplyr::summarise(prediction_error=median(prediction_error,na.rm=TRUE)) %>%
  #   dplyr::select(prediction_error) %>%
  #   unlist

	# names(summary)<-c("Prediction","Training")

	# return(list(result_data=result_data,summary=summary))
  return(result_data)
}

#' @title Manual Learn
#'
#' @description
#' Given a set of moneyness and maturity boundaries and a option data set, this function calculates in-sample and out-of-sample clustering errors.
#' @param raw_data The option data set given in the format of \link{uslfin_ds_1}.
#' @param CallPut It denotes whether to use the call or put options.
#' @param moneyness_breaks Vector of moneyness boundaries.
#' @param moneyness_breaks Vector of maturity boundaries.
#' @export
manual_learn<-function(raw_data,CallPut,moneyness_breaks=c(0.899,0.94,0.97,1.00,1.03,1.06,1.101),maturity_breaks=c(0,42,126,252)){

  #First define the manual clusters
	transformed_data <-
    raw_data %>%
    dplyr::filter(type==CallPut) %>%
		dplyr::mutate( moneyness_limits=cut(moneyness,moneyness_breaks),
				           maturity_limits=cut(maturity,maturity_breaks))

  #Get the average error in each cluster
	manual_centers <-
    transformed_data %>%
		dplyr::filter(t_or_p=="Training") %>%
		dplyr::group_by(moneyness_limits,maturity_limits) %>%
		dplyr::summarise(model_estimate=mean(pricing_error,na.rm=TRUE))

  #Calculate the prediction error
	result_data <-
    transformed_data %>%
  	dplyr::left_join(.,manual_centers,by=c("moneyness_limits","maturity_limits")) %>%
  	dplyr::mutate(prediction_error=abs(pricing_error-model_estimate)/pmax(abs(pricing_error),0.01))

	# summary<- result_data %>% group_by(TrainPred) %>% summarise(MAPE=mean(MAPE,na.rm=TRUE)) %>% select(MAPE) %>% unlist
	# names(summary)<-c("Prediction","Training")
	# return(list(result_data=result_data,summary=summary))
  return(result_data)
}

#' @title DM Learn
#'
#' @description
#' Implements different data mining algorithms (SVM, CIT and DT).
#' @param raw_data The option data set given in the format of \link{uslfin_ds_1}.
#' @param model_name The name of the data mining model to use. Currently there are three options: Support Vector Machine (\code{"svm"}) of \code{e1071} package, Decision Tree (\code{"dt"}) of \code{rpart} package and Conditional Inference Tree \code{("cit")} of the \code{partykit} package.
#' @param CallPut It denotes whether to use the call or put options.
#' @param randseed To set the randomness seed to a known value. Good for reproducibility.
#' @param ... Parameters to pass to data mining algorithm functions. (See individual functions for further explanations.)
#' @export
dm_learn<-function(raw_data,model_name="svm",CallPut="call",randseed=0,...){

	if(randseed>0){
		set.seed(randseed)
	}
	transformed_data <- dplyr::filter(raw_data,type==CallPut)
	# training_data<-filter(transformed_data,TrainPred=="Training")
	the_formula<- as.formula(paste0("pricing_error"," ~ ","moneyness + maturity"))
  #Support vector machine
	if(model_name=="svm"){
		the_model<-e1071::svm(formula=the_formula,data=transformed_data,subset=(transformed_data$t_or_p=="Training"),...)
	}else if(model_name=="cit"){
    #Conditional inference tree
		the_model<-partykit::ctree(the_formula, data=transformed_data,subset=(transformed_data$t_or_p=="Training"),...)
	}else if(model_name=="dt"){
    #Decision tree
		the_model<-rpart::rpart(the_formula, data=transformed_data,subset=(transformed_data$t_or_p=="Training"),...)
	}else{
		stop("Wrong model name!")
	}

	result_data <-
    transformed_data %>%
	 	dplyr::mutate(model_estimate=predict(the_model,.)) %>%
		dplyr::mutate_(prediction_error=lazyeval::interp(~(abs(pricing_error-model_estimate)/pmax(abs(pricing_error),0.01))))

	# summary<- result_data %>% group_by(TrainPred) %>% summarise(MAPE=mean(MAPE,na.rm=TRUE)) %>% select(MAPE) %>% unlist
	# names(summary)<-c("Prediction","Training")
	# return(list(result_data=result_data,summary=summary))
  return(result_data)
}
