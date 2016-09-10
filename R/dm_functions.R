#Below functions are individual DM algorithms and other supplementary functions used in uslfin package

#' @export
kmeans_learn<-function(raw_data,CallPut="call",error_type="ARPE",randseed=0,moneyness_interval,maturity_interval){
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
	n_cluster <- min(round(nrow(training_matrix)/100),200)

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

#' @export
manual_learn<-function(raw_data,CallPut,moneyness_breaks,maturity_breaks){

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

#' @export
dm_learn<-function(raw_data,model_name="svm",CallPut="call",randseed=0){

	if(randseed>0){
		set.seed(randseed)
	}
	transformed_data <- dplyr::filter(raw_data,type==CallPut)
	# training_data<-filter(transformed_data,TrainPred=="Training")
	the_formula<- as.formula(paste0("pricing_error"," ~ ","moneyness + maturity"))
  #Support vector machine
	if(model_name=="svm"){
		the_model<-e1071::svm(formula=the_formula,data=transformed_data,subset=(transformed_data$t_or_p=="Training"),kernel="radial",scale=FALSE)
	}else if(model_name=="cit"){
    #Conditional inference tree
		the_model<-partykit::ctree(the_formula, data=transformed_data,subset=(transformed_data$t_or_p=="Training"))
	}else if(model_name=="dt"){
    #Decision tree
		the_model<-rpart::rpart(the_formula, data=transformed_data,subset=(transformed_data$t_or_p=="Training"))
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
