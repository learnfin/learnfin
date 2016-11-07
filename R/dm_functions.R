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
kmeans_learn<-function(raw_data,CallPut="call",randseed=0,moneyness_interval=c(0.9,1.1),maturity_interval=c(4,252),n_cluster=0,export_plots=FALSE){
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
		dplyr::mutate(prediction_error=abs(pricing_error-model_estimate))
		# dplyr::mutate_(prediction_error=lazyeval::interp(~(abs(pricing_error-model_estimate)/pmax(abs(pricing_error),0.01))))

	#Plot
	if(export_plots){
  		if(!dir.exists("learnfin_plots")){
  			dir.create("learnfin_plots", showWarnings = TRUE, recursive = TRUE, mode = "0777")
  		}

		# data_mesh <-
		# 	expand.grid(scaled_moneyness=0:100,scaled_maturity=0:100) %>%
		# 	dplyr::rowwise() %>%
		# 	dplyr::mutate(clusters=get_the_cluster(scaled_moneyness,scaled_maturity,kmeans_centers)) %>%
		# 	dplyr::ungroup() %>%
		# 	dplyr::left_join(.,kmeans_center_errors,by="clusters")

  		plot_export_kmeans(result_data)
  	}


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


#' @title Plot K-Means
#'
#' @description
#' Plot the Kmeans graphs.
plot_export_kmeans<-function(the_df){

	#General theme options
	plot_theme_opts <- ggplot2::scale_color_gradientn(colors=c("green","yellow","red","purple","black"),values=scales::rescale(c(0,10,25,50,100),to=c(0,1),from=c(0,100)),limits=c(0,100),oob=scales::squish)

	ellipse_display <- 	ggplot2::stat_ellipse(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Training"),geom="polygon",level=0.999,ggplot2::aes(group=model_estimate,fill=model_estimate),alpha=0.2,linetype=0,show.legend=FALSE)

	fill_opts <-
	ggplot2::scale_fill_gradientn(colors=c("green","yellow","red","purple","black"),values=scales::rescale(c(0,10,25,50,100),to=c(0,1),from=c(0,100)),limits=c(0,100),oob=scales::squish,guide="none")

	the_plot <-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Training"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	ellipse_display +
 	fill_opts +
	ggplot2::geom_point(ggplot2::aes(color=pricing_error),alpha=0.75,size=2) +
	ggplot2::labs(title="Training Data Pricing Errors (K-Means Method)",color="") +
	plot_theme_opts +
	ggplot2::theme_bw()

	ggplot2::ggsave("learnfin_plots/kmeans_method_training_pricing_error.png",the_plot,width=12,height=5)

	the_plot <-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Prediction"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	ellipse_display +
 	fill_opts +
	ggplot2::geom_point(ggplot2::aes(color=pricing_error),alpha=0.75,size=2) +
	ggplot2::labs(title="Prediction Data Pricing Errors (K-Means Method)",color="") +
	plot_theme_opts +
	ggplot2::theme_bw()

	ggplot2::ggsave("learnfin_plots/kmeans_method_prediction_pricing_error.png",the_plot,width=12,height=5)

	the_plot <-
	ggplot2::ggplot() +
	ggplot2::stat_ellipse(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Training"),geom="polygon",level=0.999,ggplot2::aes(x=moneyness,y=maturity,group=model_estimate,fill=model_estimate),alpha=0.4,linetype=0,show.legend=FALSE) +
	fill_opts +
	ggplot2::guides(fill="colourbar") +
	ggplot2::labs(fill="",title=paste0("Cluster Estimates (K-Means Method)")) +
	ggplot2::theme_bw()

	ggplot2::ggsave(paste0("learnfin_plots/kmeans_method_cluster_estimate_pricing_error.png"),the_plot,width=12,height=5)

	the_plot<-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Training"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	    ggplot2::geom_point(ggplot2::aes(color=prediction_error),alpha=0.7,size=2) +
		ggplot2::labs(title="Training Data Prediction Error (K-Means Method)",color="APE") +
		plot_theme_opts +
		ggplot2::theme_bw()

	ggplot2::ggsave("learnfin_plots/kmeans_method_training_prediction_error.png",the_plot,width=12,height=5)

	the_plot<-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Prediction"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	    ggplot2::geom_point(ggplot2::aes(color=prediction_error),alpha=0.7,size=2) +
		ggplot2::labs(title="Prediction Data Prediction Error (K-Means Method)",color="APE") +
		plot_theme_opts +
		ggplot2::theme_bw()

	ggplot2::ggsave("learnfin_plots/kmeans_method_prediction_prediction_error.png",the_plot,width=12,height=5)




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
manual_learn<-function(raw_data,CallPut,moneyness_breaks=c(0.899,0.94,0.97,1.00,1.03,1.06,1.101),maturity_breaks=c(0,42,126,252),export_plots=FALSE){

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
	dplyr::mutate(prediction_error=abs(pricing_error-model_estimate))
	# dplyr::mutate_(prediction_error=lazyeval::interp(~(abs(pricing_error-model_estimate)/pmax(abs(pricing_error),0.01))))

  #Plot
  	if(export_plots){
		if(!dir.exists("learnfin_plots")){
			dir.create("learnfin_plots", showWarnings = TRUE, recursive = TRUE, mode = "0777")
		}
		plot_export_manual(result_data,moneyness_breaks=moneyness_breaks,maturity_breaks=maturity_breaks)
	}

	# summary<- result_data %>% group_by(TrainPred) %>% summarise(MAPE=mean(MAPE,na.rm=TRUE)) %>% select(MAPE) %>% unlist
	# names(summary)<-c("Prediction","Training")
	# return(list(result_data=result_data,summary=summary))
  return(result_data)
}

#' @title Plot Export (Manual Method)
#'
#' @description
#' Explanatory plots for manual method.
plot_export_manual<-function(the_df,moneyness_breaks,maturity_breaks){

	#General theme options
	plot_theme_opts <- ggplot2::scale_color_gradientn(colors=c("green","yellow","red","purple","black"),values=scales::rescale(c(0,10,25,50,100),to=c(0,1),from=c(0,100)),limits=c(0,100),oob=scales::squish)

	# the_plot<-
	# ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Training"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	#     ggplot2::geom_point(ggplot2::aes(color=pricing_error),alpha=0.7,size=2) +
	#     ggplot2::geom_vline(xintercept=as.numeric(moneyness_breaks)) +
	#     ggplot2::geom_hline(yintercept=as.numeric(maturity_breaks)) +
	# 	ggplot2::labs(title="Training Data Pricing Error (Manual Method)",color="APE") +
	# 	plot_theme_opts +
	# 	ggplot2::theme_bw()
	#
	# ggplot2::ggsave("learnfin_plots/manual_method_training_pricing_error.png",the_plot,width=12,height=5)
	#
	# the_plot<-
	# ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Prediction"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	#     ggplot2::geom_point(ggplot2::aes(color=pricing_error),alpha=0.7,size=2) +
	#     ggplot2::geom_vline(xintercept=as.numeric(moneyness_breaks)) +
	#     ggplot2::geom_hline(yintercept=as.numeric(maturity_breaks)) +
	# 	ggplot2::labs(title="Prediction Data Pricing Error (Manual Method)",color="APE") +
	# 	plot_theme_opts +
	# 	ggplot2::theme_bw()
	#
	# ggplot2::ggsave("learnfin_plots/manual_method_prediction_pricing_error.png",the_plot,width=12,height=5)

	#Calculate moneyness-maturity group corners
	mon_range<-data.frame(mon_min=moneyness_breaks[-length(moneyness_breaks)],mon_max=moneyness_breaks[-1])
	mat_range<-data.frame(mat_min=maturity_breaks[-length(maturity_breaks)],mat_max=maturity_breaks[-1])

	mon_mat_range<-data.frame(mon_min=numeric(),mon_max=numeric(),mat_min=numeric(),mat_max=numeric())

	for(i in 1:nrow(mat_range)){
		suppressWarnings(mon_mat_range<-rbind(mon_mat_range,cbind(mon_range,mat_range[i,])))
	}

	mon_mat_range <-
		mon_mat_range %>%
		dplyr::mutate(mon_mid=(mon_min+mon_max)/2,mat_mid=(mat_min+mat_max)/2) %>%
		dplyr::mutate( moneyness_limits=cut(mon_mid,moneyness_breaks),
				           maturity_limits=cut(mat_mid,maturity_breaks)) %>%
		dplyr::left_join(.,the_df %>%
		dplyr::distinct(moneyness_limits,maturity_limits,model_estimate), by=c("moneyness_limits","maturity_limits"))


	the_plot<-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Training"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	#Shaded area plot
	ggplot2::geom_rect(data=mon_mat_range,ggplot2::aes(x=mon_mid,y=mat_mid,xmin=mon_min,xmax=mon_max,ymin=mat_min,ymax=mat_max,fill=model_estimate),alpha=0.25) +
	ggplot2::scale_fill_gradientn(colors=c("green","yellow","red","purple","black"),values=scales::rescale(c(0,10,25,50,100),to=c(0,1),from=c(0,100)),limits=c(0,100),oob=scales::squish,guide="none") +
	#Geom point plot
    ggplot2::geom_point(ggplot2::aes(color=pricing_error),alpha=0.75,size=2) +
    ggplot2::geom_vline(xintercept=as.numeric(moneyness_breaks)) +
    ggplot2::geom_hline(yintercept=as.numeric(maturity_breaks)) +
	ggplot2::labs(title="Training Data Pricing Errors (Manual Method)",color="") +
	plot_theme_opts +
	ggplot2::theme_bw()

	ggplot2::ggsave("learnfin_plots/manual_method_training_pricing_error.png",the_plot,width=12,height=5)

	the_plot<-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Prediction"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	#Shaded area plot
	ggplot2::geom_rect(data=mon_mat_range,ggplot2::aes(x=mon_mid,y=mat_mid,xmin=mon_min,xmax=mon_max,ymin=mat_min,ymax=mat_max,fill=model_estimate),alpha=0.25) +
	ggplot2::scale_fill_gradientn(colors=c("green","yellow","red","purple","black"),values=scales::rescale(c(0,10,25,50,100),to=c(0,1),from=c(0,100)),limits=c(0,100),oob=scales::squish,guide="none") +
	#Geom point plot
    ggplot2::geom_point(ggplot2::aes(color=pricing_error),alpha=0.75,size=2) +
    ggplot2::geom_vline(xintercept=as.numeric(moneyness_breaks)) +
    ggplot2::geom_hline(yintercept=as.numeric(maturity_breaks)) +
	ggplot2::labs(title="Prediction Data Pricing Errors (Manual Method)",color="") +
	plot_theme_opts +
	ggplot2::theme_bw()

	ggplot2::ggsave("learnfin_plots/manual_method_prediction_pricing_error.png",the_plot,width=12,height=5)

	the_plot <-
	ggplot2::ggplot() +
	ggplot2::geom_rect(data=mon_mat_range,ggplot2::aes(x=mon_mid,y=mat_mid,xmin=mon_min,xmax=mon_max,ymin=mat_min,ymax=mat_max,fill=model_estimate),alpha=0.75) +
	ggplot2::scale_fill_gradientn(colors=c("green","yellow","red","purple","black"),values=scales::rescale(c(0,10,25,50,100),to=c(0,1),from=c(0,100)),limits=c(0,100),oob=scales::squish,guide="none") +
	ggplot2::labs(fill="",title=paste0("Cluster Estimates (Manual Method)")) +
	ggplot2::theme_bw()

	ggplot2::ggsave(paste0("learnfin_plots/manual_method_cluster_estimate_pricing_error.png"),the_plot,width=12,height=5)

	the_plot<-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Training"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	    ggplot2::geom_point(ggplot2::aes(color=prediction_error),alpha=0.7,size=2) +
	    ggplot2::geom_vline(xintercept=as.numeric(moneyness_breaks)) +
	    ggplot2::geom_hline(yintercept=as.numeric(maturity_breaks)) +
		ggplot2::labs(title="Training Data Prediction Error (Manual Method)",color="APE") +
		plot_theme_opts +
		ggplot2::theme_bw()

	ggplot2::ggsave("learnfin_plots/manual_method_training_prediction_error.png",the_plot,width=12,height=5)

	the_plot<-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Prediction"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	    ggplot2::geom_point(ggplot2::aes(color=prediction_error),alpha=0.7,size=2) +
	    ggplot2::geom_vline(xintercept=as.numeric(moneyness_breaks)) +
	    ggplot2::geom_hline(yintercept=as.numeric(maturity_breaks)) +
		ggplot2::labs(title="Prediction Data Prediction Error (Manual Method)",color="APE") +
		plot_theme_opts +
		ggplot2::theme_bw()

	ggplot2::ggsave("learnfin_plots/manual_method_prediction_prediction_error.png",the_plot,width=12,height=5)


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
dm_learn<-function(raw_data,model_name="svm",CallPut="call",randseed=0,export_plots=FALSE,...){

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
    # print(unlist(the_model$info$control[1:7]))
	}else if(model_name=="dt"){
    #Decision tree
		the_model<-rpart::rpart(the_formula, data=transformed_data,subset=(transformed_data$t_or_p=="Training"),...)
	}else{
		stop("Wrong model name!")
	}

	result_data <-
    transformed_data %>%
	 	dplyr::mutate(model_estimate=predict(the_model,.)) %>%
		dplyr::mutate(prediction_error=abs(pricing_error-model_estimate))
		# dplyr::mutate_(prediction_error=lazyeval::interp(~(abs(pricing_error-model_estimate)/pmax(abs(pricing_error),0.01))))

	#Plot
	if(export_plots & model_name %in% c("dt","cit")){
  		if(!dir.exists("learnfin_plots")){
  			dir.create("learnfin_plots", showWarnings = TRUE, recursive = TRUE, mode = "0777")
  		}

		data_mesh <-
		expand.grid(seq(from=min(result_data$maturity),to=max(result_data$maturity),length.out=1001),
		seq(from=min(result_data$moneyness),to=max(result_data$moneyness),length.out=1001)) %>%
		dplyr::rename(maturity=Var1,moneyness=Var2) %>%
		dplyr::mutate(node=predict(the_model,.,type = "node")) %>%
		dplyr::mutate(model_estimate=predict(the_model,.)) %>%
		dplyr::group_by(node) %>%
		dplyr::summarise(mon_min=min(moneyness),mon_max=max(moneyness),mat_min=min(maturity),mat_max=max(maturity),model_estimate=mean(model_estimate)) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(mon_mid=(mon_min+mon_max)/2,mat_mid=(mat_min+mat_max)/2) %>%
		dplyr::tbl_df()


  		plot_export_dm(result_data,data_mesh,model_name)
  	}


	# summary<- result_data %>% group_by(TrainPred) %>% summarise(MAPE=mean(MAPE,na.rm=TRUE)) %>% select(MAPE) %>% unlist
	# names(summary)<-c("Prediction","Training")
	# return(list(result_data=result_data,summary=summary))
  return(result_data)
}

plot_export_dm<-function(the_df,data_mesh,model_name){

	plot_theme_opts <- ggplot2::scale_color_gradientn(colors=c("green","yellow","red","purple","black"),values=scales::rescale(c(0,10,25,50,100),to=c(0,1),from=c(0,100)),limits=c(0,100),oob=scales::squish)

	plot_rect<-
	ggplot2::geom_rect(data=data_mesh,ggplot2::aes(x=mon_mid,y=mat_mid,xmin=mon_min,xmax=mon_max,ymin=mat_min,ymax=mat_max,fill=model_estimate),alpha=0.25)

	fill_opts <-
	ggplot2::scale_fill_gradientn(colors=c("green","yellow","red","purple","black"),values=scales::rescale(c(0,10,25,50,100),to=c(0,1),from=c(0,100)),limits=c(0,100),oob=scales::squish,guide="none")

	the_plot <-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Training"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	plot_rect +
	fill_opts+
	ggplot2::geom_point(ggplot2::aes(color=pricing_error),alpha=0.75,size=2) +
	ggplot2::labs(title=paste0("Training Data Pricing Errors (",toupper(model_name)," Method)"),color="APE") +
	plot_theme_opts +
	ggplot2::theme_bw()

	ggplot2::ggsave(paste0("learnfin_plots/",model_name,"_method_training_pricing_error.png"),the_plot,width=12,height=5)

	the_plot <-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(pricing_error) %>% dplyr::filter(t_or_p=="Prediction"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	plot_rect +
	fill_opts+
	ggplot2::geom_point(ggplot2::aes(color=pricing_error),alpha=0.75,size=2) +
	ggplot2::labs(title=paste0("Prediction Data Pricing Errors (",toupper(model_name)," Method)"),color="APE") +
	plot_theme_opts +
	ggplot2::theme_bw()

	ggplot2::ggsave(paste0("learnfin_plots/",model_name,"_method_prediction_pricing_error.png"),the_plot,width=12,height=5)

	the_plot<-
	ggplot2::ggplot() + 	ggplot2::geom_rect(data=data_mesh,ggplot2::aes(x=mon_mid,y=mat_mid,xmin=mon_min,xmax=mon_max,ymin=mat_min,ymax=mat_max,fill=model_estimate),alpha=0.75) +
	fill_opts +
	ggplot2::guides(fill="colourbar") +
	ggplot2::labs(fill="",title=paste0("Cluster Estimates (",toupper(model_name)," Method)")) +
	ggplot2::theme_bw()

	ggplot2::ggsave(paste0("learnfin_plots/",model_name,"_method_cluster_estimate_pricing_error.png"),the_plot,width=12,height=5)

	the_plot<-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(desc(pricing_error)) %>% dplyr::filter(t_or_p=="Training"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	    ggplot2::geom_point(ggplot2::aes(color=prediction_error),alpha=0.7,size=2) +
		ggplot2::labs(title=paste0("Training Data Prediction Error (",toupper(model_name)," Method)"),color="APE") +
		plot_theme_opts +
		ggplot2::theme_bw()

	ggplot2::ggsave(paste0("learnfin_plots/",model_name,"_method_training_prediction_error.png"),the_plot,width=12,height=5)

	the_plot<-
	ggplot2::ggplot(data=the_df %>% dplyr::arrange(desc(pricing_error)) %>% dplyr::filter(t_or_p=="Prediction"),mapping=ggplot2::aes(x=moneyness,y=maturity)) +
	    ggplot2::geom_point(ggplot2::aes(color=prediction_error),alpha=0.7,size=2) +
		ggplot2::labs(title=paste0("Prediction Data Prediction Error (",toupper(model_name)," Method)"),color="APE") +
		plot_theme_opts +
		ggplot2::theme_bw()

	ggplot2::ggsave(paste0("learnfin_plots/",model_name,"_method_prediction_prediction_error.png"),the_plot,width=12,height=5)

}
