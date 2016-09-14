#These are the main functions

#' @importFrom dplyr "%>%"
NULL
# #' @export
# abcdo_something<-function(the_df){
#   the_return<-
#   the_df %>% dplyr::select(date)
#   return(the_return)
# }
#' @title Run Full Experiment
#'
#' @description This function helps to automate a series of experiments with predetermined parameters sets with the help of an xlsx file. An example file is given with the package on the GitHub page. Experiment results can be exported to RData and xlsx files with convenient file names.
#' @param data_set Name of the data frame to be used in quotes (not as an object).
#' @param error_type Pricing error type. It can be ARPE, RPE, SE or APE.
#' @param method Data mining method. It can be svm, cit, dt, kmeans or manual.
#' @param path_name Path of the input parameter excel file. (Default: working directory \code{getwd()})
#' @param file_name Name of the input parameter excel file.
#' @param randseed Randomness seed for reproducibility.
#' @param verbal_feedback Do you want to be informed during the progress? (Useful for long runs.)
#' @param export_raw_results Should resulting data frames be exported as RData files?
#' @param export_folder Result raw file export folder.
#' @param summary_to_excel Should prediction error summaries automatically be exported to excel files?
#' @export
run_full_experiment<-function(data_set="uslfin_ds_1",error_type="ARPE",method="svm",path_name=getwd(),file_name="input_parameters.xlsx",randseed=0,verbal_feedback=TRUE,export_raw_results=FALSE,export_folder="results",summary_to_excel=FALSE){

      if(export_raw_results){
        export_path<-paste0(path_name,"/",export_folder,"/")
        if(!dir.exists(export_path)){
          if(verbal_feedback){
            print("Creating export directory for the data sets.")
          }
          dir.create(export_path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        }
      }

      the_df<-eval(as.name(data_set))

      the_df <-
      the_df %>%
      #Calculate pricing error according to the pricing error at hand
      dplyr::mutate(pricing_error=calculate_pricing_error(market_price,model_price,error_type=error_type)) %>%
      dplyr::tbl_df()

    the_parameters <-
      readxl::read_excel(paste0(path_name,"/",file_name),sheet=method) %>%
      tidyr::gather(.,parameter,value,-model_num) %>%
      filter(!is.na(value))

    parametrizations<-unique(the_parameters$model_num)

    if(verbal_feedback){
      print(paste0("There are ",length(parametrizations)," different parametrizations in this experimental set."))
    }

    the_summary <-  data.frame(parameter_set=character(),type=character(),t_or_p=character(),contracts=numeric(),mean_error=numeric(),sd_error=numeric(),quantile_25=numeric(),median_error=numeric(),quantile_75=numeric())

    for(i in parametrizations){
      if(verbal_feedback){
        print(paste0("Running ",method," parametrization ",i," ..."))
      }

      individual_parameters <-
        the_parameters %>% dplyr::filter(model_num==i) %>% tidyr::spread(parameter,value) %>% dplyr::select(-model_num) %>% unlist()

      if(method %in% c("svm","dt","cit")){
        result_table<-do.call("dm_learn",c(list(raw_data=the_df,model_name=method,CallPut="call",randseed=randseed),as.list(individual_parameters)))
        result_table <- rbind(result_table,do.call("dm_learn",c(list(raw_data=the_df,model_name=method,CallPut="put",randseed=randseed),as.list(individual_parameters))))
      }else if(method == "kmeans"){
        if(is.na(individual_parameters["n_cluster"])){
          individual_parameters["n_cluster"] <- 0L
        }
        result_table<-kmeans_learn(raw_data=the_df,CallPut="call",randseed=randseed,moneyness_interval=c(0.9,1.1),maturity_interval=c(4,252),n_cluster=individual_parameters["n_cluster"])
        result_table<-rbind(result_table,kmeans_learn(raw_data=the_df,CallPut="put",randseed=randseed,moneyness_interval=c(0.9,1.1),maturity_interval=c(4,252),n_cluster=individual_parameters["n_cluster"]))
      }else if(method=="manual"){
        result_table<-manual_learn(raw_data=the_df,CallPut="call",moneyness_breaks=sort(individual_parameters[grepl("moneyness",names(individual_parameters))]),maturity_breaks=sort(individual_parameters[grepl("maturity",names(individual_parameters))]))
        result_table<-rbind(result_table,manual_learn(raw_data=the_df,CallPut="put",moneyness_breaks=sort(individual_parameters[grepl("moneyness",names(individual_parameters))]),maturity_breaks=sort(individual_parameters[grepl("maturity",names(individual_parameters))])))
      }else{
        stop("Wrong method value")
      }
      if(export_raw_results){
        save(result_table,file=paste0(export_path,data_set,"_",error_type,"_",method,"_with_parameter_set_",i,".RData"))
      }
      the_summary<-
        result_table %>%
        dplyr::group_by(type,t_or_p) %>%
        dplyr::summarise(contracts=n(),mean_error=mean(prediction_error),sd_error=sd(prediction_error),quantile_25=quantile(prediction_error,0.25),median_error=median(prediction_error),quantile_75=quantile(prediction_error,0.75)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(parameter_set=i) %>%
        dplyr::select(parameter_set,dplyr::everything()) %>%
        rbind(the_summary,.)

      the_summary <-
        result_table %>%
        dplyr::summarise(contracts=n(),mean_error=mean(prediction_error),sd_error=sd(prediction_error),quantile_25=quantile(prediction_error,0.25),median_error=median(prediction_error),quantile_75=quantile(prediction_error,0.75)) %>%
        dplyr::mutate(parameter_set=i,type="all",t_or_p="all") %>%
        dplyr::select(parameter_set,type,t_or_p,dplyr::everything()) %>%
        rbind(the_summary,.)
    }
    if(summary_to_excel){
      xlsx::write.xlsx2(data.frame(the_summary),path.expand(paste0(path_name,"/",data_set,"_",error_type,"_",method,"_method_","results_summary.xlsx")),row.names=FALSE)
    }
    return(the_summary)
}
