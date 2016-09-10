#Below functions are supplementary functions used in uslfin package

#' @export
calculate_pricing_error<-function(mar_price,mod_price,error_type){
  if(error_type=="ARPE"){
    #Absolute Relative Pricing Error
    return(abs((mod_price-mar_price)/mar_price))
  }
  if(error_type=="RPE"){
    #Relative Pricing Error
    return((mod_price-mar_price)/mar_price)
  }
  if(error_type=="APE"){
    #Absolute Pricing Error
    return(abs(mod_price-mar_price))
  }
  if(error_type=="SE"){
    #Squared Error
    return((mod_price-mar_price)^2)
  }
  #If error type is incorrect show the warning
  stop("Incorrect error type")
}

#' @export
rescale_error<-function(errors,mederror=NA){
	if(is.na(mederror)){
    #If there is no predetermined medium error value, use median errors to scale the errors.
		ifelse(errors <= median(errors),(errors-min(errors))/(median(errors)-min(errors))*0.5,(errors-median(errors))/(max(errors)-median(errors))*0.5+0.5)
	}else{
		ifelse(errors <= mederror,(errors-min(errors))/(mederror-min(errors))*0.5,(errors-mederror)/(max(errors)-mederror)*0.5+0.5)
	}
}

#' @export
get_the_cluster<-function(Mon_sc,Mat_sc,center_matrix){

	which.min(colSums((t(center_matrix)-c(Mon_sc,Mat_sc))^2))

}
