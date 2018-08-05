# function to review basic properties of the dataset
basic_features <- function( d, html=html_flip, ... ){
  # browser()
  cat("Rows = ",scales::comma(dim(d)[1]), "\nCols = ", scales::comma(dim(d)[2]))
  cat("\nObject Size: ")
  pryr::object_size(d) %>% print()
  cat("\n")
  d1 <- d %>% summarize_all(n_distinct) %>% t()
  # str(d1)
  d2 <- data.frame(
    "column_name"     = attr(d1,"dimnames")[[1]],
    "n_unique_values" = scales::comma(d1[,1])
  ) %>% 
    dplyr::select(n_unique_values, column_name) 
  d2 <- d2 %>% neat( html=html, ... )
  return(d2)
}#Usage:
# ds %>% basic_features
# dto[["cmg"]] %>% basic_features(format="pandoc")


# function to convert encoding to ASCII
convert_to_ascii <- function( x ) {
  iconv(x, "latin1", "ASCII//TRANSLIT")
}


# Some variables have different character codes for missing values
# Translate various character values into NA values
replace_with_na <- function(x){
  # x <- ds_location_map$facility_name
  na_tokens <- c(
    "^NULL$"
    ,"^-$"
    ,"^NA$"
    ,"^\\{blank\\}$"
    ,"^n/a$"
    ,"\\{Unknown\\}"
    ,"\\{Undefined\\}"
    ,"\\{No Event Detail\\}"
  )
  for(token in na_tokens){
    if(is.character(x)){
      x <- gsub(token,NA,x)
    }
  }
  return(x)
}
# Usage:
# ds_patient_profiles <- ds_patient_profiles %>% 
  # dplyr::mutate_all(dplyr::funs(replace_with_na) )

