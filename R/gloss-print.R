#' print_selected_time()
#'
#' Prints annotations from a given time
#'
#' @param data data frame that contains the data from the ELAN file
#' @param start_annotation starting time in seconds to include
#' @param end_annotation end time of the annotations to include
#'
#' @return a dataframe with the selected annotations
#'
#' @examples
#' start_annotation = 120
#' end_annotation= 200
#' print_selected_time(data,start_annotation,end_annotation)
#'
#' @import dplyr
#' @import tidyverse
#' @export

print_selected_time <- function(data,start_annotation,end_annotation){
  start_annotation <- as.numeric(start_annotation)
  end_annotation <- as.numeric(end_annotation)
  data$annotation <- as.character(data$annotation)
  annotations_selected <- data %>% filter(start_time_s >= start_annotation & end_time_s <= end_annotation)
  annotations_selected <- annotations_selected[order(annotations_selected$start_time_s),]
  return(annotations_selected)
}

  gloss <- function(dataframe){
    dataframe <- as_tibble(dataframe)
    for (i in 1:nrow(dataframe)){
  ex <- paste(c("\\begingl",
                paste(c("\\gla", " ", dataframe[[i,"gla"]], "//"), collapse = ""),
                paste(c("\\glb", " ", dataframe[[i,"glb"]], "//"), collapse = ""),
                paste(c("\\glft", " ", "`", dataframe[[i,"glft"]], "'", "//"), collapse = ""),
                "\\endgl"), sep = "\n  ", collapse = "\n  ")
  #asis_output(ex)
  print(ex)
  }}

#' print_all_words()
#'
#' Prints annotations that contain a certain regex sequence
#'
#' @param data data frame that contains the data from the ELAN file
#' @param word word or regular expression that are searching for
#'
#' @return a dataframe of all annotations with the selected search item
#'
#' @examples
#' word = "string"
#' print_all_words(data,word)
#'
#' @import dplyr
#' @export

print_all_words <- function(data,word){
  word <- as.character(word)
  annotations_selected <- filter(data, grepl(word,annotations,ignore.case=T))
  # print(cat(paste(toupper(annotations_selected$participant_name),';\t\\begingl\\gla ',annotations_selected$annotations,'//\\endgl\n',sep="")))
  #return(annotations_selected)
  return(cat(paste(toupper(annotations_selected$participant_name),';\t\\begingl \\gla ',annotations_selected$annotations,'//\\endgl\n',sep="")))
}
