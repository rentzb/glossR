#' elan.annotations
#'
#' Takes an ELAN .eaf transcription in XML format and saves it as a dataframe
#'
#' @param data the name of the .eaf file to be converted to a dataframe
#'
#' @return a dataframe with the ELAN annotations, participants, start time, and end time
#'
#' @examples
#' example <- elan.annotations("data.eaf")
#'
#' @import xml2
#' @import dplyr
#' @import rvest
#'
#' @export

elan.annotations <- function(data){
  data1 <- read_xml(data)
  data.nodes <- xml_nodes(data1,"ANNOTATION_VALUE")
  #print(data.nodes)
  #xml_text(data.nodes)

  data.tiers <- xml_find_all(data1,".//TIER")
  #data.tier.list <- as_list(data.tiers)
  #data.tiers[[]]
  data.participants <- as.matrix(xml_attr(data.tiers,"PARTICIPANT"))
  data.participants.length <- length(data.participants)
  # for loop to select all the participants

  df_total = data.frame()
  df2 = data.frame()


  for (participant in 1:data.participants.length){

    data.tier.use = data.tiers[[participant]]
    data.tier.use.children = xml_children(data.tier.use)
    data.tier.use.children.children = xml_children(data.tier.use.children)
    data.tier.use.children.length= length(as.matrix(data.tier.use.children))
    # print(data.tier.use.children.length)
    annotations = xml_text(data.tier.use.children,"ANNOTATION")

    # print(annotations)
    length.annotations = length(annotations)
    # df.participants <- data.frame(data.participants[participant])


    for (annotation in 1:length.annotations){
      # annotationlist[[annotation]]<-annotations[annotation]
      # need way to include the participant too
      # participantlist[[participant]] <- participant
      start_time_code <- xml_attr(data.tier.use.children.children[annotation],"TIME_SLOT_REF1")
      end_time_code <- xml_attr(data.tier.use.children.children[annotation],"TIME_SLOT_REF2")
      df <- data.frame(annotations[annotation])

      #df2 <- data.frame(start_time_code)
      # df3 <- data.frame(end_time_code)
      df4 <- cbind(df,participant,start_time_code,end_time_code)
      #df5 <- cbind(df2,df3)
      #df2 <- data.frame(data.participants[participant])
      df_total <- rbind(df_total,df4)
      # df_total <- rbind(df_total,participant)

    }
  }

  df.names <- data.frame(cbind(1:data.participants.length,data.participants))
  df.names$X1 <- as.integer(df.names$X1)
  df.names$X2 <- as.factor(df.names$X2)
  df_total <- df_total %>% left_join(df.names,c("participant" = "X1"))
  names(df_total)[names(df_total) == 'X2'] <- 'participant_name'
  #df_total$participant_name <- ifelse(df_total$participant==df.names$X1,df.names$X2,NA)

  ## now get time codes
  data.time.order <- xml_find_all(data1,".//TIME_ORDER")
  data.time.order.time_slot <- xml_children(data.time.order)
  data.time.order.time_slot.length <- length(as.matrix(data.time.order.time_slot))
  time_df = data.frame()
  # loop through all them
  for (time in 1:data.time.order.time_slot.length){
    time_slot_id = xml_attr(data.time.order.time_slot[time],"TIME_SLOT_ID")
    time_value = xml_attr(data.time.order.time_slot[time],"TIME_VALUE")
    time_df1 = data.frame(cbind(time_slot_id,time_value))
    time_df = rbind(time_df,time_df1)
  }
  time_df$time_slot_id <- as.character(time_df$time_slot_id)
  time_df$time_value <- as.character(time_df$time_value)
  df_total$start_time_code <- as.character(df_total$start_time_code)
  df_total$end_time_code <- as.character(df_total$end_time_code)
  df_total <- df_total %>% left_join(time_df,c("start_time_code"="time_slot_id"))
  names(df_total)[names(df_total) == 'time_value'] <- 'start_time_ms'
  df_total <- df_total %>% left_join(time_df,c("end_time_code"="time_slot_id"))
  names(df_total)[names(df_total) == 'time_value'] <- 'end_time_ms'
  names(df_total)[names(df_total) == 'annotations.annotation.'] <- 'annotations'
  df_total$start_time_s <- as.numeric(df_total$start_time_ms)/1000
  df_total$end_time_s <- as.numeric(df_total$end_time_ms)/1000
  return(df_total)
}
