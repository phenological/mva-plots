#' Plot input for ggpairs objects.
#'
#' This function handles the shape (SH), size (SZ) and alpha (AL) input for the ggpairs and ggplot objects. Each may be either a numeric or a metadata variable such as age, gender, bmi etc.
#' @param CO Colour.
#' @param SZ Size.
#' @param AL Alpha.
#' @param SH Shape.
#' @return A list of objects for ggplot and ggpairs objects.


plotInput<- function(screeCumulativeThresholdObject, CO, SH, SZ, AL){

  output <- screeCumulativeThresholdObject

  #colour, use is(CO)[1]=="numeric" instead?
  CO <- if(class(CO) == "numeric") {
          output$data$pcdf$CO = rep_len(CO, nrow(output$data$pcdf))
          CO = output$data$pcdf$CO
        }else if(class(CO) == "character"){
          CO = output$data$pcdf[,CO]
        }
  #size
  SZ <- if(class(SZ) == "numeric") {
          SZ = SZ
        }else if (class(SZ) == "character"){
          SZ = output$data$pcdf[,SZ]
        }

  #alpha
  AL <- if(class(AL) == "numeric") {
          AL = AL
        }else if (class(AL) == "character"){
          AL = output$data$pcdf[,AL]
        }

  #shape
  SH <- if(class(SH) == "NULL") {
          output$data$pcdf$SH = rep_len("circle", nrow(output$data$pcdf))
          SH = output$data$pcdf$SH
        }else if(class(SH) == "character"){
          SH = output$data$pcdf[,SH]
        }

  output <- append(output, list(CO = CO,
                                SH = SH,
                                SZ = SZ,
                                AL = AL))

  return(output)
}
# plotinput<- function(screecumulativethresholdobject, CO, SH, SZ, AL){
#
#   output <- screecumulativethresholdobject
#
#   # Get required data for plotting
#   #colour
#   CO <- if(class(CO) == "numeric") {
#     output$data$pcdf$CO = rep_len(CO, nrow(output$data$pcdf))
#     CO = output$data$pcdf[,CO]
#   }else if(class(CO) == "character"){
#     CO = output$data$pcdf[,CO]
#   }
#
#   # if(class(CO) == "numeric"){
#   #   output$data$pcdf$CO = rep_len(CO, nrow(output$data$pcdf))
#   # }
#   # CO = output$data$pcdf[,CO]
#
#   #size
#   SZ <- if(class(SZ) == "numeric") {
#     output$data$pcdf$SZ = rep_len(SZ, nrow(output$data$pcdf))
#     SZ = output$data$pcdf[,SZ]
#   }else if (class(SZ) == "character"){
#     SZ = output$data$pcdf[,SZ]
#   }
#
#   #alpha
#   AL <- if(class(AL) == "numeric") {
#     output$data$pcdf$AL = rep_len(AL, nrow(output$data$pcdf))
#     AL = output$data$pcdf[,AL]
#   }else if (class(AL) == "character"){
#     AL = output$data$pcdf[,AL]
#   }
#
#   #shape
#   SH <- if(class(SH) == "NULL") {
#     #output$data$pcdf$SH = rep_len(1, nrow(output$data$pcdf))
#     SH = 1
#   }else if(class(SH) == "character"){
#     SH = output$data$pcdf[,SH]
#   }
#
#   output<- append(output, list(CO=CO,
#                               SH = SH,
#                               SZ = SZ,
#                               AL = AL))
#
# return(output)
# }
