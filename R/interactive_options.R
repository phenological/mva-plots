#make the PCAgrid plot interactive

interactive_options<-function(interactive){ if(interactive == FALSE){
                      plot<-(gpairs_lower(P))
                        }else if (interactive == TRUE){
                            plot<-ggplotly(gpairs_lower(P))
                          }
                    }
