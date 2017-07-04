# Plot SOM Heatmap for variable 
#
# Shane Lynn 13/1/2014

# this function is to plot the heatmap of a variable

plotHeatMap <- function(som_model, data, variable=0){    
  # Plot a heatmap for any variable from the data set "data".
  # If variable is 0, an interactive window will be provided to choose the variable.
  # If not, the variable in "variable" will be plotted.
  
  require(dummies)
  require(kohonen)
  source('coolBlueHotRed.R')
  
  interactive <- TRUE
  
  while (interactive == TRUE){
    
    if (variable == 0){
      #show interactive window.
      color_by_var <- select.list(names(data), multiple=FALSE,
                                  graphics=TRUE, 
                                  title="Choose variable to color map by.")
      # check for user finished.
      if (color_by_var == ""){ # if user presses Cancel - we quit function        
        return(TRUE)
      }
      interactive <- TRUE
      color_variable <- data.frame(data[, color_by_var])
          
    } else {
      color_variable <- data.frame(data[, variable])
      color_by_var <- names(data)[variable]
      interactive <- FALSE
    }
      
    #if the variable chosen is a string or factor - 
    #Get the levels and ask the user to choose which one they'd like.
    
    if (class(color_variable[,1]) %in% c("character", "factor", "logical")){
      #want to spread this out into dummy factors - but colour by one of those.
      temp_data <- dummy.data.frame(color_variable, sep="_")
      chosen_factor <- select.list(names(temp_data), 
                                   multiple=FALSE,
                                   graphics=TRUE, 
                                   title="Choose level of variable for colouring")
      color_variable <- temp_data[, chosen_factor]
      rm(temp_data, chosen_factor)
      color_by <- color_variable
    } else {      
      #impute the missing values with the mean.
      color_variable[is.na(color_variable[,1]),1] <- mean(color_variable[,1], na.rm=TRUE)
      #color_by <- capVector(color_variable[,1])
      #color_by <- scale(color_by)  
      color_by <- color_variable[,1]
    }
    unit_colors <- aggregate(color_by, by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)
    plot(som_model, type = "property", property=unit_colors[,2], main=color_by_var, palette.name=coolBlueHotRed)    
  }
}