require(ggplot2)
require(grid)

# Define a function which creates a frequency table for every categorical and logical variable.
# The parameter is a dataframe.
frequency_table <- function(data) {
  lapply(data[, sapply(data,is.factor)], table) # Check if the variable is categorical or logical then print their frequency table. 
}

# Define a function which creates a summary statistics table for each numerical variable.
# The parameter is a dataframe.
printSummary <-function(data){
  lapply(data[, sapply(data,is.numeric)], summary) # Check if the variable is numeric then print their summary statistics.
}

# Define a function that returns a dataframe that contains each pair of column names in the first column
# and the associated r-square value in the second column.
# The parameter is a dataframe.
R_squared <- function(data){
  num <- sapply(data, is.numeric) # Check the numeric columns of the dataframe and assign them to a logical vector num.
  new_data <- data[,num] # Create a new dataframe with only numeric vectors.
  names <- colnames(new_data) # Assign the vector of column names of the new dataframe to a new vector names
  combonames <- combn(names, 2) # Assign all the combination of any 2 column names (namely n choose 2) to combonames
  combo <- combn(length(colnames(new_data)), 2)# Assign all the combination of column names' indices to combo
  VariablePairs <- paste(combonames[1,], combonames[2,], sep = '-') # Assign the pairs of column names linked with "-" to a new vector variable
  Rsquared <- c() # Assign an empty vector to the column of R square 
  
  for(i in 1:length(VariablePairs)){ # Loop for each element in variable
    linear <- lm(new_data[,combo[1,i]] ~ new_data[,combo[2,i]], data=data) # Create the linear regression model for each combination of columns.
    r2 <- summary(linear)$r.squared # Assign the r squared value acquired from each linear regression model to a vector r2.
    Rsquared[i] <- r2[1] # Assign the desired r squared value from r2 to the vector of Rsquared.
  }
  return(data.frame(VariablePairs, Rsquared)) # Outputs the dataframe containing pairs of column names and their R square value.
}

# Define a function that returns a dataframe that contains each pair of column names in the first column in a single string separated by a -,
# and their corresponding Pearson correlation coefficient in the second column.
# The parameter is a dataframe.
Pearson <- function(data){
  num <- sapply(data, is.numeric) # Check the numeric columns of the dataframe and assign them to a logical vector num.
  new_data <- data[,num] # Create a new dataframe with only numeric vectors.
  names <- colnames(new_data) # Assign the vector of column names of the new dataframe to a new vector names
  combonames <- combn(names, 2) # Assign the combination of vector names (here we used n choose 2, which give the combination of choosing 2 elements from the vector names) to combonames
  combo <- combn(length(colnames(new_data)), 2) # Assign the similar combination as the above line to the length of the vector colnames(new_data), namely the vector names
  variable <- paste(combonames[1,], combonames[2,], sep = '-') # Assign the pairs of column names linked with "-" to a new vector variable
  Pcorcoeff <- c() # Assign an empty vector to the column of Pearson correlation coefficient
  
  for(i in 1:length(variable)){ # Loop for each element in variable
    p <- cor(x= new_data[combo[1,i]], y = new_data[combo[2,i]]) # Assign the correlation of pairs of column to p.
    Pcorcoeff[i] <- p[1] # Assign p with the desired Pearson correlation coefficient to the vector of Pcorcoeff.
  }
  return(data.frame(variable, Pcorcoeff)) # Outputs the dataframe containing pairs of column names and their Pearson correlation coefficient
}

# Define a function that takes a dataframe containing pairs of Pearson correlation coefficients of 2 variables and
# return the pairs with absolute values greater than the threshold.
# The parameter is a dataframe of pairs of Pearson correlation coefficients and a value of threshold.
abs_pearson <- function(dataset, threshold){
  row_index <- which(abs(dataset[,2]) > threshold) # Assign the columns with absolute value greater than the threshold to row_index.
  return(dataset[row_index, ]) # Return a new dataframe with the pairs of coefficients whose absolute value is greater than the threshold.
}

# This is a function that combines multiple plots. We will use it to plot the combination of plots later.
# It is acquired from the following webpage: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Define a function that: 
# If the plot switch parameter is "on" or "grid", then plot a pair of blue histograms with a vertical red line at the mean (one using counts and the other density)
# for every numerical variable at each number of bins integer specified in the bin vector parameter. 
# If the plot switch is set to "grid", then the function prints a grid for each count-bin combination and a separate grid for each density-bin size combination.
# The parameter is a dataframe, a string with values of "off", "on", or "grid", and a vector containing bin sizes.
numeric_plot <- function(data, plot_switch, binVec) {
  num <- sapply(data, is.numeric)         #check to see which column is numeric
  data <- data[,num]                      #extract numeric columns
  for(name in colnames(data)) {           #loop through the columns in the dataset
    
    if(plot_switch == "on"){              #Case when switch is "on"
      grid.newpage()          
      m <- lapply(data[name], mean)       #find the mean of that currently iterated column
      plot1 <- ggplot(data, aes_string(name)) + geom_histogram(fill="blue") + geom_vline(xintercept = m[[1]], colour="red") 
      plot2 <- ggplot(data, aes_string(name)) + geom_histogram(aes(y= ..density..), fill="blue") + geom_vline(xintercept = m[[1]], colour="red")
      #multiplot(plot1, plot2, cols = 1)
      pushViewport(viewport(layout = grid.layout(1, 2)))
      print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    }
    
    if(plot_switch == "grid"){          #Case when switch is "grid"
      count_plots <- list()             #Create a empty list to store the count histogram subplots of each bin size
      density_plots <- list()           #Create a empty list to store the density histograms subplots of each bin size
      if(missing(binVec)){              #This takes of the case when the vector is null, prints histogram with default bins 30
        print(ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue")+ labs(title= "default bins"))
        print(ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue")+ labs(title= "default bins"))
      }else{                            #This takes care of the case when the user enters a vector
        for(i in 1:length(binVec)) {    #loop through each bin size and create a subplot
          k <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue", bins = binVec[i])+ labs(title= paste(binVec[i], "bins"))
          count_plots[[i]] <- k           #Push each subplot to a list 
        }
        multiplot(plotlist = count_plots, cols = 2)     
        
        for(i in 1:length(binVec)) {    #loop through each bin size and create a subplot
          k <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue", bins = binVec[i])+ labs(title= paste(binVec[i], "bins"))
          density_plots[[i]] <- k       #Push each subplot to a list
        }
        multiplot(plotlist = density_plots, cols = 2)
        
      }
    }
  }
}

# Define a function that checks if a vector is binary
# The parameter is a vector
is.binary <- function(vect) {
  x <- unique(vect)                    #check all the distinct and put those in a vector x
  length(x) - sum(is.na(x)) == 2L         #check to see if x only contains 2 distinct values
}


# Define a function that plots a gray bar graph for every categorical and binary variable.
# when the plot switch parameter is "on" or "grid"
# The parameter is a dataframe and a string with values of "off", "on", or "grid"
cata_binary_plot <-function(data, plot_switch){
  cata_binary <- sapply(data, function(x) (is.factor(x) || is.logical(x)) || is.binary(x))    #check categorical and binary	columns
  cata_binary_data <- data[cata_binary]     #extract those columns
  
  if(plot_switch == "on" || plot_switch == "grid") {      #check condition
    for(name in colnames(cata_binary_data)) {             #loop through the sorted dataframe and plot bar graphs for each column
      j <- ggplot(cata_binary_data, aes_string(name), color = "grey") + geom_bar(fill="grey")
      print(j)
    }
  }
}


################## Main Function ###################

explore <- function(dataframe, plot_switch, threshold, binVec){
  
  Freq_table <- frequency_table(dataframe)
  Numeric_Summary <- printSummary(dataframe)
  Coeff_table <- Pearson(dataframe)
  AbsCoeff_table <-abs_pearson(Coeff_table, threshold)
  Rsquare_table <- R_squared(dataframe)
  numeric_plot(dataframe, plot_switch, binVec)
  cata_binary_plot(dataframe, plot_switch)
  new_list <-list(Freq_table, Numeric_Summary, Rsquare_table, AbsCoeff_table)
  return(new_list)
  
  
  
}