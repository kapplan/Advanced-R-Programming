# Name: Basak Kaplan
# Student ID: 124084

# Libraries
library(tibble)
library(nnet)

# Define the createFitter function
createFitter <- function(size) {
  # The returned function (fitter)
  function(formula, data) {
    # Check if data is data.frame
    if (!inherits(data, "data.frame")) {
      stop("data must be a data frame or an object that inherits from data.frame")
    }
    
    # Ensure the formula is evaluated in the correct environment
    formula <- as.formula(formula)
    
    # Extract the target and feature variables
    target <- all.vars(formula)[1]
    features <- all.vars(formula)[-1]
    
    # Check if the target and features exist in the data
    if (!all(c(target, features) %in% names(data))) {
      stop("All variables in the formula must exist in the data")
    }
    
    # Create the neural network 
    model <- nnet::nnet(formula, data = data, size = size, linout = TRUE, maxit = 100, trace = TRUE)
    
    # Return the predictor function
    function(newData) {
      # Check if newData is a data frame
      if (!inherits(newData, "data.frame")) {
        stop("newData must be a data frame or an object that inherits from data.frame")
      }
      
      predictions <- predict(model, newData)
      newData[[paste0(target, "_pred")]] <- predictions
      return(newData)
    }
  }
}

# Lines method for data.frame class
lines.data.frame <- function(x, col = "black", lwd = 1, ...) {
  if (!"x" %in% names(x) || !"y_pred" %in% names(x)) {
    stop("Data frame must contain columns named 'x' and 'y_pred'")
  }
  graphics::lines(x$x, x$y_pred, col = col, lwd = lwd, ...)
}
setGeneric("lines")
setMethod("lines", "data.frame", lines.data.frame)

# Example 1
# Creating a simple data set - Training set
d <- tibble(
  x = rnorm(10^3),
  y = x^2 + 3 * cos(x) + rnorm(10^3, sd = 0.4)
)

# Creating the test dataset
dTest <- tibble(
  x = seq(
    from = min(d$x),
    to = max(d$x),
    length.out = 100
  )
)

# Creating a fitter with 10 neurons in the hidden layer
fitter <- createFitter(size = 10)

# Creating predictor function
predictor <- fitter(formula = y ~ x, data = d)

# Predicting new values
dTest_pred <- predictor(newData = dTest)

# Visualizing results
#x11()
plot(d, pch = 20,
     col = rgb(1, 0, .5, 0.2),
     xlab = "feature", ylab = "target") 
grid(lty = "solid", col = "lightgray")
lines(x = predictor(newData = dTest), col = rgb(.5, 0, 1, .9),
      lwd = 2)



# Example 2
### Creating the training dataset
d <- tibble(
  x = rnorm(10^3),
  y = x^2 + 3 * cos(x) + rnorm(10^3, sd = 0.4)
)
### Creating the test dataset
dTest <- tibble( x = seq(
  from = min(d$x),
  to = max(d$x),
  length.out = 100)) ## features

### Creating a fitter. Here, we use only 3 neurons in the hidden ### layer.
fitter <- createFitter(size = 3)

### Creating predictor.
predictor <- fitter(formula = y ~ x, data = d)

### Visualizing results
#x11()
plot(d, pch = 20,
     col = rgb(1, 0, .5, 0.2),
     xlab = "feature", ylab = "target") 
grid(lty = "solid", col = "lightgray")
lines(x = predictor(newData = dTest), col = rgb(.5, 0, 1, .9),
      lwd = 2)


# Example 3 
# Creating two fitters for small and large networks
### Creating a training dataset
d1 <- tibble(
  x = rnorm(10^3, mean = -5),
  y = (x + 5)^2 + 3 * cos(x) + rnorm(10^3, sd = 0.4) )
d2 <- tibble(
  x = rnorm(10^3, mean = 5),
  y = (x - 5)^2 + 3 * cos(x) + rnorm(10^3, sd = 0.4) )
d <- rbind(d1, d2)

### Creating a test dataset
dTest <- tibble( x = seq(
  from = min(d$x),
  to = max(d$x),
  length.out = 100)) ## features

### Creating two fitters for small and large networks
fitter3 <- createFitter(size = 3)
fitter20 <- createFitter(size = 20)

### Creating predictors
predictor3 <- fitter3(formula = y ~ x, data = d) 
predictor20 <- fitter20(formula = y ~ x, data = d)

### Visualizing results
#x11()
plot(d, pch = 20,
     col = rgb(1, 0, .5, 0.2),
     xlab = "feature", ylab = "target") 
grid(lty = "solid", col = "lightgray")
lines(x = predictor3(newData = dTest), col = rgb(.5, 0, 1, .9),
      lwd = 2)
lines(x = predictor20(newData = dTest), col = rgb(0, .5, 1, .9),
      lwd = 2)
