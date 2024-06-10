# Name: Basak Kaplan
# Student ID: 124084

library(R6)
library(zoo)
library(xts)
library(ggplot2)
library(tibble)
library(R6)
library(zoo)
library(xts)
library(ggplot2)
library(tibble)

timeSeries <- R6Class("timeSeries",
                      private = list(
                        times = NULL,
                        values = NULL,
                        operations = list(),
                        models = list(),
                        paths = list()
                      ),
                      
                      public = list(
                        initialize = function(times, values) {
                          private$times <- times
                          private$values <- values
                        },
                        
                        getTimes = function() {
                          return(private$times)
                        },
                        
                        getValues = function() {
                          return(private$values)
                        },
                        
                        getTimeSeries = function() {
                          return(xts(private$values, order.by = private$times))
                        },
                        
                        opsAppend = function(...) {
                          args <- list(...)
                          for (name in names(args)) {
                            private$operations[[name]] <- args[[name]]
                          }
                        },
                        
                        opsRemove = function(opName) {
                          private$operations[[opName]] <- NULL
                        },
                        
                        opsList = function() {
                          return(names(private$operations))
                        },
                        
                        modelsAppend = function(...) {
                          args <- list(...)
                          for (name in names(args)) {
                            private$models[[name]] <- args[[name]]
                          }
                        },
                        
                        modelsRemove = function(modelName) {
                          private$models[[modelName]] <- NULL
                        },
                        
                        modelsList = function() {
                          return(names(private$models))
                        },
                        
                        pathsAppend = function(pathName, operations, model) {
                          private$paths[[pathName]] <- list(operations = operations, model = model)
                        },
                        
                        pathsRemove = function(...) {
                          pathNames <- list(...)
                          for (name in pathNames) {
                            private$paths[[name]] <- NULL
                          }
                        },
                        
                        pathsList = function() {
                          return(names(private$paths))
                        },
                        
                        pathsRun = function(pathName) {
                          if (!pathName %in% names(private$paths)) {
                            stop("Path not found")
                          }
                          
                          path <- private$paths[[pathName]]
                          ts <- self$getTimeSeries()
                          
                          for (op in path$operations) {
                            if (!op %in% names(private$operations)) {
                              stop(paste("Operation", op, "not found"))
                            }
                            ts <- private$operations[[op]](ts)
                          }
                          
                          model_name <- path$model
                          if (!model_name %in% names(private$models)) {
                            stop(paste("Model", model_name, "not found"))
                          }
                          result <- private$models[[model_name]](ts)
                          
                          return(result)
                        }
                      )
)

# Example 1:
### Creating a random time series
ts <- yearmon(2009) + (0:(5 * 12 - 1)) / 12
vs <- seq_along(ts) / 5 + cumsum(rnorm(5 * 12))

### Initializing an instance
y <- timeSeries$new(times = ts, values = vs)

### Getting values
cat("\n\n")
y$getValues()

### Getting times
cat("\n\n")
y$getTimes()

### Getting time series as an xts object
cat("\n\n")
y$getTimeSeries()

### A typical plot
ggplot(
  data = tibble(time = y$getTimes(), value = y$getValues()),
  mapping = aes(x = time, y = value)
) +
  geom_line() +
  geom_point() +
  labs(title = "Example of a time series") +
  theme_light()


# Example 2
### Creating a time series
ts <- yearmon(2009) + (0:(5 * 12 - 1)) / 12
vs <- seq_along(ts) / 5 + cumsum(rnorm(5 * 12))

### Initializing an instance
y <- timeSeries$new(times = ts, values = vs)

### Appending operations
y$opsAppend(
  differencing = function(x) { diff(x = x, lag = 1, differences = 1) },
  logs_abs = function(x) { log(abs(x)) },
  na_omit = na.omit
)

### Listing operations
y$opsList()

### Removing operations
y$opsRemove("logs_abs")

### Listing operations
cat("\n\n")
y$opsList()


# Example 3
### Creating data
ts <- yearmon(2009) + (0:(5 * 12 - 1)) / 12
vs <- seq_along(ts) / 5 + cumsum(rnorm(5 * 12))

### Initializing a new instance
y <- timeSeries$new(times = ts, values = vs)

### Appending a linear forecast model
y$modelsAppend(
  linear_prediciton = function(x) {
    dtemp <- data.frame(
      t = seq_along(coredata(x)),
      z = coredata(x)
    )
    m <- lm(formula = z ~ t, data = dtemp)
    p <- predict(
      object = m,
      newdata = data.frame(t = last(dtemp$t) + 1:6)
    )
    xts(
      x = p,
      order.by = index(last(x)) + (1:6) * deltat(x)
    )
  }
)

## Appending an identity model
y$modelsAppend(
  identity_prediction = function(x) {
    x
  }
)

### Listing models
y$modelsList()

### Removing model
y$modelsRemove("identity_prediction")

### Listing models
cat("\n\n")
y$modelsList()


# Example 4
### Creating data
ts <- yearmon(2009) + (0:(2 * 12 - 1)) / 12
vs <- seq_along(ts) / 10 + cumsum(rnorm(2 * 12))

### Initializing instance
y <- timeSeries$new(times = ts, values = vs)

### Appending operations
y$opsAppend(
  differencing = function(x) { diff(x = x, lag = 1, differences = 1) },
  logs_abs = function(x) { log(abs(x)) },
  na_omit = na.omit
)

### Appending linear forecasting model
y$modelsAppend(
  linear_prediciton = function(x) {
    dtemp <- data.frame(
      t = seq_along(coredata(x)),
      z = coredata(x)
    )
    m <- lm(formula = z ~ t, data = dtemp)
    p <- predict(
      object = m,
      newdata = data.frame(t = last(dtemp$t) + 1:6)
    )
    xts(
      x = p,
      order.by = index(last(x)) + (1:6) * deltat(x)
    )
  }
)

## Appending identity model
y$modelsAppend(
  identity_prediction = function(x) {
    x
  }
)

### Listing operations and models
y$opsList()
cat("\n\n")
y$modelsList()

### Appending path with linear model
y$pathsAppend(
  pathName = "linear with differencing",
  operations = c("differencing", "na_omit"),
  model = "linear_prediciton"
)

### Appending path with identity forecasting. We can use that to see
### what comes into the forecasting model.
y$pathsAppend(
  pathName = "identity with differencing",
  operations = c("differencing", "na_omit"),
  model = "identity_prediction"
)

### Listing paths
cat("\n\n")
y$pathsList()

### Running paths
z <- y$pathsRun(pathName = "identity with differencing")
zF <- y$pathsRun(pathName = "linear with differencing")

### Creating figure
x11()
ggplot(
  data = tibble(time = index(z), value = coredata(z)),
  mapping = aes(x = time, y = value)
) +
  geom_line() +
  geom_point() +
  geom_line(data = tibble(time = index(zF), value = coredata(zF)), color = "red") +
  geom_point(data = tibble(time = index(zF), value = coredata(zF)), color = "red") +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(title = "Time series and forecast") +
  theme_light()



