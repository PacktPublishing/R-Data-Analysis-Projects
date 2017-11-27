# Time series data
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
king.ts <- ts(kings)
king.ts

install.packages("TTR")
library(TTR)

par(mfrow=c(2 ,2))
plot(SMA(king.ts, n=2), main = "n=2")
plot(SMA(king.ts, n=5), main = "n=5")
plot(SMA(king.ts, n=10), main = "n=10")
plot(SMA(king.ts, n=15), main = "n=15")
par(mfrow=c(1,1))

smooth.king <- SMA(king.ts, n=5)
smooth.king

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births.ts <- ts(births, frequency = 12)
births.comps <- decompose(births.ts)
plot(births.comps)


library(zoo)
library(quantmod)

data <- as.zoo(smooth.king)
x1 <- Lag(data,1)
new.data <- na.omit(data.frame(Lag.1 = x1, y = data))
head(new.data)

model <- lm(y ~ Lag.1, new.data)
model
plot(model)


plot(king.ts)
# Introducing MXNet library
library(mxnet)

zero.matrix <- mx.nd.zeros(c(3,3))
zero.matrix
ones.matrix <- mx.nd.ones(c(3,3))
ones.matrix

# Context
mx.ctx.default()

# To create a matrix in GPU
zero.matrix.gup <- mx.nd.zeros(c(3,3), mx.gpu(0))

# Moving data between devices
help("mx.nd.copyto")
zero.copy <- mx.nd.copyto(zero.matrix, mx.cpu())

# Operations
mx.nd.dot(ones.matrix, ones.matrix)
mx.nd.elemwise.add(ones.matrix,ones.matrix)

# Sampling
mu <- mx.nd.array(c(0.0,2.5))
sigma <- mx.nd.array(c(1,3))
mx.nd.sample.normal(mu = mu, sigma = sigma)


# Symbolic Programming
a <- mx.symbol.Variable("a")
a
b <- mx.symbol.Variable("b")
b
c <- a + b
c

arg_lst <- list(symbol = c, ctx = mx.ctx.default(), a = dim(ones.matrix),
                b= dim(ones.matrix), grad.req="null")

pexec <- do.call(mx.simple.bind, arg_lst)
pexec

input_list <-  list(a = ones.matrix,b = ones.matrix)

mx.exec.update.arg.arrays(pexec, input_list)

mx.exec.forward(pexec)

pexec$arg.arrays

pexec$outputs

######## Another operation ###############

d <- mx.symbol.dot(a, b)

arg_lst <- list(symbol = d, ctx = mx.ctx.default(), a = dim(ones.matrix),
                b= dim(ones.matrix), grad.req="null")

pexec <- do.call(mx.simple.bind, arg_lst)
pexec

input_list <-  list(a = ones.matrix,b = ones.matrix)

mx.exec.update.arg.arrays(pexec, input_list)
mx.exec.forward(pexec)

pexec$arg.arrays
pexec$outputs


#############   XOR Learning  ############
mx.set.seed(1)

### Generate some data.
x.1 <- mx.nd.sample.normal(mu = mx.nd.array(c(0,0)), 
                           sigma = mx.nd.array(c(0.001,0.001)), shape = (1000))
y.1 <- rep(0, 1000)
x.2 <- mx.nd.sample.normal(mu = mx.nd.array(c(0,1)), 
                           sigma = mx.nd.array(c(0.001,0.001)), shape = (1000))
y.2 <- rep(1,1000)
x.3 <- mx.nd.sample.normal(mu = mx.nd.array(c(1,0)), 
                           sigma = mx.nd.array(c(0.001,0.001)), shape = (1000))
y.3 <- rep(1,1000)
x.4 <- mx.nd.sample.normal(mu = mx.nd.array(c(1,1)), 
                           sigma = mx.nd.array(c(0.001,0.001)), shape = (1000))
y.4 <- rep(0,1000)

X <- data.matrix(mx.nd.concat(list(x.1,x.2,x.3,x.4)) )
Y <- c(y.1,y.2,y.3,y.4)


############## Define the Network #########

# Input layer
data <- mx.symbol.Variable("data")

# Hidden Layer
hidden.layer <- mx.symbol.FullyConnected(data = data
                                         , num_hidden = 2)

# Hidden Layer Activation
act <- mx.symbol.Activation(data = hidden.layer, act_type = "relu")

# Output Layer
out.layer <- mx.symbol.FullyConnected(data = act
                                      , num.hidden = 2)

# Softmax of output
out <- mx.symbol.SoftmaxOutput(out.layer)

# Build the model


model <- mx.model.FeedForward.create(out, X=X
                                     , y=Y
                                     , ctx = mx.ctx.default()
                                     , array.layout = "rowmajor"
                                     , learning.rate = 0.01
                                     , momentum = 0.9
                                     , array.batch.size = 50
                                     , num.round = 20
                                     , eval.metric = mx.metric.accuracy
   #                                  , initializer = mx.init.normal(c(0.0,0.1))
)

# Perform Predictions
X_test = data.matrix(rbind(c(0,0),c(1,1),c(1,0),c(0,1) ) )
preds = predict(model, X_test, array.layout = "rowmajor")
pred.label <- max.col(t(preds)) -1
pred.label


# Visualize the model
graph.viz(model$symbol)
model$arg.params

########################################


######### Regression Neural Network ############

# Prepare the data
library(mxnet)
library(quantmod, quietly = TRUE)
library(ggplot2)

stock.data <- new.env()
tickers <- ('AAPL')
stock.data <- getSymbols(tickers, src = 'yahoo', from = '2000-01-01', env = FALSE, auto.assign = F)    

data <- stock.data$AAPL.Close
head(data)


plot.data <- na.omit(data.frame(close_price = data))
names(plot.data) <- c("close_price")
ggplot(plot.data,aes(x = seq_along(close_price))) + geom_line(aes(y = close_price, color ="Close Price"))


# Feature generation
data.ts <- as.ts(data)
data.zoo <- as.zoo(data.ts)


x.data <- list()
  for (j in 1:31){
    var.name <- paste("x.lag.",j)
    x.data[[var.name]] <- Lag(data.zoo,j)
  }
  

final.data <- na.omit(data.frame(x.data, Y = data.zoo))
head(final.data)


set.seed(100)
# Train/test split
train.perc = 0.8
train.indx = 1:as.integer(dim(final.data)[1] * train.perc)

train.data <- final.data[train.indx,]
test.data  <- final.data[-train.indx ,]

train.x.data <- data.matrix(train.data[,-1])
train.y.data <- train.data[,1]

test.x.data <- data.matrix(test.data[,-1])
test.y.data <- test.data[,1]



mx.set.seed(100)
deep.model <- mx.mlp(data = train.x.data, label = train.y.data,
                     hidden_node = c(1000,500,250)
                    ,out_node = 1
                    ,dropout = 0.50
                    ,activation = c("relu", "relu","relu")
                    ,out_activation = "rmse"
                    , array.layout = "rowmajor"
                    , learning.rate = 0.01
                    , array.batch.size = 100
                    , num.round = 100
                    , verbose = TRUE
                    , optimizer = "adam"
                    , eval.metric = mx.metric.mae

                    
                    
)


model.evaluate <- function(deep.model, new.data, actual){
  preds = predict(deep.model, new.data, array.layout = "rowmajor")
  error <- actual - preds
  return(mean(abs(error)))
  
}

print("Train Error")
model.evaluate(deep.model,train.x.data, train.y.data)


print("Test Error")
model.evaluate(deep.model,test.x.data, test.y.data)


preds = predict(deep.model,  train.x.data, array.layout = "rowmajor")
plot.data <- data.frame(actual = train.y.data, predicted = preds[1,])
ggplot(plot.data,aes(x = seq_along(actual))) + geom_line(aes(y = actual, color = "Actual")) + geom_line(aes(y = predicted, color = "Predicted"))

preds = predict(deep.model,  test.x.data, array.layout = "rowmajor")
plot.data <- data.frame(actual = test.y.data, predicted = preds[1,])
ggplot(plot.data,aes(x = seq_along(actual))) + geom_line(aes(y = actual, color ="actual")) + geom_line(aes(y = predicted, color ="predicted"))

graph.viz(deep.model$symbol)

library(Matrix)
# Look at the graph plot for the name of each layer
# alternatively call deep.model$arg.params$  to see the name
weights <- deep.model$arg.params$fullyconnected102_weight
dim(weights)
image(as.matrix(weights))

weights.1 <- deep.model$arg.params$fullyconnected19_weight
dim(weights.1)
image(as.matrix(weights.1))

weights.2 <- deep.model$arg.params$fullyconnected20_weight
dim(weights.2)
image(as.matrix(weights.2))






random.search <- function(){

  # Sample layers
  count.layers <- sample(2:5, 1)
  no.layers <- c()
  activations <- c()
  for (i in 1:count.layers-1){
    # Sample node per layers
    no.nodes <- sample(10:50,1)
    no.layers[i] <- no.nodes
    activations[i] <- "relu"
  }
  
  no.layers <- append(no.layers, 1)
  activations <- append(activations, "relu")
  
  deep.model <- mx.mlp(data = train.x.data, label = train.y.data,
                       hidden_node = no.layers
                       , out_node = 1
                       , dropout = 0.50
                       , activation = activations
                       ,out_activation = "rmse"
                       , array.layout = "rowmajor"
                       , learning.rate = 0.01
                       , array.batch.size = 100
                       , num.round = 10
                       , verbose = TRUE
                       , optimizer = "adam"
                       , eval.metric = mx.metric.mae
                       
                       
                       
  )
  
  
  train.error <- model.evaluate(deep.model,train.x.data, train.y.data)
  test.error <- model.evaluate(deep.model,test.x.data, test.y.data)
  
  output <- list(layers = no.layers, activations <- activations
                 , train.error = train.error, test.error = test.error)
  return(output)
}

final.output = list()
for (i in 1:2){
  out <- random.search()
  final.output[[i]] <- out
}



