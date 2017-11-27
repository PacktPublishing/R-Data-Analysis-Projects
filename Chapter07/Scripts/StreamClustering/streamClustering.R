install.packages("stream")

library(stream, quietly = TRUE)
set.seed(100)
gaussian.stream <- DSD_Gaussians(k = 3, d = 3, noise = .01)
gaussian.stream

data <- get_points(gaussian.stream, n = 100, class = TRUE)
head(data)

library(plotly)
p <- plot_ly(data, x = ~X1, y = ~X2, z = ~X3, color = ~class) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'X1'),
                      yaxis = list(title = 'X2'),
                      zaxis = list(title = 'X3')))
p



# Drift generators
drift.stream <- DSD_MG(dim = 2)
cluster.1 <- MGC_Linear(dim = 2)

add_keyframe(cluster.1, time=1,  dens=50, par=5, center=c(0,0))
add_keyframe(cluster.1, time=20, dens=50, par=5, center=c(50,50))
add_keyframe(cluster.1, time=40,dens=50, par=5, center=c(50,100))
add_cluster(drift.stream,cluster.1)

drift.stream
data <- get_points(drift.stream, n = 1000)
plot_ly(data, x = ~X1, y= ~X2) %>%  add_markers() %>% layout(title = 'time around 1')

drift.stream
data <- get_points(drift.stream, n = 1000)
plot_ly(data, x = ~X1, y= ~X2) %>%  add_markers() %>% layout(title = 'time around 20')

drift.stream
data <- get_points(drift.stream, n = 1000)
plot_ly(data, x = ~X1, y= ~X2) %>%  add_markers() %>% layout(title = 'time around 40')


# replay data
random.stream <- DSD_Gaussians(k = 2, d = 4, mu = rbind(c(200,77,20,750),c(196,80,16,790)))
data.un <- get_points(random.stream, n =2000)
head(data.un)

replayer <- DSD_Memory(data.un, k = NA)
replayer

get_points(replayer, n=5)
replayer

reset_stream(replayer, pos = 2)
replayer



# Sample Data Stream Clustering

stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)
stream
clus.alg <- DSC_DBSTREAM(r = 0.03)
clus.alg
update(clus.alg, stream, n =100)
clus.alg
update(clus.alg, stream, n =100)
clus.alg
head(get_centers(clus.alg))

# plot the data
plot(clus.alg, stream)


evaluate(clus.alg, stream)

# Properties of the cluster
get_centers(clus.alg)
get_weights(clus.alg)


# Simulate the data from a sensor network
sensor.stream <- DSD_Gaussians(k = 2, d = 4, mu = rbind(c(200,77,20,750),c(196,80,16,790)))
sensor.stream

par(mfrow = c(2,2))
data.un <- get_points(sensor.stream, n =100)
head(data.un)

plot(density(data.un$X1), main = "Raw")
plot(density(data.un$X2), main = "Raw")
plot(density(data.un$X3), main = "Raw")
plot(density(data.un$X4), main = "Raw")


stream.norm   <- DSD_ScaleStream(sensor.stream)
stream.norm

data <- get_points(stream.norm, n = 100)
head(data)
plot(density(data$X1), main = "Normalized")
plot(density(data$X2), main = "Normalized")
plot(density(data$X3), main = "Normalized")
plot(density(data$X4), main = "Normalized")

par(mfrow = c(1,1))

# Perform Clustering
micro.alg <- DSC_Sample(k = 10)
macro.alg <- DSC_Kmeans(k = 2)
pipe <- DSC_TwoStage(micro = micro.alg, macro = macro.alg)

update(pipe, stream.norm, n = 1000)
pipe

evaluate(pipe, stream.norm,measure = c("numMicroClusters", "purity"),type = "micro",n = 500)



