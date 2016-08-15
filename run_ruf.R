# Variable importance by Random Uniform Forest

library(randomUniformForest)

# data
set.seed(2016)
n = 1000
p = 10
X = simulationData(n, p)
X = fillVariablesNames(X)

epsilon1 = runif(n, -1, 1)
epsilon2 = runif(n, -1, 1)

rule = 2 * (X[, 1] * X[, 2] + X[, 3]) + X[, 4] + epsilon1 * X[, 5] + epsilon2 * X[, 6]
Y = as.factor(ifelse(rule > mean(rule), "yes", "no"))

# ruf
synth.model.run = randomUniformForest(X, as.factor(Y), ntree = 400)
summary(synth.model.run)

# reduce table size by variable importance
vi <- synth.model.run$forest$variableImportance
threshold = 10
idx <- which(vi$percent.importance < threshold)
X <- X[, -idx]

synth.model.run = randomUniformForest(X, as.factor(Y), ntree = 400)
summary(synth.model.run)
