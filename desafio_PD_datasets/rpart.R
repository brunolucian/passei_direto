require(rpart)
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)

form <- as.factor(cyl) ~ .

fit <- rpart(form, data=mtcars, control = rpart.control(cp = 0.05, minsplit=5))
fancyRpartPlot(fit)

