# Exercise 1

# year 1
1 - 0.056237 * 0.024080
1000/1.05*0.9986458

# year 2
(1 - 0.056237) * (1 - 0.062360) * (1 - 0.024080) * (1 - 0.026831)
(1 - 0.056237) * (1 - 0.062360) * 0.024080
(1 - 0.056237) * (1 - 0.062360) * (1 - 0.024080) * 0.026831
(1 - 0.024080) * (1 - 0.026831) * 0.056237
(1 - 0.024080) * (1 - 0.026831) * (1 - 0.056237) * 0.062360 
1000/1.05^2*0.994215

# Exercise 2

library(KMsurv)
library(survival)
library(muhaz)
data('tongue')

# a
# Create a survival object
dfsurv <- Surv(tongue$time, tongue$delta)

plot(survfit(dfsurv ~ type, data = tongue), col = 1:2)
legend('topright', c("Aneuploid", 'Diploid'), col = 1:2, lwd = 1)

plot(survfit(dfsurv ~ type, data = tongue), col = 1:2, conf.int = T)
legend('topright', c("Aneuploid", 'Diploid'), col = 1:2, lwd = 1)

# b
survdiff(dfsurv ~ type, data = tongue)

# c
plot(survfit(dfsurv ~ type, data = tongue), col = 1:2)
lines(survfit(dfsurv ~ type, data = tongue, type = 'fleming-harrington'),
     col = 1:2, lty = 3)
legend('topright', c("Aneuploid - KM", 'Diploid - KM', 
                     "Aneuploid - NA", 'Diploid - NA'),
       col = c(1:2, 1:2), lty = c(1, 1, 3, 3), lwd = 1)

# d
plot(survfit(dfsurv ~ type, data = tongue, type = 'fleming-harrington'),
     col = 1:2, fun = 'cumhaz', xlab = 'Time', ylab = 'Cumulative Hazard')
legend('topright', c("Aneuploid", 'Diploid'), col = 1:2, lwd = 1)


nafit <- survfit(dfsurv ~ type, data = tongue, type = 'fleming-harrington')
timevec <- 1:400
sf1 <- stepfun(nafit[1]$time, c(1, nafit[1]$surv))
sf2 <- stepfun(nafit[2]$time, c(1, nafit[2]$surv))
cumhaz1 <- - log(sf1(timevec))
cumhaz2 <- - log(sf2(timevec))
plot(timevec, cumhaz1 / cumhaz2, type = 'l',
     xlab = 'Time', ylab = 'Hazard Ratio', ylim = c(0, 1))
legend('bottomright', 'Aneuploid / Diploid', lwd = 1)


# plot(muhaz(tongue$time, tongue$delta, tongue$type == 1, max.time = 400),
#      col = 1, ylim = c(0, 0.02))
# lines(muhaz(tongue$time, tongue$delta, tongue$type == 2, max.time = 400), col = 2)
# abline(v=211)
# legend('topright', c("Aneuploid", 'Diploid'), col = 1:2, lwd = 1)

plot(muhaz(tongue$time, tongue$delta, tongue$type == 2), col = 2,
     xlim = c(0, 110))
lines(muhaz(tongue$time, tongue$delta, tongue$type == 1), col = 1)
legend("topright", c("Aneuploid Tumor","Diploid Tumor"), col = 2:1, lwd = 2)
title("Smoothed Hazard Rate Estimates for Two Types of Tumor")

