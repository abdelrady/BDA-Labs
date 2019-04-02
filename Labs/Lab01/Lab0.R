setX <- c(45, 80, 83, 78, 75, 77, 79, 83, 83, 100)
# Prob 1
#1.a
set.mean <- mean(setX)
#1.b
set.median <- median(setX)
#1.c
set.variance <- var(setX)
#1.d
set.std <- sd(setX)
#1.e
set.iqr <- IQR(setX)

# Prob 3
# 3.a
set.3a = setX < set.mean

# 3.b
setX[setX < set.mean]

# 3.c
setX[seq(1, length(setX), by = 2)]

# 3.d
scores.matrix <- matrix(setX, nrow=2, ncol=5, byrow = TRUE)
print(scores.matrix)

# 3.e
scores.matrix[,c(1,ncol(scores.matrix))]

# 3.f
dimnames(scores.matrix) <- list(
  paste("Student_", seq(1:nrow(scores.matrix))),
  paste("Quiz_", seq(1:ncol(scores.matrix)))
)
print(scores.matrix)


# Prob 4
Name = c("Pomana", "Williams","Stanford","Princeton", "Yale")
State = c("CA", "MA", "CA","NJ", "CT")
Cost = c(62632, 64020, 62801, 58965, 63970)
Population = c(1610, 2150, 18346, 8014, 12109)
colleges.info = data.frame(Name, State, Cost, Population)

# 4.a
summary(colleges.info$State)
summary(colleges.info$Cost)

#4.b
subset(colleges.info, Population > 5000)

#4.b
colleges.info.2016 <- colleges.info[,c(1:2,4)]
colleges.info.2016$Cost <- round(colleges.info$Cost + colleges.info$Cost * 0.5, 0)
print(colleges.info.2016)



