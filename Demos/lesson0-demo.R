x<-10
x
mode(x)
x
2^5
8%%3
15%%4
(5<=8)&(4>=9)
(5<=8)|(4>=9)
v <- 2:8
v
y<-5
print(y %in% v)

firstname <- 'Donald'
lastname <- 'Trump'
fullname <- paste(firstname, lastname)
fullname
fullname <- paste(firstname, lastname, sep = ", ")
fullname

names <- c('joe','andy','bob')
ages <- c(25, 45, 34)

namesAndages <- c(names, ages)
namesAndages
length(namesAndages)

#Any value written within a pair of single quote or double quotes in R is treated as a string
hs.year <- c("Senior", "Freshman","Junior", "Senior", "Sophmore", "Freshman")
mode(hs.year)
h <- factor(hs.year)
mode(hs.year)
summary(hs.year)
summary(h)
m <- ordered(hs.year)
m

seq(1, by=2, length = 5)

seq(1, 15, by=2)

data <- c(90,95,92,78,46,51,77,98,90,85,78,61,100,89,87)
i <- 1
repeat{
  if(data[i] > 80) {
    print(data[i])
    i <- i+1
  } 
  else
  break
}

grades <- matrix(data, nrow = 5, ncol = 3, byrow = TRUE)
grades

dim(grades)
nrow(grades)
ncol(grades)

grades[1,2]
grades[1:5,2:3]
grades[c(1,3,5),2:3]

dimnames(grades) <- list(c("bob", "joe", "andy", "mary","alice"), c("English", "Math", "Science"))
grades


name <- c("Jennifer", "Alex", "Wes", "Ryan")
sex <- c("Female", "Male","Male","Male")
age.diagnosis <- c(49, 57,69,75)
year.diagnosis <- c(10, 5,2,12)
hosp.info <- data.frame(name,sex,age.diagnosis, year.diagnosis)
hosp.info
hosp.info$new.age <- hosp.info$age.diagnosis + hosp.info$year.diagnosis
hosp.info[1:2,1]

ac <- c(35,49,59,61,62,64,65,66,66,68,68,68,69,70,70,72,77,77,84,86)
summary(ac)

ab <- c(1,2,3,4,5,6,7,8)
sum(ab)/length(ab)
summary(ab)
IQR(ab)

ae <- c( 45, 80, 83, 78, 75, 77, 79, 83, 83, 100)
ad <- sort(ae)
ad
ae < mean(ae)
quantile(ad, 0.25)
IQR(ad)

function(x){
  
}