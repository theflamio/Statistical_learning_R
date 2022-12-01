### Vectors, data, matrices, subsetting

x=c(2,7,5)

y=seq(from=4,length=3,by=3)
?seq() ### show documentation on seq
y

x+y
x/y # division by element like python
x^y # x power to y element wise expenentiation

x[2] # gives us the 2 element of x

x[2:3] # gives elements from 2 to 3

x[-2] # remove element 2

# OBS no scalar in R only vectors "scalar is just a vector of length 1"

z=matrix(seq(1,12),4,3)
z

z[3:4,2:3]
z[,2:3]
z[,1]

ls()

x=runif(50) # random uniform "creates 50 random numbers"
y=rnorm(50)  #Random normal from 0 to 1
plot(x,y) #plots x and y
plot(x,yxlab="Random Uniform", ylab="Random Normal", pch="*", col = "blue")
par(mfrow=c(2,1))
plot(x,y)
hist(y) #Make a histogram of y
par(mfrow=c(1,1)) #Reset the mfrow command


### Reading in data
#Auto=read.csv("Auto.csv")   #Already have data via ISLR package so just call data
data(Auto)

names(Auto)           #Gives names of variables

dim(Auto)

class(Auto)        #What type of object

str(Auto)          #Structure of data

summary(Auto)    #Gives summary of data

plot(Auto$cylinders,Auto$mpg)
plot(Auto$cyl,Auto$mpg)

attach(Auto)       #Attach dataframe, creates a workspace
search()        #You can see "Auto" available for direct use so you don't need to use $cylinders or $mpg

plot(cylinders,mpg)   #Use variables directly
cylinders=as.factor(cylinders)  #Cast as factor
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
pdf(file="../mpg.pdf")
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()
quartz
     2
pairs(Auto,col="brown")
pairs(mpg~cylinders+acceleration+weight,Auto)

#TO SUBSET DATA TO OBTAIN ROWS THAT FALL OUTSIDE OF ROWS 10-85
Auto2 = Auto[-10:-85, ]


#TO PARTITION DATASET INTO TRAIN AND TEST SETS
# load the library that contains the data set
library(ISLR)
# readme for data set
?Auto
# readme for sample() function
?sample

# create a vector of row indexes
training <- sample(nrow(Auto), size=200)

# create training data set with 200 obs
train_set <- Auto[training,]

# create test data set with the remaing obs
test_set <- Auto[-training,]