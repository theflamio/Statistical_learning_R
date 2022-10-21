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
y=rnorm(50)
plot(x,y)
plot(x,yxlab="Random Uniform", ylab="Random Normal", pch="*", col = "blue")
par(mfrow=c(2,1))
plot(x,y)
hist(y)
par(mfrow=c(1,1))

### Reading in data

#read.csv()
