setwd("C:/RW/Teaching/BC/STA9700S2023/Lecture notes") # Change path to where you save data file on your computer


# Matrix
data.A = c(2, #First row
           9, 
           4, 
           7, #First row
           5, 
           3, 
           6, #First row
           1, 
           8, 
           12, #First row
           11, 
           10)

A = matrix(data.A, 3, 4)

A

# 2 7 6 12
# 9 5 1 11
# 4 3 8 10

# Transpose of matrix
t(A)

# 2 9 4
# 7 5 3
# 6 1 8
# 12 11 10

# Matrix addition & subtraction

A = matrix(c(1,2,3,4,5,6), 3, 2)

B = matrix(c(1,2,3,2,3,4), 3, 2)

A + B

A - B

# Matrix multiplication

A = matrix(c(3,4,6,10), 2, 2) 

B = matrix(c(8,11,7,2,5,9), 2, 3)

A%*%B

# Diagonal matrix
diag(c(1,2,3),3)

# Scalar matrix
diag(x=5,3)

# Identity matrix
diag(3)

# All-ones matrix
n=5; matrix(rep(1,n^2),n,n)

# Zero matrix
n=5; matrix(rep(0,n^2),n,n)

# Inverse of square matrix
A=matrix(c(2,3,4,1),2,2)
solve(A)


### Example: Westwood company

(Westwood=read.csv("Westwood.csv"))
m<-lm(Man.Hours~Lot.Size,data=Westwood)
summary(m)

(Y=as.matrix(Westwood$Man.Hours))
(X=cbind(rep(1,10),Westwood$Lot.Size))
t(X)%*%X
solve(t(X)%*%X)
t(X)%*%Y
(b=solve(t(X)%*%X)%*%(t(X)%*%Y))
MSE=7.5; (s2.b=MSE*solve(t(X)%*%X))

# Hat matrix
(H=X%*%solve(t(X)%*%X)%*%t(X))

# Fitted values
(Y.hat=X%*%b)
H%*%Y

# Residuals
(e=Y-Y.hat)
(diag(10)-H)%*%Y

# Estimation of mean response
X.h=c(1,55)
(Yhat.h=X.h%*%b)
(s2.Yhat.h=MSE*t(X.h)%*%solve(t(X)%*%X)%*%X.h)

# ANOVA sums of squares
J=matrix(rep(1,100),10,10)
(SSTO=t(Y)%*%(diag(10)-J/10)%*%Y)
(SSE=t(Y)%*%(diag(10)-H)%*%Y)
(SSR=t(Y)%*%(H-J/10)%*%Y)
anova(m)


