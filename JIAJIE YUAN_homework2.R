#question 1
set.seed(123)
options(digits = 4)
n_rows=1000;n_cols=12
X_data = matrix(data = runif(n_rows*n_cols,1,15),nrow = n_rows,ncol = n_cols)
colnames(X_data) = paste("X_data",1:ncol(X_data),sep="_")
betas = sample(1:5,12,replace = TRUE)
betas = matrix(data=betas,ncol = 1, nrow = 12)
#question 2
Noise = rnorm(n_rows,mean = 0,sd = 4)
Noise = matrix(data=Noise,ncol = 1, nrow = n_rows)
Y_data=X_data%*%betas+Noise
#question 3
beta_hat=solve(t(X_data)%*%X_data)%*%(t(X_data)%*%Y_data)
#question 4
estimation_error=cbind(beta_hat,betas)
colnames(estimation_error)=c("beta_hat","betas")
max_difference=max(abs(beta_hat-betas))
#question 5
max_error_obtained=matrix(NA,nrow = 20,ncol = 1)
rownames(max_error_obtained)=paste("n_rows",seq(1000,20000,1000),sep = "_")
i=seq(1000,20000,1000)
my_function=function(i){
  set.seed(123)
  options(digits = 4)
  n_rows=i;n_cols=12
  X_data = rbind(matrix(data = runif(n_rows*n_cols,1,15),nrow = n_rows,ncol = n_cols))
  #colnames(X_data) = paste("X_data",1:ncol(X_data),sep="_")
  betas = sample(1:5,12,replace = TRUE)
  betas = rbind(matrix(data=betas,ncol = 1, nrow = 12))
  Noise = rnorm(n_rows,mean = 0,sd = 4)
  Noise = rbind(matrix(data=Noise,ncol = 1, nrow = n_rows))
  Y_data=X_data%*%betas+Noise
  beta_hat=solve(t(X_data)%*%X_data)%*%(t(X_data)%*%Y_data)
  #estimation_error=cbind(beta_hat,betas)
  #colnames(estimation_error)=c("beta_hat","betas")
  return(max(abs(beta_hat-betas)))
}
result=matrix(sapply(i,my_function))
for (j in 1:20){
  max_error_obtained[j,1]=result[j,1]
}
barplot(max_error_obtained,beside = TRUE,col = blues9,main = "max_error_obtained",ylim = c(0,0.07),legend.text = "n_rows_number")

