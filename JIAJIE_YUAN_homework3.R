##clear memory [1]
rm(list=ls()); gc(); graphics.off()
#question 1
#the first function print"solution one is equal to sol_1" and "solution two is equal to sol_2" using paste function;
#the second function return a vector whcih contains sol_1 and sol_2;
#two function use different ways to display the solution
#question 2
fibonacci=function(n){
     if(is.integer(n)==FALSE){
             print("The function must only work when N is a positive integer. ")
         }
       else if(n==1|n==2){
             return(1)
       }
       else if (n=0){
         return(0)
       }
       else
         {
               return(fibonacci(N-1)+fibonacci(N-2))
           }
}
#question 3
matrixMul <- function(A,B)
{
  rows <- nrow(A)
  cols <- ncol(B)
  
  matOut <- matrix(nrow = rows, ncol = cols) # empty matrix
  
  for (i in 1:rows) 
  {
    for(j in 1:cols)
    {
      vec1 <- A[i,]
      vec2 <- B[,j]
      
      mult1 <- vec1 * vec2
      
      matOut[i,j] <- sum(mult1)
    }
  }
  
  return(matOut) 
}
A = matrix(c(0,5,3,5,5,2),nrow=2,ncol=3)
B = matrix(c(3,3,4,4,-2,-2),nrow=3,ncol=2)
C = matrixMul(A,B)
print(C)
#question 4
list_number=1:100000
cards_Received=as.list(list_number)
cards_function=function(N){
       five_location=sample(x=1:(13*4),size = 5)
       zero_vector=sample(x=0,size=13*4,replace = TRUE)
       zero_vector[five_location]=1
       cardsReceived=matrix(data=zero_vector,nrow = 13,ncol = 4)
       colnames(cardsReceived)=c("Spades","Clubs","Hearts","Diamonds")
       result=assign(paste("cardsReceived",N,sep = ""),cardsReceived)
       return(result)
}
for(i in 1:100000){
  cards_Received[[i]]=cards_function(i)
}

n=0
for(i in 1:100000){
  for(j in 1:13){
    if(sum(cards_Received[[i]][j,])==2){
      n=n+1
      }
  }
}
N=n/(100000)
print(paste("the probability of times you get 2 cards of the same number is equal to ",N))
#question 5
a<-matrix(0,5,5)
a[1,2]=45;a[1,3]=89;a[1,4]=78;a[1,5]=24;
a[2,3]=70;a[2,4]=51;a[2,5]=18;
a[3,4]=104;a[3,5]=44;
a[4,5]=32
b<-a+t(a)
b[b==0]<-Inf
get_the_short_direction<-function(A){
       n<-nrow(A)
       D<-A
       path<-matrix(0,n,n)
       for(i in 1:n){
             for(j in 1:n){
                   if(is.finite(D[i,j])==T){path[i,j]=j}
               }
         }
    for(k in 1:n){
             for(i in 1:n){
                   for(j in 1:n){
                         if(D[i,k]+D[k,j]<D[i,j]){
                               D[i,j]=D[i,k]+D[k,j];
                               path[i,j]=path[i,k]
                           }
                     }
               }
         }
       return(list(D=D,path=path))
   }
get_the_short_direction(b)



