rm(list = ls())

AR_prep<- function(df,x.col.names,y.col.name,lag,RNN=TRUE){
  
  #recreating the data frame containing only the columns related
  df1<- df[,x.col.names]
  
  #getting the dimensions of the new data frame
  n<-dim(df1)[1]
  p<-dim(df1)[2]
  
  #creating a new data frame for the implementation as well as
  #the position of the response 
  df.new<- matrix(rep(0,(n-lag)*p*lag),ncol =p*lag) 
  y<- df1[(lag+1):n,y.col.name]
  
  # getting the dimensions
  n1<- dim(df.new)[1]
  
  for (i in 1:p){
    for(j in 1:lag){
      df.new[,j+(i-1)*lag]<-df1[j:(n-lag+j-1),i] 
    }
  }
  
  #if RNN is the target, then this code will be executed
  if (RNN==TRUE) {
    
    df.new<-array(data = df.new,dim = c(dim(df.new)[1],lag,dim(df1)[2]))
    z<-list(df.new,y)
    
  }else{
    # if the implementation of the Auto Regression is the target, then,
    # this code will be executed
    df.new<- as.data.frame(df.new)
    
    #creating the name of the columns of the new data frame
    for (i in 1:p){
      for (j in 1:lag){
        colnames(df.new)[j+(i-1)*lag]<-paste0(colnames(df1)[i],"_",j)
      }
    }
    z<-list(df.new,y)
    
  }
  return(z)
}

#let's do an example

#creating a new data frame for testing as well as
# corresponding column names

set.seed(0)
data.f<-as.data.frame(matrix(rep(0,6*100),ncol = 6))


c=rep(NA,dim(data.f)[2])
for (i in 1:dim(data.f)[2]){
  data.f[,i]<-runif(100,1,8*i)
  c[i]<- paste0("a",i)
}

# assigning the new column names to the data frame created
colnames(data.f)<-c

#running our implementation resulting in a list "x" containing
#two variables:
# the first variable is the newly implemented data frame
# the second variable is the new response vector

x=AR_prep(data.f, c("a1","a5","a3"), "a3", 5,RNN=FALSE)
x[[1]] # the resulted data frame
x[[2]] # the resulted response vector

