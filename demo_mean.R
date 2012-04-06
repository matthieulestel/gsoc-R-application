mean_perso <-
     function (x, na.rm = FALSE)
{
R=checkData(x,method="matrix")

    columns = ncol(R)
    columnnames=colnames(R)

for(column in 1:columns) {

   x = as.vector(na.omit(R[,column]))
   #x = R[,column]
   
   if(!is.numeric(x)) stop("The selected column is not numeric")

   # Remove NAs:
   if (na.rm) x = x[!is.na(x)]

   # Warnings:
   if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
      warning("argument is not numeric or logical: returning NA")
      return(as.numeric(NA))}

   #mean
   n = length(x)
   if (is.integer(x)) x = as.numeric(x)
   mean =sum(x)/n
   return(mean)
}
}

library("PerformanceAnalytics")

data(managers)
print("Mean of HAM1 return for the year 1996 with my function :")
print(mean_perso(managers['1996',1]))
print("Mean of HAM1 return with the function already present :")
print(mean(managers['1996', 1]))
print("Mean of HAM5 return for the year 2000 with my function :")
print(mean_perso(managers['2000',5]))
print("Mean of HAM5 return with the function already present :")
print(mean(managers['2000', 5]))
print("my function handle missing data !")
print("Mean of HAM5 return for the year 1996 with my function :")
print(mean_perso(managers['1996', 5]))
print("My function handle with not numerical data !")
