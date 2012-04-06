mean_peroso <-
     function (x,na.rm = FALSE)
{
R=checkData(x,method="matrix")

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

   result = mean
}
}

library("PerformanceAnalytics")

data(managers)
print("moyenne")
print(mean(managers))
