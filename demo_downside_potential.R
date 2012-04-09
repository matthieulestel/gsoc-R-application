library("PerformanceAnalytics")

DownsideDeviation_perso <-
function (R, MAR = 0, method=c("subset","full"), ..., potential=FALSE)
{ # @author Peter Carl
  # @author Matthieu Lestel
    # DESCRIPTION:
    # Downside deviation, similar to semi deviation, eliminates positive returns
    # when calculating risk.  To calculate it, we take the returns that are less
    # than the target (or Minimum Acceptable Returns (MAR)) returns and take the
    # differences of those to the target.  We sum the squares and divide by the
    # total number of returns to get a below-target semi-variance.

    # This is also useful for calculating semi-deviation by setting
    # MAR = mean(x)

    method = method[1] 

    R0 <- R
    R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
        R = na.omit(R)

        r = subset(R, R < MAR)

        if(!is.null(dim(MAR))){
            if(is.timeBased(index(MAR))){
                MAR <-MAR[index(r)] #subset to the same dates as the R data
            } else{
                MAR = mean(checkData(MAR, method = "vector"))
                # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period
            }   
        }
        
        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch
        p=2
        if(potential) p=1 # calculates downside potential instead
        result = sqrt(sum((MAR - r)^p)/len)
        return(result)
	reclass(result, R0)
    }
    else {
        result = apply(R, MARGIN =2, DownsideDeviation_perso, MAR = MAR, method = method, potential=potential, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        if(potential)
            rownames(result) = paste("Downside Potential (MAR = ", round(mean(MAR)*100,1),"%)", sep="")
        else
            rownames(result) = paste("Downside Deviation (MAR = ", round(mean(MAR)*100,1),"%)", sep="")
        return(result)
    }
}

DownsidePotential_perso <-
function (R, ...)
{ # @author Peter Carl
  # @author Matthieu Lestel

    # DESCRIPTION:
    # This function is just a wrapper of DownsideDeviation with
    # MAR = mean(x) and potential = TRUE
    # see below

    # FUNCTION:
	R0 <- R
        R=checkData(R,method="matrix") 

	if(ncol(R) == 1 || is.null(R) || is.vector(R))
	{
	 R = na.omit(R)
	 result <- (DownsideDeviation_perso(R, MAR=mean(R), method="full", potential=TRUE))
	}
	else
	{
	 MAR=mean(R)
	 result <- apply(R, 2, DownsidePotential_perso, ...)
	 result <- matrix(result, nrow=1)
	 colnames(result) = colnames(R)
	 rownames(result) = paste("Downside Potential (MAR = ", round(mean(MAR)*100,1),"%)", sep="")
	}
	return(result)
	reclass(result,R0)

}

data(managers)

print("one column data")
print(">DownsideDeviation(managers['1996',1], MAR=0.010825, method=full)")
print(DownsideDeviation(managers['1996',1], MAR=0.010825, method="full"))
print(">DownsideDeviation_perso(managers['1996',1], MAR=0.010825, method=full)")
print(DownsideDeviation_perso(managers['1996',1], MAR=0.010825, method="full"))
print("")
print("")
print(">DownsidePotential(managers['1996',1])")
print(DownsidePotential(managers['1996',1]))
print("error in the function already present !")
print(">DownsideDeviation_perso(managers['1996',1], MAR=0.010825, method=full, potential=TRUE)")
print(DownsideDeviation_perso(managers['1996',1], MAR=0.010825, method="full", potential=TRUE))
print("DownsidePotential_perso(managers['1996',1])")
print(DownsidePotential_perso(managers['1996',1]))
print("My new function is working ! I used a calculator and the mathematic formula to check the result")
print("")
print("multi-column data")
print(">DownsideDeviation(managers['1996',1:5], MAR=0.010825, method=full)")
print(DownsideDeviation(managers['1996',1:5], MAR=0.010825, method="full"))
print(">DownsideDeviation_perso(managers['1996',1:5], MAR=0.010825, method=full)")
print(DownsideDeviation_perso(managers['1996',1:5], MAR=0.010825, method="full"))
print("DownsidePotential_perso(managers['1996',1:5])")
print(DownsidePotential_perso(managers['1996',1:5]))
print("My new function can handle with multi-column data !")