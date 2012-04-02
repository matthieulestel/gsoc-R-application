#!/usr/bin/env Rscript

print("In this demo we are going to work on a fictionary fund.")
print("To study a fund well, we need to look at the result of the fund and their properties first, then to compare the result of the fund with other funds with a similar gestion and lastly to compare it with a benchmark.")

library("PerformanceAnalytics")
library(tcltk)

mywait <- function() {
    tt <- tktoplevel()
    tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
        side='bottom')
    tkbind(tt,'<Key>', function()tkdestroy(tt) )

    tkwait.window(tt)
}

print("load a set of fictionary data")
print("!!! PerformanceAnalytics works on returns and not on prices")
print(">data(managers)")
mywait()
data(managers)
print("display the data")
print(">manager")
mywait()
print(managers)

print("name of the funds analysed")
print(">colnames(managers)")
mywait()
print(colnames(managers))

print("let's isolate the first fund which interest us")
print(">main.col = 1")
mywait()
main.col = 1
print("let's group the comparative funds")
print(">comp.cols = c(2,3,4,5,6)")
mywait()
comp.cols = c(2,3,4,5,6)
print("let's group the benchmarks")
print(">bench.cols = c(7,8)")
mywait()
bench.cols = c(7,8)

print("=================================")

print("Now it's time to analyse the fund")
print("Let's view it with a readable view")
print("!!!returns have been multiplied by 100 to have percentages")
print(">t(table.CalendarReturns( managers[,c(main.col)], digit = 2) )")
mywait()
print(t(table.CalendarReturns( managers[,c(main.col)], digit = 2) ))

print("we can also compare it with the benchmark")
print(">t(table.CalendarReturns( managers[,c(main.col,bench.cols)], digit = 2) )")
mywait()
print(t(table.CalendarReturns( managers[,c(main.col,bench.cols)], digit = 2) ))


print("It's not so easy to compare, isn't it. Let's draw a graph to see it better...")
charts.PerformanceSummary(managers[,c(main.col)], colorset=tim12equal, lwd=2, ylog=TRUE)
par(ask=TRUE)
print("... and compare it with the benchmark")
charts.PerformanceSummary(managers[,c(main.col,bench.cols)], colorset=tim12equal, lwd=2, ylog=TRUE)
print("the two one are with a log scale to appreciate the evolution better")

print("Now let's explore it with more details...")
print(">table.Stats(managers[,c(main.col)])")
mywait()
print(table.Stats(managers[,c(main.col)]))

print("... and compare it with the other funds")
print(">table.Stats(managers[,c(main.col, comp.cols)])")
mywait()
print(table.Stats(managers[,c(main.col, comp.cols)]))


print("you can try to find the law with an histogramm")
chart.Histogram(managers[,c(main.col)], method= c("add.normal"))
print("and compare the funds with a boxplot")
chart.Boxplot(managers[c(main.col, comp.cols, bench.cols)], main = "Returns")
print("you can also examine the rolling performance (!!!take a little time)")
charts.RollingPerformance(managers[, c(main.col, comp.cols, bench.cols)])
print("and the relative performance relative to others funds (here with SP500)")
chart.RelativePerformance(managers[,c(main.col)], managers[,c(8)], main="Relative performance", xaxis=TRUE, legend.loc="topleft")

print("if you think you don't have enough informations you just can diplay more stats to compare with it")
print(">table.CAPM(managers[,1,drop=FALSE], managers[,8,drop=FALSE])")
mywait()
print(table.CAPM(managers[,1,drop=FALSE], managers[,8,drop=FALSE]))

print("You may also want to study the correlation between different funds to diverify your investment")
print(">table.Correlation(managers[,c(main.col, comp.cols)], managers[,8])")
mywait()
print(table.Correlation(managers[,c(main.col, comp.cols)], managers[,8]))
print("and if not visual enough you can as usual have a graph if you prefer")
chart.RollingCorrelation(managers[,c(main.col, comp.cols)], managers[,8], main="Correlation", legend.loc="bottomleft", colorset=tim6equal)
print("To conclude it's pretty important to have some stats about the downside risk. Here we go !")
print(">table.DownsideRisk(managers[,1:6]")
mywait()
print(table.DownsideRisk(managers[,1:6]))