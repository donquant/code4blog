# Global Equity Momentum with ETFs.

require(quantmod)
require(PerformanceAnalytics)
require(reshape2)
require(ggplot2)
require(extrafont)
require(plotly)

# ETF Adjusted Prices.

ETF_prices = as.xts(read.zoo(file = "https://raw.githubusercontent.com/donquant/code4blog/master/data/GEM/ETF_prices.csv", header = T, index.column = 1, sep = ","))

# ETFs Returns.

look_back = as.list(seq(105, 252, 21))

GEMs = lapply(look_back, function(x) rollapply(ETF_prices, width = x, 
                                               function(y) (coredata(y[[x]]) - y[1]) / y[1], by = 1, fill = NULL))

# Endpoints.

GEMs = append(lapply(GEMs, function(x) x[endpoints(index(x), on = "months"),]["2008/"]),
              lapply(GEMs, function(x) x[endpoints(index(x), on = "months"),]["2008/"][seq(1, dim(x[endpoints(index(x), on = "months"),]["2008/"])[1], 2)]))


# Explanatory Plot of Momentum. Part I - Returns. 

GEM_12_2M = GEMs[[16]] * 100

names(GEM_12_2M) = c("SPY","CWI","AGG")

GEM_12_2M = ggplot2::fortify(GEM_12_2M, scale = ts.scale, is.date = is.date, index.name = index.name)

GEM_12_2M = melt(GEM_12_2M, id="Index")  

names(GEM_12_2M)[2] = "Return"


# Signals/Weights.

GEMs = lapply(GEMs, function(x) as.xts(t(apply(x, 1, rank)), dateFormat = "Date"))

GEMs = lapply(GEMs, function(x) { y = unlist(x)
                                  y[y != 3] = 0
                                  y[y == 3] = 1 
                                  return(y) })


# Explanatory Plot of Momentum. Part II - Allocations.

Allocations = GEMs[[16]]

Allocations = data.frame(Start = index(Allocations)[1:length(index(Allocations)) - 1], 
                         End = index(Allocations)[2:length(index(Allocations))],
                         Allocation = as.vector(apply(Allocations[1:dim(Allocations)[1] - 1], 1, which.max)))

Allocations[Allocations == 1] = "SPY"; Allocations[Allocations == 2] = "CWI"; Allocations[Allocations == 3] = "AGG" 

G_12_2M = ggplot(GEM_12_2M, aes(x = Index, y = value, colour = Return)) +
          geom_point() +
          geom_line(linetype = "dashed") +
          scale_color_manual(values = c("#00BA38", "#619CFF", "#F8766D")) + 
          geom_rect(data = Allocations, inherit.aes = F, 
                    aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = Allocation),  alpha = 0.35) +
          scale_fill_manual(values = c("#F8766D", "#619CFF", "#00BA38")) +
          scale_y_continuous(labels = function(x) paste0(x, " % ")) +
          theme(text = element_text(size = 14,  family = "Cambria Bold"), 
                legend.position = "bottom", legend.justification=c(1, 0),
                legend.text = element_text(size = 14, family = "Cambria Bold", colour = "gray35"),
                legend.margin = margin(t = -.7, unit = 'cm'),
                panel.grid.minor = element_blank(),
                plot.margin = unit(c( .2, .2, .2, -.4), "cm")) +
          xlab("") +
          ylab("")


G_12_2M


# GEM Returns

ETF_ret = cbind(dailyReturn(ETF_prices[,1]),
                dailyReturn(ETF_prices[,2]),
                dailyReturn(ETF_prices[,3]))

GEM_returns = lapply(GEMs, function(x) Return.portfolio(ETF_ret, x))


## Plots and Performance Statistics

GEM_ret = do.call(cbind, GEM_returns)

Ret = cbind(GEM_ret, ETF_ret[,1]["2008-02-01/"])

names(Ret) = c("GEM 5 M" ,"GEM 6 M" ,"GEM 7 M" ,"GEM 8 M" ,"GEM 9 M" ,"GEM 10 M" ,"GEM 11 M" ,"GEM 12 M" ,
               "GEM 5 2M","GEM 6 2M","GEM 7 2M","GEM 8 2M","GEM 9 2M","GEM 10 2M","GEM 11 2M","GEM 12 2M","SPY")

mRet = apply.monthly(Ret, function(x) apply(x, 2, function(y) prod(y + 1) - 1)) # monthly returns

aRet = apply.yearly(Ret, function(x) apply(x, 2, function(y) prod(y + 1) - 1)) # annual returns


# Performance.

btStat = data.frame(t(rbind(apply(mRet, 2, function(x) prod(x + 1)^(1/10) - 1) * 100,
                            apply(aRet, 2, mean) * 100,
                            apply(aRet, 2, sd) * 100,
                            apply(mRet, 2, SharpeRatio.annualized),
                            apply(mRet, 2, maxDrawdown) * -100,
                            cbind(do.call(cbind, lapply(GEMs, function(x)
                                          dim(x[!c(FALSE, apply(lag(x) == x, 1, all)[-1]),])[1]/10)), 1/10))))

names(btStat) = c("CAGR", "AAR", "SD", "Sharpe" , "MDD", "Trades Per Year")

round(btStat, 2)

# Equity Curves Interactive Plot.

cRet = rbind(mRet, xts( t(rep(0, length(GEMs) + 1)), index(ETF_prices[,1]["2008"][1])))

cRet = ggplot2::fortify(round(cumprod(1 + cRet)*10000, 0), scale = ts.scale, is.date = is.date, index.name = index.name)

cRet = melt(cRet, id="Index")  

names(cRet) = c("Date","Model","Value")

G3M = ggplot(cRet, aes(x = Date, y = Value, colour = Model)) +
      geom_line() + 
      scale_y_continuous(labels = function(x) paste0("$ ", x)) +
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            plot.margin = unit(c( .65, 0, .5, .5), "cm")) +
      xlab("") + 
      ylab("")

G3M$labels$colour = ""

ggplotly(G3M)

# Equity Curves Static Plot.

G3M = ggplot(cRet, aes(x = Date, y = Value, colour = Model)) +
      geom_line() + 
      scale_y_continuous(labels = function(x) paste0("$ ", x)) +
      theme(text = element_text(size = 14,  family = "Cambria Bold"), 
            plot.margin = unit(c( .2, .2, .2, -.4), "cm")) +
      xlab("") + 
      ylab("")

G3M
