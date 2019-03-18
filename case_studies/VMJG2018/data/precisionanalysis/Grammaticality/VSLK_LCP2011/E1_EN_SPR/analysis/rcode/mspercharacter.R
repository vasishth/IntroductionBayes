## example showing the problem with using milliseconds per character as a dependent
## measure:

## data from expt 1 of S. Vasishth, K. Suckow, R. L. Lewis, and S. Kern. 
## Short-term forgetting in sentence comprehension: 
## Crosslinguistic evidence from head-final structures. 
## Language and Cognitive Processes, 25(4):533-567, 2010.

load("critdata.Rda")

summary(critdata)

## reading time per character:
num.char<-nchar(as.character(critdata$word))
rtperchar<-critdata$value/num.char

## plot RT against character length:
plot(critdata$value~num.char)
abline(lm(critdata$value~num.char))

## plot rt per character against character length:
plot(rtperchar~num.char)
abline(lm(rtperchar~num.char))

