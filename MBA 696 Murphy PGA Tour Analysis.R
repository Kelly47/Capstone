library(dplyr)
library(psych)
library(stringi)
library(car)
library(mvoutlier)
library(ggplot2)
library(rcompanion)
library(scales)
library(doBy)

# Function to remove outliers
        outlierKD <- function(dt, var) {
                var_name <- eval(substitute(var),eval(dt))
                na1 <- sum(is.na(var_name))
                m1 <- mean(var_name, na.rm = T)
                par(mfrow=c(2, 2), oma=c(0,0,3,0))
                boxplot(var_name, main="With outliers")
                hist(var_name, main="With outliers", xlab=NA, ylab=NA)
                outlier <- boxplot.stats(var_name)$out
                mo <- mean(outlier)
                var_name <- ifelse(var_name %in% outlier, NA, var_name)
                boxplot(var_name, main="Without outliers")
                hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
                title("Outlier Check", outer=TRUE)
                na2 <- sum(is.na(var_name))
                cat("Outliers identified:", na2 - na1, "n")
                cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
                cat("Mean of the outliers:", round(mo, 2), "n")
                m2 <- mean(var_name, na.rm = T)
                cat("Mean without removing outliers:", round(m1, 2), "n")
                cat("Mean if we remove outliers:", round(m2, 2), "n")
                response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
                if(response == "y" | response == "yes"){
                        dt[as.character(substitute(var))] <- invisible(var_name)
                        assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
                        cat("Outliers successfully removed", "n")
                        return(invisible(dt))
                } else{
                        cat("Nothing changed", "n")
                        return(invisible(var_name))
                }
        }
        


setwd('/Users/Kelly/Documents/UMT/Spring 2018/MBA 696/PGATour/Data')

#Import the data
strokeData = read.csv('StrokeLevel2003-2018.TXT', header = TRUE, sep = ';')

#Strip down the stroke data
strokeData = strokeData %>%  select(Year, Course.., Player.., Player.First.Name, Player.Last.Name,
                                     Round, Hole, Hole.Score, Par.Value, Shot, From.Location.Scorer., 
                                     To.Location.Scorer.,Distance, Distance.to.Pin, Distance.to.Hole.after.the.Shot,
                                    In.the.Hole.Flag, Time, Lie, Elevation, Slope,
                                    Date, Strokes.Gained.Baseline, Strokes.Gained.Category,
                                    Recovery.Shot)

# Filter the data by shots around the green and by shot locations
aroundTheGreenData = strokeData %>% filter(Strokes.Gained.Category == 'Around the Green')
filteredAroundTheGreenData = aroundTheGreenData %>% filter(From.Location.Scorer. == "Green Side Bunker" |
                                                                   From.Location.Scorer. == "Fairway" |
                                                                   From.Location.Scorer. == "Primary Rough") %>% droplevels()
filteredAroundTheGreenData = filteredAroundTheGreenData %>% filter(Elevation == "With" & Slope == "Level")
filteredAroundTheGreenData = filteredAroundTheGreenData %>% filter(From.Location.Scorer. == "Primary Rough" |
                                                                   From.Location.Scorer. == "Green Side Bunker" |
                                                                   From.Location.Scorer. == "Fairway") %>% droplevels()

filteredAroundTheGreenData = filteredAroundTheGreenData %>% filter((From.Location.Scorer. == "Fairway" & Lie == "Good") |
                                                                           (From.Location.Scorer. == "Green Side Bunker" & Lie == "Good") |
                                                                           (From.Location.Scorer. == "Primary Rough" & Lie == "N/A" )) %>% droplevels()
        
        

#2017 Data
        data2017 = filteredAroundTheGreenData %>% filter(Year == 2017 & In.the.Hole.Flag == "N") %>% droplevels()
        
        # Correlation
                corData = filteredAroundTheGreenData %>% filter(Year == 2017) %>% select(Distance.to.Pin, Distance.to.Hole.after.the.Shot) 
                
                cor(corData)

                
        # Mean Centering
                meanData = filteredAroundTheGreenData
                outlierKD(meanData, Distance.to.Pin)
                y
                
                meanData2017 = filteredAroundTheGreenData %>% filter(Year == 2017) %>% group_by(From.Location.Scorer. , Distance.to.Pin) %>% summarise(Avg.Distance.to.Hole.After.Shot = mean(Distance.to.Hole.after.the.Shot))
                
                meanPlot = ggplot(meanData2017, aes(x=Distance.to.Pin/36, y=Avg.Distance.to.Hole.After.Shot, color=From.Location.Scorer.)) +
                        xlab("Distance to the Pin Before the Shot (yards)") + 
                        ylab("Distance to the Hole After the Shot (inches)") +
                        ggtitle("Mean Centered Regression Plot") +
                        labs(color = "From Location") +
                        geom_point(alpha = 0.15) + 
                        geom_smooth(method=lm)
                print(meanPlot)
                
                meanresult = lm(Avg.Distance.to.Hole.After.Shot ~ Distance.to.Pin +  From.Location.Scorer. + From.Location.Scorer.:Distance.to.Pin, data = meanData2017)
                summary(meanresult)
                
        #Identify outliers in data
                outlierKD(data2017, Distance.to.Hole.after.the.Shot)
                y
        
        #  Remove outliers from dataset
                data2017 = data2017 %>% filter(!is.na(Distance.to.Hole.after.the.Shot)) 
                data2017 = data2017 %>% filter(!is.na(Distance.to.Pin))
                data2017 = data2017 %>% filter(!is.na(Distance)) 
        
        # Transform Distance.to.Hole.after.the.Shot and Distance.to.Pin data
                data2017 = data2017 %>% mutate(tuk.Distance.to.Hole.after.the.Shot = (Distance.to.Hole.after.the.Shot)^.275)
                data2017 = data2017 %>% mutate(tuk.Distance.to.Pin = (Distance.to.Pin)^.025)
        
        
                result1 = lm(tuk.Distance.to.Hole.after.the.Shot ~ tuk.Distance.to.Pin +  From.Location.Scorer. + From.Location.Scorer.:tuk.Distance.to.Pin, data = data2017)
                summary(result1)
                
                plot(result1)
                
                result2 = aov(tuk.Distance.to.Hole.after.the.Shot ~ From.Location.Scorer., data = data2017)
                summary(result2)
                
                summaryBy(tuk.Distance.to.Hole.after.the.Shot ~ From.Location.Scorer., data = data2017,
                          FUN = list(mean, median, sd))
                
                par(mfrow=c(2,2))
                plot(result3, col = alpha('black',.05))
        
        # Plot the results
                sPlot1 = ggplot(data2017, aes(x=tuk.Distance.to.Pin, y=tuk.Distance.to.Hole.after.the.Shot, color=From.Location.Scorer.)) +
                        xlab("Distance to the Pin Before the Shot") + 
                        ylab("Distance to the Hole After the Shot") +
                        ggtitle("Regression Plot With Transformed Data") +
                        labs(color = "From Location") +
                        geom_point(alpha = 0.05) + 
                        geom_smooth(method=lm)
                print(sPlot1)
                
                sPlot2 = ggplot(data2017, aes(x=Distance.to.Pin/36, y=Distance.to.Hole.after.the.Shot, color=From.Location.Scorer.)) +
                        xlab("Distance to the Pin Before the Shot (yards)") + 
                        ylab("Distance to the Hole After the Shot (inches)") +
                        ggtitle("Regression Plot") +
                        labs(color = "From Location") +
                        geom_point(alpha = 0.05) + 
                        geom_smooth(method=lm)
                print(sPlot2)
        
        #QQ Plots
                par(mfrow=c(1,2))
                
                qqnorm(data2017$Distance.to.Hole.after.the.Shot, main = 'Before Transformation')
                qqline(data2017$Distance.to.Hole.after.the.Shot, col = 'red')
                
                qqnorm(data2017$tuk.Distance.to.Hole.after.the.Shot, main = 'After Transformation')
                qqline(data2017$tuk.Distance.to.Hole.after.the.Shot, col = 'red')
                title("QQ Plots of Distance to the Hole After the Shot", outer = TRUE)
                
                
                qqnorm(data2017$Distance.to.Pin, main = 'Before Transformation')
                qqline(data2017$Distance.to.Pin, col = 'red')
                
                qqnorm(data2017$tuk.Distance.to.Pin, main = 'After Transformation')
                qqline(data2017$tuk.Distance.to.Pin, col = 'red')
                title("QQ Plots of Distance to the Pin Before the Shot", outer = TRUE)
                
                
                mysample = data2017 %>% sample_n(size = 5000)
                
                transformTukey(mysample$Distance.to.Pin)


# Proportion tests
        # In the hole data
                inTheHole = filteredAroundTheGreenData %>% filter(Year == 2017)
                
                inTheHole = inTheHole %>% group_by(From.Location.Scorer.) %>% count(In.the.Hole.Flag)
        
        # In the hole prop.test
        
                fairway = c(811, 31277) 
                rough = c(444, 28372)
                bunker = c(202, 19982)
                
                fairwayRough = prop.test(x = c(811, 444), n = c(31277, 28372))
                fairwayBunker = prop.test(x = c(811, 202), n = c(31277, 19982))
                roughBunker = prop.test(x = c(444, 202), n = c(28372, 19982))
                
                fairwayRough
                fairwayBunker
                roughBunker

       
# Create a plot for expected results of analysis     
        lie <- c('Fairway','Fairway','Primary Rough', 'Primary Rough', 'Green Side Bunker', 'Green Side Bunker')
        xvalues <- c(2.5, 45, 3, 45, 4, 45)
        yvalues <- c(25, 125, 60, 329, 100, 600)
        graphData = data.frame(lie, xvalues, yvalues)
        
        generalPlot = ggplot(graphData, aes(x = xvalues, y = yvalues, color = lie)) +
                geom_line() + 
                xlab('Distance to the Pin Before the Shot') + 
                ylab('Distance to the Hole After the Shot') +
                ggtitle("Expected Results of Analysis") +
                labs(color = "From Location") +
                theme(axis.text = element_blank(),
                      axis.ticks = element_blank())
        print(generalPlot)
        
# Create a frequency plot for each shot location
        freqPlot = ggplot(data2017, aes(From.Location.Scorer., fill = From.Location.Scorer.)) + 
                geom_bar() +
                theme(legend.position = "none")+
                ggtitle("Frequency of Shots From Each Shot Location") +
                ylab('Count') + 
                xlab('From Location')
        print(freqPlot)
        