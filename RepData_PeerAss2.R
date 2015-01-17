download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
              "StormData.csv.bz2", method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
              "StormDataDoc.pdf", method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf",
              "StormDataFAQ.pdf", method = "curl")
library(R.utils)
library(ggplot2)
#bunzip2("StormData.csv.bz2")
storm.data <- read.csv("StormData.csv")

# Change all column names to lower case
names(storm.data) <- tolower(names(storm.data))

# Sum up number of fatalities and injuries per each type of event then merge into a single data frame. 
event.fat <- aggregate(fatalities ~ evtype, data = storm.data, sum)
event.inj <- aggregate(injuries ~ evtype, data = storm.data, sum)
event <- merge(event.fat, event.inj, "evtype")
event$total <- event$fatalities + event$injuries
# remove '0' values
event.pop <- event[rowSums(event[, -1] > 0) != 0, ]

# Summary 
summary(event.pop)

# Subset only values larger than the mean
event.high <- event.pop[event$total > mean(event.pop$total), ]
event.high <- event.high[order(event.high$total, decreasing = TRUE), ]

ggplot(event.high, aes(x=reorder(evtype, total), y=total, fill = evtype)) + 
    theme_bw() + geom_bar(stat="identity") + coord_flip() + 
    ggtitle("Population harm caused by weather effects") +
    theme(axis.title.y = element_blank(), legend.position = "none") +
    ylab("Sum of fatalities and injuries")
    



# Data in untidy format needs to be extracted and altered before proceeding
evtype <- storm.data$evtype
prop.d <- storm.data$propdmg
prop.x <- storm.data$propdmgexp
crop.d <- storm.data$cropdmg
crop.x <- storm.data$cropdmgexp

# Set data frame and select only rows in which a modifier for the damage is presant 
dmg <- data.frame(evtype, prop.d, prop.x, crop.d, crop.x)
dmg <- subset(dmg, grepl("[MmBb]", dmg$prop.x) | grepl("[MmBb]", dmg$crop.x))

# Rather than keeping the value and units seperate, generate real number figure for each obs.
for (i in 1:nrow(dmg)) {
    if (grepl("[Mm]", dmg$prop.x[i])) {
        dmg$prop.d[i] <- 1e+06 * dmg$prop.d[i]
    }
    if (grepl("[Bb]", dmg$prop.x[i])) {
        dmg$prop.d[i] <- 1e+09 * dmg$prop.d[i]
    }
    if (grepl("[Mm]", dmg$crop.x[i])) {
        dmg$crop.d[i] <- 1e+06 * dmg$crop.d[i]
    }
    if (grepl("[Bb]", dmg$crop.x[i])) {
        dmg$crop.d[i] <- 1e+09 * dmg$crop.d[i]
    }
}

# Sum up all property and crop damage per each event type and merge into a single data frame with total
prop <- aggregate(prop.d ~ evtype, data = dmg, sum)
crop <- aggregate(crop.d ~ evtype, data = dmg, sum)
damage <- merge(prop, crop, "evtype")
damage$total <- damage$crop + damage$prop

summary(damage)

# Select only values larger than the mean
damage.high <- damage[damage$total > mean(damage$total), ]
damage.high <- damage.high[order(damage.high$total, decreasing = TRUE), ]

# Plot results. Coord_flip for purpose of fitting names
ggplot(damage.high, aes(x=reorder(evtype, total), y=total, fill = evtype)) + 
    theme_bw() + geom_bar(stat="identity") + coord_flip() +
    theme(axis.title.y = element_blank(), legend.position = "none") +
    ggtitle("Economic consequenses of weather effects") +
    ylab("Sum of property and crop damage ($)")