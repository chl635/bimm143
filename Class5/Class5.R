#' ---
#' title: "Data exploration and visualization"
#' author: "Christopher Lee"
#' date: "October 15, 2019"
#' output: github_document
#' ---



# Class 5 Data Visualization
x <- rnorm(1000)

# Some summary stats
mean(x)
sd(x)

summary(x)
boxplot(x)

hist(x)
rug(x)

# Section 2 Scatterplot
# Lets read our input file first

baby <- read.table("bimm143_05_rstats/weight_chart.txt", header = TRUE)
plot(baby$Age, baby$Weight, type = "o", 
     pch = 15, cex = 1.5, lwd = 2, ylim = c(2,10), 
     xlab = "Age (months)", ylab = "Weight (kg)", 
     main = "Baby Weight with Age", col = "blue")
?plot


mouse <- read.table(file = "bimm143_05_rstats/feature_counts.txt", header = TRUE, sep = "\t")
mouse
par(mar=c(3.1, 11.1, 4.1, 2))
barplot(mouse$Count, horiz = TRUE, 
        names.arg = mouse$Feature, 
        las = 1, 
        main = "Number of Features in the Mouse GRCm38 Genome", 
        ylab = "", xlim = c(0,80000))


sex <- read.table("bimm143_05_rstats/male_female_counts.txt", header = TRUE, sep = "\t")
sex
barplot(sex$Count, ylab = "Counts", names.arg = sex$Sample, las = 2, col = c("blue2", "red2"))


        