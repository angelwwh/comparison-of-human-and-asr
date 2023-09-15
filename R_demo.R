rm(list = ls())  # delete all variables, clear cache

#install.packages('stringr')
#install.packages("tidyverse")
install.packages("cvms")
library("tidyverse")
library("ggplot2")
library(stringr)
library("cvms")

#set variables for input directory (where the result files are) and output directory (Anova output, etc)
input_dir = "/Users/waihuenwong/Desktop/RESULTS/INPUTS"
output_dir = "/Users/waihuenwong/Desktop/RESULTS/OUTPUTS"
#input_dir = "C:/Users/j.schlittenlacher/OneDrive - University College London/R/demo/results"
#output_dir = "C:/Users/j.schlittenlacher/OneDrive - University College London/R/demo"

#set working directory (to the input directory)
setwd( input_dir )

filenames = dir()[grep(".txt", dir())]        # get filenames of all text files in the working directory
#filenames = dir()[grep("^2.*.txt$", dir())]   # create variable with filenames of all text files in the working directory that start with "2" and end with ".txt"

num_files = length( filenames ) # length of filenames is the number of files that we want to iterate through

all_res= NULL # table for all data, empty for now

i=1

experiment_list = c("1", "2", "1", "2", "2", "1")

for ( i in 1:num_files ) # iterate through all files
{
  this_res = read.table(filenames[i], header=F)                                 # read table from a (text) file. First row in that file contains data, not column headers
  subject = substr(filenames[i],3,7)                                            # participant id is encoded in the filename, 3rd to 6th character of it
  block_number = substr(filenames[i],1,2)
  experiment = experiment_list[as.numeric(block_number)]
  responded_phoneme = this_res[,2]                                               # dependent variable is in second column
  a = do.call(rbind, strsplit(as.character(this_res[,1]),split="/") )           # first column contains filenames in format "sounds/vc4_143.wav". Split by "/"
  b = do.call(rbind, strsplit(as.character(a[,2]),split='_') )                  # split "vc4_143.wav" by the underscore.
  presented_phoneme = b[,1]                                                             # The condition is represented by the part before the underscore
  #bands = as.numeric( gsub("[^0-9.-]", "", predicted) )                         # The number of bands are represented by the number in the condition
  response = str_detect(presented_phoneme, responded_phoneme ) * 1                                 # 0 if not 'vc', 1 if vc. You could add a 2 for a third type if necessary. If many, it is better coding to store the name of the vocoder!
  #recognition_rate = consonant_type*100
  sound_id = b[,2]                                                              # Not used for analysis, the part after the _ represents the recording that was used
  duration = gsub(".wav,","", b[,3])
  this_table = cbind(experiment, block_number, subject, duration, presented_phoneme, responded_phoneme, response, sound_id) # form a table with 6 columns of the variables that we just created
  all_res = rbind(all_res,this_table)                                           # attach this table at bottom of results table all_res
}
all_res = data.frame(all_res)                                                   # transform to data frame type

all_res = all_res %>%  filter(duration != "undefined,")

# calculate means and standard deviations for each condition using "aggregate"
#analyse two experiment independently = create 2 variables 
all_res1 = subset(all_res, experiment == 1) #*$
all_res2 = subset(all_res, experiment == 2)

all_res1_25 = all_res1 %>%  filter(duration == 25)
all_res1_50 = all_res1 %>%  filter(duration == 50)
all_res1_100 = all_res1 %>%  filter(duration == 100)
all_res1_200 = all_res1 %>%  filter(duration == 200)


all_res2_25 = all_res2 %>%  filter(duration == 25)
all_res2_50 = all_res2 %>%  filter(duration == 50)
all_res2_100 = all_res2 %>%  filter(duration == 100)
all_res2_200 = all_res2 %>%  filter(duration == 200)

# learning effects
all_recall_rate_blk = aggregate(as.numeric(all_res$response), list(block_number=all_res$block_number),mean)
# standard deviation
all_recall_rate_blk_subject = aggregate(as.numeric(all_res$response), list(block_number=all_res$block_number, subject=all_res$subject),mean)
all_recall_rate_blk_sd = aggregate(as.numeric(all_recall_rate_blk_subject$x), list(block_number=all_recall_rate_blk_subject$block_number),sd)
# plotting
ggplot(data = all_recall_rate_blk, aes(x = block_number, y = x, group = 1)) +
  labs(x = 'run number', y = 'recall') + 
  geom_line(linetype = "dashed") +
  geom_errorbar(
    aes(
      ymin = all_recall_rate_blk$x - all_recall_rate_blk_sd$x, 
      ymax = all_recall_rate_blk$x + all_recall_rate_blk_sd$x
    ), width = .2, position = position_dodge(.9)) +
  ylim(0.5, 1.0) + 
  geom_point()

blk1_recall_rate = aggregate(as.numeric(all_res1$response), list(duration=all_res1$duration,presented_phoneme=all_res1$presented_phoneme),mean)
blk2_recall_rate = aggregate(as.numeric(all_res2$response), list(duration=all_res2$duration,presented_phoneme=all_res2$presented_phoneme),mean)

blk1_precision = aggregate(as.numeric(all_res1$response), list(duration=all_res1$duration,responded_phoneme=all_res1$responded_phoneme),mean)
blk2_precision = aggregate(as.numeric(all_res2$response), list(duration=all_res2$duration,responded_phoneme=all_res2$responded_phoneme),mean)

blk1_accuracy = aggregate(as.numeric(all_res1$response), list(duration=all_res1$duration),mean)
blk2_accuracy = aggregate(as.numeric(all_res2$response), list(duration=all_res2$duration),mean)

blk1_recall_rate_subject = aggregate(as.numeric(all_res1$response), list(duration=all_res1$duration,presented_phoneme=all_res1$presented_phoneme,subject=all_res1$subject),mean)
blk1_precision_rate_subject = aggregate(as.numeric(all_res1$response), list(duration=all_res1$duration,responded_phoneme=all_res1$responded_phoneme,subject=all_res1$subject),mean)
blk1_accuracy_subject = aggregate(as.numeric(all_res1$response), list(duration=all_res1$duration,subject=all_res1$subject),mean)
blk1_recall_rate_sd = aggregate(as.numeric(blk1_recall_rate_subject$x), list(duration=blk1_recall_rate_subject$duration,presented_phoneme=blk1_recall_rate_subject$presented_phoneme),sd)
blk1_precision_sd = aggregate(as.numeric(blk1_precision_rate_subject$x), list(duration=blk1_precision_rate_subject$duration,responded_phoneme=blk1_precision_rate_subject$responded_phoneme),sd)

blk2_recall_rate_subject = aggregate(as.numeric(all_res2$response), list(duration=all_res2$duration,presented_phoneme=all_res2$presented_phoneme,subject=all_res2$subject),mean)
blk2_precision_rate_subject = aggregate(as.numeric(all_res2$response), list(duration=all_res2$duration,responded_phoneme=all_res2$responded_phoneme,subject=all_res2$subject),mean)
blk2_accuracy_subject = aggregate(as.numeric(all_res1$response), list(duration=all_res2$duration,subject=all_res2$subject),mean)
blk2_recall_rate_sd = aggregate(as.numeric(blk2_recall_rate_subject$x), list(duration=blk2_recall_rate_subject$duration,presented_phoneme=blk2_recall_rate_subject$presented_phoneme),sd)
blk2_precision_sd = aggregate(as.numeric(blk2_precision_rate_subject$x), list(duration=blk2_precision_rate_subject$duration,responded_phoneme=blk2_precision_rate_subject$responded_phoneme),sd)

#recall_rate = aggregate(as.numeric(all_res$response), list(duration=all_res$duration,presented_phoneme=all_res$presented_phoneme),mean)
#precision = aggregate(as.numeric(all_res$response), list(duration=all_res$duration,responded_phoneme=all_res$responded_phoneme),mean)
#accuracy = aggregate(as.numeric(all_res$response), list(duration=all_res$duration),mean)

#plot a confusion matrix = look up for functions in R 
#mean / response for each presented, responded phoneme
#x-axis: duration, y= recall/ precision, colours= phonemes, sd = across participants (error bar)


# 2-factors within-subject ANOVA
setwd( output_dir )
sink("ANOVA experiment_blk1recall.txt")
summary(aov(x~as.factor(duration)*as.factor(presented_phoneme)+Error(as.factor(subject)/(as.factor(duration)*as.factor(presented_phoneme))),blk1_recall_rate_subject))
sink()

setwd( output_dir )
sink("ANOVA experiment_blk1precision.txt")
summary(aov(x~as.factor(duration)*as.factor(responded_phoneme)+Error(as.factor(subject)/(as.factor(duration)*as.factor(responded_phoneme))),blk1_precision_rate_subject))
sink()

setwd( output_dir )
sink("ANOVA experiment_blk1accuracy.txt")
summary(aov(x~as.factor(duration)+Error(as.factor(subject)/(as.factor(duration))),blk1_accuracy_subject))
sink()

# 2-factors within-subject ANOVA
setwd( output_dir )
sink("ANOVA experiment_blk2recall.txt")
summary(aov(x~as.factor(duration)*as.factor(presented_phoneme)+Error(as.factor(subject)/(as.factor(duration)*as.factor(presented_phoneme))),blk2_recall_rate_subject))
sink()

setwd( output_dir )
sink("ANOVA experiment_blk2precision.txt")
summary(aov(x~as.factor(duration)*as.factor(responded_phoneme)+Error(as.factor(subject)/(as.factor(duration)*as.factor(responded_phoneme))),blk2_precision_rate_subject))
sink()

setwd( output_dir )
sink("ANOVA experiment_blk2accuracy.txt")
summary(aov(x~as.factor(duration)+Error(as.factor(subject)/(as.factor(duration))),blk2_accuracy_subject))
sink()

trials_per_condition = nrow(all_res) / nrow(sd_recognition)
se = sd_recognition$x / sqrt( trials_per_condition )
ci95 = 1.96 * se
m  = mean_recognition$x

# precision and recall plots
ggplot(
  merge(blk1_precision %>% rename(mean = x), blk1_precision_sd %>% rename(sd = x)), 
  aes(x = factor(duration, level = c('25', '50', '100', '200')), y = mean, fill = responded_phoneme)
) + 
  labs(x = 'duration', y = 'precision', fill = 'consonant') + 
  scale_y_continuous(breaks = seq(0.0, 1.1, by = 0.2), limits = c(0.0, 1.1)) + 
  geom_bar(position =position_dodge(), stat = "identity", colour='black') + 
  geom_text(aes(label = round(mean, digits = 2)), position = position_dodge(0.9), vjust = 10, hjust = 0.5) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9))

ggplot(
  merge(blk1_recall_rate %>% rename(mean = x), blk1_recall_rate_sd %>% rename(sd = x)), 
  aes(x = factor(duration, level = c('25', '50', '100', '200')), y = mean, fill = presented_phoneme)
) + 
  labs(x = 'duration', y = 'recall', fill = 'consonant') + 
  scale_y_continuous(breaks = seq(0.0, 1.1, by = 0.2), limits = c(0.0, 1.1)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black') + 
  geom_text(aes(label = round(mean, digits = 2)), position = position_dodge(0.9), vjust = 10, hjust = 0.5) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9))

ggplot(
  merge(blk2_precision %>% rename(mean = x), blk2_precision_sd %>% rename(sd = x)), 
  aes(x = factor(duration, level = c('25', '50', '100', '200')), y = mean, fill = responded_phoneme)
) + 
  labs(x = 'duration', y = 'precision', fill = 'consonant') + 
  scale_y_continuous(breaks = seq(0.0, 1.1, by = 0.2), limits = c(0.0, 1.1)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black') + 
  geom_text(aes(label = round(mean, digits = 2)), position = position_dodge(0.9), vjust = 10, hjust = 0.5) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9))

ggplot(
  merge(blk2_recall_rate %>% rename(mean = x), blk2_recall_rate_sd %>% rename(sd = x)), 
  aes(x = factor(duration, level = c('25', '50', '100', '200')), y = mean, fill = presented_phoneme)
) + 
  labs(x = 'duration', y = 'recall', fill = 'consonant') + 
  scale_y_continuous(breaks = seq(0.0, 1.1, by = 0.2), limits = c(0.0, 1.1)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black') + 
  geom_text(aes(label = round(mean, digits = 2)), position = position_dodge(0.9), vjust = 10, hjust = 0.5) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position=position_dodge(0.9))

###
#confusion matrix
conf_mat1 <- confusion_matrix(targets = all_res1$presented_phoneme,
                             predictions = all_res1$responded_phoneme)

plot_confusion_matrix(conf_mat1$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

conf_mat1_25 <- confusion_matrix(targets = all_res1_25$presented_phoneme,
                             predictions = all_res1_25$responded_phoneme)

plot_confusion_matrix(conf_mat1_25$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

conf_mat1_50 <- confusion_matrix(targets = all_res1_50$presented_phoneme,
                             predictions = all_res1_50$responded_phoneme)

plot_confusion_matrix(conf_mat1_50$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

conf_mat1_100 <- confusion_matrix(targets = all_res1_100$presented_phoneme,
                             predictions = all_res1_100$responded_phoneme)

plot_confusion_matrix(conf_mat1_100$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

conf_mat1_200 <- confusion_matrix(targets = all_res1_200$presented_phoneme,
                             predictions = all_res1_200$responded_phoneme)

plot_confusion_matrix(conf_mat1_200$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)


conf_mat2 <- confusion_matrix(targets = all_res2$presented_phoneme,
                              predictions = all_res2$responded_phoneme)

plot_confusion_matrix(conf_mat2$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

conf_mat2_25 <- confusion_matrix(targets = all_res2_25$presented_phoneme,
                                 predictions = all_res2_25$responded_phoneme)

plot_confusion_matrix(conf_mat2_25$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

conf_mat2_50 <- confusion_matrix(targets = all_res2_50$presented_phoneme,
                                 predictions = all_res2_50$responded_phoneme)

plot_confusion_matrix(conf_mat2_50$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

conf_mat2_100 <- confusion_matrix(targets = all_res2_100$presented_phoneme,
                                  predictions = all_res2_100$responded_phoneme)

plot_confusion_matrix(conf_mat2_100$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

conf_mat2_200 <- confusion_matrix(targets = all_res2_200$presented_phoneme,
                                  predictions = all_res2_200$responded_phoneme)

plot_confusion_matrix(conf_mat2_200$`Confusion Matrix`[[1]], 
                      add_normalized = FALSE)

# paired t-tests
all_res2_b <- (all_res2 %>% filter(presented_phoneme == 'b'))$response
all_res2_p <- (all_res2 %>% filter(presented_phoneme == 'p'))$response
t_test_place <- t.test(x = all_res2_b, y = all_res2_p, paired = TRUE)



pairwise.t.test(blk1_recall_rate_subject$x, blk1_recall_rate_subject$presented_phoneme, p.adj='holm')
pairwise.t.test(blk2_recall_rate_subject$x, blk2_recall_rate_subject$presented_phoneme, p.adj='holm')


######

line_width = 1.5
expansion = 1.2
arrow_width = 0.05

plot( 1:4, m[1:4],  type="p", col="black", pch=1, lty=1, xlab="Number of channels", ylab="Percentage of words recognized", ylim=c(0,100), xlim=c(0.5,4.5),lwd=line_width, cex.axis=expansion, cex.lab = expansion, cex = expansion, xaxt="none" )
axis(side = 1, at = 1:4, labels = c("2","3","4","8"))
arrows(1:4, m[1:4]+se[1:4], 1:4, m[1:4]-se[1:4], angle=90, code=3,lwd=line_width,length=arrow_width  )
lines(1:4, m[5:8],  type="p", col="red", pch=0, lty=1,lwd=line_width, cex.axis=expansion, cex.lab = expansion, cex = expansion)
arrows(1:4, m[5:8]+se[5:8], 1:4, m[5:8]-se[5:8], angle=90, code=3,lwd=line_width,length=arrow_width, col="red"  )
legend(3.2, 30, legend=c("WaveGlow", "Noise vocoder"),col=c("black", "red"), pch=c(1,0), cex=1)

#######

# 2-factors within-subject ANOVA
setwd( output_dir )
sink("ANOVA experiment.txt")
summary(aov(recognition_rate~as.factor(bands)*as.factor(vocoder_type)+Error(as.factor(subject)/(as.factor(bands)*as.factor(vocoder_type))),all_res))
sink()
