install.packages("ggplot2")
install.packages("readtext")
install.packages("psych")
install.packages("reshape2")

library(readtext)
library(ggplot2)
library(psych)
library(reshape2)

# Set working directory
setwd("Documents/BookClub/BC2Emo")


# Load Expression Recognition Heatmap, removing headers
#grep "\"" result_in_6bfemsr6.txt > result_in_6bfemsr6_nh.txt
an_name <- "result_an_6bfemsr7_nh.txt"
an_test <- read.delim(an_name, sep = "", header = F, na.strings = " ", fill = T)

vg_name <- "result_vgg_6bfemsr7_nh.txt"
vg_test <- read.delim(vg_name, sep = "", header = F, na.strings = " ", fill = T)

gn_name <- "result_gn_6bfemsr7_nh.txt"
gn_test <- read.delim(gn_name, sep = "", header = F, na.strings = " ", fill = T)

rn_name <- "result_rn_6bfemsr7_nh.txt"
rn_test <- read.delim(rn_name, sep = "", header = F, na.strings = " ", fill = T)

in_name <- "result_in_6bfemsr7_nh.txt"
in_test <- read.delim(in_name, sep = "", header = F, na.strings = " ", fill = T)

ir_name <- "result_ir_6bfemsr7_nh.txt"
ir_test <- read.delim(ir_name, sep = "", header = F, na.strings = " ", fill = T)

all_test <- (an_test[,c(2,4)] + vg_test[,c(2,4)] + gn_test[,c(2,4)] + rn_test[,c(2,4)] + in_test[,c(2,4)] + ir_test[,c(2,4)])/6
in_test[,13:14] <- all_test[,1:2]

in_test_s <- in_test[order(all_test$V2),]
in_test_se <- in_test_s[in_test_s$V2.1<=0. & in_test_s$V4.1>0.8,]
in_test_sc <- in_test_s[in_test_s$V2.1>=0.8 & in_test_s$V3 != "CE",]



# Parse makeup only sessions and load
#grep -v NM predict_ir_6bmsr4.txt > predict_ir_6bmsr4_m.txt
fname <- "predict_in_6bfmsr7_mS1MK7HP.txt"
m_test <- read.delim(fname, sep = "", header = T, na.strings = " ", fill = T)
#mr <- nrow(m_test)


#Process
t2 <- m_test[,1:7]
t2[,8] <- FALSE

names(t2) <- c("Correct_class", "Score", "Guess_class", "TrustedVote", "VoteScore", "TrustedScore","File", "Flag")
for (i in 1:mr){
  if( t2$Correct_class[i] == t2$Guess_class[i] ){
    t2$Flag[i] <- TRUE
  }
}

t2r <- t2[t2$Flag == TRUE,]
t2w <- t2[t2$Flag != TRUE,]

# Set up thresholds
TVt = 0.5
VSt = 0.8

#Untrusted Accuracy
length(t2r$Score)/(length(t2r$Score) + length(t2w$Score))

#Trusted Accuracy Meta
(sum(t2r$TrustedVote >= TVt)+sum(t2w$TrustedVote < TVt))/(length(t2r$Score) + length(t2w$Score))
(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)+sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt))/(length(t2r$Score) + length(t2w$Score))
#Precision Meta
sum(t2r$TrustedVote >= TVt)/(sum(t2r$TrustedVote >= TVt) + sum(t2w$TrustedVote >= TVt))
sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)/(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt) + sum(t2w$TrustedVote >= TVt & t2w$VoteScore >= VSt))
#Recall Meta
sum(t2r$TrustedVote >= TVt)/(sum(t2r$TrustedVote >= TVt) + sum(t2r$TrustedVote < TVt)) #length(t2r$Score)
sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)/(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt) + sum(t2r$TrustedVote < TVt & t2r$VoteScore >= VSt))
#Specificity Meta
sum(t2w$TrustedVote < TVt)/(sum(t2w$TrustedVote < TVt) + sum(t2w$TrustedVote >= TVt)) #length(t2w$Score)
sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt)/(sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt) + sum(t2w$TrustedVote >= TVt & t2w$VoteScore >= VSt))




# Load Expression Recognition Heatmap, removing headers
#grep "\"" result_in_6bfmsr6.txt > result_in_6bfmsr6_nh.txt
hname <- "result_vgg_6bfmsr6_nh.txt"
h_test <- read.delim(hname, sep = "", header = F, na.strings = " ", fill = T)
#hr <- nrow(h_test)

th <- h_test[,1:2]
th[,2:9] <- h_test[,5:12]

names(th) <- c("Expr", "AN", "CE", "DS", "HP", "NE", "SA", "SC", "SR")
rownames(th) <- th[,1]
#th[,1] <- NULL

thm <- melt(th)
names(thm) <- c("Expression", "Guess", "Score")
               
ggplot(thm, aes(x = Expression, Guess)) +
  geom_tile(aes(fill = Score), alpha=0.9) +
  geom_text(aes(label = round(Score, 4)))