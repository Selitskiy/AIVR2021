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

# Parse makeup only sessions and load
#grep -v NM predict_ir_6bmsr4.txt > predict_ir_6bmsr4_m.txt
fname <- "predict_gn_6bfmsr6_m.txt"
m_test <- read.delim(fname, sep = "", header = T, na.strings = " ", fill = T)
mr <- nrow(m_test)


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
hr <- nrow(h_test)

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