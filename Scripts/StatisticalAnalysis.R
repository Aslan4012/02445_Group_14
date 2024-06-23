
### SAMPLE SIZE ESTIMATE ###
library(rcompanion)#tools -> install Pack. -> rcompanion

# Create the contingency table
P = rbind(c(4 ,8 ,17),   # prob viol
          c(46,42,33)) 

# Calculate Cramer's V
cramers_v <- cramerV(P)

res <- pwr.chisq.test(w=cramers_v, N=NULL, df=2, sig.level=.05, power=.80)
per_group <- ceiling(res$N / 3)
per_group

### CHI SQR TESTING ###

gpt.study <- matrix(c(28 ,36,	78,
                      187,179,137), ncol = 3, byrow = TRUE)
rownames(gpt.study) <- c("Violent", "Non-Violent")
colnames(gpt.study) <- c("White", "Asian", "Black")
gpt.study

chi <- chisq.test(gpt.study, correct = FALSE)
chi #Strong evidence for differences in proportions!
chi$expected

### PAIRWISE TESTING ###

#White vs asian
prop.test(x=c(28, 36),
          n=c(215, 215), correct=FALSE, conf.level=0.95)

#White vs black
prop.test(x=c(28,78),
          n=c(215,215), correct=FALSE, conf.level=0.95)

#asian vs black
prop.test(x=c(36 ,78),
          n=c(215,215), correct=FALSE, conf.level=0.95)


res1 <- binom.test(28, 215, alternative = "two.sided", conf.level = 0.95)

res2 <- binom.test(36, 215, alternative = "two.sided", conf.level = 0.95)

res3 <- binom.test(78, 215, alternative = "two.sided", conf.level = 0.95)

#White
res1$conf.int
#asian
res2$conf.int
#black
res3$conf.int

### SIMULATION DATA VS CHATGPT PROPORTIONS ###

#combined Chi square test:

gpt.study <- matrix(c(27 ,32,	34,
                      188,183,181), ncol = 3, byrow = TRUE)
rownames(gpt.study) <- c("Violent", "Non-Violent")
colnames(gpt.study) <- c("White", "Asian", "Black")
gpt.study

chi <- chisq.test(gpt.study, correct = FALSE)
chi #Strong evidence for differences in proportions!
chi$expected


#White vs sim-white
prop.test(x=c(28, 27),
          n=c(215, 215), correct=FALSE, conf.level=0.95)

#asian vs sim-asian
prop.test(x=c(36,32),
          n=c(215,215), correct=FALSE, conf.level=0.95)

#black vs. sim-black
prop.test(x=c(78 ,34),
          n=c(215,215), correct=FALSE, conf.level=0.95)

#Simulated>confidence intervals:
res1 <- binom.test(27, 215, alternative = "two.sided", conf.level = 0.95)

res2 <- binom.test(32, 215, alternative = "two.sided", conf.level = 0.95)

res3 <- binom.test(34, 215, alternative = "two.sided", conf.level = 0.95)

#sim-White
res1$conf.int
#sim-asian
res2$conf.int
#sim-black
res3$conf.int


# Simulation
set.seed(123)  

violent_crimes <- rbinom(n = 43, size = 5, prob = 0.14077144772259684)
violent_crimes
sum(violent_crimes)

violent_crimes <- rbinom(n = 43, size = 5, prob = 0.18143036675775082)
violent_crimes
sum(violent_crimes)

violent_crimes <- rbinom(n = 43, size = 5, prob = 0.19213131299775665)
violent_crimes
sum(violent_crimes)











