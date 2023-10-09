# Sugnet Lubbe, MuViSU, January 2023
# ==================================

  require(tidyverse)

  pathname <- NULL
  pathname <- "C:\\Users\\tella\\OneDrive\\Documents\\Documents for new laptop\\Postdoc_projects\\Brink_UCT MM FMT donor repository\\HREC FMT survey\\Analysis\\Analysis_Sugnet\\"
  setwd <- ("C:\\Users\\tella\\OneDrive\\Documents\\Documents for new laptop\\Postdoc_projects\\Brink_UCT MM FMT donor repository\\HREC FMT survey\\Analysis\\Analysis_Sugnet\\")


  donor.data <- tibble(read.csv("C:\\Users\\tella\\OneDrive\\Documents\\Documents for new laptop\\Postdoc_projects\\Brink_UCT MM FMT donor repository\\HREC FMT survey\\Analysis\\Analysis_Sugnet\\MTU_data.csv"))

  donor.data
  colnames(donor.data)

  donor.data <- select (donor.data, -c(3:6,11)) %>%
                rename (., ID = Participant_number,
                           Q.no = Questionnaire_number,
                           Type = Sample_type,
                           Occupation = Occupation_categories,
                           Regular.blood = Q3.V_Q4.S,
                           Organ.donor = Q4.V_Q5.S,
                           Prior.healthy = Q5.V_Q6.S,
                           Prior.transplant = Q6.V_Q7.S,
                           Prior.transplant.help = Q7.V_Q8.S,
                           Prior.sampled = Q8.V,
                           Preferred.collection = Q9.V,
                           Willing.donor = Q10.V_Q9.S,
                           Donor.frequency = Q11.V_Q10.S,
                           Prior.collection.kit = Q11.S,
                           Using.collection.kit = Q12.S,
                           Self.collect = Q12.V_Q13.S,
                           Drop.off = Q13.V_Q14.S,
                           Compensation = Q14.V_Q15.S,
                           Affect.blood = Q16.V_Q17.S,
                           Used.for = Q17.V_Q18.S,
                           Reason = Q18.V_Q19.S,
                           Helping.others = Q19.V_Q20.S,
                           Feedback = Q20.V_Q21.S,
                           Receive.transplant = Q21.V_Q22.S,
                       ) %>%
               mutate (., Amount = ifelse(Q15.V_Q16.S=="other",Q15.V_Q16.S_other,Q15.V_Q16.S),
                       .keep="unused") %>%
               relocate (., Amount, .after=Compensation) %>%
               replace_na(list(N1 = 'No', N2 = 'No', N3 = 'No', N4 = 'No', N5 = 'No', N6 = 'No', 
                               N7 = 'No', N8 = 'No', N9 = 'No', N10 = 'No')) %>%
               filter (., !is.na(Willing.donor)) 
  donor.data

  with (donor.data, table(Gender, exclude=NULL))
  donor.data$Gender[is.na(donor.data$Gender)] <- "other"
  with (donor.data, table(Gender, exclude=NULL))
  with (donor.data, table(Occupation, exclude=NULL))
  
  donor.data$Occupation[donor.data$Occupation=="administrative support"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="art"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="cleaning services"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="computer science"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="construction and property"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="education"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="engineer/technician"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="finance"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="health sciences"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="homemaker"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="hospitality/tourism"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="law"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="law enforcement"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="manager/coordinator/relations"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="other"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="researcher"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="retired"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="self-employed"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="social science"] <- "employed"
  donor.data$Occupation[donor.data$Occupation=="learner"] <- "learner/student"
  donor.data$Occupation[donor.data$Occupation=="student"] <- "learner/student"
  donor.data$Occupation[is.na(donor.data$Occupation)] <- "missing"
  with (donor.data, table(Occupation, exclude=NULL))
  
  with (donor.data, table(Regular.blood, exclude=NULL))
  with (donor.data, table(Organ.donor, exclude=NULL))
  with (donor.data, table(Prior.healthy, exclude=NULL))
  donor.data$Prior.healthy[is.na(donor.data$Prior.healthy)] <- "unsure"
  with (donor.data, table(Prior.healthy, exclude=NULL))
  with (donor.data, table(Prior.transplant, exclude=NULL))
  with (donor.data, table(Prior.transplant.help, exclude=NULL))
  with (donor.data, table(Donor.frequency, exclude=NULL))
  donor.data$Donor.frequency[is.na(donor.data$Donor.frequency)] <- "missing"
  with (donor.data, table(Donor.frequency, exclude=NULL))
  with (donor.data, table(Prior.collection.kit[Type=="stool"], exclude=NULL))
  donor.data$Prior.collection.kit[donor.data$Type=="stool"][is.na(donor.data$Prior.collection.kit[donor.data$Type=="stool"])] <- "missing"
  with (donor.data, table(Prior.collection.kit[Type=="stool"], exclude=NULL))
  with (donor.data, table(Using.collection.kit[Type=="stool"], exclude=NULL))
  donor.data$Using.collection.kit[donor.data$Type=="stool"][is.na(donor.data$Using.collection.kit[donor.data$Type=="stool"])] <- "missing"
  with (donor.data, table(Using.collection.kit[Type=="stool"], exclude=NULL))
  with (donor.data, table(Prior.sampled[Type=="vaginal"], exclude=NULL))
  with (donor.data, table(Preferred.collection[Type=="vaginal"], exclude=NULL))
  donor.data$Preferred.collection[donor.data$Type=="vaginal"][is.na(donor.data$Preferred.collection[donor.data$Type=="vaginal"])] <- "missing"
  with (donor.data, table(Preferred.collection[Type=="vaginal"], exclude=NULL))
  with (donor.data, table(Self.collect, exclude=NULL))
  donor.data$Self.collect[is.na(donor.data$Self.collect)] <- "missing"
  with (donor.data, table(Self.collect, exclude=NULL))
  with (donor.data, table(Drop.off, exclude=NULL))
  donor.data$Drop.off[is.na(donor.data$Drop.off)] <- "missing"
  with (donor.data, table(Drop.off, exclude=NULL))
  with (donor.data, table(Compensation, exclude=NULL))
  donor.data$Compensation[is.na(donor.data$Compensation)] <- "missing"
  with (donor.data, table(Compensation, exclude=NULL))
  
  with (donor.data, table(Amount, exclude=NULL))
  donor.data$Amount[is.na(donor.data$Amount)] <- "missing"
  donor.data$Amount[donor.data$Amount == ">R500"] <- ">R250"
  donor.data$Amount[donor.data$Amount == "any amount"] <- "any amount / travel cost"
  donor.data$Amount[donor.data$Amount == "larger_amount_R350-R500"] <- ">R250"
  donor.data$Amount[donor.data$Amount == "R150"] <- "<=R150"
  donor.data$Amount[donor.data$Amount == "R150-R250"] <- "(R150,R250]"
  donor.data$Amount[donor.data$Amount == "R200"] <- "(R150,R250]"
  donor.data$Amount[donor.data$Amount == "R250"] <- "(R150,R250]"
  donor.data$Amount[donor.data$Amount == "smaller_amount_R50"] <- "<=R150"
  donor.data$Amount[donor.data$Amount == "travel costs"] <- "any amount / travel cost"
  donor.data$Amount <- factor(donor.data$Amount)
  with (donor.data, table(Amount, exclude=NULL))
  
  with (donor.data, table(Affect.blood, exclude=NULL))
  donor.data$Affect.blood[is.na(donor.data$Affect.blood)] <- "missing"
  with (donor.data, table(Affect.blood, exclude=NULL))
  with (donor.data, table(Used.for, exclude=NULL))
  donor.data$Used.for[is.na(donor.data$Used.for)] <- "missing"
  with (donor.data, table(Used.for, exclude=NULL))
  with (donor.data, table(Reason, exclude=NULL))
  donor.data$Reason[is.na(donor.data$Reason)] <- "missing"
  with (donor.data, table(Reason, exclude=NULL))
  with (donor.data, table(Helping.others, exclude=NULL))
  donor.data$Helping.others[is.na(donor.data$Helping.others)] <- "missing"
  with (donor.data, table(Helping.others, exclude=NULL))
  with (donor.data, table(Feedback, exclude=NULL))
  donor.data$Feedback[is.na(donor.data$Feedback)] <- "missing"
  with (donor.data, table(Feedback, exclude=NULL))
  with (donor.data, table(Receive.transplant, exclude=NULL))
  donor.data$Receive.transplant[is.na(donor.data$Receive.transplant)] <- "unsure"
  with (donor.data, table(Receive.transplant, exclude=NULL))

# ===== STOOL SAMPLES
# ===================

  #           Willing Y/N            Willing donors           Unwilling
  #                Clinic           Donor.frequency  Receive.transplant
  #                   Age      Using.collection.kit                  N1
  #                Gender              Self.collect                  N2
  #            Occupation                  Drop.off                  N3
  #         Regular.blood                    Amount                  N4
  #           Organ.donor              Affect.blood                  N5
  #         Prior.healthy                  Used.for                  N6
  #      Prior.transplant                    Reason                  N7
  # Prior.transplant.help                  Feedback                  N8
  #  Prior.collection.kit        Receive.transplant                  N9
  #          Compensation                                           N10
  #                Amount
  #        Helping.others 

# --- Most important variables for willing donors

  library (randomForest)
  stool.forest <- randomForest (as.factor(Willing.donor) ~ Clinic + Age + Gender + Occupation + 
                                          Regular.blood + Organ.donor + 
                                          Prior.healthy + Prior.transplant + Prior.transplant.help +
                                          Prior.collection.kit + Compensation + Amount + 
                                          Helping.others,
                          data = donor.data[donor.data$Type=="stool",], importance=TRUE)
  randomForest::varImpPlot(stool.forest)

  m1.stool <- glm (I(as.numeric(factor(Willing.donor))-1) ~ Compensation, data=donor.data[donor.data$Type=="stool",])
  m1.stool
  anova(m1.stool, test="Chi")

  stool.forest <- randomForest (as.factor(Willing.donor) ~ Clinic + Age + Gender + Occupation + 
                                          Regular.blood + Organ.donor + 
                                          Prior.healthy + Prior.transplant + Prior.transplant.help +
                                          Prior.collection.kit + Helping.others,
                          data = donor.data[donor.data$Type=="stool",], importance=TRUE)
  randomForest::varImpPlot(stool.forest)
  
  m2.stool <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                          Compensation + Helping.others, 
                   data=donor.data[donor.data$Type=="stool",])
  m2.stool
  library(car)
  Anova(m2.stool, type=2)

  stool.forest <- randomForest (as.factor(Willing.donor) ~ Clinic + Age + Gender + Occupation + 
                                          Regular.blood + Organ.donor + 
                                          Prior.healthy + Prior.transplant + Prior.transplant.help +
                                          Prior.collection.kit,
                          data = donor.data[donor.data$Type=="stool",], importance=TRUE)
  randomForest::varImpPlot(stool.forest)
  
  m3.stool <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                          Compensation + Helping.others + Organ.donor, 
                   data=donor.data[donor.data$Type=="stool",])
  m3.stool
  Anova(m3.stool, type=2)
  m3.stool <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                     Compensation + Helping.others + Prior.transplant.help, 
                   data=donor.data[donor.data$Type=="stool",])
  m3.stool
  Anova(m3.stool, type=2)
  m3.stool <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                     Compensation + Helping.others + Prior.collection.kit, 
                   data=donor.data[donor.data$Type=="stool",])
  m3.stool
  Anova(m3.stool, type=2)
  m3.stool <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                          Compensation + Helping.others + Gender, 
                   data=donor.data[donor.data$Type=="stool",])
  m3.stool
  Anova(m3.stool, type=2)
  m3.stool <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                          Compensation + Helping.others + Age, 
                   data=donor.data[donor.data$Type=="stool",])
  m3.stool
  Anova(m3.stool, type=2)
  summary(m3.stool)
  stool.classification <- ifelse(predict(m3.stool, type="response")<0.5,0,1)
  stool.observed <- as.numeric(factor(donor.data$Willing.donor[donor.data$Type=="stool"]))-1
  table (stool.classification, stool.observed)

# --- Most important variables for willing donors
#     Compensation yes
#     Helping others yes
#     [Older age]  

# --- Willing donors

  subdat <- select(donor.data[donor.data$Type=="stool" & donor.data$Willing.donor=="yes",], 
                   Donor.frequency, Using.collection.kit, Self.collect, Drop.off, Amount, Affect.blood,
                   Used.for, Reason, Feedback, Receive.transplant) %>%
            filter (Donor.frequency != "never", Used.for != "none")
  subdat
  library(ca)
  mjca (subdat, lambda="JCA")$levelnames
  out <- mjca (subdat, lambda="JCA", subsetcat=(1:36)[-c(1,4,10,17,24,27,31)])
#  plot(out)
  coords <- out$colcoord[,1:2]
  CLPs <- out$levelnames

  counts.donor.freq <- with(subdat, table(Donor.frequency))
  counts.donor.freq
  counts.using.kit <- with (subdat, table(Using.collection.kit))
  counts.using.kit
  counts.self.collect <- with (subdat, table(Self.collect))
  counts.self.collect
  counts.drop <- with (subdat, table(Drop.off))
  counts.drop
  counts.amount <- with (subdat, table(Amount))
  counts.amount
  counts.affect.blood <- with (subdat, table(Affect.blood))
  counts.affect.blood
  counts.used.for <- with (subdat, table(Used.for))
  counts.used.for
  counts.reason <- with (subdat, table(Reason))
  counts.reason
  counts.feedback <- with (subdat, table(Feedback))
  counts.feedback
  counts.receive <- with (subdat, table(Receive.transplant))
  counts.receive

  levels(donor.data$Amount)
  donor.data$Amount <- fct_relevel(donor.data$Amount, c("missing", "none","any amount / travel cost",
                                                        "<=R150","(R150,R250]",">R250"))
  donor.data$Donor.frequency <- fct_relevel(donor.data$Donor.frequency, c("missing","never","monthly","weekly"))

  #dev.new()
  #par(mfrow=c(2,5))
  barplot (counts.donor.freq, col=c("grey", "#F6FEA8", "#F6FEA8"))
  barplot (counts.using.kit, col=c("grey", "#D6F7AF", "#D6F7AF"))
  barplot (counts.self.collect[c(3,1,2)], col=c("#B1D7B5","#B1D7B5","#B1D7B5"))
  barplot (counts.drop, col=c("grey", "#9CE8E3", "#9CE8E3"))
  counts.amount
  barplot (counts.amount[c(5,6,4,2,1,3)], col=c("grey","#A0D2F1","#A0D2F1","#A0D2F1","#A0D2F1","#A0D2F1"))
  barplot (counts.affect.blood, col=c("#9DB6FC","#9DB6FC","#9DB6FC"))
  barplot (counts.used.for[c(3,4,2,1)], col=c("grey","#DBC7FE","#DBC7FE","#DBC7FE"))
  barplot (sort(counts.reason), col=colorRampPalette(c("grey","#FBC6EC"))(5))
  barplot (counts.feedback, col=c("grey","#F4CDC4","#F4CDC4"))
  barplot (counts.receive, col=c("#FBC497","#FBC497","#FBC497"))

  plot.CLPs <- function (coords, col, prop, names)
  {
     for (i in 1:nrow(coords))
       {
          theta <- seq(from=0, to=360*prop[i], by=1)*pi/180
          pie.slice <- rbind(c(0,0),0.4*cbind(cos(theta),sin(theta)))
          pie.coords <- pie.slice + matrix(1,ncol=1,nrow=length(theta)+1)%*% coords[i,]
          polygon (pie.coords, col=col, border=NA)
       }
     text (coords[,1], coords[,2]+0.7, names, cex=0.65, col=col) 
  }

  #dev.new()
  plot (coords, asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
  CLPs[1:3]
  counts.donor.freq
  plot.CLPs (coords[1:2,], "#F6FEA8", 
             (counts.donor.freq/sum(counts.donor.freq))[-1], 
                  names(counts.donor.freq)[-1])
  CLPs[3:7]
  counts.using.kit
  plot.CLPs (coords[3:4,], "#D6F7AF", (counts.using.kit/sum(counts.using.kit))[-1], 
                  names(counts.using.kit)[-1])
  CLPs[5:8]
  counts.self.collect
  plot.CLPs (coords[5:7,], "#B1D7B5", counts.self.collect/sum(counts.self.collect), 
                   names(counts.self.collect))
  CLPs[8:11]
  counts.drop
  plot.CLPs (coords[8:9,], "#9CE8E3", (counts.drop/sum(counts.drop))[-1], 
                  names(counts.drop)[-1])
  CLPs[10:18]
  counts.amount
  plot.CLPs (coords[10:14,], "#A0D2F1", (counts.amount/sum(counts.amount))[c(1,2,3,4,6)], 
                  names(counts.amount)[c(1,2,3,4,6)])
  CLPs[15:18]
  counts.affect.blood
  plot.CLPs (coords[15:17,], "#9DB6FC", counts.affect.blood/sum(counts.affect.blood), 
                  names(counts.affect.blood))
  CLPs[18:21]
  counts.used.for
  plot.CLPs (coords[18:20,], "#DBC7FE", (counts.used.for/sum(counts.used.for))[-3], 
                  names(counts.used.for)[-3])
  CLPs[21:27]
  counts.reason
  plot.CLPs (coords[21:24,], "#FBC6EC", (counts.reason/sum(counts.reason))[-2], 
                  names(counts.reason)[-2])
  CLPs[25:28]
  counts.feedback
  plot.CLPs (coords[25:26,], "#F4CDC4", (counts.feedback/sum(counts.feedback))[-1], 
                   names(counts.feedback)[-1])
  CLPs[27:length(CLPs)]
  counts.receive
  plot.CLPs (coords[27:29,], "#FBC497", counts.receive/sum(counts.receive), 
                   names(counts.receive))
  

# --- NOT Willing

  dev.new()
  par(mfrow=c(1,2))
  counts <- with (donor.data[donor.data$Type=="stool" & donor.data$Willing.donor=="no",], 
                  table(Receive.transplant))
  barplot (counts, col=c("burlywood3","salmon","firebrick"))
  
  counts <- with (donor.data[donor.data$Type=="stool" & donor.data$Willing.donor=="no",], 
                  c(N1=sum(N1=="X"), N2=sum(N2=="X"), N3=sum(N3=="X"), N4=sum(N4=="X"),
                    N5=sum(N5=="X"), N6=sum(N6=="X"), N7=sum(N7=="X"), N8=sum(N8=="X"),
                    N9=sum(N9=="X"), N10=sum(N10=="X")))
  counts
  barplot (rev(sort(counts)))
  
  counts <- with (donor.data[donor.data$Type=="stool" & donor.data$Willing.donor=="yes",], 
                  table(Receive.transplant))
  barplot (counts, col=c("burlywood3","salmon","firebrick"))
                
# ===== VAGINAL SAMPLES
# =====================

  #           Willing Y/N            Willing donors           Unwilling
  #                Clinic           Donor.frequency  Receive.transplant
  #                   Age      Preferred.collection                  N1
  #                Gender              Self.collect                  N2
  #            Occupation                  Drop.off                  N3
  #         Regular.blood                    Amount                  N4
  #           Organ.donor              Affect.blood                  N5
  #         Prior.healthy                  Used.for                  N6
  #      Prior.transplant                    Reason                  N7
  # Prior.transplant.help                  Feedback                  N8
  #         Prior.sampled         Receive.transplant                 N9
  #          Compensation                                           N10
  #                Amount
  #        Helping.others 

# --- Most important variables for willing donors

  library (randomForest)
  vaginal.forest <- randomForest (as.factor(Willing.donor) ~ Clinic + Age + Occupation + 
                                    Regular.blood + Organ.donor + 
                                    Prior.healthy + Prior.transplant + Prior.transplant.help +
                                    Prior.sampled + Compensation + Amount + 
                                    Helping.others,
                                  data = donor.data[donor.data$Type=="vaginal",], importance=TRUE)
  randomForest::varImpPlot(vaginal.forest)
  
  m1.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ Helping.others, 
                     data=donor.data[donor.data$Type=="vaginal",])
  m1.vaginal
  anova(m1.vaginal, test="Chi")
  m1.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ Helping.others, 
                     data=donor.data[donor.data$Type=="vaginal" & donor.data$Helping.others!="missing",])
  m1.vaginal
  anova(m1.vaginal, test="Chi")
  
  vaginal.forest <- randomForest (as.factor(Willing.donor) ~ Clinic + Age + Occupation + 
                                    Regular.blood + Organ.donor + 
                                    Prior.healthy + Prior.transplant + Prior.transplant.help +
                                    Prior.sampled + Compensation + Amount,
                                  data = donor.data[donor.data$Type=="vaginal",], importance=TRUE)
  randomForest::varImpPlot(vaginal.forest)
  
  m2.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                       Helping.others + Prior.transplant.help, 
                     data=donor.data[donor.data$Type=="vaginal",])
  m2.vaginal
  library(car)
  Anova(m2.vaginal, type=2)
  
  chisq.test(with(donor.data[donor.data$Type=="vaginal",], table(Helping.others, Prior.transplant.help)))
  
  vaginal.forest <- randomForest (as.factor(Willing.donor) ~ Clinic + Age + Occupation + 
                                    Regular.blood + Organ.donor + 
                                    Prior.healthy + Prior.transplant + 
                                    Prior.sampled + Compensation + Amount,
                                  data = donor.data[donor.data$Type=="vaginal",], importance=TRUE)
  randomForest::varImpPlot(vaginal.forest)
  
  m3.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                       Helping.others + Prior.transplant.help + Prior.healthy, 
                     data=donor.data[donor.data$Type=="vaginal",])
  m3.vaginal
  Anova(m3.vaginal, type=2)
  vif(m3.vaginal)
  
  chisq.test(with(donor.data[donor.data$Type=="vaginal",], table(Prior.healthy, Prior.transplant.help)))
  m3.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                       Helping.others + Prior.healthy, 
                     data=donor.data[donor.data$Type=="vaginal",])
  m3.vaginal
  Anova(m3.vaginal, type=2)
  
  vaginal.forest <- randomForest (as.factor(Willing.donor) ~ Clinic + Age + Occupation + 
                                    Regular.blood + Organ.donor + 
                                    Prior.transplant + 
                                    Prior.sampled + Compensation + Amount,
                                  data = donor.data[donor.data$Type=="vaginal",], importance=TRUE)
  randomForest::varImpPlot(vaginal.forest)
  
  m4a.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                        Helping.others + Prior.transplant.help + Prior.transplant, 
                      data=donor.data[donor.data$Type=="vaginal",])
  m4a.vaginal
  Anova(m4a.vaginal, type=2)
  m4b.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                        Helping.others + Prior.healthy + Prior.transplant, 
                      data=donor.data[donor.data$Type=="vaginal",])
  m4b.vaginal
  Anova(m4b.vaginal, type=2)
  
  m4a.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                        Helping.others + Prior.transplant.help + Prior.sampled, 
                      data=donor.data[donor.data$Type=="vaginal",])
  m4a.vaginal
  Anova(m4a.vaginal, type=2)
  m4b.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                        Helping.others + Prior.healthy + Prior.sampled, 
                      data=donor.data[donor.data$Type=="vaginal",])
  m4b.vaginal
  Anova(m4b.vaginal, type=2)
  
  
  m4a.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                        Helping.others + Prior.transplant.help + Organ.donor, 
                      data=donor.data[donor.data$Type=="vaginal",])
  m4a.vaginal
  Anova(m4a.vaginal, type=2)
  m4b.vaginal <- glm (I(as.numeric(factor(Willing.donor))-1) ~ 
                        Helping.others + Prior.healthy + Organ.donor, 
                      data=donor.data[donor.data$Type=="vaginal",])
  m4b.vaginal
  Anova(m4b.vaginal, type=2)
  
  Anova(m2.vaginal, type=2)
  summary(m2.vaginal)
  Anova(m3.vaginal, type=2)
  summary(m3.vaginal)
  
  table(donor.data$Willing.donor[donor.data$Type=="vaginal"])
  
  vaginal.classification <- ifelse(predict(m2.vaginal, type="response")<0.5,0,1)
  vaginal.observed <- as.numeric(factor(donor.data$Willing.donor[donor.data$Type=="vaginal"]))-1
  table (vaginal.classification, vaginal.observed)
  
  vaginal.classification <- ifelse(predict(m3.vaginal, type="response")<0.5,0,1)
  vaginal.observed <- as.numeric(factor(donor.data$Willing.donor[donor.data$Type=="vaginal"]))-1
  table (vaginal.classification, vaginal.observed)
  
  
  # --- Most important variables for willing donors
  #     Helping others yes
  #     Prior healthy
  #     Prior transplant help

  table(donor.data$Willing.donor[donor.data$Type=="vaginal"])
  
  #shantelle added for participant characteristics table
  table(donor.data$Clinic[donor.data$Type=="vaginal"])
  table(donor.data$Age[donor.data$Type=="vaginal"])
  table(donor.data$Occupation[donor.data$Type=="vaginal"])
  table(donor.data$Regular.blood[donor.data$Type=="vaginal"])
  table(donor.data$Organ.donor[donor.data$Type=="vaginal"])
  table(donor.data$Prior.healthy[donor.data$Type=="vaginal"])
  table(donor.data$Prior.transplant[donor.data$Type=="vaginal"])
  table(donor.data$Prior.transplant.help[donor.data$Type=="vaginal"])
  table(donor.data$Prior.sampled[donor.data$Type=="vaginal"])
  table(donor.data$Compensation[donor.data$Type=="vaginal"])
  table(donor.data$Amount[donor.data$Type=="vaginal"])
  table(donor.data$Helping.others[donor.data$Type=="vaginal"])
  
  #shantelle added for participant characteristics table
  #willing donor section (n=91)
  table(donor.data$Willing.donor[donor.data$Type=="vaginal"])
  table(donor.data$Donor.frequency[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Preferred.collection[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Self.collect[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Drop.off[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Amount[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Affect.blood[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Used.for[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Reason[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Feedback[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  table(donor.data$Receive.transplant[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  
# --- Willing donors

  subdat <- select(donor.data[donor.data$Type=="vaginal" & donor.data$Willing.donor=="yes",], 
                   Donor.frequency, Preferred.collection, Self.collect, Drop.off, Amount, Affect.blood,
                   Used.for, Reason, Feedback, Receive.transplant) 
  subdat
  library(ca)
  mjca (subdat, lambda="JCA")$levelnames
  out <- mjca (subdat, lambda="JCA", subsetcat=(1:40)[-c(1,6,9,12,15,22,30,35)])
#  plot(out)
  coords <- out$colcoord[,1:2]
  CLPs <- out$levelnames

  counts.donor.freq <- with(subdat, table(Donor.frequency))
  counts.donor.freq
  counts.pref.collect <- with (subdat, table(Preferred.collection))
  counts.pref.collect
  counts.self.collect <- with (subdat, table(Self.collect))
  counts.self.collect
  counts.drop <- with (subdat, table(Drop.off))
  counts.drop
  counts.amount <- with (subdat, table(Amount))
  counts.amount
  counts.affect.blood <- with (subdat, table(Affect.blood))
  counts.affect.blood
  counts.used.for <- with (subdat, table(Used.for))
  counts.used.for
  counts.reason <- with (subdat, table(Reason))
  counts.reason
  counts.feedback <- with (subdat, table(Feedback))
  counts.feedback
  counts.receive <- with (subdat, table(Receive.transplant))
  counts.receive

  donor.data$Amount <- fct_relevel(donor.data$Amount, c("missing", "none","any amount / travel cost",
                                                        "<=R150","(R150,R250]",">R250"))
  donor.data$Donor.frequency <- fct_relevel(donor.data$Donor.frequency, c("missing","never","monthly","weekly"))

  #dev.new()
  #par(mfrow=c(2,5))
  barplot (counts.donor.freq, col=c("grey", "#F6FEA8", "#F6FEA8"))
  barplot (counts.pref.collect[c(3,2,4,1)], col=c("grey", "#D6F7AF", "#D6F7AF","#D6F7AF"))
  barplot (counts.self.collect[c(3,1,2)], col=c("#B1D7B5","#B1D7B5","#B1D7B5"))
  barplot (counts.drop, col=c("grey", "#9CE8E3", "#9CE8E3"))
  counts.amount
  barplot (counts.amount[c(4,5,6,3,2,1)], col=c("#A0D2F1","#A0D2F1","#A0D2F1","#A0D2F1","#A0D2F1","#A0D2F1"))
  barplot (counts.affect.blood[c(2,1,3,4)], col=c("grey","#9DB6FC","#9DB6FC","#9DB6FC"))
  barplot (counts.used.for[c(3,4,2,1)], col=c("#DBC7FE","#DBC7FE","#DBC7FE","#DBC7FE"))
  barplot (sort(counts.reason), col=colorRampPalette(c("grey","#FBC6EC"))(5))
  barplot (counts.feedback, col=c("grey","#F4CDC4","#F4CDC4"))
  barplot (counts.receive, col=c("#FBC497","#FBC497","#FBC497"))


  plot.CLPs <- function (coords, col, prop, names)
  {
     for (i in 1:nrow(coords))
       {
          theta <- seq(from=0, to=360*prop[i], by=1)*pi/180
          pie.slice <- rbind(c(0,0),0.4*cbind(cos(theta),sin(theta)))
          pie.coords <- pie.slice + matrix(1,ncol=1,nrow=length(theta)+1)%*% coords[i,]
          polygon (pie.coords, col=col, border=NA)
       }
     text (coords[,1], coords[,2]+0.7, names, cex=0.65, col=col) 
  }

  #dev.new()
  plot (coords, asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
  CLPs[1:4]
  counts.donor.freq
  plot.CLPs (coords[1:2,], "#F6FEA8", 
             (counts.donor.freq/sum(counts.donor.freq))[-1], 
             names(counts.donor.freq)[-1])
  CLPs[3:8]
  counts.pref.collect
  plot.CLPs (coords[3:5,], "#D6F7AF", (counts.pref.collect/sum(counts.pref.collect))[-3], 
                  names(counts.pref.collect)[-3])
  CLPs[6:10]
  counts.self.collect
  plot.CLPs (coords[6:8,], "#B1D7B5", (counts.self.collect/sum(counts.self.collect))[-2], 
                   names(counts.self.collect)[-2])
  CLPs[9:12]
  counts.drop
  plot.CLPs (coords[9:10,], "#9CE8E3", (counts.drop/sum(counts.drop))[-1], 
                  names(counts.drop)[-1])
  CLPs[11:17]
  counts.amount
  plot.CLPs (coords[11:15,], "#A0D2F1", (counts.amount/sum(counts.amount))[-5], 
                  names(counts.amount)[-5])
  CLPs[16:20]
  counts.affect.blood
  plot.CLPs (coords[16:18,], "#9DB6FC", (counts.affect.blood/sum(counts.affect.blood))[-2], 
                  names(counts.affect.blood)[-2])
  CLPs[19:23]
  counts.used.for
  plot.CLPs (coords[19:22,], "#DBC7FE", (counts.used.for/sum(counts.used.for)), 
                  names(counts.used.for))
  CLPs[23:28]
  counts.reason
  plot.CLPs (coords[23:27,], "#FBC6EC", (counts.reason/sum(counts.reason))[-2], 
                  names(counts.reason)[-2])
  CLPs[28:32]
  counts.feedback
  plot.CLPs (coords[28:29,], "#F4CDC4", (counts.feedback/sum(counts.feedback))[-1], 
                   names(counts.feedback)[-1])
  CLPs[30:length(CLPs)]
  counts.receive
  plot.CLPs (coords[30:32,], "#FBC497", counts.receive/sum(counts.receive), 
                   names(counts.receive))
  
# --- NOT Willing

  #dev.new()
  #par(mfrow=c(1,2))
  counts <- with (donor.data[donor.data$Type=="vaginal" & donor.data$Willing.donor=="no",], 
                  table(Receive.transplant))
  barplot (counts, col=c("burlywood3","salmon","firebrick"))
  
  #Shantelle added to get table
  table(donor.data$Receive.transplant[donor.data$Type=="vaginal"& donor.data$Willing.donor=="no"])
  table(donor.data$Receive.transplant[donor.data$Type=="vaginal"& donor.data$Willing.donor=="yes"])
  
  
  counts <- with (donor.data[donor.data$Type=="vaginal" & donor.data$Willing.donor=="no",], 
                  c(N1=sum(N1=="X"), N2=sum(N2=="X"), N3=sum(N3=="X"), N4=sum(N4=="X"),
                    N5=sum(N5=="X"), N6=sum(N6=="X"), N7=sum(N7=="X"), N8=sum(N8=="X"),
                    N9=sum(N9=="X"), N10=sum(N10=="X")))
  counts
  barplot (rev(sort(counts)))
                
  counts <- with (donor.data[donor.data$Type=="vaginal" & donor.data$Willing.donor=="yes",], 
                  table(Receive.transplant))
  barplot (counts, col=c("burlywood3","salmon","firebrick"))
           
           