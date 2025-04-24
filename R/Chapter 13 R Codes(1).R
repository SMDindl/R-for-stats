############### Analysis of Variance (ANOVA) R Codes #######################



############## Example in ANOVA Lecture Notes ###############

Distance <- c(251.2,245.1,248,251.1,260.5,250,253.9,244.6,263.2,262.9,265,254.5,264.3,257,262.8,264.4,
           269.7,263.2,277.5,267.4,270.5,265.5,270.7,272.9,251.6,248.6,249.4,242,246.5,251.3,261.8,249)

Brand <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4)



model <- aov(Distance ~ factor(Brand))
summary(model)




#### Post Hoc Tests (Multiple Comparisons) ##########

TukeyHSD(model)

plot(TukeyHSD(model, conf.level = 0.95),las=1, col = "red")# easier to look at the plot



####### Other Post Hoc (or Multiple Comparison) Approaches ##########

pairwise.t.test(Distance, factor(Brand), p.adj = "bonf")# bonferroni 

pairwise.t.test(Distance, factor(Brand), p.adj = "holm")# holm 

pairwise.t.test(Distance, factor(Brand), p.adj = "none")# using pooled variance  


