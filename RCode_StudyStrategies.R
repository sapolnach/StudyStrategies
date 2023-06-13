library(stats)
library(rstatix)
library(MKinfer)
library(lmboot)
library(effectsize)

df = read.csv("SS_data.csv")

#Assumption tests
shapiro.test(df$GradeChange)
levene_test(GradeChange ~ Modification, data = df)
levene_test(GradeChange ~ Q7, data = df)
levene_test(GradeChange ~ Q3, data = df)
levene_test(GradeChange ~ Tipsheets, data = df)
boxplot(df[,c('GradeChange')])

# ANOVA
res.aov <- aov(GradeChange ~ Modification, data = df) #Fig 8
summary(res.aov)
eta_squared(res.aov, partial = TRUE)

#T tests
t.test(GradeChange ~ Q7, data = df, var.equal = TRUE) #Fig 7
t.test(GradeChange ~ Q3, data = df, var.equal = TRUE) #Fig 6
t.test(GradeChange ~ Tipsheets, data = df, var.equal = TRUE) #Fig 9
cohens_d(GradeChange ~ Q7, data = df, var.equal = TRUE)
cohens_d(GradeChange ~ Q3, data = df, var.equal = TRUE)
cohens_d(GradeChange ~ Tipsheets, data = df, var.equal = TRUE)

#ANOVA Bootstrap
myANOVA <- ANOVA.boot(GradeChange ~ Modification, data = df, B = 9999)
myANOVA$`p-values`
myANOVA$origFStats
myANOVA$df

#T-test bootstrap - https://search.r-project.org/CRAN/refmans/MKinfer/html/boot.t.test.html
boot.t.test(GradeChange ~ Q7, data = df)
boot.t.test(GradeChange ~ Q3, data = df)
boot.t.test(GradeChange ~ Tipsheets, data = df)
