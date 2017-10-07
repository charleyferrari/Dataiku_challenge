library(dplyr)
library(ggplot2)
library(GGally)
library(vcd)
library(reshape2)

setwd('/Users/Charley/Downloads/us_census_full')
data <- read.csv('census_income_test_clean.csv')



categoricals <- c('class.of.worker', 'detailed.industry.recode',
                  'detailed.occupation.recode', 'education', 'education.recode',
                  'enroll.in.edu.inst.last.wk', 'marital.stat', 'major.industry.code',
                  'major.occupation.code', 'race', 'hispanic.origin', 'sex',
                  'member.of.a.labor.union', 'full.or.part.time.employment.stat',
                  'region.of.previous.residence', 'state.of.previous.residence',
                  'detailed.household.and.family.stat', 
                  'detailed.household.summary.in.household',
                  'migration.code.change.in.reg', 'migration.code.move.within.reg',
                  'num.persons.worked.for.employer', 'family.members.under.18',
                  'country.of.birth.father', 'country.of.birth.mother',
                  'country.of.birth.self', 'citizenship',
                  'own.business.or.self.employed',
                  'fill.inc.questionnaire.for.veteran.s.admin', 
                  'veterans.benefits', 'year')


recoded.education <- c('Grade School', 'High School', 'Grade School', 
                       'High School', 'Masters', 'Bachelors', 'Grade School', 
                       'High School', 'Grade School', 'Grade School', 
                       'Grade School', 'Grade School', 'Grade School' ,
                       'High School', 'Grade School', 'Masters', 'Masters')

education.recode <- data.frame(education = unique(data$education),
                               education.recode = recoded.education)

data <- merge(education.recode, data, by='education')

large.categoricals <- c()

small.categoricals <- c()

for(category in categoricals){
  if(length(levels(data[,category])) > 15){
    large.categoricals <- c(large.categoricals, category)
  } else{
    small.categoricals <- c(small.categoricals, category)
  }
}

category <- 'class.of.worker'

test <- dcast(data, as.formula(paste(category, ' ~ income.group', sep='')), 
      fun.aggregate = length)
rownames(test) <- test[,category]
colnames(test) <- c(category, 'less than $50,000', 'more than $50,000')
test <- test[, -which(colnames(test) == category)]

phm <- plot_ly(z = as.matrix(test), y = rownames(test), x = colnames(test), type='heatmap') %>%
  layout(yaxis = list(showticklabels = FALSE))

plot_ly(data, x = ~major.industry.code, color = ~income.group, type = 'histogram') %>%
  subplot(nrows = 2)

p1 <- plot_ly(filter(data, income.group == unique(data$income.group)[1]),
              x = as.formula(paste('~',category,sep='')), type = 'histogram',
              name = unique(data$income.group)[1])

p2 <- plot_ly(filter(data, income.group == unique(data$income.group)[2]),
              x = as.formula(paste('~',category,sep='')), type = 'histogram',
              marker = list(color = 'orange'), name = unique(data$income.group)[2])

s <- subplot(p1, p2, nrows = 2) %>%
  layout(xaxis = list(showticklabels = FALSE), xaxis2 = list(showticklabels = FALSE))

subplot(s, phm)

category <- 'wage.per.hour'

plot_ly(data, x = ~income.group, y = as.formula(paste('~ ',category,sep='')),
        type = 'box')

plot_ly(data, x = as.formula(paste('~ ',category,sep='')), type ='histogram' )
        