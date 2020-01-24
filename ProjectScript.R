##### PROJECT SCRIPT #####

Suicide <- read.table("SuicideRatesOverview1985to2016.csv", sep=",",header=T)

drops<-c("HDI.for.year", "gdp_for_year", "country.year")
Suicide <- Suicide [,!(names(Suicide) %in% drops)]

head(Suicide)

cor(Suicide$suicides_no, Suicide$population)
cor(Suicide$suicides_no, Suicide$year )
cor(Suicide$suicides_no, Suicide$gdp_per_capita....) 

#plot of suicide rate for each country
plot(Suicide$country,Suicide$suicides.100k.pop, type = "h", pch =15, xlab = "Country", ylab = "Suicides per 100k Pop.", main = "Suicides Per Country for Years 1985-2016")

#Suicide No vs Sex
Suicide$sex <- factor(Suicide$sex)
mylogit <- glm(Suicide$sex~Suicide$suicides_no  , data = Suicide, family = "binomial")
summary(mylogit)

#Total Suicides by Year
frame = data.frame(Suicide)
totalSui = aggregate(frame['suicides_no'], by=frame['year'], sum)
plot(totalSui$year, totalSui$suicides_no, xlab = "Year", ylab = "Total Suicides")

#Total By Generation
totalSuiGen = aggregate(frame['suicides_no'], by=frame['generation'], sum)
plot(totalSuiGen$generation, totalSuiGen$suicides_no, xlab = "Generation", ylab = "Total Suicides")

frame = data.frame(Suicide)
totalSuiCountry = aggregate(frame['suicides_no'], by=frame['country'], sum)
plot(totalSuiCountry$country, totalSuiCountry$suicides_no)


