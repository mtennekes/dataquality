## Load example data sources
data(sam)
data(reg_t1)
data(reg_t2)


######################################
## 1. Technical checks
######################################


## Dimension 1.2 File declaration compliance

# extended summary
describe(sam)
describe(reg_t1)
describe(reg_t2)

# visualise each data source with small mulitples
visualise(sam)
visualise(reg_t1)
visualise(reg_t2)

# visualise each data source with one plot
tableplot(sam, sortCol=age)
tableplot(reg_t1)
tableplot(reg_t2)

# start the GUI:
# tableGUI()

######################################
## 2. Accuracy
######################################

## 2.1 Authenticity

#check if the values in the id column of sam precede with an "S", than followed by a number:
checkFormat(sam, "id", pattern="^S[0-9]+$")

#the same for reg_t1:
checkFormat(reg_t1, "id_code", pattern="^S[0-9]+$")

#if the values should precede with an "A":
checkFormat(reg_t1, "id_code", pattern="^A[0-9]+$")


#check the number of changed records
changed_value(reg_t1, sam, source_t1.keys="id_code", source_t2.keys="id", variable="age")
changed_value(reg_t1, sam, source_t1.keys="id_code", source_t2.keys="id", variable="sex")



## 2.2 Inconsistent objects & 2.3 Dubious objects

# overview of 1-to-n and m-1 relations between two variables
relation(sam, "id", "household", graph.lowerbound.numberV1perV2=7, draw=TRUE) # plot large households (7 or more people)
relation(sam, "id", "household", graph.lowerbound.numberV2perV1=2, draw=TRUE) # plot persons who belong to multiple households


## 2.5 Inconsistent values & 2.6 Dubious values

# the following example illustrates the editrules package
# for this example, we create a (temporary) text file with edit rules:

tmp <- tempfile()  # if you want to keep the file in the working directory, run tmp <- "mixed_rules.txt" instead
file.create(tmp)
con <- file(tmp)
writeLines(c("#categorical edit rules",
	"sex %in% c('male','female')",
	"employed %in% c('yes','no')",
	"province %in% c('Akkerwinde', 'Grasmalen', 'Nieuwekans','Lommerdal','Smeulde','Stapelrade','Vuilpanne')",
	"education %in% c('no formal education','primary','lower secondary','upper secondary', 'bachelor','master')",
	"",
	"#numerical edit rules",
	"age<=120",
	"age>=0",
	"income<=2500",
	"income>=0",
	"",
	"#mixed edit rules",
	"if (age<18) income<=0",
	"if (age>65) income<=0",
	"if (age<16) education %in% c('no formal education','primary','lower secondary','upper secondary')",
	"if (employed=='no') income<=10"), con=con)
close(con)

# load the mixed edit rules from the text file, and print them
(E<-editfile(tmp, package="editrules"))

# plot the edit rules (uncomment and run the following two lines):
# require(igraph0)
# plot(E)

# apply edit rules on samplon
result <- violatedEdits(E, sam)
summary(result)

# require(igraph0)
plot(result)



######################################
## 3. Completeness
######################################


## 3.1 Undercoverage
undercoverage(reg_t1, sam, "id_code", "id")
undercoverage(reg_t1, sam, "id_code", "id", percentage=TRUE) ## what does percentage do?


## 3.2 Overcoverage
overcoverage(sam, reg_t1, source.keys="id", reference.keys="id_code")
overcoverage(sam, reg_t1, c(2,4), c(1,2))
overcoverage(sam, reg_t1, c("row.names"), c("row.names"))


## 3.3 Selectivity
# visualise data distributions with histograms
visualise(sam, reg_t1)
visualise(sam, reg_t2, useRelative=TRUE)


## 3.4 Redundancy & 3.5 Missing values

redundancy(sam)
redundancy(sam, percentages=TRUE)
redundancy(reg_t1)


######################################
## 4. Time-related
######################################

## 4.1 - 4.4 For these indications, the function dateDiff can be used:
dateDiff("15-01-2012", "17-02-2012", unit = "weeks")

## 4.5 Dynamics of objects
birth(reg_t1, reg_t2, "id_code")   ## in t2 but not in t1, overcoverage t2 with number of records at t2 as base
death2(reg_t1, reg_t2, "id_code")   ## undercoverage at t2 (records in t1 that are absent in t2)
death1(reg_t1, reg_t2, "id_code")   ## undercoverage at t2 with number of records at t1 as base (records in t1 that are absent in t2)
change(reg_t1, reg_t2, "id_code")
change_fast(reg_t1, reg_t2, "id_code")

# check number and fraction of changed values
changed_value(reg_t1, reg_t2, keys="id_code", variable = "age")
changed_value(reg_t1, reg_t2, keys="row.names", variable = "age")
unchanged_value(reg_t1, reg_t2, keys="id_code", variable = "age")


## 4.6 Stability of variables

# visualise variables of two data sources
visualise(reg_t1, reg_t2)

# visualise alligned objects
compare(reg_t1, reg_t2)

# Cramer's V:
cramerV.test(source1.data=reg_t1, source2.data=reg_t2, source1.keys="id_code", source2.keys="id_code", source1.variable="blood_group", source2.variable="blood_group")
cramerV.test(source1.data=reg_t1, source2.data=reg_t2, source1.keys="id_code", source2.keys="id_code", source1.variable="sex", source2.variable="sex")
# note: chi-square tests applied to numerical data can be done directly with chisq.test


######################################
## 5. Integrability
######################################

## 5.1 Comparability of objects
coverage(reg_t1, sam, "id_code", "id")

## 5.2 Alignment of objects
coverage(reg_t1, reg_t2, "id_code")
coverage(reg_t1, reg_t2, "sex")
coverage(reg_t1, reg_t2, c("sex", "age"))

alive(reg_t1, reg_t2, "id_code")   ## = coverage (records matched) with number of records at t2 as base
alive(reg_t1, reg_t2, c("sex", "age"))   ## = coverage (records matched) with number of records at t2 as base

## 5.3 Linking variable 
# check linking variables
redundancy(sam, "id")
redundancy(reg_t1, "id_code")
redundancy(reg_t2, "id_code")

checkFormat(sam, "id", pattern="^S[0-9]+$")
checkFormat(reg_t1, "id_code", pattern="^S[0-9]+$")
checkFormat(reg_t2, "id_code", pattern="^S[0-9]+$")

## 5.4 Comparability of variables

# Cramer's V:
cramerV.test(source1.data=sam, source2.data=reg_t2, source1.keys="id", source2.keys="id_code", source1.variable="blood_group", source2.variable="blood_group")
cramerV.test(source1.data=sam, source2.data=reg_t2, source1.keys="id", source2.keys="id_code", source1.variable="sex", source2.variable="sex")
# note: chi-square tests applied to numerical data can be done directly with chisq.test

# check number and fraction of unchanged values
unchanged_value(reg_t1, reg_t2, keys="id_code", variable = "age")

# visualise linked objects
compare(reg_t1, sam, key1="id_code", key2="id", vars1=c("sex", "age"), vars2=c("sex", "age"))
