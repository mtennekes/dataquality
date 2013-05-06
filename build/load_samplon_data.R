## load samplon2a
sam <- read.csv2(file="./build/data/samplon2a_extra_kolom_tot_meerdere_huishoudens.csv", na.strings="#LEEG!", colClasses=c("numeric", "factor", "factor", "factor", "numeric", "factor", "numeric", "factor", "numeric", "numeric"))

names(sam) <- c("sort", "id", "province", "sex", "age", "employed", "income", "education", "blood_group", "household")

levels(sam$sex) <- c("male", "female")
levels(sam$province) <- c("Akkerwinde", "Grasmalen", "Nieuwekans", "Lommerdal", "Smeulde", "Stapelrade", "Vuilpanne")
levels(sam$employed) <- c("yes", "no")
levels(sam$education) <- c("primary", "no formal education", "upper secondary", "bachelor",  "lower secondary", "lower secondary", "bachelor", "master")

sam$education <- factor(sam$education, levels=c("no formal education", "primary", "lower secondary", "upper secondary", "bachelor", "master"))

sam$blood_group <- factor(sam$blood_group, levels=1:0, labels=c("A", "O"))


## load samplon2r1 
reg_t1 <- read.csv2(file="build/data/samplon2r1.csv", na.strings="#LEEG!", colClasses=c("factor", "factor", "numeric", "numeric"))
names(reg_t1) <- c("id_code", "sex", "age", "blood_group")

levels(reg_t1$sex) <- c("male", "female")
reg_t1$blood_group <- factor(reg_t1$blood_group, levels=1:0, labels=c("A", "O"))

## load samplon2r2 
reg_t2 <- read.csv2(file="build/data/samplon2r2.csv", na.strings="#LEEG!", colClasses=c("factor", "factor", "numeric", "numeric"))
names(reg_t2) <- c("id_code", "sex", "age", "blood_group")
levels(reg_t2$sex) <- c("male", "female")
reg_t2$blood_group <- factor(reg_t2$blood_group, levels=1:0, labels=c("A", "O"))


save(sam, file="./pkg/data/sam.rda")
save(reg_t1, file="./pkg/data/reg_t1.rda")
save(reg_t2, file="./pkg/data/reg_t2.rda")

