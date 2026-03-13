## TAC constraint tests ##
table <- read.table("clipboard", header=TRUE)
rownames(table) <- table$Year
save(table, file="data/TAC_utilization.RData")

lastY <- "2025"
TAC_F <- 788
TAC_NS3a <- 328566 

NO3alastY <- 0.273
C_3a <- (table[lastY,"WBSS_C.fleet"]+table[lastY,"NSAS_C.fleet"])-NO3alastY

propEU3aC <- C_3a/(C_3a+table[lastY,"WBSS_D.fleet"]+table[lastY,"NSAS_D.fleet"])
propEU3aD <- 1-propEU3aC
NO3a_max <- 250

splitC <- table[lastY,"split_C"]
splitD <- table[lastY,"split_D"]

propA <- (table[lastY,"A.fleet.NSAS."])/(table[lastY,"A.fleet.NSAS."]+table[lastY,"B.Fleet"])
TAC_A <- TAC_NS3a*propA
transfer <- 0

splitA <- table[lastY,"WBSS_IVaE"]/(table[lastY,"A.fleet.NSAS."]+table[lastY,"WBSS_IVaE"])

# Old method by fleet

# A <- (388542+22793-(0.7749*969+200))*0.008559687676
# C <- (0.7749*969+200)*30.33/100
# D <- (0.2251*969)*14.20/100

A <- ((TAC_NS3a+EUextra)+transfer-(propEU3aC*969+NO3a_max)+(propEU3aD*969))*splitA
C <- (propEU3aC*969+NO3a_max)*splitC
D <- (propEU3aD*969)*splitD
F <- TAC_F

total1 <- A+C+D+F

round(total1)


# New method

catch_WBSS_43a <- sum(table[lastY,"WBSS_IVaE"], table[lastY,"WBSS_C.fleet"], table[lastY,"WBSS_D.fleet"])
catch_NSAS_43a <- sum(table[lastY,"A.fleet.NSAS."], table[lastY,"B.Fleet"], table[lastY,"NSAS_C.fleet"], table[lastY,"NSAS_D.fleet"])

splitABCD <- catch_WBSS_43a/sum(catch_WBSS_43a, catch_NSAS_43a) 

EUextra <- 560 # from https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=OJ:L_202600249

total2 <- splitABCD*(TAC_NS3a+EUextra)+F

round(total2)


