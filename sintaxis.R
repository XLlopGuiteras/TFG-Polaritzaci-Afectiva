setwd("C:/Users/xavie/Desktop/Metodologia TFG")

# Carreguem les bases de dades.
library(haven)

dades <- read_sav("2a 2022.sav")



# Elaborem subbases de dades amb les variables que ens resulten d'interès
dt<-data.frame(dades$SEXE, dades$EDAT, dades$IDEOL_0_10, dades$SATIS_DEMOCRACIA,
               dades$INT_PARLAMENT_VOT, dades$SIMPATIA_ESQUERRES_0_10, 
               dades$SIMPATIA_DRETES_0_10, dades$SIMPATIA_INDEPENDENTISTES_0_10,
               dades$SIMPATIA_UNIONISTES_0_10, dades$ESP_CAT_0_10, dades$VAL_GOV_CAT,
               dades$ACTITUD_INDEPENDENCIA, dades$SIT_LAB, dades$ESTUDIS_1_15,
              dades$VAL_GOV_ESP, dades$INGRESSOS_1_15, dades$LLENGUA_IDENTIFICACIO,
              dades$SIMPATIA_VOTANTS_ERC_0_10, dades$SIMPATIA_VOTANTS_JXCAT_0_10,
              dades$SIMPATIA_VOTANTS_CUP_0_10, dades$SIMPATIA_VOTANTS_PSC_0_10,
              dades$SIMPATIA_VOTANTS_PP_0_10, dades$SIMPATIA_VOTANTS_VOX_0_10,
              dades$SIMPATIA_VOTANTS_Cs_0_10, dades$SIMPATIA_VOTANTS_CEC_0_10)

names(dt)=c("sexe","edat","ideologia","satdemo","vot","simpesq","simpdr","simpind", 
            "simpuni", "catalanisme", "valGen", "independencia", "sitlab","estudis",
            "valEsp", "ingressos", "llengua", "simpERC", "simpJxCAT", "simpCUP",
            "simpPSC", "simpPP", "simpVox", "simpCS", "simpECP")

# neteja i tractament de variables.
library(descr)
library(car)

freq(dt$sexe)
dt$sexe<-factor(dt$sexe, levels=c(1,2), labels=c("Masculí", "Femení"))


freq(dt$edat)
dt$edat<-as.numeric(dt$edat)

freq(dt$ideologia)
dt$ideologia[dt$ideologia==98]<-NA
dt$ideologia[dt$ideologia==99]<-NA
dt$ideologia<-as.numeric(dt$ideologia)

freq(dt$simpesq)
dt$simpesq[dt$simpesq==98]<-NA
dt$simpesq[dt$simpesq==99]<-NA
dt$simpesq<-as.numeric(dt$simpesq)
#Histograma simpatia "gent d'esquerres"
library(ggplot2)
hist1<-ggplot(dt)+aes(x=simpesq)+geom_histogram(bins = 10, color="black", fill="lightblue")
hist1+labs(x="Simpatia envers gent d'esquerres (0-10)", y="Freqüència")
descr(dt$simpesq)

freq(dt$simpdr)
dt$simpdr[dt$simpdr==98]<-NA
dt$simpdr[dt$simpdr==99]<-NA
dt$simpdr<-as.numeric(dt$simpdr)
#Histograma simpatia "gent de dretes"
hist2<-ggplot(dt)+aes(x=simpdr)+geom_histogram(bins = 10, color="black", fill="lightblue")
hist2+labs(x="Simpatia envers gent de dretes (0-10)", y="Freqüència")
descr(dt$simpdr)

freq(dt$simpind)
dt$simpind[dt$simpind==98]<-NA
dt$simpind[dt$simpind==99]<-NA
dt$simpind<-as.numeric(dt$simpind)
#Histograma simpatia "independentistes"
hist3<-ggplot(dt)+aes(x=simpind)+geom_histogram(bins = 10, color="black", fill="lightblue")
hist3+labs(x="Simpatia envers independentistes (0-10)", y="Freqüència")
descr(dt$simpind)

freq(dt$simpuni)
dt$simpuni[dt$simpuni==98]<-NA
dt$simpuni[dt$simpuni==99]<-NA
dt$simpuni<-as.numeric(dt$simpuni)
#Histograma simpatia "unionistes"
hist4<-ggplot(dt)+aes(x=simpuni)+geom_histogram(bins = 10, color="black", fill="lightblue")
hist4+labs(x="Simpatia envers unionistes (0-10)", y="Freqüència")
descr(dt$simpuni)

freq(dt$valGen)
dt$valGen[dt$valGen==98]<-NA
dt$valGen[dt$valGen==99]<-NA
dt$valGen<-as.numeric(dt$valGen)

freq(dt$valEsp)
dt$valEsp[dt$valEsp==98]<-NA
dt$valEsp[dt$valEsp==99]<-NA
dt$valEsp<-as.numeric(dt$valEsp)

freq(dt$catalanisme)
dt$catalanisme[dt$catalanisme==98]<-NA
dt$catalanisme[dt$catalanisme==99]<-NA
dt$catalanisme<-as.numeric(dt$catalanisme)

freq(dt$satdemo)
dt$satdemo[dt$satdemo==98]<-NA
dt$satdemo[dt$satdemo==99]<-NA
dt$satdemo<-factor(dt$satdemo,levels=c(1,2,3,4),labels=c("Molt satisfet","Bastant satisfet", "Poc satisfet", "Gens satisfet"))

freq(dt$sitlab)
dt$sitlab[dt$sitlab==99]<-NA
dt$sitlab<-factor(dt$sitlab, levels=c(1,2,3), labels=c("Treballa", "No treballa", "Baixa temporal"))

freq(dt$llengua)
dt$llengua[dt$llengua==98]<-NA
dt$llengua[dt$llengua==99]<-NA
dt$llengua[dt$llengua==80]<-NA
dt$llengua[dt$llengua==4]<-NA
dt$llengua<-factor(dt$llengua, levels=c(1,2,3), labels=c("Català", "Castellà", "Ambdues"))

#Recodificar Estudis
freq(dt$estudis)
dt$estudis[dt$estudis==98]<-NA
dt$estudis[dt$estudis==99]<-NA
dt$estudis[dt$estudis==1]<-1
dt$estudis[dt$estudis==2]<-2
dt$estudis[dt$estudis==3]<-4
dt$estudis[dt$estudis==4]<-3
dt$estudis[dt$estudis==5]<-4
dt$estudis[dt$estudis==6]<-5
dt$estudis[dt$estudis==7]<-5
dt$estudis[dt$estudis==8]<-6
dt$estudis[dt$estudis==9]<-6
dt$estudis[dt$estudis==10]<-6
dt$estudis[dt$estudis==11]<-6
dt$estudis[dt$estudis==12]<-6
dt$estudis[dt$estudis==13]<-7
dt$estudis[dt$estudis==14]<-7
dt$estudis[dt$estudis==15]<-7

dt$estudis<-factor(dt$estudis, levels=c(1,2,3,4,5,6,7), 
                   labels=c("Sense estudis", "Primària", "Sec.obligatòria", "FP inicial i mig", "FP superior i batxillerat", "Estudis universitaris", "Estudis post-grau"))

#Recodificar Ingressos
freq(dt$ingressos)
dt$ingressos[dt$ingressos==98]<-NA
dt$ingressos[dt$ingressos==99]<-NA
dt$ingressos[dt$ingressos==1]<-1
dt$ingressos[dt$ingressos==2]<-2
dt$ingressos[dt$ingressos==3]<-2
dt$ingressos[dt$ingressos==4]<-2
dt$ingressos[dt$ingressos==5]<-2
dt$ingressos[dt$ingressos==6]<-2
dt$ingressos[dt$ingressos==7]<-3
dt$ingressos[dt$ingressos==8]<-3
dt$ingressos[dt$ingressos==9]<-3
dt$ingressos[dt$ingressos==10]<-3
dt$ingressos[dt$ingressos==11]<-3
dt$ingressos[dt$ingressos==12]<-4
dt$ingressos[dt$ingressos==13]<-4
dt$ingressos[dt$ingressos==14]<-4
dt$ingressos[dt$ingressos==15]<-4

dt$ingressos<-factor(dt$ingressos, levels=c(1,2,3,4), labels=c("Sense ingressos",
                                                               "Ingressos baixos", "Ingressos mitjans", "Ingressos alts"))

#Recodificar Vot
freq(dt$vot)
dt$vot[dt$vot==99]<-NA
dt$vot[dt$vot==98]<-NA
dt$vot[dt$vot==97]<-NA
dt$vot[dt$vot==96]<-NA
dt$vot[dt$vot==94]<-NA
dt$vot[dt$vot==80]<-NA
dt$vot[dt$vot==19]<-NA
dt$vot[dt$vot==20]<-NA
dt$vot[dt$vot==93]<-NA

dt$vot[dt$vot==1]<-6
dt$vot[dt$vot==6]<-7
dt$vot[dt$vot==3]<-1
dt$vot[dt$vot==4]<-4
dt$vot[dt$vot==10]<-3
dt$vot[dt$vot==18]<-5
dt$vot[dt$vot==21]<-2
dt$vot[dt$vot==23]<-8

freq(dt$vot)
library(car)
dt$vot<-factor(dt$vot, levels=c(1,2,3,4,5,6,7,8), labels=c("ERC", "JxCat","CUP",
                                                           "PSC","ECP","PPC","C'S","Vox"))


#Neteja de polarització partidista
freq(dt$simpCS)
dt$simpCS[dt$simpCS==99]<-NA
dt$simpCS[dt$simpCS==98]<-NA
dt$simpCS<-as.numeric(dt$simpCS)

freq(dt$simpCUP)
dt$simpCUP[dt$simpCUP==99]<-NA
dt$simpCUP[dt$simpCUP==98]<-NA
dt$simpCUP<-as.numeric(dt$simpCUP)

freq(dt$simpECP)
dt$simpECP[dt$simpECP==99]<-NA
dt$simpECP[dt$simpECP==98]<-NA
dt$simpECP<-as.numeric(dt$simpECP)

freq(dt$simpERC)
dt$simpERC[dt$simpERC==99]<-NA
dt$simpERC[dt$simpERC==98]<-NA
dt$simpERC<-as.numeric(dt$simpERC)

freq(dt$simpJxCAT)
dt$simpJxCAT[dt$simpJxCAT==99]<-NA
dt$simpJxCAT[dt$simpJxCAT==98]<-NA
dt$simpJxCAT<-as.numeric(dt$simpJxCAT)

freq(dt$simpPP)
dt$simpPP[dt$simpPP==99]<-NA
dt$simpPP[dt$simpPP==98]<-NA
dt$simpPP<-as.numeric(dt$simpPP)

freq(dt$simpPSC)
dt$simpPSC[dt$simpPSC==99]<-NA
dt$simpPSC[dt$simpPSC==98]<-NA
dt$simpPSC<-as.numeric(dt$simpPSC)

freq(dt$simpVox)
dt$simpVox[dt$simpVox==99]<-NA
dt$simpVox[dt$simpVox==98]<-NA
dt$simpVox<-as.numeric(dt$simpVox)

#Histogrames per a cada partit
library(ggplot2)
histoERC<-ggplot(dt)+aes(x=simpERC)+geom_histogram(bins = 10, color="black", fill="lightyellow")
histoERC+labs(x="Simpatia envers votants d'ERC (0-10)", y="Freqüència")
descr(dt$simpERC)

histoJUnts<-ggplot(dt)+aes(x=simpJxCAT)+geom_histogram(bins = 10, color="black", fill="lightgreen")
histoJUnts+labs(x="Simpatia envers votants de JxCat (0-10)", y="Freqüència")
descr(dt$simpJxCAT)

histoPSC<-ggplot(dt)+aes(x=simpPSC)+geom_histogram(bins = 10, color="black", fill="red")
histoPSC+labs(x="Simpatia envers votants del PSC (0-10)", y="Freqüència")
descr(dt$simpPSC)

histoPP<-ggplot(dt)+aes(x=simpPP)+geom_histogram(bins = 10, color="black", fill="lightblue")
histoPP+labs(x="Simpatia envers votants del PPC (0-10)", y="Freqüència")
descr(dt$simpPP)

histoCS<-ggplot(dt)+aes(x=simpCS)+geom_histogram(bins = 10, color="black", fill="orange")
histoCS+labs(x="Simpatia envers votants de C'S (0-10)", y="Freqüència")
descr(dt$simpCS)

histoCUP<-ggplot(dt)+aes(x=simpCUP)+geom_histogram(bins = 10, color="black", fill="yellow")
histoCUP+labs(x="Simpatia envers votants de la CUP(0-10)", y="Freqüència")
descr(dt$simpCUP)

histoECP<-ggplot(dt)+aes(x=simpECP)+geom_histogram(bins = 10, color="black", fill="purple")
histoECP+labs(x="Simpatia envers votants d'En Comú Podem (0-10)", y="Freqüència")
descr(dt$simpECP)

histoVox<-ggplot(dt)+aes(x=simpVox)+geom_histogram(bins = 10, color="black", fill="green")
histoVox+labs(x="Simpatia envers votants de Vox (0-10)", y="Freqüència")
descr(dt$simpVox)

# Anàlisis descriptiu, gràfic de correlacions variables numèriques.
library(ggplot2)
library(corrplot)


corrplot(cor(dt[c(2,3,6,7,8,9,10,11,15,18,19,20,21,22,23,24,25)],use="pairwise"),method="square")

#Models lineals polarització
library(modelsummary)
library(ggeffects)
library(jtools)
library(sjPlot)

#Models polarització ideològica
modelesq<-lm(data=dt, simpesq~catalanisme+edat+sexe+ideologia+llengua+valEsp+
             valGen+satdemo+simpdr+sitlab+ingressos+simpind+simpuni)
modeldret<-lm(data=dt, simpdr~catalanisme+edat+sexe+ideologia+llengua+valEsp+
             valGen+satdemo+simpesq+sitlab+ingressos+simpind+simpuni)

llista1<- list ("Esquerra"=modelesq, "Dreta"=modeldret)
modelsummary(llista1, stars = T, statistic = "p.value",conf_level = .95)
tab_model(llista1, show.ci = F, dv.labels = c("Esquerra", "Dreta"), p.style = "stars")
plot_summs(llista1)
vif(modelesq)
vif(modeldret)

predesq1<-ggpredict(modelesq, terms = "catalanisme")
plot(predesq1)+labs(x="Nivell d'Espanyolisme-Catalanisme", y="Simpatia envers votants d'esquerres", title=NULL)

preddret1<-ggpredict(modeldret, terms="catalanisme")
plot(preddret1)+labs(x="Nivell d'Espanyolisme-Catalanisme", y="Simpatia envers votants de dretes", title=NULL)

predesq2<-ggpredict(modelesq, terms = "ideologia")
plot(predesq2)+labs(x="Ideologia (0-10)", y="Simpatia envers votants d'esquerres", title=NULL)

preddret2<-ggpredict(modeldret, terms="ideologia")
plot(preddret2)+labs(x="Ideologia (0-10)", y="Simpatia envers votants de dretes", title=NULL)

#Models polarització territorial
model3indepes<-lm(data=dt, simpind~catalanisme+edat+sexe+ideologia+llengua+valEsp+
                    valGen+satdemo+simpuni+sitlab+ingressos+simpesq+simpdr)

modelunionistes<-lm(data=dt, simpuni~catalanisme+edat+sexe+ideologia+llengua+valEsp+
                      valGen+satdemo+simpind+sitlab+ingressos+simpesq+simpdr)

llista2<- list ("Independentistes"=model3indepes, "Unionistes"=modelunionistes)
modelsummary(llista2, stars = T, conf_level = .95)
tab_model(llista2, show.ci = F, dv.labels = c("Independentistes", "Unionistes"), p.style = "stars")
plot_summs(llista2)
vif(model3indepes)
vif(modelunionistes)



# Model suport a la independència

# Tractem variable suport independència (dictotòmica)
freq(dt$independencia)
dt$independencia[dt$independencia==98]<-NA
dt$independencia[dt$independencia==99]<-NA
dt$independencia[dt$independencia==1]<-0
dt$independencia[dt$independencia==2]<-1

dt$independencia<-factor(dt$independencia, levels=c(0,1), labels = c("Si", "No"))

modelindependencia<-glm(data=dt, independencia~edat+sexe+ideologia+simpind+simpuni+simpdr+simpesq+catalanisme+llengua
                        +ingressos+satdemo+valGen+valEsp, family="binomial")

tab_model(modelindependencia, show.ci= F, p.style = "stars", show.aic = T)

vif(modelindependencia)

plot_coefs(modelindependencia)

#Gràfics annexos sobre independència
library(ggplot2)

caixa1<-ggplot(na.omit(dt))+aes(x=independencia, y=simpind, fill=independencia)+geom_boxplot(outlier.shape = 8, show.legend = F
                                                                                      , color="black")
caixa1+scale_fill_brewer(palette="Blues")+labs(x="Recolzament Independència", y="Simpatia envers gent independentista")

caixa2<-ggplot(na.omit(dt))+aes(x=independencia, y=simpuni, fill=independencia)+geom_boxplot(outlier.shape = 8, show.legend = F
                                                                                             , color="black")
caixa2+scale_fill_brewer(palette="Reds")+labs(x="Recolzament Independència", y="Simpatia envers gent unionista")

caixa3<-ggplot(na.omit(dt))+aes(x=independencia, y=simpesq, fill=independencia)+geom_boxplot(outlier.shape = 8, show.legend = F
                                                                                             , color="black")
caixa3+scale_fill_brewer(palette="Reds")+labs(x="Recolzament Independència", y="Simpatia envers gent d'esquerres")

caixa4<-ggplot(na.omit(dt))+aes(x=independencia, y=simpdr, fill=independencia)+geom_boxplot(outlier.shape = 8, show.legend = F
                                                                                             , color="black")
caixa4+scale_fill_brewer(palette="Blues")+labs(x="Recolzament Independència", y="Simpatia envers gent de dretes")

predindepes<-ggpredict(modelindependencia, terms="simpind")
plot(predindepes)+labs(x="Simpatia envers independentistes", y="Probabilitats de NO recolzar la independència", title=NULL)

predunionistesindep<-ggpredict(modelindependencia, terms="simpuni")
plot(predunionistesindep)+labs(x="Simpatia envers unionistes", y="Probabilitats de NO recolzar la independència", title=NULL)


# Model suport a partits
library(nnet)

modelpartits<-multinom(data=dt, formula=vot~simpERC+simpJxCAT+simpCUP+simpPSC+simpPP+
          simpECP+simpCS+simpVox+simpuni+simpind+simpdr+simpesq)
summary(modelpartits)

plot_coefs(modelpartits)

library(sjPlot)
tab_model(modelpartits,show.ci=F, p.style="stars",show.aic=T)

# Gràfics annexos al model de vot als partits. 
library(ggeffects)
library(ggplot2)
votesq<-ggpredict(modelpartits, terms="simpesq")
plot(votesq)+labs(x="Simpatia vers gent d'esquerres", y="probabilitats de vot", title=NULL)

votdret<-ggpredict(modelpartits, terms="simpdr")
plot(votdret)+labs(x="Simpatia vers gent de dretes", y="probabilitats de vot", title=NULL)

votind<-ggpredict(modelpartits, terms="simpind")
plot(votind)+labs(x="Simpatia vers gent independentista", y="probabilitats de vot", title=NULL)

votuni<-ggpredict(modelpartits, terms="simpuni")
plot(votuni)+labs(x="Simpatia vers gent unionista", y="probabilitats de vot", title=NULL)

voterc<-ggpredict(modelpartits, terms="simpERC")
plot(voterc)+labs(x="Simpatia vers votants d'ERC", y="probabilitats de vot", title=NULL)

votjxcat<-ggpredict(modelpartits, terms="simpJxCAT")
plot(votjxcat)+labs(x="Simpatia vers votants de JxCat", y="probabilitats de vot", title=NULL)

votCUP<-ggpredict(modelpartits, terms="simpCUP")
plot(votCUP)+labs(x="Simpatia vers votants de la CUP", y="probabilitats de vot", title=NULL)

votPSC<-ggpredict(modelpartits, terms="simpPSC")
plot(votPSC)+labs(x="Simpatia vers votants del PSC", y="probabilitats de vot", title=NULL)

votECP<-ggpredict(modelpartits, terms="simpECP")
plot(votECP)+labs(x="Simpatia vers votants d'ECP", y="probabilitats de vot", title=NULL)

votPP<-ggpredict(modelpartits, terms="simpPP")
plot(votPP)+labs(x="Simpatia vers votants del PP", y="probabilitats de vot", title=NULL)

votCS<-ggpredict(modelpartits, terms="simpCS")
plot(votCS)+labs(x="Simpatia vers votants de C'S", y="probabilitats de vot", title=NULL)

votvox<-ggpredict(modelpartits, terms="simpVox")
plot(votvox)+labs(x="Simpatia vers votants de Vox", y="probabilitats de vot", title=NULL)
