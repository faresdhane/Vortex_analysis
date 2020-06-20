library(vortexR)
library(data.table, quietly=TRUE)
library(gridExtra, quietly=TRUE)
library(tidyverse)

########### ST single factor#########
#Load data
woylie.st.classic <- collate_dat("Final_project", 3, scenario = "ST", save2disk=FALSE, verbose=FALSE)
#Summary of the parameters's values used in the ST
lkup.st.classic <- lookup_table(data=woylie.st.classic, project="Final_project",scenario="ST",
                                pop="Population1", SVs=c("SV1", "SV2", "SV3", "SV4", "SV5", "SV6"), save2disk=FALSE)
setnames(lkup.st.classic, c("Scenario", "JuvMor", "AduMor","FemBree","MaleBree", "K", "InitPopS"))
head(lkup.st.classic)
# write.csv(lkup.st.classic, file = "parameters.csv")
#Data visualisation
woylie.st.classic <- woylie.st.classic %>% mutate(Prob_extinct = 1-PExtant)
lineplot.st.classic <- line_plot_year(data=woylie.st.classic, project="Final_project", scenario="ST",
                                      params=c("Prob_extinct", "Nextant","Nall", "Het", "Nalleles",
                                               "r.stoch"), save2disk=FALSE)#all scenarios

dot <- dot_plot(data=woylie.st.classic, project="woylie.st.classic", scenario="ST", yrs=c(1, 100),
                params=c("PExtinct", "Nextant","Nall", "Het", "Nalleles"), save2disk=FALSE)


#Statistics analysis: sensitivity coef (SC) and the strictly standardized mean differences (SSMD)
pairw<-pairwise(data=woylie.st.classic, project="Final_project", scenario="ST",
                params=c("Nall", "Nextant", "Het", "Inbr", "Nalleles" ),
                yrs=100, ST=T, type="Single-Factor", SVs=c("SV1", "SV2", "SV3","SV4", "SV6"),
                save2disk=FALSE)

#p-value table
pval<-pairw[[3]]
pval$SSMD_Nalleles100<-round(pval$SSMD_Nalleles100, 4)
pval$SSMD_Nall100<-round(pval$SSMD_Nall100, 4)
pval$SSMD_Nextant100<-round(pval$SSMD_Nextant100, 4)
pval$SSMD_Het100<-round(pval$SSMD_Het100, 4)
pval$SSMD_Inbr100<-round(pval$SSMD_Inbr100, 4)
pval
# write.csv(pval, file = "P_values.csv")

#Cumulative probability of extinction
woylie.st.classic.ext <- collate_run("Final_project", scenario = "ST", npops = 1,
                                     save2disk=FALSE, verbose=FALSE)
# View(head(woylie.st.classic.ext))

p_ext <- Pextinct(woylie.st.classic.ext$lrun, project="Final_project", scenario="ST",
                  ST = T, save2disk = F,dir_out='DataAnalysis/Pextinct')#cumulative probability of extinction at the end of the simulation

p_ext$PextTable#table output of p-values. p-values < 0.05 means the scenario is different from baseline = parameters is important
# write.csv(p_ext$PextTable, file = "prob_ext_p_value.csv")

#P-values of extinction
extinction <- lineplot.st.classic$Final_project_ST_Prob_extinct_plot$data
extinction_y_100 <- extinction[extinction$Year == "100",]
extinction_y_100 <- extinction_y_100 %>% select(scen.name, Nall, PExtant,Het,Inbr, Nalleles, Prob_extinct)

#Population avec > 0.4 extant

extinction_y_100_res <- extinction_y_100[extinction_y_100$Prob_extinct > 0.4,]# compare it with pairwise #male monopolisation has no effect

p_ext_table <- p_ext$PextTable %>% select(Scenario, pvalues)
extinc <- left_join(extinction_y_100_res, p_ext_table, by = c("scen.name"="Scenario"))
# write.csv(extinc, file = "extinction_scenarios.csv")

##looking for the parameter rank rather than the scenario rank
pairw[[9]]#mean SSMD table pvalues
pairw[[11]]
# write.csv(pairw[[11]], file = "parameters_rank.csv")

############Evaluate the main and interaction effects of some of the parameters used in the simulations############
run <- collate_run(project="Final_project", scenario="SF_ST", 1, save2disk=FALSE, verbose=FALSE)
lrun.SF_ST.no.base <- run[[2]][!run[[2]]$Scenario == "SF_ST(Base)", ]

st_lsh <- collate_dat("Final_project", 3, scenario = "SF_ST", save2disk=FALSE, verbose=FALSE)
st_lsh[1:5, 1:5]
stdat.SF_ST.no.base <- st_lsh[!st_lsh$scen.name == "SF_ST(Base)", ]
lkup.SF_ST <- lookup_table(data=stdat.SF_ST.no.base, project="Final_project", scenario="SF_ST", pop="Population1",
                           SVs=c("SV1", "SV2", "SV3", "SV4", "SV5", "SV6"), save2disk=FALSE)
scatter.plot <- m_scatter(data=stdat.SF_ST.no.base[1:33], data_type="dat", lookup=lkup.SF_ST, yr=100,
                          popn=1, param="Nall", vs=c("SV1", "SV2", "SV3", "SV4", "SV5","SV6"), save2disk=FALSE)
scatter.plot
reg <- fit_regression(data=lrun.SF_ST.no.base, lookup=lkup.SF_ST, census=F, project="Final_project",
                      scenario="SF_ST", popn=1, param="N", vs=c("SV1", "SV2", "SV3", "SV4", "SV5", "SV6"),
                      l=2, ncand=30, save2disk=FALSE)
plot(reg, type="s")
