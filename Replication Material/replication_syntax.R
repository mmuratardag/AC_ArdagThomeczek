
packages <- c("reshape2", "tidyverse", "knitr", "janitor","multcomp", "sjPlot", 
              "gridExtra", "ggrepel", "ggpubr", "ggridges", "ggExtra", "ggiraphExtra","ggeffects", "ggcorrplot",
              "psych", "lavaan", "semPlot", "semTools", "mirt", "scales",
              "factoextra", "FactoMineR", "cluster","mclust", "tidyLPA", "Gifi",
              "mice","visdat", "naniar",
              "stm", "stmBrowser", "quanteda","tidytext", "drlib", "tidystm", "ldatuning",
              "future")
for(i in packages){
  if(!require(i, character.only = T)) install.packages(i, dependencies=T)
  library(i, character.only = T)
}
options(knitr.kable.NA = '')
plan(multiprocess) # to use multiple cores of the processor ### the function might need to change according to OS

load("data.RData")
colnames(d)

##########################
#
# descriptive statistics
#
##########################
# participants by referer
se <- c("google", "gmx", "bing", "yahoo", "t-online", "web")
p_se <- d %>%  dplyr::select(c(referer)) %>% filter(str_detect(referer, se)) # 12 obs 
p_kk <- d %>%  dplyr::select(c(referer)) %>% filter(str_detect(referer, "qualtrics")) # app 200; round (200/839, 2) 24%
p_fb <- d %>%  dplyr::select(c(referer)) %>% filter(str_detect(referer, "facebook")) # app 517; round (517/839, 2) 62 %
# 839-717 = 122; round(122/839, 2) ~ 14% ~ 15%

si_table <- describe(d[,c(7,18:54)])
si_table <- round(si_table,2)

pc_pre.mat <- d %>% dplyr::select("PC1_pre","PC2_pre","PC3_pre")
psych::alpha(pc_pre.mat) # 0.56 6 1.1
ae_pre.mat <- d %>% dplyr::select("AE1_pre","AE2_pre","AE3_pre")
psych::alpha(ae_pre.mat) # 0.84 4.8 1.6
mw_pre.mat <- d %>% dplyr::select("MW1_pre","MW2_pre","MW3_pre")
psych::alpha(mw_pre.mat) # 0.44 3.1 1.2
auth.mat <- d %>% dplyr::select("auth01sub1","auth02sub2R","auth03agg1","auth04agg2R","auth05con1","auth05con2R")
psych::alpha(auth.mat) # 0.78 2.8 0.83
natid_cen.mat <- d %>% dplyr::select("nice01","nice02","nice03")
psych::alpha(natid_cen.mat) # 0.85 2.9 1.1
natid_sol.mat <- d %>% dplyr::select("niso01","niso02","niso03")
psych::alpha(natid_sol.mat) # 0.85 3.5  1
natid_sat.mat <- d %>% dplyr::select("nisa01","nisa02","nisa03")
psych::alpha(natid_sat.mat) # 0.87 3.6 0.98
natid_sst.mat <- d %>% dplyr::select("niss01","niss02","niss03")
psych::alpha(natid_sst.mat) # 0.9 3.1  1
natid_gho.mat <- d %>% dplyr::select("niho01","niho02","niho03")
psych::alpha(natid_gho.mat) # 0.81 3.1 0.86
colnar.mat <- d %>% dplyr::select("cn2","cn3","cn5","cn6","cn8")
psych::alpha(colnar.mat) # 0.86 3.1 1.4

sjt.itemanalysis(pc_pre.mat, show.kurtosis = T, encoding = "UTF-8")
sjt.itemanalysis(ae_pre.mat, show.kurtosis = T, encoding = "UTF-8")
sjt.itemanalysis(mw_pre.mat, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(d [,c(18:26)]),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of items in the populism scale") + theme_bw()

sjt.itemanalysis(auth.mat, show.kurtosis = T, encoding = "UTF-8")
sjt.itemanalysis(colnar.mat, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(d [,c(29:34,50:55)]),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of items in the RWA & collective narcissism scales") + theme_bw()

sjt.itemanalysis(natid_cen.mat, show.kurtosis = T, encoding = "UTF-8")
sjt.itemanalysis(natid_sol.mat, show.kurtosis = T, encoding = "UTF-8")
sjt.itemanalysis(natid_sat.mat, show.kurtosis = T, encoding = "UTF-8")
sjt.itemanalysis(natid_sst.mat, show.kurtosis = T, encoding = "UTF-8")
sjt.itemanalysis(natid_gho.mat, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(d [,c(35:49)]),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of items in the national identification scale") + theme_bw()

describe(d[,56:57])
dvds_bg <- describeBy(d[56:57], group = d$ET)
conG <- dvds_bg$control
conG <- round(conG,2)
PEG <- dvds_bg$PEGIDA
PEG <- round(PEG,2)
LEP <- dvds_bg$LEIPZIG
LEP <- round(LEP,2)
#sjt.itemanalysis(d[,56:57], show.kurtosis = T, encoding = "UTF-8")
d %>% dplyr::select(ET,sl_rel,sl_pol) %>% filter(ET%in%"control") %>% naniar::vis_miss()
d %>% dplyr::select(ET,sl_rel,sl_pol) %>% filter(ET%in%"PEGIDA") %>% naniar::vis_miss()
d %>% dplyr::select(ET,sl_rel,sl_pol) %>% filter(ET%in%"LEIPZIG") %>% naniar::vis_miss()
HiR <- d %>% dplyr::select(ET,sl_rel,sl_pol) %>% ggplot(aes(x=sl_rel, fill=ET, group=ET)) + 
  geom_histogram(position="dodge", bins = 20) + theme_bw() + theme(legend.position="top") +
  ylab("")
HiP <- d %>% dplyr::select(ET,sl_rel,sl_pol) %>% ggplot(aes(x=sl_pol, fill=ET, group=ET)) + 
  geom_histogram(position="dodge", bins = 20) + theme_bw() + theme(legend.position="none") +
  ylab("")
grid.arrange(HiR,HiP, top = "Histogram of outcome items by experimental group", left = "count")

describe(d$age)
SC_AG <- d %>% dplyr::select(age) %>% ggplot(., aes(x=age)) + geom_histogram() + 
  geom_vline(xintercept = 41, linetype = "dashed") + geom_vline(xintercept = 38, linetype = "solid") + theme_bw() +
  xlab("Age") +
  ylab("Frequency") + 
  labs(subtitle="Age\n Dashed line: mean (41), solid line: median (38)")

round(prop.table(table(d$gender)),2)
SC_GE <-d %>% dplyr::select(gender) %>% ggplot(., aes(x=gender)) +
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("Gender") +
  ylab("Frequency") + 
  labs(subtitle="Gender") +
  coord_flip()

describe(d$PolOrLR)
SC_LR <- d %>% dplyr::select(PolOrLR) %>% ggplot(., aes(x=PolOrLR)) + geom_histogram(color="black", fill="white") +
  geom_vline(xintercept = 4.49, linetype = "dashed") + geom_vline(xintercept = 5, linetype = "dotted") + theme_bw() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9)) +
  xlab("Left - Right") +
  ylab("Frequency") + 
  labs(subtitle = "Self-Report political orientation\n Dashed line: mean (4.49), dotted line: median (5)",
       caption = "Higher values indicate right orientation")

round(prop.table(table(d$PrtyChc)),2)
SC_VR <- d %>% dplyr::select(PrtyChc) %>% ggplot(., aes(x=PrtyChc)) +
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("Party") +
  ylab("Frequency") + 
  labs(subtitle="Vote Recall") +
  coord_flip()

grid.arrange(SC_AG,SC_GE,SC_LR,SC_VR, nrow=2, ncol=2, top = "Sample Characteristics 1 // Age, Gender, Political Orientation & Vote-Recall")

round(prop.table(table(d$BornIn)),2)
SC_BI <-d %>% dplyr::select(BornIn) %>% ggplot(., aes(x=BornIn)) +
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("% of Federal State in Data") +
  ylab("Frequency") + 
  labs(subtitle="Born In") +
  coord_flip()
round(prop.table(table(d$LivesIn)),2)
SC_LI <-d %>% dplyr::select(LivesIn) %>% ggplot(., aes(x=LivesIn)) +
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("% of Federal State in Data") +
  ylab("Frequency") + 
  labs(subtitle="Lives In") +
  coord_flip()
round(prop.table(table(d$BornInUrOrRur)),2)
SC_BI_UR <-d %>% dplyr::select(BornInUrOrRur) %>% ggplot(., aes(x=BornInUrOrRur)) +
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("Born in a city with a population ...") +
  ylab("Frequency") + 
  labs(subtitle="Urban vs. Rural") +
  coord_flip()
round(prop.table(table(d$LivesInUrOrRur)),2)
SC_LI_UR <-d %>% dplyr::select(LivesInUrOrRur) %>% ggplot(., aes(x=LivesInUrOrRur)) +
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("Lives in a city with a population ...") +
  ylab("Frequency") + 
  labs(subtitle="Urban vs. Rural") +
  coord_flip()

grid.arrange(SC_BI,SC_LI,SC_BI_UR,SC_LI_UR, nrow=2, ncol=2, top = "Sample Characteristics 2 // Federal State & Urban vs. Rural")

round(prop.table(table(d$ImmBG)),2)
SC_IB <-d %>% dplyr::select(ImmBG) %>% ggplot(., aes(x=ImmBG)) +
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("PArents' citizenship") +
  ylab("Frequency") + 
  labs(subtitle="Immigration Background") +
  coord_flip()

round(prop.table(table(d$nationality)),2)
SC_CI <-d %>% dplyr::select(nationality) %>% ggplot(., aes(x=nationality)) +
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("Citizenship") +
  ylab("Frequency") + 
  labs(subtitle="Nationality") +
  coord_flip()

grid.arrange(SC_IB,SC_CI, nrow=1, ncol=2, top = "Sample Characteristics 3 // Immigration Background & Citizenship" )

###############################
#
# check the psychometric 
# quality of the scale
#
###############################

# people centrism
pc_pre.irt <- mirt(pc_pre.mat,
                   model = 1,
                   itemtype = "graded",
                   technical=list(removeEmptyRows=T))
summary(pc_pre.irt) # factor loadings
coef (pc_pre.irt, IRTpars=T)
pc_iif <- plot(pc_pre.irt, type = "infotrace", main = "People-centrism Item Information Curves") # item information function
pc_ccc <- plot(pc_pre.irt, type = "trace", main = "People-centrism Category Characteristic Curves") # category characteristic curves
pc_iise <- plot(pc_pre.irt, type = "infoSE", main = "People-centrism Information & SE interaction") # interdependence between item information and standard error of measurement
pc_tcc <- plot(pc_pre.irt, main = "People-centrism Test Characteristic Curve") # test characteristic curve
gridExtra::grid.arrange(pc_iif,pc_ccc,pc_iise,pc_tcc, ncol = 2, nrow=2)

# anti elitism
ae_pre.irt <- mirt(ae_pre.mat,
                   model = 1,
                   itemtype = "graded",
                   technical=list(removeEmptyRows=T))
summary(ae_pre.irt) # factor loadings
coef (ae_pre.irt, IRTpars=T) # coefficients
ae_iif <- plot(ae_pre.irt, type = "infotrace", main = "Anti-elitism IIC") 
ae_ccc <- plot(ae_pre.irt, type = "trace", main = "Anti-elitism CCC") 
ae_iise <- plot(ae_pre.irt, type = "infoSE", main = "Anti-elitism info & SE interaction") 
ae_tcc <- plot(ae_pre.irt, main = "Anti-elitism TCC")
gridExtra::grid.arrange(ae_iif,ae_ccc,ae_iise,ae_tcc, ncol = 2, nrow=2)

# manichean
mw_pre.irt <- mirt(mw_pre.mat,
                   model = 1,
                   itemtype = "graded",
                   technical=list(removeEmptyRows=T))
summary(mw_pre.irt) # factor loadings
coef (mw_pre.irt, IRTpars=T) # coefficients
mw_iif <- plot(mw_pre.irt, type = "infotrace", main = "Manichean politics IIC") 
mw_ccc <- plot(mw_pre.irt, type = "trace", main = "Manichean politics CCC") 
mw_iise <- plot(mw_pre.irt, type = "infoSE", main = "Manichean politics info & SE interaction") 
mw_tcc <- plot(mw_pre.irt, main = "Manichean politics TCC")
gridExtra::grid.arrange(mw_iif,mw_ccc,mw_iise,mw_tcc, ncol = 2, nrow=2)

# rwa
rwa.irt <- mirt(auth.mat,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(rwa.irt) # factor loadings
coef (rwa.irt, IRTpars=T) # coefficients
au_iif <- plot(rwa.irt, type = "infotrace", main = "RWA IIC") 
au_ccc <- plot(rwa.irt, type = "trace", main = "RWA CCC") 
au_iise <- plot(rwa.irt, type = "infoSE", main = "RWA info & SE interaction") 
au_tcc <- plot(rwa.irt, main = "RWA TCC")
gridExtra::grid.arrange(au_iif,au_ccc,au_iise,au_tcc, ncol = 2, nrow=2)

# collective narcissism
coln.irt <- mirt(colnar.mat,
                 model = 1,
                 itemtype = "graded",
                 technical=list(removeEmptyRows=T))
summary(coln.irt) # factor loadings
coef (coln.irt, IRTpars=T) # coefficients
col_iif <- plot(coln.irt, type = "infotrace", main = "Collective Narcissism IIC") 
col_ccc <- plot(coln.irt, type = "trace", main = "Collective Narcissism CCC") 
col_iise <- plot(coln.irt, type = "infoSE", main = "Collective Narcissism info & SE interaction") 
col_tcc <- plot(coln.irt, main = "Collective Narcissism TCC")
gridExtra::grid.arrange(col_iif,col_ccc,col_iise,col_tcc, ncol = 2, nrow=2)

# national identity centrality
cen.irt <- mirt(natid_cen.mat,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(cen.irt) # factor loadings
coef (cen.irt, IRTpars=T) # coefficients
cen_iif <- plot(cen.irt, type = "infotrace", main = "Nat.ID. Centrality IIC") 
cen_ccc <- plot(cen.irt, type = "trace", main = "Nat.ID. Centrality CCC") 
cen_iise <- plot(cen.irt, type = "infoSE", main = "Nat.ID. Centrality info & SE interaction") 
cen_tcc <- plot(cen.irt, main = "Nat.ID. Centrality TCC")
gridExtra::grid.arrange(cen_iif,cen_ccc,cen_iise,cen_tcc, ncol = 2, nrow=2)

# national identity solidarity
sol.irt <- mirt(natid_sol.mat,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(sol.irt) # factor loadings
coef (sol.irt, IRTpars=T) # coefficients
sol_iif <- plot(sol.irt, type = "infotrace", main = "Nat.ID. Solidarity IIC") 
sol_ccc <- plot(sol.irt, type = "trace", main = "Nat.ID. Solidarity CCC") 
sol_iise <- plot(sol.irt, type = "infoSE", main = "Nat.ID. Solidarity info & SE interaction") 
sol_tcc <- plot(sol.irt, main = "Nat.ID. Solidarity TCC")
gridExtra::grid.arrange(sol_iif,sol_ccc,sol_iise,sol_tcc, ncol = 2, nrow=2)

# national identity satisfaction
sat.irt <- mirt(natid_sat.mat,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(sat.irt) # factor loadings
coef (sat.irt, IRTpars=T) # coefficients
sat_iif <- plot(sat.irt, type = "infotrace", main = "Nat.ID. Satisfaction IIC") 
sat_ccc <- plot(sat.irt, type = "trace", main = "Nat.ID. Satisfaction CCC") 
sat_iise <- plot(sat.irt, type = "infoSE", main = "Nat.ID. Satisfaction info & SE interaction") 
sat_tcc <- plot(sat.irt, main = "Nat.ID. Satisfaction TCC")
gridExtra::grid.arrange(sat_iif,sat_ccc,sat_iise,sat_tcc, ncol = 2, nrow=2)

# national identity self-stereotyping
ss.irt <- mirt(natid_sst.mat,
               model = 1,
               itemtype = "graded",
               technical=list(removeEmptyRows=T))
summary(ss.irt) # factor loadings
coef (ss.irt, IRTpars=T) # coefficients
ss_iif <- plot(ss.irt, type = "infotrace", main = "Nat.ID. Self-Stereotyping IIC") 
ss_ccc <- plot(ss.irt, type = "trace", main = "Nat.ID. Self-Stereotyping CCC") 
ss_iise <- plot(ss.irt, type = "infoSE", main = "Nat.ID. Self-Stereotyping info & SE interaction") 
ss_tcc <- plot(ss.irt, main = "Nat.ID. Self-Stereotyping TCC")
gridExtra::grid.arrange(ss_iif,ss_ccc,ss_iise,ss_tcc, ncol = 2, nrow=2)

# national identity homogeneity
ho.irt <- mirt(natid_gho.mat,
               model = 1,
               itemtype = "graded",
               technical=list(removeEmptyRows=T))
summary(ho.irt) # factor loadings
coef (ho.irt, IRTpars=T) # coefficients
ho_iif <- plot(ho.irt, type = "infotrace", main = "Nat.ID. In-Group Homogeneity IIC") 
ho_ccc <- plot(ho.irt, type = "trace", main = "Nat.ID. In-Group Homogeneity CCC") 
ho_iise <- plot(ho.irt, type = "infoSE", main = "Nat.ID. In-Group Homogeneity info & SE interaction") 
ho_tcc <- plot(ho.irt, main = "Nat.ID. In-Group Homogeneity TCC")
gridExtra::grid.arrange(ho_iif,ho_ccc,ho_iise,ho_tcc, ncol = 2, nrow=2)

###############################
#
# build the measurement models
#
###############################

# CFA populist attitudes
model_pre <- '
PC =~ PC1_pre + PC2_pre + PC3_pre
AE =~ AE1_pre + AE2_pre + AE3_pre
MW =~ MW1_pre + MW2_pre + MW3_pre
'
fit.model_pre <- cfa (model_pre, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(fit.model_pre, fit.measures=T, standardized=T, rsquare=T)
semPaths(fit.model_pre, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4, curve = 2)
title("CFA: Populist Attitudes",line=0.5)
round(reliability(fit.model_pre),2)
pop_fs <- lavPredict(fit.model_pre, type = "lv", method = "EBM", label = T, fsm = F)
pop_fs <- as.data.frame(pop_fs)
d$PC <- scales::rescale(pop_fs$PC, to = c(0, 1), from = range(pop_fs$PC, na.rm = F, finite = T))
d$AE <- scales::rescale(pop_fs$AE, to = c(0, 1), from = range(pop_fs$AE, na.rm = F, finite = T)) 
d$MV <- scales::rescale(pop_fs$MW, to = c(0, 1), from = range(pop_fs$MW, na.rm = F, finite = T)) 
pop_dp <- ggplot(melt(d[,c(58:60)]),aes(x=value)) + geom_density() + facet_wrap(~variable) + 
  labs(subtitle="Pc:people-centrism; AE:anti-elitism; MV:Manicehan view of politics",
       title="Density plots of factor scores ", 
       caption = "Rescaled to range between 0-1")

# CFA rwa
model.rwa <-'rwa =~ auth02sub2R + auth03agg1 + auth04agg2R + auth05con1 + auth05con2R'
fit.model.rwa <- cfa (model.rwa, data=d, estimator="mlr", std.ov=T, std.lv =T, mimic="mplus", missing="fiml")
summary (fit.model.rwa, fit.measures=T, standardized=T, rsquare=T)
semPaths(fit.model.rwa, "mod", "std", intercepts = F, rotation = 4, edge.label.cex=1.1)
title("CFA: RWA",line=0.5)
round(reliability(fit.model.rwa),2)
aut_fs <- lavPredict(fit.model.rwa, type = "lv", method = "EBM", label = T, fsm = F)
aut_fs <- as.data.frame(aut_fs)
d$RWA <- scales::rescale(aut_fs$rwa, to = c(0, 1), from = range(aut_fs$rwa, na.rm = F, finite = T))
rwa_dp <- ggplot(d ,aes(x=RWA)) + geom_density() + 
  labs(subtitle="Right-wing authoritarianism",
       title="Density plots of factor scores ", 
       caption = "Rescaled to range between 0-1")

# CFA collective narcissism
model.cn <- 'ColNar =~ cn2 + cn3 + cn5 + cn6 + cn8'
fit.model_cn <- cfa (model.cn, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", std.ov=T, missing="fiml")
summary(fit.model_cn, fit.measures=T, standardized=T, rsquare=T)
semPaths(fit.model_cn, "mod", "std", intercepts = F, edge.label.cex=1, layout = "tree3", rotation = 4)
title("CFA: Collective Narcissism",line=0.5)
round(reliability(fit.model_cn),2)
CN_fs <- lavPredict(fit.model_cn, type = "lv", method = "EBM", label = T, fsm = F)
CN_fs <- as.data.frame(CN_fs)
d$CN <- scales::rescale(CN_fs$ColNar, to = c(0, 1), from = range(CN_fs$ColNar, na.rm = F, finite = T))
cn_dp <- ggplot(d, aes(x=CN)) + geom_density() + 
  labs(subtitle="Collective narcissism",
       title="Density plots of factor scores ", 
       caption = "Rescaled to range between 0-1")

# CFA national identity
model.nid <- '
Cent =~ nice01 + nice02 + nice03
Soli =~ niso01 + niso02 + niso03
Sati =~ nisa01 + nisa02 + nisa03
SSte =~ niss01 + niss02 + niss03
InHo =~ niho01 + niho02 + niho03
SInv =~ Cent + Soli + Sati
SDef =~ SSte + InHo
SInv ~~ SDef
'
fit.model.nid <- cfa (model.nid, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", std.ov=T, missing="fiml")
summary(fit.model.nid, fit.measures=T, standardized=T, rsquare=T)
semPaths(fit.model.nid, "mod", "std", intercepts = F, edge.label.cex=1, layout = "tree3", rotation = 4)
title("CFA: National Identity",line=0.8)
round(reliability(fit.model.nid),2)
round(reliabilityL2(fit.model.nid, "SInv"),2)
round(reliabilityL2(fit.model.nid, "SDef"),2)
ni_fs <- lavPredict(fit.model.nid, type = "lv", method = "EBM", label = T, fsm = F)
ni_fs <- as.data.frame(ni_fs)
d$SInv <- scales::rescale(ni_fs$SInv, to = c(0, 1), from = range(ni_fs$SInv, na.rm = F, finite = T))
d$SDef <- scales::rescale(ni_fs$SDef, to = c(0, 1), from = range(ni_fs$SDef, na.rm = F, finite = T))
ni_dp <- ggplot(melt(d[,c(63:64)]),aes(x=value)) + geom_density() + facet_wrap(~variable) +
  labs(subtitle="SInv:self-investment; SDef:self-definition",
       title="Density plots of factor scores ", 
       caption = "Rescaled to range between 0-1")

gridExtra::grid.arrange(pop_dp,rwa_dp,cn_dp,ni_dp, ncol = 2, nrow=2)

save(d, file = "data_wFS.RData")

rm(list = ls(all.names = T)); load("data_wFS.RData")

###############################
#
# LPA
#
###############################

# mclust requires no missing data
# impute factor scores before the LPA

sjt.itemanalysis(d[,58:64], scale = F, show.kurtosis = T, encoding = "UTF-8")
describe(d[,58:64])

dsmv <- vis_miss(d[,58:64], sort_miss = T) + theme_bw()
gg_miss_upset(d[,58:64], nsets = n_var_miss(d[,58:64]))
dsmvl <- gg_miss_var_cumsum(d[,58:64])
grid.arrange(dsmv, dsmvl)

df <- d %>% dplyr::select(58:64) %>% drop_na()
df_mv <- ggplot(melt(df),aes(x=value)) + geom_density() + facet_wrap(~variable) + ggtitle("Density plots before imputation") + theme_bw()

dmv <- mice(d [, c(58:64)], m=10, maxit=100, method = "pmm", seed = 123)
plot(dmv)
stripplot(dmv)
dmvi <- mice::complete(dmv,10)
df_wmv <- ggplot(melt(dmvi),aes(x=value)) + geom_density() + facet_wrap(~variable) + ggtitle("Density plots before imputation") + theme_bw()
grid.arrange(df_mv, df_wmv, nrow=2, ncol=1)
d[,58:64] <- dmvi

sjt.itemanalysis(d[,58:64], scale = F, show.kurtosis = T, encoding = "UTF-8")
describe(d[,58:64])

mc <- Mclust(d[,58:64])
summary(mc)
fviz_mclust(mc, "BIC", palette = "jco")
mc_LRT <- mclustBootstrapLRT(d[,58:64], modelName = "VVV", maxG = 4)
mclustModelNames("VVV")
print(mc_LRT)
plot(mc_LRT, G = 1)

results_LPA <- d %>% dplyr::select(PC,AE,MV,RWA,CN,SInv,SDef) %>%
  estimate_profiles(2, variances = c("varying"), covariances = c("varying"))

plot_profiles(results_LPA, rawdata = F, palette = "jco") + ggtitle("Estimated Latent Profiles")
pp <- plot_profiles(results_LPA, rawdata = F, bw=T, add_line = T) + ggtitle("Estimated Latent Profiles")

grid.arrange(pp, bottom = "PC = people-centrism, AE = anti-elitism, MV = Manichean view of politics
             RWA = right-wing authoritarianism, CN = collective narcissism
             SInv = self-investment, SDef = self-definition")

get_estimates(results_LPA)
get_data(results_LPA)
get_fit(results_LPA)

results_LPA_m <- d %>% dplyr::select(PC,AE,MV,RWA,CN,SInv,SDef) %>%
  estimate_profiles(1:3, variances = c("varying"), covariances = c("varying"))
get_fit(results_LPA_m)

d$profile <- results_LPA[["model_6_class_2"]][["dff"]][["Class"]]
d$profile <- factor(d$profile, levels = c(1,2), labels = c("profile1","profile2"))
round(prop.table(table(d$profile)),2)
d$profileOi <- results_LPA[["model_6_class_2"]][["dff"]][["Class"]]
d$profileOi <- car::recode(d$profileOi, "1=0;2=1")

round(prop.table(table(d$profile)),2)

save(d, file = "data_wFSaPR.RData")

mcLPAresults_2P <- Mclust(d[,58:64], modelName = "VVV", G=2)

DE_mv_pca=fviz_mclust(mcLPAresults_2P, "classification", geom = "point",
                      pointsize = 1.5, palette = "jco",
                      main = "A. Cluster Plot PCA")
DE_mv_pca_un=fviz_mclust(mcLPAresults_2P, "uncertainty", palette = "jco", main = "B. Cluster Plot PCA")
DE_mv_ni=fviz_mclust(mcLPAresults_2P, "classification", geom = "point",
                     pointsize = 1.5, palette = "jco",
                     choose.vars = c("SInv", "SDef"),
                     main = "C. Cluster Plot Individual Differences in National Identity",
                     xlab = "Self-Investment", ylab = "Self-Definition")
DE_mv_an=fviz_mclust(mcLPAresults_2P, "classification", geom = "point",
                     pointsize = 1.5, palette = "jco",
                     choose.vars = c("RWA", "CN"),
                     main = "D. Cluster Plot Individual Differences in RWA & Collective Narcissism",
                     xlab = "RWA", ylab = "Collective Narcissism")
grid.arrange(DE_mv_pca,DE_mv_pca_un,
             DE_mv_ni,DE_mv_an, 
             nrow=2, ncol=2)

describe(d[,c(58:64)])
describeBy(d[,c(58:64)], group=d$profile)

dso <- describe(d[,c(58:64)])
dso <- round(dso,2)
dso_bg <- describeBy(d[,c(58:64)], group=d$profile)
dso_bg_g1 <- round(dso_bg$profile1,2)
dso_bg_g2 <- round(dso_bg$profile2,2)

d_temp <- d
d_temp$BornEW <- fct_collapse(d_temp$BornIn,
                              East=c("Sachsen-Anhalt", "Thüringen", "Sachsen","Berlin","Mecklenburg-Vorpommern","Brandenburg"),
                              West=c("Baveria","Rheinland-Pfalz","Baden-Wuerttemberg","Schleswig-Holstein",
                                     "Saarland","Niedersachsen","Nordrhein-Westfalen","Hessen",
                                     "Hamburg","Bremen"))
round(prop.table(table(d_temp$BornEW)),2)

d_temp$POPpRd <- ifelse(d_temp$PrtyChc=="AfD" , 1, 0)
d_temp$POPpLd <- ifelse(d_temp$PrtyChc=="Die Linke", 1, 0)
round(prop.table(table(d_temp$POPpR)),2)
round(prop.table(table(d_temp$POPpL)),2)
pop_vote_cor_mat <- d_temp %>% dplyr::select(POPpRd,POPpLd,profileOi)
print(corr.test(pop_vote_cor_mat, ci=T), short = F)
apaTables::apa.cor.table(pop_vote_cor_mat)


PolOrLRbin <- bootnet::binarize(d_temp$PolOrLR, na.rm = T, removeNArows = F)
d_temp$PolOrLRbin <- PolOrLRbin$x
d_temp$PolOrLRbin <- factor(d_temp$PolOrLRbin, levels = c(0,1), labels = c("Left","Right"), ordered = T)

d_temp %>% dplyr::select(age,gender,BornEW,PolOrLR,PC,AE,MV,RWA,CN,SInv,SDef,profile) %>% drop_na() %>%
  ggplot(aes(x = PolOrLR, fill = profile)) +
  geom_density(alpha = .3) +
  facet_grid(BornEW ~ gender, margins = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme_bw() + scale_fill_manual(values=c("#0073C299", "#EFC00099")) + 
  ggtitle("Estimated profiles by gender, East vs. West Germany & political orientation") +
  xlab("Political Orientation (Higher values indicate right orientation)")

mod.glm <- glm(profile ~ gender + BornEW + age + PolOrLR, family = binomial(link="logit"), data = d_temp)
summary(mod.glm)
d_temp$predicted <- predict(mod.glm, d_temp, type="response")
pROC_obj <- pROC::roc(d_temp$profile,d_temp$predicted,
                      smoothed = T,
                      # arguments for ci
                      ci=T, ci.alpha=0.9, stratified=F,
                      # arguments for plot
                      plot=T, auc.polygon=T, max.auc.polygon=T, grid=T,
                      print.auc=T, show.thres=T)
sens.ci <- pROC::ci.se(pROC_obj)
plot(sens.ci, type="shape", col="pink")
plot(sens.ci, type="bars")
fmsb::NagelkerkeR2(mod.glm)

mar_eff_prof <- margins::margins(mod.glm)
summary(mar_eff_prof)
plot(mar_eff_prof)

#texreg::htmlreg(mod.glm, file = "mod.glm.doc")

mydf_EW <- ggeffects::ggpredict(mod.glm, terms = c("BornEW"))
#plot(mydf_EW)
mod.glm_EW <- mydf_EW %>% ggplot(aes(x=x, y=predicted)) +
  geom_point() + geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  xlab("Born in ...") +
  ylab("") + theme_bw()

mydf_PO <- ggeffects::ggpredict(mod.glm, terms = c("PolOrLR"))
#plot(mydf_PO, ci.style = "errorbar", dot.size = 1.5)
mod.glm_PO <- mydf_PO %>% ggplot(aes(x=x, y=predicted)) + geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(limits = c("Left","2","3","4","5","6","7","8","Right")) +
  xlab("Political Orientation") +
  ylab("") + theme_bw()

mydf_Ge <- ggeffects::ggpredict(mod.glm, terms = c("gender"))
#plot(mydf_Ge)
mod.glm_Ge <- mydf_Ge %>% ggplot(aes(x=x, y=predicted)) +
  geom_point() + geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  xlab("Gender") +
  ylab("") + theme_bw()

mydf_Ag <- ggeffects::ggpredict(mod.glm, terms = c("age [all]"))
#plot(mydf_Ag)
mod.glm_Ag <- mydf_Ag %>% ggplot(aes(x = x)) +
  geom_line(aes(y = predicted)) +
  geom_line(aes(y = conf.high), linetype = 2) +
  geom_line(aes(y = conf.low), linetype = 2) +
  xlab("Age") +
  ylab("") + theme_bw()

grid.arrange(mod.glm_EW,mod.glm_PO,
             mod.glm_Ge,mod.glm_Ag,
             ncol=2, nrow=2,
             top = "Effects of socio-demographics on profile",
             left = "Predicted Probability")


cto <- descr::CrossTable(d$ET,d$profile)
print(cto)
chisq.test(table(d$ET,d$profile))

ggplot(d, aes(ET, profile)) +
  geom_jitter(aes(color = ET), size = 1) +
  theme(legend.position = "none") + 
  xlab("Experimental Conditions") +
  ylab("Estimated Profiles") + 
  labs(subtitle="Cross Tabulation",
       title="Experimental Treatment & Latent Profile") + theme_bw()

rm(list = ls(all.names = T)); load("data_wFSaPR.RData")

###############################
#
# STM 
# experimental manipulation
# check
#
###############################

DE_csw <- scan("DE_sw.txt",what = "character", sep = "\n")
dtm <- dfm(d$POPtext, remove_numbers = T, remove_punct = T, remove_symbols = T, remove = DE_csw)
dtm_topmod <- convert(dtm, to = "topicmodels")

# search for the number of topics with LDA tuning
result <- FindTopicsNumber(
  dtm_topmod,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 123),
  mc.cores = 7L,
  verbose = T
)
FindTopicsNumber_plot(result)

DE_processed <- textProcessor(documents = d$POPtext,
                              metadata = d,
                              language = "german",
                              onlycharacter = T,
                              stem = T,
                              removestopwords=T,
                              customstopwords = DE_csw)
DE_prep.docs.output <- prepDocuments(DE_processed$documents,
                                     DE_processed$vocab,
                                     DE_processed$meta,
                                     lower.thresh = 8)
DE_docs.pop <- DE_prep.docs.output$documents
DE_vocab.pop <- DE_prep.docs.output$vocab
DE_meta.pop <- DE_prep.docs.output$meta

# search for the number of topics with STM
K<-c(2,3,4,5,6,7,8,9,10)
kresults <- searchK(DE_docs.pop, DE_vocab.pop, K, prevalence =~ ET + profile, data = DE_meta.pop, seed = 123)
#plot(kresults)
kresults_df <- kresults$results
sc_lp <- ggplot(data=kresults_df, aes(x=K, y=semcoh)) + geom_line() + geom_point() +
  labs(x = "Number of Topics", y = "Semantic Coherence",
       #title = "Diagnostic Values by Number of Topics",
       subtitle = "Semantic Coherence") + theme_bw()
re_lp <- ggplot(data=kresults_df, aes(x=K, y=residual)) + geom_line() + geom_point() +
  labs(x = "Number of Topics", y = "Residuals",
       #title = "Diagnostic Values by Number of Topics",
       subtitle = "Residuals") + theme_bw()
grid.arrange(sc_lp,re_lp, ncol=2, top = "Diagnostic Values by Number of Topics")

DE_stm_pop <- stm(DE_docs.pop,
                  DE_vocab.pop,
                  K = 4,
                  prevalence =~ ET + profile,
                  data = DE_meta.pop, init.type = "LDA", seed = 12345)

td_beta <- tidy(DE_stm_pop)
topic_summ <- td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "A. Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics") + theme_bw()

td_gamma <- tidy(DE_stm_pop, matrix = "gamma",                    
                 document_names = rownames(DE_docs.pop))
td_gamma$profile <- NA
td_gamma$profile <- rep(DE_meta.pop$profile,4)
class(td_gamma$profile)
gamma_dist <- ggplot(td_gamma, aes(gamma, fill = profile)) +
  geom_density(alpha = 0.1, show.legend = T) +
  facet_wrap(~ topic, ncol = 4) +
  labs(title = "B. Distribution of document probabilities for each topic by profile",
       #subtitle = "",
       #caption = "",
       y = "density", x = "expression(gamma)") + theme_bw()

#grid.arrange(topic_summ, gamma_dist, nrow=2, ncol=1)

labelTopics(DE_stm_pop)

theta <- DE_stm_pop$theta
thetas <- data.frame(topic1 = theta[,1],
                     topic2 = theta[,2],
                     topic3 = theta[,3],
                     topic4 = theta[,4])
meta_full <- cbind(DE_meta.pop, thetas)
colnames(meta_full)

cormat <- cor(meta_full[,67:70])
tp_c <- ggcorrplot(cormat, hc.order = F, 
                   type = "lower", 
                   lab = T, 
                   lab_size = 3, 
                   method="circle", 
                   colors = c("tomato2", "white", "springgreen3"), 
                   title="C. Correlogram of Topic Proportions", 
                   ggtheme=theme_bw)

grid.arrange(arrangeGrob(topic_summ, gamma_dist),tp_c, ncol = 2)

t1_ols <- lm(topic1 ~ profile+ET, data = meta_full); apastats::describe.glm(t1_ols, dtype=3) %>% kable()
t2_ols <- lm(topic2 ~ profile+ET, data = meta_full); apastats::describe.glm(t2_ols, dtype=3) %>% kable()
t3_ols <- lm(topic3 ~ profile+ET, data = meta_full); apastats::describe.glm(t3_ols, dtype=3) %>% kable()
t4_ols <- lm(topic4 ~ profile+ET, data = meta_full); apastats::describe.glm(t4_ols, dtype=3) %>% kable()

#texreg::htmlreg(list(t1_ols,t2_ols,t3_ols,t4_ols), file = "mod.ols.doc")
apaTables::apa.reg.table(t1_ols, filename = "mod.ols_APA_t1.doc", table.number = 1)
apaTables::apa.reg.table(t2_ols, filename = "mod.ols_APA_t2.doc", table.number = 1)
apaTables::apa.reg.table(t3_ols, filename = "mod.ols_APA_t3.doc", table.number = 1)
apaTables::apa.reg.table(t4_ols, filename = "mod.ols_APA_t4.doc", table.number = 1)

plot(DE_stm_pop, type="perspectives", topics=c(1,2))
plot(DE_stm_pop, type="perspectives", topics=c(1,3))
plot(DE_stm_pop, type="perspectives", topics=c(1,4))
plot(DE_stm_pop, type="perspectives", topics=c(2,3))
plot(DE_stm_pop, type="perspectives", topics=c(2,4))
plot(DE_stm_pop, type="perspectives", topics=c(3,4))

thoughts1 <- findThoughts(DE_stm_pop,
                          texts = meta_full$POPtext,
                          n = 20,
                          topics = 1, meta=meta_full, where = Topic1>.6); thoughts1

thoughts2 <- findThoughts(DE_stm_pop,
                          texts = meta_full$POPtext,
                          n = 20,
                          topics = 2, meta=meta_full, where = Topic2>.6); thoughts2

thoughts3 <- findThoughts(DE_stm_pop,
                          texts = meta_full$POPtext,
                          n = 50,
                          topics = 3, meta=meta_full, where = Topic3>.6); thoughts3

thoughts4 <- findThoughts(DE_stm_pop,
                          texts = meta_full$POPtext,
                          n = 20,
                          topics = 4, meta=meta_full, where = Topic4>.4); thoughts4

my_comparisons <- list( c("control", "PEGIDA"), c("PEGIDA", "LEIPZIG"), c("control", "LEIPZIG") )

tp1bpEX <- ggboxplot(meta_full, x = "ET", y = "topic1", palette = "grey",
                     xlab = "", ylab = "") +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") + # Add pairwise comparisons p-value
  stat_compare_means(label.y = .8)  +   # Add global p-value +                 # Pairwise comparison against all
  labs(title = 'Topic 1 Proportions', subtitle = "Comments & criticism about PEGIDA")

tp2bpEX <- ggboxplot(meta_full, x = "ET", y = "topic2", palette = "grey",
                     xlab = "", ylab = "") +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") + # Add pairwise comparisons p-value
  stat_compare_means(label.y = .9)  +   # Add global p-value +                 # Pairwise comparison against all
  labs(title = 'Topic 2 Proportions', subtitle = "Comments about GDR & Monday Demonstrations")

tp3bpEX <- ggboxplot(meta_full, x = "ET", y = "topic3", palette = "grey",
                     xlab = "", ylab = "") +
  stat_compare_means(comparisons = my_comparisons, label.y = c(.8,.75,.7), label = "p.signif") + # Add pairwise comparisons p-value
  stat_compare_means(label.y = .5)  +   # Add global p-value +                 # Pairwise comparison against all
  labs(title = 'Topic 3 Proportions', subtitle = "Comments about the Peace March")

tp4bpEX <- ggboxplot(meta_full, x = "ET", y = "topic4", palette = "grey",
                     xlab = "", ylab = "") +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") + # Add pairwise comparisons p-value
  stat_compare_means(label.y = .5)  +   # Add global p-value +                 # Pairwise comparison against all
  labs(title = 'Topic 4 Proportions', subtitle = "Comments about the slogan and its connotations")

grid.arrange(tp1bpEX,tp2bpEX,tp3bpEX,tp4bpEX, ncol = 2, nrow=2,
             top = "Topics by experimental treatment",
             bottom = "Experimental Conditions",
             left = "% of words associated with the topic in the open ended responses")


tp1bpEXpr<-ggboxplot(meta_full, x = "ET", y = "topic1", add = "median",
                     xlab = "", ylab = "",
                     color = "profile", palette = "jco") +
  stat_compare_means(aes(group = profile), label = "p.signif") +
  labs(title = 'Topic 1 %', subtitle = "Comments & criticism about PEGIDA")

tp2bpEXpr<-ggboxplot(meta_full, x = "ET", y = "topic2", add = "median",
                     xlab = "", ylab = "",
                     color = "profile", palette = "jco") +
  stat_compare_means(aes(group = profile), label = "p.signif") +
  labs(title = 'Topic 2 %', subtitle = "Comments about GDR & Monday Demonstrations")

tp3bpEXpr<-ggboxplot(meta_full, x = "ET", y = "topic3", add = "median",
                     xlab = "", ylab = "",
                     color = "profile", palette = "jco") +
  stat_compare_means(aes(group = profile), label = "p.signif") +
  labs(title = 'Topic 3 %', subtitle = "Comments about the Peace March")

tp4bpEXpr<-ggboxplot(meta_full, x = "ET", y = "topic4", add = "median",
                     xlab = "", ylab = "",
                     color = "profile", palette = "jco") +
  stat_compare_means(aes(group = profile), label = "p.signif") +
  labs(title = 'Topic 4 %', subtitle = "Comments about the slogan and its connotations")

grid.arrange(tp1bpEXpr,tp2bpEXpr,tp3bpEXpr,tp4bpEXpr, ncol = 2, nrow=2,
             top = "Topics by experimental treatment & profile",
             bottom = "Experimental Conditions",
             left = "% of words associated with the topic in the open ended responses")

t1r <- ggplot(meta_full, aes(x=topic1, y=profile, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) + facet_wrap(~ET) +
  labs(subtitle="Comments & criticism about PEGIDA", x="",y="",
       title="Topic 1 %") + theme_bw() + theme(legend.position = "none")
t2r <- ggplot(meta_full, aes(x=topic2, y=profile, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) + facet_wrap(~ET) +
  labs(subtitle="Comments about GDR & Monday Demonstrations", x="",y="",
       title="Topic 2 %") + theme_bw()
t3r <- ggplot(meta_full, aes(x=topic3, y=profile, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) + facet_wrap(~ET) +
  labs(subtitle="Comments about the Peace March", x="",y="",
       title="Topic 3 %") + theme_bw() + theme(legend.position = "none")
t4r <- ggplot(meta_full, aes(x=topic4, y=profile, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) + facet_wrap(~ET) +
  labs(subtitle="Comments about the slogan and its connotations", x="",y="",
       title="Topic 4 %") + theme_bw()
grid.arrange(t1r,t2r,t3r,t4r, ncol=2, nrow=2,
             top = "Plots showing topics by experimental treatment & profile with quantiles",
             bottom = "% of words associated with the topic in the open ended responses",
             left = "Latent profiles")

save(meta_full, file = "data_wFSaPRaTP.RData"); rm(list = ls(all.names = T)); load("data_wFSaPRaTP.RData")

###############################
#
# SEM
# path model
#
###############################


path_model <- 'sl_rel + sl_pol ~ profileOi'
path_model_res_fit <- sem (path_model, meta_full, estimator="mlr", mimic="mplus", missing="fiml", fixed.x = F, std.ov=T,
                           group = "ET", sample.mean = T,
                           group.equal=c("means", "intercepts", "regressions", "residual.covariances")) # restricted model
path_model_fre_fit <- sem (path_model, meta_full, estimator="mlr", mimic="mplus", missing="fiml", fixed.x = F, std.ov=T,
                           group = "ET", sample.mean = T,
                           group.partial=c("means", "intercepts", "regressions", "residual.covariances")) # free model

anova(path_model_res_fit, path_model_fre_fit)
cf <- compareFit(path_model_res_fit, path_model_fre_fit, nested=T)
summary(cf)
anova_table <- cf@nested
anova_table <- round(anova_table,3)
anova_table %>% kable()
GoF_table <- cf@fit[c("chisq","df","pvalue",
                      "cfi.robust","tli.robust",
                      "rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust",
                      "srmr_mplus")]
GoF_table <- round(GoF_table,3)
GoF_table <- t(GoF_table)
GoF_table %>% kable()

summary(path_model_res_fit, fit.measures=T, standardized=T, rsquare=T)
summary(path_model_fre_fit, fit.measures=T, standardized=T, rsquare=T)

semPaths(path_model_res_fit, "std", "std", intercept=F, rotation = 2, edge.label.cex=2.3, sizeMan = 15, 
         layout = "tree", nodeLabels = c("Relev.", "Polar.", "Prof."),
         edge.color="black",
         style = "lisrel", curveAdjacent = 0,
         ask = F, panelGroups = T)
semPaths(path_model_fre_fit, "std", "std", intercept=F, rotation = 2, edge.label.cex=2.3, sizeMan = 15,
         layout = "tree", nodeLabels = c("Relev.", "Polar.", "Prof."),
         edge.color="black",
         style = "lisrel", curveAdjacent = 0,
         ask = F, panelGroups  = T)

semPaths(path_model_fre_fit, "mod", intercept=F, rotation = 2, edge.label.cex=2.3, sizeMan = 15,
         layout = "tree", nodeLabels = c("Relev.", "Polar.", "Prof."),
         edge.color="black",
         style = "lisrel", curveAdjacent = 0,
         ask = F, panelGroups  = T)

parameterEstimates(path_model_res_fit, standardized=T) %>% 
  filter(op == "~") %>% 
  mutate(stars = ifelse(pvalue < .001, "***", 
                        ifelse(pvalue < .01, "**", 
                               ifelse(pvalue < .05, "*", "")))) %>%
  dplyr::select(Outcome=lhs, 
                Indicator=rhs,
                ExpT=group,
                B=est, 
                SE=se, Z=z, 
                Beta=std.all, 
                sig=stars) %>% 
  kable(digits = 3, format="pandoc", caption=" Restricted Regression Coefficients")

parameterEstimates(path_model_fre_fit, standardized=T) %>% 
  filter(op == "~") %>% 
  mutate(stars = ifelse(pvalue < .001, "***", 
                        ifelse(pvalue < .01, "**", 
                               ifelse(pvalue < .05, "*", "")))) %>%
  dplyr::select(Outcome=lhs, 
                Indicator=rhs,
                ExpT=group,
                B=est, 
                SE=se, Z=z, 
                Beta=std.all, 
                sig=stars) %>% 
  kable(digits = 3, format="pandoc", caption="Free Regression Coefficients")
