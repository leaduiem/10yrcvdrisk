#Data available at https://github.com/leaduiem/10yrcvdrisk
#Please, address all correspondence about this code to adrian.sotom@incmnsz.mx

# Working directory setup. Make sure you adapt this to your computer.
setwd("C:/Users/adria/Dropbox/UIEM/LEAD/Proyectos/Laitram")

# Ensuring reproducibility of this script.
# Since "easystats" is currently not available in CRAN, if you don't have it,
# you'll need to install manually.
# install.packages("easystats", repos = "https://easystats.r-universe.dev")

# Now, confirm you have "pacman" installed. If you don't have "pacman" but want
# to install it, remove the # in the line below and press "Enter".
# install.packages("pacman") 

#Packages setup
pacman::p_load(dplyr,tidyr,ggstatsplot,readxl,tableone,easystats,
               patchwork,MASS,see,qqplotr,bootStepAIC,performance,
               rpart,rpart.plot,gtools,broom,lmtest,visdat,report,
               parameters,ggcharts,conflicted,car,rattle,cvms,
               mlogit,MLmetrics,beepr)

#Solving duplicate functions conflicts
conflict_prefer("select","dplyr")
conflict_prefer("filter", "dplyr")

#Data upload
data <- read_excel("data.xlsx")
names <- names(data)

#Descriptive statistics
CreateTableOne(data=data,strata="visit",
               vars = c("age","bmi","fglu","crp","aic","tc","hdl","ldl","tg",
                        "sbp","dbp","alt","ast","ins","homair","tgtohdl",
                        "cvd10yr","absrdiff","rrr"),)

# Plots
bmi <- ggwithinstats(
  data = data,
  x = visit,
  y = bmi,
  type = "np",
  ylab = "BMI",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "BMI accross visits"
)
bmi

aic <- ggwithinstats(
  data = data,
  x = visit,
  y = aic,
  type = "np",
  ylab = "A1c (%)",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "HbA1c accross visits"
)
aic

crp <- ggwithinstats(
  data = data,
  x = visit,
  y = crp,
  type = "np",
  ylab = "C-reactive protein (mg/dL)",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "C-reactive protein accross visits"
)
crp

tc <- ggwithinstats(
  data = data,
  x = visit,
  y = tc,
  type = "np",
  ylab = "Total cholesterol (mg/dL)",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "Total Cholesterol accross visits"
)
tc

hdl <- ggwithinstats(
  data = data,
  x = visit,
  y = hdl,
  type = "np",
  ylab = "HDL (mg/dL)",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "HDL accross visits"
)
hdl

sbp <- ggwithinstats(
  data = data,
  x = visit,
  y = sbp,
  type = "np",
  ylab = "Systolic Blood Pressure (mmHg)",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "Systolic Blood Pressure accross visits"
)
sbp

dbp <- ggwithinstats(
  data = data,
  x = visit,
  y = dbp,
  type = "np",
  ylab = "Diastolic Blood Pressure (mmHg)",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "Diastolic Blood Pressure accross visits"
)
dbp

absdiff <- ggwithinstats(
  data = data,
  x = visit,
  y = cvd10yr,
  type = "np",
  ylab = "AHA 10 year Cardiovascular Risk Absolute Reduction (%)",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "AHA 10 year Cardiovascular Risk Absolute Reduction"
)
absdiff

rrr <- ggwithinstats(
  data = data,
  x = visit,
  y = rrr,
  type = "np",
  ylab = "AHA 10 year Cardiovascular Risk Relative Reduction (%)",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "AHA 10 year Cardiovascular Risk Relative Reduction"
)
rrr

homair <- ggwithinstats(
  data = data,
  x = visit,
  y = homair,
  type = "np",
  ylab = "HOMA-IR",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "HOMA-IR across visits"
)
homair

tgtohdl <- ggwithinstats(
  data = data,
  x = visit,
  y = tgtohdl,
  type = "np",
  ylab = "TG/HDL",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "TG/HDL across visits"
)
tgtohdl

fgluc <- ggwithinstats(
  data = data,
  x = visit,
  y = tg,
  type = "np",
  ylab = "TG/HDL",
  effsize.type = "biased",
  pairwise.comparisons = TRUE,
  outlier.tagging = TRUE,
  outlier.coef = 1.5,
  outlier.label.color = "red",
  mean.plotting = TRUE,
  mean.color = "darkblue",
  package = "yarrr",
  palette = "info2",
  title = "TG/HDL across visits"
)
fgluc


# Saving plots into your computer
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Users/adria/Dropbox/UIEM/LEAD/Proyectos/Laitram")

