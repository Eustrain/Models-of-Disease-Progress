rm(list = ls())
#######################################################
#######################################################

evaluacion<- c(1:10)
porcentaje_pe <- c(0,4,9,15,20,30,35,38,39,39)
porpocion_e <- porcentaje_pe/100

cuadro_3 <- data.frame(evaluacion,porcentaje_pe,porpocion_e)


model_fitting<- function(data, time,value, modelo){
require(epiR)
  require(ggplot2)
df <- data
df0<- df %>% select(t={{time}},y={{value}}) %>%  # transform to proportion
  mutate(exponential = log(y),
         monomolecular = log(1 / (1 - y)),
         logistic = log(y / (1 - y)),
         gompertz = -log(-log(y))) %>% 
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))
  #### models
  modelo_mono <- lm(monomolecular ~ t, data = df0) 
  mono_lin <- epi.ccc(df0$monomolecular,predict(modelo_mono))
  modelo_exp <- lm(exponential~t, data = df0)
  exp_lin <- epi.ccc(df0$exponential, predict(modelo_exp))
  modelo_log <- lm(logistic~t, data = df0)
  log_lin <- epi.ccc(df0$logistic, predict(modelo_log))
  modelo_gomp <- lm(gompertz~t, data = df0)
  gomp_lin <- epi.ccc(df0$gompertz, predict(modelo_gomp)) 
###############################################
#### data_frame  of models evaluations  ######
table_lin <- c((mono_lin$rho.c[1]),exp_lin$rho.c[1],log_lin$rho.c[1],gomp_lin$rho.c[1])
table_lin <- as.character.numeric_version(table_lin)
table_se <- c(summary(modelo_mono)$sigma,summary(modelo_exp)$sigma,
              summary(modelo_log)$sigma,summary(modelo_gomp)$sigma)
models <- c("monomolecular","exponential","logistic","gompertz")
data_evals <- data.frame("Model"=models,"Lins"=table_lin,"RSE"=table_se)  
 ############################################################################3
##############################################################################
######## calculating of the valuos rm and Y0

##### monomolecular
rm <-   modelo_mono$coefficients[2] ## 
y0m <-  modelo_mono$coefficients[1]  ## 
y0M<- 1-exp(-y0m) ####
#####expo
re <- modelo_exp$coefficients[2]
y0e <- modelo_exp$coefficients[1]
y0E <- exp(y0e)
####logistic
rl <- modelo_log$coefficients[2]
y0l <- modelo_log$coefficients[1]
y0L <- 1 / (1 + exp(-y0l))
#### Gompertz
rg <- modelo_gomp$coefficients[2]
y0g <- modelo_gomp$coefficients[1]
y0G <- exp(-exp(-y0g))
###############################################################################
table_r <- c(rm,re,rl,rg)
table_y <- c(y0M,y0E,y0L,y0G)

#* monomolecular
mon <- function(t) 1 - ((1 - y0M) * exp(-rm * t))
# Exponecial
exps <- function(t) y0E * exp(re * t)
#* Logistic
logs <- function(t) 1 / (1 + ((1 - y0L) / y0L) * exp(-rl * t))
#*Gompe
gomps <- function(t) exp(log(y0G) * exp(-rg * t))
 #### Table with value predicted
data_base<- df0 %>% select(t, y) %>% 
  mutate(exponential = exps(t),
         monomolecular = mon(t),
         logistic = logs(t),
         gompertz = gomps(t))

##############################################################################
### out 

table_values <- data.frame("Model"=models,"y0"=table_y,"r"=table_r)
out <- list("data" = df0, "Lins"=data_evals, "values"=table_values,"predicted"=data_base)

print(out)
          
 }


example<- enfermedades(data = cuadro_3,evaluacion,porpocion_e)


### this are the  example data
y0 <- example$values[1,2]
r <- example$values[1,3]
#* monomolecular
mon <- function(t) 1 - ((1 - y0) * exp(-r * t))


 
t2 <- seq(0,10,1)
nw <- data.frame(t=t2, y = mon(t2))

ggplot(data = nw, aes(x =t, y = y, group= 1))+
  geom_line()+
  geom_point(data = example$data,aes(x = t, y =y))+
  scale_y_continuous(limits = c(0,1))

