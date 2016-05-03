######################################################
#  file: ensemble_stats_boxplot.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - boxplots of ensemble NINO3 stats in R
#       - load in multiple CESM model NINO3 stats
#       - plot ensemble boxplots of those stats
#  Run in R (open R w/ >R on command line)
#  >source("ensemble_stats_boxplot.R")
######################################################

library("stats")
library("biwavelet") #for morlet wavelet analysis
library("moments") #for skewness calculation
library("Hmisc")
## load in the .txt monthly ENSO temperature data
## columns are time,nino3_sst,nino3_climate_anom,
## nino3_smoothed_anom,sst_longterm_rm,nino3_longterm_rm

####Input parameters
time_interval = 50 #either 100 or 50 year intervals
stat_choice = "Skewness"
## stat_choice - plot hist of Variance,STD,Skewness,NinoCount,or NinaCount
region = "nino34" #choices are nino34 and nino3
main_string = "Standard deviation"
#axis_string = expression(paste("variance (",""*degree,"C"^2,")",sep=""))
axis_string = expression(paste("standard deviation (",""*degree,"C)",sep=""))
#axis_string="count"
####

if (time_interval == 50){
time_periods = c("2575","19401990","20402090")
time_strings = c("control","1940-1990","2040-2090")
start_years=c(25.,1940.,2040.)
end_years=c(75.,1990.,2090.)
forced_runs = c("42","43","44","45","46","47",
        "48","49","50","51","52","53","54","55",
        "56","57","58","59","60","61","62","63",
        "64","65","66","67","68","69","70","71",
        "72","73","74","75","76","77","78","79",
        "80","81","82","83","84","85","86","87",
        "88","89","90","91")}

if (time_interval == 100){
time_periods = c("0100","18901990","19902090")
time_strings = c("control","1890-1990","1990-2090")
start_years=c(0.,1890.,1990.)
end_years=c(100.,1990.,2090.)
forced_runs = c("43","44","45","46","47",
        "48","49","50","51","52","53","54","55",
        "56","57","58","59","60","61","62","63",
        "64","65","66","67","68","69","70","71",
        "72","73","74","75","76","77","78","79",
        "80","81","82","83","84","85","86","87",
        "88","89","90")}

time_periods = time_periods[c(1,2,3)]
colors=c("red",rgb(0.7,0.3,0),rgb(0,1,0,0.5),rgb(0,0.7,0.3),rgb(0,0,1,0.5),rgb(0.3,0,0.7))
colors=colors[c(1,2,3,4,5,6)]

stat_list = list()
for (i in 1:length(time_periods)){

#set the time period for which we calc the stat
time_period = time_periods[i]
start_year = start_years[i]
end_year = end_years[i]
print(time_period)

for (j in 1:length(forced_runs)){

model_run = forced_runs[j]
#print(model_run)
if (i==1){
nino3_file = paste("../model_time_series/control",model_run,"_",region,"_",toString(start_year),toString(end_year),".txt",sep="")
}
else{nino3_file=paste("../model_time_series/model",model_run,"_",region,"_",toString(start_year),toString(end_year),".txt",sep="")
}
nino3_data = read.table(nino3_file,header=TRUE)
#attach(nino3_data)

if (region=="nino34"){region_longterm_rm = nino3_data$nino34_longterm_rm}
if (region=="nino3"){region_longterm_rm = nino3_data$nino3_longterm_rm}

used_indices = which(nino3_data$time>start_year & nino3_data$time<end_year+0.001)
if(stat_choice == "Variance"){
stat_val=var(region_longterm_rm[used_indices])}
if(stat_choice == "STD"){
stat_val=sd(region_longterm_rm[used_indices])}
if(stat_choice == "Skewness"){
stat_val=skewness(region_longterm_rm[used_indices])}
if(stat_choice == "NinoCount"){   #frequency of El Nino events
elnino_counter=0
three_mon_temp = numeric(length(used_indices))
for(k in used_indices){three_mon_temp[(k-used_indices[1]+1)] = mean(region_longterm_rm[(k-1):(k+1)])}
three_mon_temp = three_mon_temp - mean(three_mon_temp)
for(k in 2:(length(used_indices)-5)){
    if(three_mon_temp[k] > 0.5 & three_mon_temp[k-1] < 0.5 & three_mon_temp[k+1] >0.5
	& three_mon_temp[k+2] > 0.5 & three_mon_temp[k+3] > 0.5 & three_mon_temp[k+4]>0.5){
	elnino_counter=elnino_counter+1}}
stat_val=elnino_counter}
if(stat_choice =="NinaCount"){#frequency of La Nina events
lanina_counter=0
three_mon_temp = numeric(length(used_indices))
for(k in used_indices){three_mon_temp[(k-used_indices[1]+1)] = mean(region_longterm_rm[(k-1):(k+1)])}
three_mon_temp = three_mon_temp - mean(three_mon_temp)
for(k in 2:(length(used_indices)-5)){
    if(three_mon_temp[k] < -0.5 & three_mon_temp[k-1] > -0.5 & three_mon_temp[k+1] < -0.5
        & three_mon_temp[k+2] < -0.5 & three_mon_temp[k+3] < -0.5 & three_mon_temp[k+4] < -0.5){
        lanina_counter=lanina_counter+1}}
stat_val=lanina_counter}
 
	
#print(stat_val)
#detach(nino3_data)

if(j==1){
    stat_array = c(stat_val)
}
else{
    stat_array = append(stat_array,stat_val)
}

if(j==length(forced_runs)){
stat_list = c(stat_list,list(stat_array))

if(i==2){
##Calculate value of statistic for the ERSST data
ersst_time = time_periods[2]
ersst_file = paste("../model_time_series/ersst_",region,"_",ersst_time,".txt",sep="")
ersst_data = read.table(ersst_file,header=TRUE)
if (region=="nino34"){region_longterm_rm = ersst_data$nino34_longterm_rm}
if (region=="nino3"){region_longterm_rm = ersst_data$nino3_longterm_rm}

used_indices = which(ersst_data$time>start_year & ersst_data$time<end_year+0.001)
if(stat_choice == "Variance"){
ersst_val=var(region_longterm_rm[used_indices])}
if(stat_choice == "STD"){
ersst_val=sd(region_longterm_rm[used_indices])}
if(stat_choice == "Skewness"){
ersst_val=skewness(region_longterm_rm[used_indices])}
if(stat_choice == "NinoCount"){   #frequency of El Nino events
elnino_counter=0
three_mon_temp = numeric(length(used_indices))
for(k in used_indices){three_mon_temp[(k-used_indices[1]+1)] = mean(region_longterm_rm[(k-1):(k+1)])}
three_mon_temp = three_mon_temp - mean(three_mon_temp)
for(k in 2:(length(used_indices)-5)){
    if(three_mon_temp[k] > 0.5 & three_mon_temp[k-1] < 0.5 & three_mon_temp[k+1] >0.5 
	& three_mon_temp[k+2] > 0.5 & three_mon_temp[k+3] > 0.5 & three_mon_temp[k+4]>0.5){
        elnino_counter=elnino_counter+1}}
ersst_val=elnino_counter}
if(stat_choice =="NinaCount"){#frequency of La Nina events
lanina_counter=0
three_mon_temp = numeric(length(used_indices))
for(k in used_indices){three_mon_temp[(k-used_indices[1]+1)] = mean(region_longterm_rm[(k-1):(k+1)])}
three_mon_temp = three_mon_temp - mean(three_mon_temp)
for(k in 2:(length(used_indices)-5)){
    if(three_mon_temp[k] < -0.5 & three_mon_temp[k-1] > -0.5 & three_mon_temp[k+1] < -0.5 
	& three_mon_temp[k+2] < -0.5 & three_mon_temp[k+3] < -0.5 & three_mon_temp[k+4] < -0.5){
        lanina_counter=lanina_counter+1}}
ersst_val=lanina_counter}
}

if(i==length(time_periods)){

if (stat_choice == "Variance"){
    used_ylim = c(0.35,0.65)}
else if(stat_choice == "STD"){
    used_ylim = c(0.55,0.85)}
else if(stat_choice == "Skewness"){
    used_ylim = c(-0.3,0.9)}
else if((stat_choice == "NinoCount") & (time_interval == 50.)){used_ylim=c(8,17)}
else if((stat_choice == "NinaCount") & (time_interval == 50.)){used_ylim=c(8,17)}
else if((stat_choice == "NinoCount") & (time_interval == 100.)){used_ylim=c(17,30)}
else if((stat_choice == "NinaCount") & (time_interval == 100.)){used_ylim=c(17,30)}

pdf(paste(stat_choice,"_",region,"_",time_interval,"yr_boxwhiskers.pdf",sep=""))
par(mar=c(5,6,4,2)+0.1)
box_stuff = boxplot(stat_list,notch=TRUE,plot=FALSE,
    names=time_strings)
box_stuff$stats[,1] = quantile(stat_list[[1]],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
box_stuff$stats[,2] = quantile(stat_list[[2]],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
box_stuff$stats[,3] = quantile(stat_list[[3]],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
bxp(box_stuff,notch=TRUE,outline=FALSE,main=main_string,
    boxwex=0.5,cex.axis=1.5,cex.main=2.0,cex.lab=1.5,cex.sub=1.5,
    boxlwd="3",medlwd="3",whisklwd="3",staplelwd="3",outlwd="3",yaxt="n",ylim=used_ylim)
axis(2,cex.axis=1.5,las=2)
mtext(axis_string,cex=1.5,side=2,line=4)
minor.tick(nx=0,ny=2)
points(c(2),ersst_val,cex=2.0,lwd=4,pch=4,type="o",col="red")

#NOW t-tests, 95% or 99%, two-sided, also calc sensitivity (required % change in mean for detection)
c_i = 95
if(c_i==95){t_req = 1.9845} #assuming dof ~98
if(c_i==99){t_req = 2.6270} #assuming dof ~98

test1 = t.test(stat_list[[1]],stat_list[[2]],mu=0,
	conf.level=c_i/100.,var.equal=FALSE,alternative="two.sided")
test1_sensitivity = (1./mean(stat_list[[1]]))*t_req*(sd(stat_list[[1]])^2/length(forced_runs) +
	sd(stat_list[[2]])^2/length(forced_runs))^0.5

test2 = t.test(stat_list[[1]],stat_list[[3]],mu=0,
	conf.level=c_i/100.,var.equal=FALSE,alternative="two.sided")
test2_sensitivity = (1./mean(stat_list[[1]]))*t_req*(sd(stat_list[[1]])^2/length(forced_runs) +
        sd(stat_list[[3]])^2/length(forced_runs))^0.5

test3 = t.test(stat_list[[2]],stat_list[[3]],mu=0,
        conf.level=c_i/100.,var.equal=FALSE,alternative="two.sided")
test3_sensitivity = (1./mean(stat_list[[2]]))*t_req*(sd(stat_list[[2]])^2/length(forced_runs) +
        sd(stat_list[[3]])^2/length(forced_runs))^0.5

print(paste(region," ",stat_choice," t-test for control and ",time_strings[2],sep=""))
print(test1)
print(paste("change/fractional change that would be detected at ",c_i,"% confidence: ",
	test1_sensitivity*mean(stat_list[[1]]),"/",test1_sensitivity,sep=""))
print(paste(region," ",stat_choice," t-test for control and ",time_strings[3],sep=""))
print(test2)
print(paste("change/fractional change that would be detected at ",c_i,"% confidence: ",
	test2_sensitivity*mean(stat_list[[1]]),"/",test2_sensitivity,sep=""))
print(paste(region," ",stat_choice," t-test for ",time_strings[2]," and ",time_strings[3],sep=""))
print(test3)
print(paste("change/fractional change that would be detected at ",c_i,"% confidence: ",
	test3_sensitivity*mean(stat_list[[2]]),"/",test3_sensitivity,sep=""))

#pdf(paste(stat_choice,"_",region,"_ttestplot.pdf",sep=""))

x=seq(-4,4,length=1000)
degf=length(stat_list[[1]])+length(stat_list[[2]])
#plot(x,dt(x,df=degf),type="l",lwd=5)
#    abline(v=test1$statistic,col="red",lwd=5)
#    abline(v=qt(0.05,df=degf),lwd=2,lty="dashed")
#    abline(v=qt(0.95,df=degf),lwd=3,lty="dashed")
}}
}}

####Save boxplot for multiplot
save(box_stuff,ersst_val,file=paste(stat_choice,"_boxplot.RData",sep=""))

dev.off()


###########################

