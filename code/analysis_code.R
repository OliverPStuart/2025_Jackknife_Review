
# Code for generating figures for the jackknife/rxy paper


############################################
############################################
###                SETUP                 ###
############################################
############################################


# Directories
setwd("/Users/ollie/Documents/UCC_Sync/Manuscripts/2025_Jackknife_Review/")
dir.create("figures",showWarnings = F)

# Libraries
library(ggplot2)
library(magrittr)
library(dplyr)
library(patchwork)

# Functions
# Function to generate bootstrap resample
bootstrap <- function(x,n=NULL){
  
  # Storage vector
  vec <- c()
  
  # If no number of resamples specified, simply use the length of the vector
  if(is.null(n)){n=length(x)}
  
  # Resample the input vector n times with replacement
  for(i in 1:n){
    vec[i] <- mean(x[sample(1:length(x),size=length(x),replace=T)])
  }
  
  return(vec)
  
}

# Function to generate jack-knife resample
jackknife <- function(x){
  
  # Storage vector
  vec <- c()
  
  # Leave-one-out and recalculate statistic
  for(i in 1:length(x)){
    vec[i] <- mean(x[-i])
  }
  
  return(vec)
  
}

# Standard error from jackknife distribution
jackknife_se <- function(x){
  sqrt(((length(x) - 1) / length(x)) * sum((x - mean(x))^2))
}

# Classify MAFs
classify_maf <- function(x){
  if(x <= 0.05){return("absent")} else
    if (x & x < 0.95 ){return("segregating")} else
      if (x >= 0.95 ){return("fixed")}
}


############################################
############################################
###             TOY EXAMPLE              ###
############################################
############################################


# How do bootstrap and jackknife distributions compare?
samples <- rnorm(100,0,1)
p1 <- data.frame(Mean=c(jackknife(samples),
                              bootstrap(samples)),
                       Type=rep(c("Jackknife","Bootstrap"),each=100)) %>%
  ggplot(aes(x=Mean,after_stat(scaled),fill=Type)) +
  geom_density() + 
  scale_y_continuous(limits=c(0,1.05),expand=c(0,0)) +
  coord_cartesian(xlim=c(-0.2,0.2)) + 
  theme_bw() + 
  theme(panel.grid=element_blank(),
        axis.ticks=element_blank()) + 
  labs(y="Scaled density",x="Mean") + 
  scale_fill_manual(values=c("#B679E2","limegreen"))

png("figures/jackknife_v_bootstrap_dist.png",res=300,width=4,height=4,units='in')
p1
dev.off()


# How biased is the jackknife/bootstrap wrt the true SE

# Generate some random samples
Ns <- c(100,200,300,400,500,600,700,800,900,1000)
Reps <- 100

results <- data.frame(Ns=rep(Ns,each=Reps),
           Empirical_SE=NA,
           Bootstrap_SD=NA,
           Jackknife_SD=NA,
           Jackknife_Mean_Bias=NA,
           Bootstrap_Mean_Bias=NA)

for(i in 1:nrow(results)){
  
  N=results$Ns[i]
  if(i %% Reps==1){print(N)}

  # Take sample
  samples <- rnorm(N,mean=0,sd=1)
    
  # Calculate the empirical SE and SDs from bootstrap and jackknife
  results$Empirical_SE[i]=sd(samples)/sqrt(N)
  results$Bootstrap_SD[i]=sd(bootstrap(samples))
  results$Jackknife_SD[i]=sd(jackknife(samples))
  results$Jackknife_SE[i]=jackknife_se(jackknife(samples))

}

# How bad do jackknife and bootstrap do at estimating the standard error?

# The bootstrap simply estimates the SE with precision commensurate to N

p1 <- ggplot(results,aes(x=Ns)) + 
  geom_point(aes(y=Bootstrap_SD/Empirical_SE),position=position_jitter(width=10)) + 
  geom_hline(aes(colour="tmp",yintercept=1),
             linewidth=1.15,linetype="dashed") + 
  theme_bw() +
  coord_cartesian(xlim=c(50,1050),expand=0,
                  ylim=c(0.75,1.25)) + 
  labs(y=bquote(over(SD[Bootstrap], SE[Empirical])),
       x="Sample size") + 
  theme(axis.title.y=element_text(angle=0,vjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(hjust=0),
        legend.position = "inside",
        legend.position.inside = c(0.89, 0.85),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        legend.background = element_rect(colour="black")) + 
  scale_colour_manual(values="#B679E2",
                      breaks="tmp",
                      labels=c(bquote('y' == 1))) + 
  scale_x_continuous(breaks=c(200,400,600,800,1000))

# The jackknife actually underestimates the SE, when treated as a bootstrap

p2 <- ggplot(results,aes(x=Ns)) + 
  geom_point(aes(y=Jackknife_SD/Empirical_SE),position=position_jitter(width=10)) + 
  geom_line(data=data.frame(Ns=1:5000,
                            Underestimation=(1:5000)^-0.5),
            mapping=aes(x=Ns,y=Underestimation,colour="tmp"),
            linewidth=1.15,linetype="dashed") + 
  theme_bw() +
  coord_cartesian(xlim=c(50,1050),expand=0,
                  ylim=c(0.028,0.115)) + 
  labs(y=bquote(over(SD[Jackknife], SE[Empirical])),
       x="Sample size") + 
  theme(axis.title.y=element_text(angle=0,vjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(hjust=0),
        legend.position = "inside",
        legend.position.inside = c(0.89, 0.85),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        legend.background = element_rect(colour="black")) + 
  scale_colour_manual(values="limegreen",
                      breaks="tmp",
                      labels=c(bquote('y' == 'x'^-0.5))) + 
  scale_x_continuous(breaks=c(200,400,600,800,1000))

# However, when calculated using the correct equation, it accurately estimates SE

p3 <- ggplot(results,aes(x=Ns)) + 
  geom_point(aes(y=Jackknife_SE/Empirical_SE),position=position_jitter(width=10)) + 
  geom_hline(aes(colour="tmp",yintercept=1),
             linewidth=1.15,linetype="dashed") + 
  theme_bw() +
  coord_cartesian(xlim=c(50,1050),expand=0,
                  ylim=c(0.5,1.5)) + 
  labs(y=bquote(over(SE[Jackknife], SE[Empirical])),
       x="Sample size") + 
  theme(axis.title.y=element_text(angle=0,vjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(hjust=0),
        legend.position = "inside",
        legend.position.inside = c(0.89, 0.85),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        legend.background = element_rect(colour="black")) + 
  scale_colour_manual(values="cornflowerblue",
                      breaks="tmp",
                      labels=c(bquote('y' == 1))) + 
  scale_x_continuous(breaks=c(200,400,600,800,1000))

png("figures/jackknife_v_bootstrap_se.png",res=300,width=7,height=9,units='in')
p2 + p1 + p3 + plot_layout(ncol=1,axes="collect")
dev.off()


############################################
############################################
###             REAL EXAMPLE             ###
############################################
############################################


# Read in allele frequency data
effects <- read.table("data/allele_frequency_data.txt.gz",header=T)

# Make an ID column for later
effects$ID <- paste0(effects$Scaffold,"_",effects$Position)

# Filter down to the data used for Rxy calculation
rxy <- effects %>% 
  mutate(LXY=MAF_lhisi*(1-MAF_wild),LYX=MAF_wild*(1-MAF_lhisi)) %>%
  filter(!is.na(LXY),!is.na(LYX),LXY!=Inf,LYX!=Inf,LXY!=-Inf,LYX!=-Inf) %>%
  dplyr::select(Scaffold,Position,LXY,LYX,Variant_Effect)

# Order the data.frame by Scaffold and Position
rxy <- rxy[order(rxy$Scaffold, rxy$Position), ]

# Initialise block column
rxy$Block <- NA

# Get scaffolds
scafs <- unique(rxy$Scaffold)

# Total number of blocks desired
# This is what gives us exactly 100 blocks of roughly equal size
total_blocks <- 94

# Total number of entries
total_entries <- nrow(rxy)

# Calculate the rough number of entries per block
rough_block_size <- ceiling(total_entries / total_blocks)

# Loop over scaffolds to define blocks algorithmically
for(i in 1:length(scafs)){
  
  # Get entries
  entries <- rxy[rxy$Scaffold==scafs[i],]
  
  # How many blocks roughly in scaffold?
  blocks_in <- ceiling(nrow(entries)/rough_block_size)
  
  # Make a vector of block assignments
  blocks_assign <- sort(rep(1:blocks_in, length.out=nrow(entries)))
  
  # Add block assignments to data.frame
  # If it's not the first scaffold, add previous values to get correct block ID
  if(i == 1){
    
    rxy$Block[rxy$Scaffold==scafs[i]] <- blocks_assign
    
  } else {
    
    blocks_assign <- blocks_assign + max(rxy$Block[rxy$Scaffold==scafs[i-1]])
    rxy$Block[rxy$Scaffold==scafs[i]] <- blocks_assign
    
  }
}

n_blocks <- max(rxy$Block)

# Now jackknife over the blocks to get Rxy estimates for each variant effect
# Jackknife, blocks
for(i in 1:n_blocks){
  
  # Remove block, calculate RXY, assign to temp data.frame
  assign(paste0("jack_rxy_",i),
         rxy %>% filter(Block != i) %>% 
           group_by(Variant_Effect) %>%
           dplyr::summarise(RXY=sum(LXY)/sum(LYX),Rep=i,Type="Jackknife"))
  
}

# Collate results 
table_names <- ls(pattern = "^jack_rxy_")
table_list <- mget(table_names)
rxy_jack <- do.call(rbind, table_list)
rm(list=ls(pattern = "^jack_rxy_"))

# Do the same but bootstrap
for(i in 1:n_blocks){
  
  if(i %% 10 == 0){print(i)}
  
  # Decide which blocks to select
  blocks <- sort(sample(1:n_blocks,n_blocks,replace=T))
  
  # Now generate block resamples
  for(j in 1:n_blocks){
    assign(paste0("tmp_",j),
           rxy[rxy$Block==blocks[j],])
  }
  
  # Paste into a single data.frame
  table_names <- ls(pattern = "^tmp_")
  table_list <- mget(table_names)
  rxy_tmp <- do.call(rbind, table_list)
  rm(list=ls(pattern = "^tmp_"))
  
  # Calculate boostrapped Rxy for all classes
  assign(paste0("boot_rxy_",i),
         rxy_tmp %>% 
           group_by(Variant_Effect) %>%
           dplyr::summarise(RXY=sum(LXY)/sum(LYX),Rep=i,Type="Bootstrap"))
  
  rm(rxy_tmp)
}

# Collate results 
table_names <- ls(pattern = "^boot_rxy_")
table_list <- mget(table_names)
rxy_boot <- do.call(rbind, table_list)
rm(list=ls(pattern = "^boot_rxy_"))

# Combine them! And refactor for plot ordering
rxy_dat <- rbind(rxy_jack,rxy_boot) %>%
  mutate(Variant_Effect = factor(Variant_Effect,
                                 levels=c("MODIFIER",
                                          "LOW",
                                          "MODERATE",
                                          "HIGH"),
                                 labels=c("MODIFIER",
                                          "LOW",
                                          "MODERATE",
                                          "HIGH")))

# Calculate per-rep per-type the standardised RXY (wrt neutral/modifier alleles)
rxy_dat <- rxy_dat %>%
  group_by(Rep,Type) %>%
  mutate(Order=ifelse(Variant_Effect=="MODIFIER",0,1)) %>%
  arrange(Order) %>%
  mutate(RXY_St=RXY/first(RXY))

# Plot the raw distributions of RXY for the three mutation types
rxy_dists <- ggplot(rxy_dat %>% filter(Variant_Effect != "MODIFIER"),
       aes(x=Variant_Effect,y=RXY_St)) + 
  geom_boxplot(outlier.size = 0.8) + facet_wrap(~Type,nrow=1) +
  scale_x_discrete(labels=c("Low","Moderate","High")) + 
  scale_y_continuous(breaks=c(0.6,0.8,1,1.2),
                     limits=c(0.5,1.22)) + 
  geom_hline(yintercept=1,linewidth=1,linetype="dashed",colour="cornflowerblue") + 
  theme_bw() + 
  theme(axis.ticks=element_blank(),
        panel.grid=element_blank(),
        axis.text.x=element_text(angle=45,hjust=1,vjust=1.1)) + 
  labs(y=expression(R[(Captive/Wild)]),
       x="Predicted impact")
png("figures/jackknife_v_bootstrap_empirical_dist.png",res=300,width=3,height=4,units='in')
rxy_dists
dev.off()

# Now calculate the distribution of differences between each class and 0
rxy_dat <- rxy_dat %>%
  group_by(Rep,Type) %>%
  mutate(Order=ifelse(Variant_Effect=="MODIFIER",0,1)) %>%
  arrange(Order) %>%
  mutate(RXY_Diff=RXY-first(RXY))

# Produce a figure of points and standard errors
rxy_dat_jack <- rxy_dat %>% filter(Variant_Effect != "MODIFIER",
                                   Type=="Jackknife") %>%
  group_by(Variant_Effect) %>%
  dplyr::summarise(Mean=mean(RXY_Diff),
                   SD=sd(RXY_Diff),
                   SE=jackknife_se(RXY_Diff))
rxy_dat_boot <- rxy_dat %>% filter(Variant_Effect != "MODIFIER",
                                   Type=="Bootstrap") %>%
  group_by(Variant_Effect) %>%
  dplyr::summarise(Mean=mean(RXY_Diff),
                   SD=sd(RXY_Diff))

rxy_dat_jack %>%
  ggplot(aes(x=Variant_Effect,Mean)) +
  geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=0) + 
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0,linewidth=1.5) + 
  geom_point(size=2)

rxy_dat_boot %>%
  ggplot(aes(x=Variant_Effect,Mean)) +
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0) + 
  geom_point(size=2)


# Do three sets of tests on differences between deleterious and neutral

# 1. SD of bootstrap, accurate estimate of SE
rxy_dat %>%
  filter(Type=="Bootstrap",Variant_Effect!="MODIFIER") %>%
  group_by(Variant_Effect) %>%
  dplyr::summarise(Mean_RXY=mean(RXY_Diff),
                   SE_RXY=sd(RXY_Diff)) %>%
  mutate(T_Stat=Mean_RXY / SE_RXY,
         P_Value=2 * pt(-abs(T_Stat), df = n_blocks - 1))

# 2. SD of jackknife, inaccurate estimate of SE
rxy_dat %>%
  filter(Type=="Jackknife",Variant_Effect!="MODIFIER") %>%
  group_by(Variant_Effect) %>%
  dplyr::summarise(Mean_RXY=mean(RXY_Diff),
                   SE_RXY=sd(RXY_Diff)) %>%
  mutate(T_Stat=Mean_RXY / SE_RXY,
         P_Value=2 * pt(-abs(T_Stat), df = n_blocks - 1))

# 3. SE of jackknife, accurate estimate of SE
rxy_dat %>%
  filter(Type=="Jackknife",Variant_Effect!="MODIFIER") %>%
  group_by(Variant_Effect) %>%
  dplyr::summarise(Mean_RXY=mean(RXY_Diff),
                   SE_RXY=jackknife_se(RXY_Diff)) %>%
  mutate(T_Stat=Mean_RXY / SE_RXY,
         P_Value=2 * pt(-abs(T_Stat), df = n_blocks - 1))





