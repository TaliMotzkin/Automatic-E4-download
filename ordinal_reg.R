# Load libraries
library(readxl)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(brant)
library(gvlma)
library(zoo)

#install.packages("pwr")
library(pwr)

#install.packages("ordinal")
library(ordinal)

#Read Excell and take out missing data
df = read_excel("new_notes_8.xlsx", sheet = "cleaner")
df <- df[complete.cases(df$Preference),]


############################ Define the Function (by the algorithm shown in notebook)####################

# Create an empty dictionary
group_dictio <- list()

# Add key-value pairs to the dictionary
# For the cleaner2 version (cleaning the low PET subjects below 30, subject id numbers: 3,16,17,91,92,93,94,95,107,108,109,110,113,114,115,116,117,118,121,122,123,277,278,295,296,)
# The amount of subjects in each group is: 70 (comfort), 94(Sensation), 117(Preference)
# For the cleaner (full subject group) is 72 (comfort), 79 (Sensation), 106(Preference)


group_dictio$Comfort_group <- 70
#group_dictio$Comfort_group <- 72
group_dictio$Sensation_group <- 94
#group_dictio$Sensation_group <- 79
group_dictio$Preference_group <- 117
#group_dictio$Preference_group <- 106

# Create list of values to proportions of columns
veg_propotions <- list(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)


df_dict <- list()

#The function for finding the odds ratio to each group
# I had to change the comfort_group\Preference_group manually because it 
#Didn't work as a variable... so there is three of those..
group_func_comf <- function(group_dictio, veg_propotions, df, df_dict) {
  
  # Iterate over proportions of the back view
  for (prop in veg_propotions) {
  
    #Set what we will want to see in the final answer
    # Start and End is in which % of vegetation the group is framed
    df_dict[[as.character(prop)]] <- data.frame(start = numeric(),
                                                end = numeric(),
                                                spearman = numeric(),
                                                p_spearman = numeric(),
                                                ordinal = numeric(),
                                                ordinal_way = numeric(),
                                                p_ordinal = numeric(),
                                                avg_PET = numeric(),
                                                avg_measure = numeric(),
                                                avg_dur = numeric(),
                                                stringsAsFactors = FALSE)
    
    #Calculating the new vegetation percentage of all subjects by the proportions
    ##Change here manually to the right thermal answer
    temp_df <- df[c("Vegetation_Pixels", "Other_Vegetation_Pixel", "Comfort_group", "PET","Duration_Before_Ex")]
    temp_df$new_veg <- ((temp_df$"Vegetation_Pixels" + prop * temp_df$"Other_Vegetation_Pixel")*100)/(1424767 + prop * 1424767)
    #Order the data by the new veg%
    temp_df <- temp_df[order(temp_df$new_veg), ]
    
    #Take unique veg values so we can iterate them
    list_of_veg <- unique(temp_df$new_veg)
    
    #For each of the new veg%:
    for (i in seq_along(list_of_veg)) {
      veg <- list_of_veg[i]
      
      if (veg == tail(list_of_veg, 1)) {
        break
      }
      
      #See where to start and cut the right data group by the group_dictio
      ##Change here manually to the right thermal answer
      start_index <- which(temp_df$new_veg == veg)[1]
      subset_df <- temp_df[start_index:(start_index + group_dictio[["Comfort_group"]] - 1), ]
      
      if (is.na(tail(subset_df$new_veg, 1))) {
        break
      }
      
      #Ordinal regression calculation
      ##Change here manually to the right thermal answer
      m = polr(as.factor(Comfort_group) ~ PET , data = subset_df, Hess=TRUE)
      odds_prob = brant(m)[6]
      (table <- coef(summary(m)))
      p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2
      pordinal = p[1]
      
      #If the ordinal regression isn't significant or not fulfilling assumptions - add 1 more subject to the veg group
      while ((odds_prob < 0.05 || pordinal > 0.05)) {
        if (start_index == nrow(temp_df) -10){
          break
        }
        start_index <- start_index + 1
        ##Change here manually to the right thermal answer twice!!
        subset_df <- temp_df[start_index:(start_index + group_dictio[["Comfort_group"]] - 1), ]
        m = polr(as.factor(Comfort_group) ~ PET , data = subset_df, Hess=TRUE)
        odds_prob = brant(m)[6]
        (table <- coef(summary(m)))
        p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2
        pordinal = p[1]
      }
      
      if (start_index == nrow(temp_df) - 10) {
        break
      } else {
        # See if the odds ratio are down scaling or up scaling
        if (exp(coef(m)) <= 1) {
          x <- (1 - exp(coef(m))) * 100
          y <- -1
        } else {
          x <- (exp(coef(m)) - 1) * 100
          y <- 1
        }
        
        #Calculate also Spearman correlation
        ##Change here manually to the right thermal answer
        correlation <- cor.test(subset_df$Comfort_group, subset_df$PET, method = "spearman")
        if (is.na(tail(subset_df$new_veg, 1))) {
          break
        }
        #Add the veg group to the final dictionary
        df_dict[[as.character(prop)]] <- rbind(df_dict[[as.character(prop)]], 
                                               data.frame(start = veg,
                                                          end = tail(subset_df$new_veg, 1),
                                                          spearman = correlation$estimate,
                                                          p_spearman = correlation$p.value,
                                                          ordinal =x,
                                                          ordinal_way = y,
                                                          p_ordinal = pordinal,
                                                          avg_PET = mean(subset_df$PET),
                                                          ##Change here manually to the right thermal answer
                                                          avg_measure = mean(subset_df$Comfort_group),
                                                          avg_dur = mean(subset_df$Duration_Before_Ex),
                                                          stringsAsFactors = FALSE))
        
      }
    }
  }
  return(df_dict)
}
group_func_pref <- function(group_dictio, veg_propotions, df, df_dict) {
  
  # Iterate over proportions of the back view
  for (prop in veg_propotions) {
    
    #Set what we will want to see in the final answer
    # Start and End is in which % of vegetation the group is framed
    df_dict[[as.character(prop)]] <- data.frame(start = numeric(),
                                                end = numeric(),
                                                spearman = numeric(),
                                                p_spearman = numeric(),
                                                ordinal = numeric(),
                                                ordinal_way = numeric(),
                                                p_ordinal = numeric(),
                                                avg_PET = numeric(),
                                                avg_measure = numeric(),
                                                avg_dur = numeric(),
                                                stringsAsFactors = FALSE)
    
    #Calculating the new vegetation percentage of all subjects by the proportions
    ##Change here manually to the right thermal answer
    temp_df <- df[c("Vegetation_Pixels", "Other_Vegetation_Pixel", "Preference_group", "PET","Duration_Before_Ex")]
    temp_df$new_veg <- ((temp_df$"Vegetation_Pixels" + prop * temp_df$"Other_Vegetation_Pixel")*100)/(1424767 + prop * 1424767)
    #Order the data by the new veg%
    temp_df <- temp_df[order(temp_df$new_veg), ]
    
    #Take unique veg values so we can iterate them
    list_of_veg <- unique(temp_df$new_veg)
    
    #For each of the new veg%:
    for (i in seq_along(list_of_veg)) {
      veg <- list_of_veg[i]
      
      if (veg == tail(list_of_veg, 1)) {
        break
      }
      
      #See where to start and cut the right data group by the group_dictio
      ##Change here manually to the right thermal answer
      start_index <- which(temp_df$new_veg == veg)[1]
      subset_df <- temp_df[start_index:(start_index + group_dictio[["Preference_group"]] - 1), ]
      
      if (is.na(tail(subset_df$new_veg, 1))) {
        break
      }
      
      #Ordinal regression calculation
      ##Change here manually to the right thermal answer
      m = polr(as.factor(Preference_group) ~ PET , data = subset_df, Hess=TRUE)
      odds_prob = brant(m)[6]
      (table <- coef(summary(m)))
      p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2
      pordinal = p[1]
      
      #If the ordinal regression isn't significant or not fulfilling assumptions - add 1 more subject to the veg group
      while ((odds_prob < 0.05 || pordinal > 0.05)) {
        if (start_index == nrow(temp_df) -10){
          break
        }
        start_index <- start_index + 1
        ##Change here manually to the right thermal answer twice!!
        subset_df <- temp_df[start_index:(start_index + group_dictio[["Preference_group"]] - 1), ]
        m = polr(as.factor(Preference_group) ~ PET , data = subset_df, Hess=TRUE)
        odds_prob = brant(m)[6]
        (table <- coef(summary(m)))
        p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2
        pordinal = p[1]
      }
      
      if (start_index == nrow(temp_df) - 10) {
        break
      } else {
        # See if the odds ratio are down scaling or up scaling
        if (exp(coef(m)) <= 1) {
          x <- (1 - exp(coef(m))) * 100
          y <- -1
        } else {
          x <- (exp(coef(m)) - 1) * 100
          y <- 1
        }
        
        #Calculate also Spearman correlation
        ##Change here manually to the right thermal answer
        correlation <- cor.test(subset_df$Preference_group, subset_df$PET, method = "spearman")
        if (is.na(tail(subset_df$new_veg, 1))) {
          break
        }
        #Add the veg group to the final dictionary
        df_dict[[as.character(prop)]] <- rbind(df_dict[[as.character(prop)]], 
                                               data.frame(start = veg,
                                                          end = tail(subset_df$new_veg, 1),
                                                          spearman = correlation$estimate,
                                                          p_spearman = correlation$p.value,
                                                          ordinal =x,
                                                          ordinal_way = y,
                                                          p_ordinal = pordinal,
                                                          avg_PET = mean(subset_df$PET),
                                                          ##Change here manually to the right thermal answer
                                                          avg_measure = mean(subset_df$Preference_group),
                                                          avg_dur = mean(subset_df$Duration_Before_Ex),
                                                          stringsAsFactors = FALSE))
        
      }
    }
  }
  return(df_dict)
}
group_func_sens <- function(group_dictio, veg_propotions, df, df_dict) {
  
  # Iterate over proportions of the back view
  for (prop in veg_propotions) {
    
    #Set what we will want to see in the final answer
    # Start and End is in which % of vegetation the group is framed
    df_dict[[as.character(prop)]] <- data.frame(start = numeric(),
                                                end = numeric(),
                                                spearman = numeric(),
                                                p_spearman = numeric(),
                                                ordinal = numeric(),
                                                ordinal_way = numeric(),
                                                p_ordinal = numeric(),
                                                avg_PET = numeric(),
                                                avg_measure = numeric(),
                                                avg_dur = numeric(),
                                                stringsAsFactors = FALSE)
    
    #Calculating the new vegetation percentage of all subjects by the proportions
    ##Change here manually to the right thermal answer
    temp_df <- df[c("Vegetation_Pixels", "Other_Vegetation_Pixel", "Sensation_group", "PET","Duration_Before_Ex")]
    temp_df$new_veg <- ((temp_df$"Vegetation_Pixels" + prop * temp_df$"Other_Vegetation_Pixel")*100)/(1424767 + prop * 1424767)
    #Order the data by the new veg%
    temp_df <- temp_df[order(temp_df$new_veg), ]
    
    #Take unique veg values so we can iterate them
    list_of_veg <- unique(temp_df$new_veg)
    
    #For each of the new veg%:
    for (i in seq_along(list_of_veg)) {
      veg <- list_of_veg[i]
      
      if (veg == tail(list_of_veg, 1)) {
        break
      }
      
      #See where to start and cut the right data group by the group_dictio
      ##Change here manually to the right thermal answer
      start_index <- which(temp_df$new_veg == veg)[1]
      subset_df <- temp_df[start_index:(start_index + group_dictio[["Sensation_group"]] - 1), ]
      
      if (is.na(tail(subset_df$new_veg, 1))) {
        break
      }
      
      #Ordinal regression calculation
      ##Change here manually to the right thermal answer
      m = polr(as.factor(Sensation_group) ~ PET , data = subset_df, Hess=TRUE)
      odds_prob = brant(m)[6]
      (table <- coef(summary(m)))
      p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2
      pordinal = p[1]
      
      #If the ordinal regression isn't significant or not fulfilling assumptions - add 1 more subject to the veg group
      while ((odds_prob < 0.05 || pordinal > 0.05)) {
        if (start_index == nrow(temp_df) -10){
          break
        }
        start_index <- start_index + 1
        ##Change here manually to the right thermal answer twice!!
        subset_df <- temp_df[start_index:(start_index + group_dictio[["Sensation_group"]] - 1), ]
        m = polr(as.factor(Sensation_group) ~ PET , data = subset_df, Hess=TRUE)
        odds_prob = brant(m)[6]
        (table <- coef(summary(m)))
        p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2
        pordinal = p[1]
      }
      
      if (start_index == nrow(temp_df) - 10) {
        break
      } else {
        # See if the odds ratio are down scaling or up scaling
        if (exp(coef(m)) <= 1) {
          x <- (1 - exp(coef(m))) * 100
          y <- -1
        } else {
          x <- (exp(coef(m)) - 1) * 100
          y <- 1
        }
        
        #Calculate also Spearman correlation
        ##Change here manually to the right thermal answer
        correlation <- cor.test(subset_df$Sensation_group, subset_df$PET, method = "spearman")
        if (is.na(tail(subset_df$new_veg, 1))) {
          break
        }
        #Add the veg group to the final dictionary
        df_dict[[as.character(prop)]] <- rbind(df_dict[[as.character(prop)]], 
                                               data.frame(start = veg,
                                                          end = tail(subset_df$new_veg, 1),
                                                          spearman = correlation$estimate,
                                                          p_spearman = correlation$p.value,
                                                          ordinal =x,
                                                          ordinal_way = y,
                                                          p_ordinal = pordinal,
                                                          avg_PET = mean(subset_df$PET),
                                                          ##Change here manually to the right thermal answer
                                                          avg_measure = mean(subset_df$Sensation_group),
                                                          avg_dur = mean(subset_df$Duration_Before_Ex),
                                                          stringsAsFactors = FALSE))
        
      }
    }
  }
  return(df_dict)
}


####Run the function####
#First we run it on the entire comfort data (without NA's) when the group size was set to 72
comfort_res1 = group_func_comf(group_dictio, veg_propotions, df, df_dict)

####Plot the Answer####
#Set the data
p <- ggplot(comfort_res1[["0.5"]], aes(x = (start+end)/2, y = ordinal) )+
  labs(x = "Vegetation (%)", y = "Odds Ratio") 
#Add horizontal segments that coloured by the mean PET
p <- p + 
  geom_segment(data = comfort_res1[["0.5"]], aes(x = start, xend = end, y = ordinal, yend = ordinal, colour =avg_PET),  lwd = 2, alpha = 0.5)+ 
  scale_color_gradientn(colours = c( "#1338BE", "#0492C2",  "#63C5DA", "#F2FE1E", "#FBD412", "#FFA612", "#FE230A", "#E600A9"),
                        values = c(0,0.1,0.15, 0.2,0.25, 0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.8, 0.85, 0.9,0.95, 1)) 
#Add a trend line (smoothed by rollmean)
p <- p + theme_bw()+
  geom_line(aes(y = rollmean(comfort_res1[["0.5"]]$ordinal, 5, na.pad = TRUE, align = "center")), linewidth = 1)
print(p)

### Running this to Preference 106 and Sensation 79 will also have weird blue decline
### At the begging of the graph (low veg groups), therefore we removed the subjects below PET 30 and put the new data in cleaner2


####Cleaner2####

#Read Excell and take out missing data
df2 = read_excel("new_notes_8.xlsx", sheet = "cleaner2")
df2 <- df2[complete.cases(df2$Preference),]

# Create an empty dictionary
group_dictio <- list()
#Define the group size in df2
group_dictio$Comfort_group <- 70
group_dictio$Sensation_group <- 94
group_dictio$Preference_group <- 117

# Run the three functions, remember to change to the right thermal
#answer in the function definition 

comfort_res2 = group_func_comf(group_dictio, veg_propotions, df2, df_dict)
preference_res2 = group_func_pref(group_dictio, veg_propotions, df2, df_dict)
sensation_res2 = group_func_sens(group_dictio, veg_propotions, df2, df_dict)

#plot the data (remember to change here too the thermal answer)
#Set the data
p <- ggplot(sensation_res2[["0.5"]], aes(x = (start+end)/2, y = ordinal) )+
  labs(x = "Vegetation (%)", y = "Odds Ratio") 
#Add horizontal segments that coloured by the mean PET
p <- p + 
  geom_segment(data = sensation_res2[["0.5"]], aes(x = start, xend = end, y = ordinal, yend = ordinal, colour =avg_PET),  lwd = 2, alpha = 0.5)+ 
  scale_color_gradientn(colours = c( "#F2FE1E", "#FBD412", "#FFA612", "#FE230A", "#E600A9"),
                        values = c(0,0.1,0.15, 0.2,0.25, 0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.8, 0.85, 0.9,0.95, 1)) 
#Add a trend line (smoothed by rollmean)
p <- p + theme_bw()+
  geom_line(aes(y = rollmean(sensation_res2[["0.5"]]$ordinal, 5, na.pad = TRUE, align = "center")), linewidth = 1)
print(p)



### Making Ordinal Graphs for each Aggregated Vegetation Group ###
############# Comfort ######################
#Comfort_0_10####
##Read data of low veg group
comf_0_10 = read_excel("new_notes_8.xlsx", sheet = "0-10-C")
comf_0_10 <- comf_0_10[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Comfort_group) ~ PET , data = comf_0_10, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#29.2 % odds that people fill discomfort as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_C_0_10 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_C_0_10 <- cbind(newdat_C_0_10, predict(m, newdat_C_0_10, type = "probs"))

##show first few rows
head(newdat_C_0_10)
lnewdat_C_0_10 <- melt(newdat_C_0_10, id.vars = c("PET"),
                variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_C_0_10)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_C_0_10, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)


#Comfort_10-40####
##Read data of low veg group
comf_10_40 = read_excel("new_notes_8.xlsx", sheet = "10-40-C")
comf_10_40 <- comf_10_40[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Comfort_group) ~ PET , data = comf_10_40, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#26.7 % odds that people fill discomfort as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_C_10_40 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_C_10_40 <- cbind(newdat_C_10_40, predict(m, newdat_C_10_40, type = "probs"))

##show first few rows
head(newdat_C_10_40)
lnewdat_C_10_40 <- melt(newdat_C_10_40, id.vars = c("PET"),
                       variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_C_10_40)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_C_10_40, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)













###############newdata for vege examination

newdat_v <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 3), 200),
  Gender_Bin = rep(0:1, each = 300),
  Vegetation_360= rep(seq(from = 0.05, to = 72.50, length.out = 100), 6))

newdat_v <- cbind(newdat_v, predict(m, newdat_v, type = "probs"))

##show first few rows


head(newdat_v)

lnewdat_v <- melt(newdat_v, id.vars = c("PET", "Gender_Bin", "Vegetation_360"),
                variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_v)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_v, aes(x = Vegetation_360 , y = Probability, colour = Level)) +
  geom_line() + facet_grid(PET~Gender_Bin, labeller="label_both")



#Comfort_40+####
##Read data of low veg group
comf_40 = read_excel("new_notes_8.xlsx", sheet = "40-74-C")
comf_40 <- comf_40[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Comfort_group) ~ PET , data = comf_40, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#10.7 % odds that people fill discomfort as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_C_40 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_C_40 <- cbind(newdat_C_40, predict(m, newdat_C_40, type = "probs"))

##show first few rows
head(newdat_C_40)
lnewdat_C_40 <- melt(newdat_C_40, id.vars = c("PET"),
                        variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_C_40)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_C_40, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)



############# Preference ######################
#Preference_0_10####
##Read data of low veg group
pref_0_10 = read_excel("new_notes_8.xlsx", sheet = "0-10-C")
pref_0_10 <- pref_0_10[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Preference_group) ~ PET , data = pref_0_10, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#31.1 % odds that people want cooler environment as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_p_0_10 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_p_0_10 <- cbind(newdat_p_0_10, predict(m, newdat_p_0_10, type = "probs"))

##show first few rows
head(newdat_p_0_10)
lnewdat_p_0_10 <- melt(newdat_p_0_10, id.vars = c("PET"),
                       variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_p_0_10)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_p_0_10, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)





#Preference_10-40####
##Read data of low veg group
pref_10_40 = read_excel("new_notes_8.xlsx", sheet = "10-40-C")
pref_10_40 <- pref_10_40[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Preference_group) ~ PET , data = pref_10_40, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#26.8 % odds that people want cooler environment as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_p_10_40 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_p_10_40 <- cbind(newdat_p_10_40, predict(m, newdat_p_10_40, type = "probs"))

##show first few rows
head(newdat_p_10_40)
lnewdat_p_10_40 <- melt(newdat_p_10_40, id.vars = c("PET"),
                       variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_p_10_40)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_p_10_40, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)





#Preference_40+####
##Read data of low veg group
pref_40 = read_excel("new_notes_8.xlsx", sheet = "40-74-C")
pref_40 <- pref_40[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Preference_group) ~ PET , data = pref_40, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#19.3 % odds that people want cooler environment as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_p_40 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_p_40 <- cbind(newdat_p_40, predict(m, newdat_p_40, type = "probs"))

##show first few rows
head(newdat_p_40)
lnewdat_p_40 <- melt(newdat_p_40, id.vars = c("PET"),
                        variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_p_40)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_p_40, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)




############# Sensation ######################

#Sensation_0_10####
##Read data of low veg group
sens_0_10 = read_excel("new_notes_8.xlsx", sheet = "0-10-C")
sens_0_10 <- sens_0_10[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Sensation_group) ~ PET , data = sens_0_10, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#59.2 % odds that people fill feel hotter as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_s_0_10 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_s_0_10 <- cbind(newdat_s_0_10, predict(m, newdat_s_0_10, type = "probs"))

##show first few rows
head(newdat_s_0_10)
lnewdat_s_0_10 <- melt(newdat_s_0_10, id.vars = c("PET"),
                       variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_s_0_10)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_s_0_10, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)















#Sensation_10_40####
##Read data of low veg group
sens_10_40 = read_excel("new_notes_8.xlsx", sheet = "10-40-C")
sens_10_40 <- sens_10_40[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Sensation_group) ~ PET , data = sens_10_40, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#42.3 % odds that people fill feel hotter as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_s_10_40 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_s_10_40 <- cbind(newdat_s_10_40, predict(m, newdat_s_10_40, type = "probs"))

##show first few rows
head(newdat_s_10_40)
lnewdat_s_10_40 <- melt(newdat_s_10_40, id.vars = c("PET"),
                       variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_s_10_40)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_s_10_40, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)





#Sensation_40+####
##Read data of low veg group
sens_40 = read_excel("new_notes_8.xlsx", sheet = "40-74-C")
sens_40 <- sens_40[complete.cases(df2$Preference),]

## fit ordered logit model and store results 'm'
m = polr(as.factor(Sensation_group) ~ PET , data = sens_40, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(table <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(table, "p value" = p))
(ci <- confint(m)) # default method gives profiled CIs
confint.default(m) # CIs assuming normality # If the 95% CI does not cross 0,
#the parameter estimate is statistically significant.
#There is significance

## odds ratios
exp(coef(m))
#48.2 % odds that people fill feel hotter as PET goes up by 1 unit

## OR and CI 
exp(cbind(OR = coef(m), ci))

##plot probability graph
newdat_s_40 <- data.frame(
  PET = rep(seq(from = 25.7, to = 39.3, length.out = 150), 3))
newdat_s_40 <- cbind(newdat_s_40, predict(m, newdat_s_40, type = "probs"))

##show first few rows
head(newdat_s_40)
lnewdat_s_40 <- melt(newdat_s_40, id.vars = c("PET"),
                        variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat_s_40)

par(mar = c(15.1, 4.2, 14.1, 2.1))

ggplot(lnewdat_s_40, aes(x = PET, y = Probability, colour = Level)) +
  geom_line() +
  ylim(0,0.8)


