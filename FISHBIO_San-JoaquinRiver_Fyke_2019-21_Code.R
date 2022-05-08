# Objective: Task 3 - 
  # Determine size differences of 
  # recaptured striped bass tagged in 
  # fyke traps by region of recapture/recovery

#____________________________________________________________
# Set-up

# load necessary packages
  library(openxlsx)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(lmtest)
  library(ggpubr)
  library(rstatix)
  library(PMCMRplus)

setwd("~/FISHBIO/FISHBIO_San-JoaquinRiver_Fyke_Data")

#______________________________________________________________
# Data import and sorting

datA <-
  read.xlsx("FISHBIO_San-JoaquinRiver_Fyke_Data_2019.xlsx",
      # import dataset
      sheet = 1,
      # sheet 1 or tab 1 ("Target Catch")
      detectDates = T)
      # allow import of the dates column as is

datB <-
  read.xlsx("FISHBIO_San-JoaquinRiver_Fyke_Data_2021.xlsx",
      # import dataset
      sheet = 4,
      # sheet 1 or tab 1 ("Target Catch")
      detectDates = T)
      # allow import of the dates column as is

# filter rows from the 2019 dataframe by
  # recaptured striped bass
recapsA1 <-
  dplyr::filter(datA,
         Species == "Striped Bass",
         Recap == "Y")

# remove recaptured striped bass that were intially captured
  # via electrofishing because they may skew fyke data
recapsA2 <- recapsA1[-c(5,27) , ]

# select columns of the 2019 striped bass data
  # that can be used to achieve the overarching
  # objective
recapsA3 <-
  dplyr::select(recapsA2,
                Site, Date,
                TL, PITTag.No)

# rename the selected columns have the same names
  # as those of the second dataset
recapsA4 <- recapsA2 %>% 
  rename(PITTagNo = PITTag.No)

# filter rows of the second dataset by
  # all captured striped bass
marksA <-
  filter(datA, Species == "Striped Bass")

# filter rows of the second dataset by
  # recaptured striped bass
recapsB1 <- 
  filter(datB, 
         OrgCode == "STB")

# select certain columns of the striped bass data
  # to achieve the overarching objective

recapsB2 <-
  dplyr::select(recapsB1, OG_Loc, SampDate,
                TLmm, PitTagNo)
recapsB3 <- recapsB2 %>% 
  rename(
    Site = OG_Loc,
    Date = SampDate,
    TL = TLmm,
    PITTagNo = PitTagNo
  )

# combine the rows of recaptured striped bass
  # from 2019 and 2021
  recaps <- rbind(recapsA4, recapsB3)

#_____________________________________________________________
#_____________________________________________________________

# Part I
  # Size comparisons for recaptured fish
  
# Prepare data for Welsh's t-tests that compare fork lengths of
  # each site to those of all other sites pooled together

  # Step 1: Identify sample sites
    unique(recaps$Site)
      # 8 unique locations
        # Hills Ferry Barrier
        # Van Clief
        # Lathrop
        # Sturgeon Bend
        # Mud Slough
        # Alegre
        # Blewett

# Step 2: Create dataframes for each sample site
    # then make a vector for total length for each site
     eastbypass <-
       filter(recaps, Site == "Eastside Bypass")
     eastbypassTL <- eastbypass$TL
     resamp_eastbypassTL <-
       sample(eastbypassTL, size = 1, replace = F)
     # 1 observation is not enough for comparing
     # the mean of Eastside Bypass to those of all other
     # sites, so resample for another estimate
     comb_eastbypassTL <- 
       rbind(eastbypassTL, resamp_eastbypassTL)

     hillsferry <-
      filter(recaps, Site == "Hills Ferry Barrier")
    hillsferryTL <- hillsferry$TL

    vanclief <-
      filter(recaps, Site == "Van Clief")
    vancliefTL <- vanclief$TL

    lathrop <-
      filter(recaps, Site == "Lathrop")
    lathropTL <- lathrop$TL

    sturgeonbend <-
      filter(recaps, Site == "Sturgeon Bend")
    sturgeonbendTL <- sturgeonbend$TL
  
    mudslough <-
      filter(recaps, Site == "Mud Slough")
    mudsloughTL <- mudslough$TL

    alegre <-
      filter(recaps, Site == "Alegre")
    alegreTL <- alegre$TL

    blewett <-
      filter(recaps, Site == "Blewett")
      # select only data from Blewett
    blewettTL <- blewett$TL
      # make total lengths from Blewett a numeric string 
    resamp_blewettTL <-
      sample(blewettTL, size = 1, replace = F)
      # 1 observation is not enough for comparing
        # the mean of Blewett to those of all other
        # sites, so resample for another estimate
    comb_blewettTL <- rbind(blewettTL, resamp_blewettTL)

# Step 3: Place total lengths from each sample site 
  # dataframe into one vector
    allTLs <- select(recaps, "Site", "TL")
    
# Step 4: Ensure "Site" is a factor that can be
  # used to reorder the data
    str(allTLs$Site) 
      # "Site" is character
    as.factor(allTLs$Site) 
      # "Site" is still as character, but it may
        # now be separated into different levels
    str(allTLs)
      # same story as last two comments
    mutate_if(allTLs, is.character, 
              as.factor)
      # another attempt to ensure "Site" becomes
        # a factor variable
        allTLs <- arrange(allTLs, Site)
      # reorder "Site" by all of its factor levels,
        # which means each sample site
    
# Step 4: Create datasets that omit on sample site
    # from a pooling of all others so that "TL"
    # data from each site can be compared with
    # pooled "TL" data from all other sites
    
      no_alegre <- allTLs[-c(1,2,3) , ]
      
      no_blewett <- allTLs[-c(4) , ]
      
      no_ebypass <- allTLs[-c(5) , ]
      
      no_hillsferry <- allTLs[-c(6:13) , ]
    
      no_lathrop <- allTLs[-c(14) , ]
      
      no_mudslough <- allTLs[-c(15:18) , ]
      
      no_sturgeonbend <- allTLs[-c(19,20) , ]
      
      no_vanclief <- allTLs[-c(21:35) , ]
      
      
# Step 5: Conduct a series of Welch's 2-sample
    # t-tests to compare the means of regions
      # to those of the all other regions
      
    # Ho is that the true difference in means
      # does not equal 0
      
    # Alegre v. all other regions
      t.test(alegreTL, # compare Algree total lengths
             no_alegre$TL, # to those from all other
                              # regions
             var.equal=F) # without the assumption
                            # that the variances in the
                            # two respective groups
                            # of total lengths are equal
                            
      # t = 1.1129 | df = 4.9671 | p = 0.3167
      
    # Blewett v. all other regions
      t.test(comb_blewettTL,
             no_blewett$TL,
             var.equal = F)
      # t = -0.13119 | df = 2.3678 | p = 0.9059
    
    # Eastside Bypass v. all other regions
      t.test(comb_eastbypassTL,
             no_ebypass$TL,
             var.equal = F)
      # t = -2.1012 | df = 1.0655 | p = 0.2705
      
    # Lathrop v. all other regions
      t.test(lathropTL,
             no_lathrop$TL,
             var.equal = F)
      # t = | df = | p =
      
    # Sturgeon Bend v. all other regions
      t.test(sturgeonbendTL,
             no_sturgeonbend$TL,
             var.equal = F)
      # t = 0.22704 | df = 1.3304 | p = 0.8569
      
    # Mud Slough v. all other regions
      t.test(mudsloughTL,
             no_mudslough$TL,
             var.equal = F)
      # t = 2.24409 | df = 6.4732 | p = 0.04747
      
    # Hills Ferry Barrier v. all other regions
      t.test(hillsferryTL,
             no_hillsferry$TL,
             var.equal = F)
      # t = 0.08745 | df = 17.484 | p = 0.9313
      
    # Van Clief v. all other regions
      t.test(vancliefTL,
             no_vanclief$TL,
             var.equal = F)
      # t = -0.92948 | df = 21.981 | p = 0.3627
      
#_______________________________________________________
# Plots to view data distribution
  
  # scatter plot
    library(ggplot2)
    ggplot(recaps, 
      aes(x=Site, y=TL)) + 
      geom_point()
  
  # violin plot
    ggplot(recaps, 
       aes(x = Site, 
           y = TL,
           fill = Site)) +
       geom_violin(fill = "light grey") +
       geom_boxplot(width = 0.1) +
       ggtitle("Striped bass recaptures on the San-Joaquin River via fyke nets (2019 - 2021)") +
       labs(fill = "") +
       theme(legend.position="bottom") +
       theme(axis.text = element_blank(),
       axis.ticks.x = element_blank()) +
       scale_y_continuous(breaks = seq(300,1000,50))
       xlab("Sample site") +
       ylab("Total length (mm)")
  
#________________________________________________________
  # Check Assumptions of Paired t-tests
      # 1. Is data continuous?
          summary(recaps$TL)
          # discrete values that are ordinal on a 
            # continuous scale
      # 2. Are observations randomly selected?
          # measurements of STB captured by fykes
      # 3. Is data normally distributed?
          # assess visually
            qqnorm(recaps$TL)
            qqline(recaps$TL,col="blue")
          # assess quantitatively
            # test 1
              library(nortest)
              ad.test(recaps$TL)
            # test 2
              shapiro.test(recaps$TL)
              
            # seems to not statistically 
              # differ from a normal distribution
              # but small sample sizes
              # often pass normality tests
            
      # 4. variances between groups of
            # comparison are not equal
          var(alegre$TL) # 14791.78
          var(blewett$TL) # 6964.3
          var(hillsferry$TL) # 2694.7
          var(lathrop$TL) # 6745.333
          var(mudslough$TL) # 3444.5
          var(sturgeonbend$TL) # 8641.731
          var(vanclief$TL) #17369.9
          
    # can quantitatively test if differences between
      # variances are different with Levene's Test,
      # but it's clear that the variances here are
      # highly variable
#________________________________________________________

# test differences in groups with resampling
  # function taken from this website:
  # http://strata.uga.edu/8370/lecturenotes/resampling.html  
      
  # Function for calculating one randomized 
      # difference in means
      randomizedDifferenceInMeans <- function(x, y) {
            nx <- length(x)
            ny <- length(y)
            
            # combine the data
            combined <- c(x, y)
            ncom <- length(combined)
            indices <- 1:ncom
            
            # initially assign all observations to y group
            group <- rep('y', ncom)
            
            # assign a subsample to to x group
            xsub <- sample(indices, nx)
            group[xsub] <- 'x'
            
            # calculate the means
            meanX <- mean(combined[group=='x'])
            meanY <- mean(combined[group=='y'])
            differenceInMeans <- meanX - meanY
            differenceInMeans
          }
          
    # Repeat that function many dDifferenceInMeans(x, y))
      
    # Calculate the difference in means from resampling
        # then view  critical values for this difference
        observedDifference <- mean(x) - mean(y)
        alpha <- 0.05
        quantile(diffMeans, alpha/2)
        quantile(diffMeans, 1-alpha/2)
       
        # and the calculated p-value from a 
          # two-tailed test
          library(scales)
          pvalue(diffMeans, 
                 observedDifference,
                 accuracy = 0.000001,
                 decimal.mark = ".",
                 prefix = c("<", "", ">"))
      
    # change x and y multiple times to compare 
        # different regions and each regions to all
        # other regions pooled then run lines 189 - 215
          x <- alegre$TL
          y <- blewett$TL
#________________________________________________________
  # Kruskal-Wallis Test followed by Mann-Whitney U Tests
    kruskal.test(recaps$TL, recaps$Site)
      # 6 degrees of freedom b/c 7 sample sites - 1
      # p-value = 0.7932
      # Kruskal-Wallis test is not effective for
        # this data b/c it seems to be heteroscedastic
        # based on the variances & initial scatter plot
        
      # test heteroscedascity
        # methodology taken from this website
          # https://datascienceplus.com/how-to-detect-heteroscedasticity-and-rectify-it/
        m1 <- lm(TL ~ Site, data = recaps)
          # linear model of total length by
            # site for the recaps data
        
        par(mfrow=c(1,1))
          # view residuals in the 1st and 3rd plots
            # the small number of data points must
              # affect the plots
        plot(m1)
        
        # statistically test for heteroscedacity
          # H0 of both tests is that residuals
            # are uniformally distributed
        library(lmtest)
        lmtest::bptest(m1)  # Breusch-Pagan test
        
        library(car)
        car::ncvTest(m1)  # NCV test
        
#________________________________________________________
# (below code does not promote the completion of 
        # main objective)  
  
  # Test assumptions of ANalysis of
    # COVAriance (ANCOVA) on data before 
    # performing test
  # https://www.datanovia.com/en/lessons/ancova-in-r/

# test linearity b/w covariate (sample date)
  # and outcome variable (fork length)

# install.packages("ggpubr")
library(ggpubr)
ggscatter(
  SJrecaps, x = "Site", y = "TL",
  facet.by  = "Site", 
  short.panel.labs = FALSE
) +
  stat_smooth(method = "loess", span = 0.9)

# ANalysis of COVAriance (ANCOVA)
  #install.packages("rstatix")
  library(rstatix)
  
  ancova <- SJrecaps %>% 
  anova_test(TotalLength ~ Location + 
               SampleDate)
  get_anova_table(ancova)

  # Results
    
# ANOVA Table (type II tests)

#     Effect  DFn DFd     F     p p<.05   ges
# 1   Location 7  27 0.538 0.798        0.123
# 2 SampleDate 1  27 0.322 0.575        0.012

# Interpretation
  # 8 sample locations and 34 sample dates, 
    # so the degrees of freedom should be 
    # 7 & 33, respectively

#_______________________________________________________________
  # Conduct a Welsh's One-Way ANOVA
  
    # one-way Welsh ANOVA of total lengths by
      # sample site
      
      # Step 1 - increase the number of observations
        # from 21 to perform the test
      resampled_recaps <-
        sample(recaps$TL, size = 35, replace = F)
      # Step 2 - ensure data is in correct format for
        # compiling into a dataframe
          # Global Environment shows "resampled_recaps"
          # as a numeric sequence
          as.vector(resampled_recaps)
          # ensure resampled_recaps is a vector
          str(resampled_recaps)
          # it is
          str(recaps$Site)
      # Step 3 - compile observations and resampled data 
          # into one dataframe
          test_dat <- 
          cbind.data.frame(TL = c(recaps$TL, resampled_recaps),
                      Site = c(recaps$Site, recaps$Site))
            # view data structure to see if dependent variable,
              # "TL", is numeric and independent variable,
              # "Site" is a factor rather than a character
              str(test_dat)
              
              test_dat <- mutate_if(test_dat, is.character, 
                          as.factor)
              # convert "Site" to a factor variable
                # as.factor(test_dat$TL) seemingly only
                  # works if the data was a string or vector
                  # not within a dataframe
                    str(test_dat$Site) # "Site" is a factor
      
      # Step 4 - Determine whether the data is 
        # normally-distributed or not
        shapiro.test(test_dat$TL)
          # based on the p-value the data is close
            # to normally-distributed
        
        # visualize distribution
          # histogram
            hist(test_dat$TL, breaks = 42) 
          # quantile-quantile plot
            qqnorm(test_dat$TL)
            qqline(test_dat$TL,col="blue")
      
      # Step 5 - Test variances
        # variances in total length between sample regions
        # seem unequal based on lines 164-172
      
      # test variances with Bartlett's & 
            # Fligner-Killeen Test
        bartlett.test(TL ~ Site, data = test_dat)
        # works for data that is normal (or near it)
          # based on the calculated p-value of 0.4592,
          # the variances between groups are close
          # enough to one another to allow
        # the low number of samples may affect the test or
          # I may be paranoid, either way, no harm in
          # using the Fligner-Killeen test
      
      # Fligner-Killeen test
        fligner.test(TL ~ Site, data = test_dat)
        # based on the p-value, the variances are
          # uniform enough for the Welsh's ANOVA
      
      # Welsh's One-Way Anova
        oneway.test(TL ~ Site, 
              data = test_dat, 
              var.equal = FALSE)
        
      # Games-Howell Post-Hoc Test for Welsh's ANOVA
        
        # try test with "rstatix" package
        library(rstatix)
        games_howell_test(test_dat, # data for test
                          TL ~ Site, # compare total length
                                        # by sample site
                          conf.level = 0.95, # 95% confidence 
                                              # int.
                          detailed = FALSE) 
        
        # try test with "PMCMRplus" package
          # create an anova model for test
          atest <- aov(TL ~ Site, test_dat)
            # save as object for test
          atest
            # view output
        library(PMCMRplus)
            # load package
        gamesHowellTest(x = atest,
                        formula = TL~Site,
                        g = Site,
                        data = test_dat)
            # run test

#____________________________________________________________
#____________________________________________________________
  
# Part II
  # Size comparisons for marked fish
        
# Specific Objective
# Compare sizes of striped bass throughout the San Francisco
    # Estuary, San Joaquin River, Sacramento River, and
    # Sacramento-San Joaquin Delta
                
  # Sequence A
    # filter dataframes "datA" & "datB" to create
      # a dataframe that compiles the data 
      # from both datasets together for analyses
        
      library(dplyr)
  
      # Step 1:
        # filter rows & select columns of the second
        # dataset by all captured striped bass
        marksA1 <-
          dplyr::filter(datA, Species == "Striped Bass")
        
        marksA2 <-
          dplyr::select(marksA1,
                        Date, TL, PITTag.No, Site, Recap)
       # Step 2:
        # Rename columns to facilitate compiling of
          # separate datasets
        marksA2 <- marksA2 %>% 
          rename(
            Location = Site,
            PITTagNo = PITTag.No
          )
        
      # Step 3:
        # filter rows of the second dataset by
        # all striped bass
        marksB1 <- 
          filter(datB, 
          OrgCode == "STB")
        
      # Step 4:
        # select columns of interest from the 
        # filtered dataset, "marksB2"
        marksB2 <-
          select(marksB1,
                 RC_Date, PitTagNo, TLmm, Region)
        
      # Step 5:
        # add a row signifying that all observations 
        # from the "marksB2" dataaframe, similar to 
        # a row in "marksA1" & "marksA2"
        marksB2$Recap <- rep("Y", 65)
    
        
      # Step 6:
        # rename columnns of the "marksB2" dataframe
        # to facilitate joining of dataframes
        marksB2 <- marksB2 %>% 
          rename(
            Date = RC_Date,
            TL = TLmm,
            PITTagNo = PitTagNo,
            Location = Region
          )
        
      # Step 7:
        # export dataframes into one Excel file
        
        # Ensure Java is downloaded for your 
        # device to use the below 
        # Java-dependent package
        
        #install.packages("xlsx")
        library(xlsx)
        
        # create an Excel file from the selected & 
          # filtered data
        write.xlsx(recaps,
          file = "~/FISHBIO/FISHBIO_San-JoaquinRiver_Fyke_Data/FISHBIO&SJRRP_STB_marks&recaptures.xlsx",
          sheetName = "Recaptures")
        
        # combine the rows of recaptured striped bass
          # from 2019 and 2021
        marks <- rbind(marksA2, marksB2)
        
        # replace NAs with 0
        marks[is.na(marks)] <- 0
        
        # add marked fish as the second sheet
          # of the newly created Excel file of 
          # selected & filtered data
        write.xlsx(marks,
            file = 
            "~/FISHBIO/FISHBIO_San-JoaquinRiver_Fyke_Data/FISHBIO&SJRRP_STB_marks&recaptures.xlsx",
            sheetName = "Marks",
            append = T)
        
  # Sequence B
    # Sort rows of "marks" by region and apportion data
        # based on this
        
    # Step 1
      # Re-order "marks" by the variable, "Location"
        str(marks) 
        # "Location" is a character variable that
          # can be ued to rearrange other variables
        marks <- arrange(marks, Location)
        
    # Step 2
      # Determine counts of observations per each
        # "Location" & make a vector that repeats the
        # names of each site the number of times
        # observations for the site occur there
        
        # Ocean = Ocean 
          # (n = 3)
          sum(marks$Location == "Ocean")
        
        # SF Bay & Estuary = Carquinez, Petaluma sloughs, 
          # San Pablo Bay, Suisun Bay, and SF bays 
          # (n = 1+1+5+1+1 = 9)
          sum(marks$Location == "Carquinez Strait")
          sum(marks$Location == "Petaluma Sloughs")
          sum(marks$Location == "San Pablo Bay")
          sum(marks$Location == "Suisan Bay")
          sum(marks$Location == "San Francisco Bay")
        
        # Delta = north and south delta 
          # (n = 46)
          sum(marks$Location == "North Delta")
          sum(marks$Location == "South Delta")
        
        # Sac = Sac 
          # (n = 1 fish [saddens Matt])
          sum(marks$Location == "Sacramento River")
        
        # SJR = any recaps in Stanislaus & SJR
          # (n = 466)
          sum(marks$Location == "Alegre")
          sum(marks$Location == "Blewett")
          sum(marks$Location == "Eastside Bypass")
          sum(marks$Location == "Hills Ferry Barrier")
          sum(marks$Location == "Lathrop")
          sum(marks$Location == "Mud Slough")
          sum(marks$Location == "Sturgeon Bend")
          sum(marks$Location == "Van Clief")
          sum(marks$Location == "Stanislaus River")
        
    # Step 3
      # Create a new column for the "marks" dataframe
        # that compiles each "Location" into a "Region"
          marks$Region <- 
            c("San Francisco Estuary", 
              rep("San Joaquin River Basin", 361),
              rep("Sacramento-San Joaquin Delta", 8),
              rep("Ocean", 3),
              "Sacramento-San Joaquin Delta",
              "Sacramento River",
              rep("San Joaquin River Basin",2),
              rep("San Francisco Estuary", 7),
              rep("Sacramento-San Joaquin Delta", 37),
              rep("San Joaquin River Basin", 6),
              "San Francisco Estuary",
              rep("San Joaquin River Basin", 161))
          
        # arrange the "marks" dataframe by ordering other
          # variables by the "Region" variable
            marks <- arrange(marks, Region)

  # Sequence C
    # make a plot showing the distribution of total
      # lengths by sampling region
      
      # make a dataframe that excludes the one data point
        # for the Sacramento River sense it will not
        # display well on a boxplot
      marks2 <- marks[-c(50), ]
      
      # make a dataframe that exludes striped bass with total
        # lengths smaller than 339 mm
          
          # create a dataframe for STB smaller than 339 mm
          smallSTB <-
            filter(marks2, TL < "339")
          
          # export dataframe as the third sheet of the
            # previously-created Excel file
          library(xlsx)
          write.xlsx(smallSTB,
            file = "~/FISHBIO/FISHBIO_San-JoaquinRiver_Fyke_Data/FISHBIO&SJRRP_STB_marks&recaptures.xlsx",
            sheetName = "Total Length < 339 mm")
      
    # plot data
      # all marked striped bass from regions 
          # other than the Sacramento River (b/c 1 observation)
      library(ggplot2)
            
      ggplot(marks2, 
        aes(x = Region, y = TL)) + 
        geom_violin(fill = "light grey") +
        geom_boxplot(aes(fill = Region), width = 0.2) +
        theme_classic() +
        xlab("Sample region") +
        ylab("Total length (mm)") +
        scale_y_continuous(breaks = seq(100, 1600, by = 100)) +
        geom_text(x= 1, y= 1000, label="N = 3", size = 3) +
        geom_text(x= 2, y= 1000, label="N = 9", size = 3) +
        geom_text(x= 3, y= 1000, label="N = 1", size = 3) +
        geom_text(x= 4, y= 1000, label="N = 46", size = 3) +
        geom_text(x= 5, y= 1000, label="N = 466", size = 3)
     
    # striped bass recaptures from the "marksB2" dataframe
      
      # sort by sample site, "Location"
      marksB2 <-
        arrange(marksB2, Location)
      # create a new column that groups sites by region
      marksB2$Region <- 
        c("San Francisco Estuary", 
          rep("Sacramento-San Joaquin Delta", 8),
          rep("Ocean", 3),
          "San Francisco Estuary",
          "Sacramento River",
          rep("San Francisco Estuary", 6),
          rep("Sacramento-San Joaquin Delta", 38),
          rep("San Joaquin River Basin", 6),
          "San Francisco Estuary")
      
      # remove Sacramento River from data for plotting
      marksB3 <- marksB2[-c(14) , ]
      
      # plot data
      library(ggplot2)
      
      ggplot(marksB3, 
             aes(x = Region, y = TL)) + 
        geom_violin(fill = "light grey") +
        geom_boxplot(aes(fill = Region),
                     width = 0.05) +
        theme_classic() +
        ylab("Total length (mm)") +
        theme(legend.position = "bottom") +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.title = element_blank()) +
        geom_text(x= 1, y= 380, label="N = 3", size = 3) +
        geom_text(x= 2, y= 380, label="N = 46", size = 3) +
        geom_text(x= 3, y= 380, label="N = 9", size = 3) +
        geom_text(x= 4, y= 380, label="N = 6", size = 3) +
        scale_y_continuous(breaks = seq(0, 1000, by = 100))
            
  # Sequence D
    # (no longer needed since a visual comparison of
      # groups in a boxplot is sufficient)
            
    # Resample regions with fewer than 466 observations
      # with replacement ("bootstrapping") so that the
      # total length distributions for these regions
      # become closer to normal distributions then
      # compile a dataframe of these observations
          
      ocean <-
        filter(marks, Region == "Ocean")
      resamp_oceanTLs <-
        sample(ocean$TL, 463, replace = T)
      ocean_labs <-
        rep("Ocean", 466)
      
      delta <-
        filter(marks, Region == "Sacramento-San Joaquin Delta")
      resamp_deltaTLs <-
        sample(delta$TL, 420, replace = T)
      delta_labs <-
        rep("Sacramento-San Joaquin Delta", 466)
      
      sac <-
        filter(marks, Region == "Sacramento River")
      resamp_sacTLs <-
        sample(sac$TL, 465, replace = T)
      sac_labs <-
        rep("Sacramento River", 466)
      
      sfestuary <-
        filter(marks, Region == "San Francisco Estuary")
      resamp_sfestuaryTLs <-
        sample(sfestuary$TL, 460, replace = T)
      sfestuary_labs <-
        rep("San Francisco Estuary", 466)
      
      sjr <-
        filter(marks, Region == "San Joaquin River")
      sjrTLs <-
        sjr$TL
      sjr_labs <-
        rep("San Joaquin River", 466)
      
      test_marksTLs <-
        c(resamp_oceanTLs, resamp_sacTLs, resamp_deltaTLs,
          resamp_sfestuaryTLs, sjrTLs)
      
      
