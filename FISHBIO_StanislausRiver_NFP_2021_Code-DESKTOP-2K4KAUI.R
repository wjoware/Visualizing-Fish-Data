#_______________________________________________________
# Set Up
#_______________________________________________________

# remove global environment objects
  rm(list = ls())

# load necessary packages
  library(openxlsx) 
  library(dplyr)
  library(zoo)
  library(ggplot2)
  library(GLDEX)
  library(lubridate)
  library(nortest)
  library(fitdistrplus)
  library(scales)
  library(tuneR)
  # (also placed where applicable below)
  
# create the right reference data repository
  setwd("~/OneDrive - ucsc.edu/FISHBIO/Summer2021 _STB_Tasks/Task 2/FISHBIO_StanislausRiver_NativeFishPredation/FISHBIO_StanislausRiver_NFP_RCode")

#________________________________________________________
  # Data Import & Selection

# import Excel (.xlsx) file
  library(openxlsx) # load necessary package
  dat<-
    read.xlsx("FISHBIO_StanislausRiver_NFP_Summary_Data_2021.xlsx", 
  # import dataset
    sheet = 3, 
  # sheet or tab 3 ("Target Catch")
    detectDates=T) 
  # allow import of the dates column as is
    head(dat)
      # view column names
    str(dat$SampleDate)
      # dates imported correctly
    factor(dat$SampleDate)
      # 108 unique dates
    colnames(dat)
      # check column names to determine 
        # which are needed

  # determine rows of the dataframe contanining missing
    # data (NAs)
      # install.packages("GLDEX")
        library(GLDEX)
        NAs <- which.na(dat) 
          # save rows with NA values as an object
  
  # X <- dat[is.na(dat)]
   # replace NAs in dat with X

# filter data from larger dataset
  library(dplyr)
    # load package needed to 
      # filter a subset of the dataset
  dat2 <- dplyr::select(dat, 
    "Species", "Count", "TotalLength", 
    "Forklength", "SamplingUnitNo", "SampleDate", 
    "PITTagNo", "FishCode", "Recapture") 
      # subset desired columns of dataset

  # select rows of dataset containing data
    # on striped bass (STB)
      dat3 <- filter(dat2, 
        Species == "STB")
    
    # review range of fork lengths
      summary(dat3$Forklength)
        # range: 165 mm - 1090 mm
        # mean = 366.9 mm
    
# -------------------------------------------------------------------------
  # Data wrangling
  
  # new variable for 50 mm fork length increments
    mutate(., 
      SizeClass = # create column called "SizeClass"
      findInterval(Forklength,
      seq(150, 1200, # ranging from 150 to 1200 mm 
                      # Note: actual data range is 165 - 1090 mm fork lengths
      by = 50))) # spaced by 50 numbers (each size class)
  
  # create a tibble that shows each size
    # class and counts of fish per size class
    group_by(dat3, SizeClass) %>%
      summarize(., 
      count = length(SizeClass)) %>%
    # add a table column that calculates the
      # proportion of counts for each size
      # class of the total counts for all size
      # classes
        mutate(., proportion =
           count/sum(count))
    # new variable for striped bass relative body size
      dat3$RelativeSize <-
        cut(dat3$Forklength, breaks = c(-Inf, 299, Inf), 
          labels = c("small", "large"))
    
    # new variable for sampling year
      dat3$SampleYear <- 
          format(dat3$SampleDate, 
             "%Y") # years in NNNN format = %Y
                    # aggregate "SampleDate" by year & create
                      # a new column with the aggregated data
    
    # new variable for sampling month & year
      dat3$SampleMonth <-
        format(dat3$SampleDate, "%m/%Y")
        # aggregate "SampleDate" by month & year
          # then create a new column with the
            # aggregated data
    
    # convert SampleDate column to a decimal number
        dat3$SampleDay <-
          yday(dat3$SampleDate)

# create dataframe for 2019 - 2021 observations
  dat3B <- filter(dat3, !SampleYear == "2018",
                  !Forklength == "NA")
                  # remove the single NA entry from June 2019
  
  # turn sample month into a factor variable with discrete levels
    dat3B$SampleMonth <- factor(dat3B$SampleMonth, 
      levels = c("03/2019", "04/2019", "05/2019", 
                 "06/2019", "02/2020", "03/2020", 
                 "05/2020", "06/2020", "02/2021", 
                 "03/2021", "04/2021", "05/2021"))
                 # facilitates plotting month on x-axis
  
  # count small & large striped bass by sampling month
    # method 1 - tibble
      dat3C <- group_by(dat3B, SampleMonth, RelativeSize) %>%
        summarise(., Count = sum(Count))
    # method 2 - table
      aggregate(dat3B$Count, 
                by = list(Category = dat3B$SampleMonth, 
                          Category = dat3B$RelativeSize), 
                FUN = sum)
  
  # create a dataframe for months f no sampling
    noSamp <- data.frame(SampleMonth = # column for sample month
                rep(c("02/2019", "04/2020" , "06/2021"), 2), 
                  # 2 rows for each month
              RelativeSize = # column for size
                c(rep("large", 3), rep("small", 3)),
                  # 2 rows for each size that align with months
              Count = rep(NA, 6)) # "NA" entries fill each row for counts
    
  # combine data
    dat3C <- bind_rows(dat3C, # sampling
                       noSamp # non-sampling
                       ) %>%
    mutate(., SampleMonth = factor(SampleMonth, 
                              # sampling month as factor variable
          levels = c("02/2019","03/2019", "04/2019", "05/2019","06/2019", 
                     "02/2020", "03/2020", "04/2020","05/2020", "06/2020",
                     "02/2021", "03/2021", "04/2021", "05/2021", "06/2021")))
                              # order months as discrete levels for plotting
    
  # create a dataframe for axis labels
    noSamp.lab <- data.frame(SampleMonth = 
                  factor(c("02/2019", "04/2020" , "06/2021")),
                  Count = 6,
                  label = "N/S")
    
  # plot bar plot
    ggplot(data = dat3C, 
      aes(x = SampleMonth, y = Count, fill = RelativeSize)) +
    xlab("Sampling Month") +
    ylab("Counts") +
    geom_bar(stat = "identity", color = "black", na.rm = T, 
             position = position_stack(reverse = TRUE)) +
    geom_text(data = noSamp.lab, 
                aes(x=SampleMonth,y=Count,label=label), inherit.aes = F) +
    scale_fill_grey(name = "Size classes", 
                  labels = c("small (FL < 300 mm)", "large (FL > 300 mm)", "N/S")) +
    theme(axis.ticks.length = unit(1, "inch")) +
    theme_classic() +
    scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, 10)) +
      # y-axis limits or bounds are just beyond
        # that of the dependent-variable
    scale_x_discrete(breaks = 
      c("02/2019", "03/2019", "04/2019", "05/2019", "06/2019", 
        "02/2020", "03/2020", "04/2019", "05/2020", "06/2020", 
        "02/2021", "03/2021", "04/2021", "05/2021", "06/2021"),
    # x-axis labels
      labels = c("Feb. 2019", "March 2019", "April 2019", "May 2019", "June 2019",
                 "Feb. 2020", "March 2020", "April 2019", "May 2020", "June 2020", 
                 "Feb. 2021", "March 2021", "April 2021", "May 2021", "June 2021"))
  
#____________________________________________
  # aggregate dates by season then 
    # create a new column for each season

    # 2021
      # Spring:March 20th - June 20th
      # Summer: June 21st - September 22nd
      # Fall: September 22nd to 
        # December 20th
      # Winter: December 21st to March 19th
  
# 2018 & 2019
  spring18 <- filter(dat3, 
              SampleYear == "2018")
  spring19 <- filter(dat3, 
              SampleDate > "2019-03-10" & 
              dat3$SampleDate <"2019-06-21")
  # Spring: March 20th - June 20th
  summer19 <- filter(dat3, 
              SampleDate == "2019-06-21")
  # Summer was June 21st - September 22nd
    # no sampling during fall &
      # winter (2018 - 2019)
        # Fall: September 23rd to 
          # December 20th
        # Winter: December 21st to 
          # March 19th
# 2020
  winter20 <- filter(dat3, 
            SampleDate > "2020-01-03" &
            SampleDate < "2020-03-20")
  winter21 <- filter(dat3, 
            SampleDate > "2021-02-09" & 
            SampleDate < "2021-03-20")
  # Winter: December 21st to March 18th
  spring20 <- filter(dat3, 
            SampleDate > "2020-03-19" & 
            SampleDate < "2020-06-20")
  spring21 <- filter(dat3, 
            SampleDate > "2021-03-19" &
            SampleDate < "2021-06-21")
# Spring: March 19th - June 19th
# No sampling during summer & fall 
    # (2020 - 2021)
  # Summer: June 20th - September 21st
  # Fall: September 22nd to December 20th

        
# combine dataframes by season by pooling years
  spring <- bind_rows(spring18,spring19,
                      spring20,spring21)
  winter <- bind_rows(winter20,winter21)
  summer <- summer19
        
#_______________________________________________
  # create data visuals of striped bass (STB)
    # fork length (FL) by sampled seasons 
      # for each year
  # spring
    # linear regression 
      # (not informative visual)
    ggplot(spring,
      # dataset for plotting
    aes(x = SampleDate,
      # independent variable
    y = Forklength)) +
      # dependent variable
    geom_point() + 
      # default settings for data points
    stat_smooth(method = "lm", 
      # model a linear regression
    col = "red")
      # color of regression line
    
    # bar plot
    ggplot(spring, 
    aes(x=SampleDate, 
        # independent variable
        y=Forklength)) +
        # dependent variable
    geom_bar(stat="summary_bin", 
        # each bar represents annual mean
          # of fork lengths
            # stat="identity" uses values
              # that do not reflect fork
                # cumulative fork lengths
                  # plotted
                    # length values
    colour = "black") +
      # bar outline color
    geom_point(shape=".")
      # small data points
                      
# histograms
    # STB fork length counts for spring (2018 - 2021)
    1 + 3.322*log10(487) 
      # = 9.927971, so use 10 bins
      # Sturge's Rule (Rule of Thumb)
        # for number of bins to sort data by
  ggplot(spring, aes(x=Forklength)) + 
    # plot fork length counts
  geom_histogram() +
  stat_bin(bins = 487) + 
    # 10 bins recommended, but 487 bins 
      # show each fish
  
# box plot 
  # (makes the most sense for the data)
  ggplot(spring, 
    aes(SampleDate, Forklength)) + 
    geom_boxplot(aes(group = 
    cut_width(Forklength, 487))) +
    # consider splitting a separate variable
      # ~ 14 factor levels
    geom_point(shape = ".") +
    ggtitle("Fork lengths of striped bass sampled in spring on the Stanislaus River") +
    xlab("Spring") +
    ylab("Fork length (mm)") +
    scale_x_date(date_labels = "%b %Y",
      date_breaks = "1 month") +
    theme(axis.text.x=element_text(angle=60, 
    hjust=1))
    # failed to add specific sampling dates to
      # the x-axis label in lines (231-253)
        # c("2018-06-06",
        # "2018-06-07",
        # "2018-06-12",
        # "2019-06-13",
        # "2019-03-11",
        # "2019-03-12",
        # "2019-04-01",
        # "2019-04-02",
        # "2019-04-03",
        # "2019-04-04",
        # "2019-04-08",
        # "2019-04-09",
        # "2019-04-29",
        # "2019-05-01",
        # "2019-05-02",
        # "2019-05-03",
        # "2019-05-06",
        # "2019-05-07",
        # "2019-05-30",
        # "2019-05-31",
        # "2019-06-03",
        # "2019-06-04",
        # "2019-06-05"))

# linear regression of total length
ggplot(spring, 
  aes(x = SamplingUnitNo,
  # independent variable
  y = Forklength)) +
  # dependent variable
  geom_point(shape = ".") +
  # default settings for data points
  stat_smooth(method = "lm",
  # model a linear regression
  col = "blue")

#___________________________________________
# data visuals for annual 
  # striped bass(STB) fork lengths (FL)
    # across all seasons
# bar plot
  ggplot(dat3, 
       aes(x = SampleDate, 
           y = Forklength,
           fill = SampleYear)) +
  geom_bar(stat = "summary_bin",
           width = 0.1,
           colour = "black") +
  ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2018-2021)") +
    ylab("Fork length (mm)") +
    xlab("Sample date (month)") +
  labs(fill = "Sample year") +
  geom_point(shape = 21, fill = "grey") +
  scale_x_date(date_labels = "%m-%Y",
               date_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, 
                                hjust=1))
  
# violin plot
  # form separate dataframes for 
    # each sampling year
  STB18 <- 
    filter(dat3,SampleYear == "2018")
  STB19 <-
    filter(dat3,SampleYear == "2019")
    # remove the row with NAs for "Forklength"
    STB19 <- STB19[-166,]
  STB20 <-
    filter(dat3,SampleYear == "2020")
  STB21 <-
    filter(dat3,SampleYear == "2021")
  
  # determine number of sampled STB for each year
  sum(STB18$Count) # 11
  sum(STB19$Count) # 278
  sum(STB20$Count) # 154
  sum(STB21$Count) # 150
  
  # 2018 - 2021 aggregated by sampling year
  ggplot(dat3, 
         aes(x = SampleYear, 
             y = Forklength,
             fill = SampleYear)) +
    geom_violin(fill = "light grey") +
    geom_boxplot(width = 0.1) +
    ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2018 - 2021)") +
    theme_classic2() +
    labs(fill = "") +
    theme(legend.position="bottom") +
    xlab("Sampling year") +
    ylab("Fork length (mm)") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_y_continuous(breaks =
          c(100, 200, 300, 400, 500, 600, 700,
            800, 900, 1000, 1100, 1200),
          limits = c(100, 1200)) +
    geom_text(x= 1, y= 1150, 
              label="N = 11", size = 3) +
    geom_text(x= 2, y= 1150, 
            label="N = 278", size = 3) +
    geom_text(x= 3, y= 1150, 
              label="N = 150", size = 3) +
    geom_text(x= 4, y= 1150, 
              label="N = 154", size = 3)
  
  # 2019 - 2021 aggregated by sampling year
  dat19to21 <- dat3[-c(1:11),]
  ggplot(dat19to21, 
         aes(x = SampleYear, 
             y = Forklength,
             fill = SampleYear)) +
    geom_violin(fill = "light grey") +
    geom_boxplot(width = 0.1) +
    ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2019 - 2021)") +
    theme_classic() +
    labs(fill = "") +
    theme(legend.position="bottom") +
    xlab("Sampling year") +
    ylab("Fork length (mm)") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_y_continuous(breaks =
              c(100, 200, 300, 400, 500, 600, 700,
              800, 900, 1000, 1100, 1200),
              limits = c(100, 1200)) +
    geom_text(x= 1, y= 1150, 
              label="N = 278", size = 3) +
    geom_text(x= 2, y= 1150, 
              label="N = 150", size = 3) +
    geom_text(x= 3, y= 1150, 
              label="N = 154", size = 3)
  
  # create dataframe for each month
  march19 <-
    filter(dat3,SampleMonth == "03/2019")
  april19 <-
    filter(dat3,SampleMonth == "04/2019")
  may19 <-
    filter(dat3,SampleMonth == "05/2019") 
  june19 <-
    filter(dat3,SampleMonth == "06/2019")
  feb20 <-
    filter(dat3,SampleMonth == "02/2020")
  march20 <-
    filter(dat3,SampleMonth == "03/2020")
  may20 <-
    filter(dat3,SampleMonth == "05/2020")
  june20 <-
    filter(dat3,SampleMonth == "06/2020")
  feb21 <-
    filter(dat3,SampleMonth == "02/2021")
  march21 <-
    filter(dat3,SampleMonth == "03/2021")
  april21 <-
    filter(dat3,SampleMonth == "04/2021")
  may21 <-
    filter(dat3,SampleMonth == "05/2021")
  
  # sum STB counts for each month of sampling
  sum(march19$Count) # 10
  sum(april19$Count) # 36
  sum(may19$Count) # 68
  sum(june19$Count) # 164
  sum(feb20$Count) # 44
  sum(march20$Count) # 20
  sum(may20$Count) # 32
  sum(june20$Count) # 58
  sum(feb21$Count) # 2
  sum(march21$Count) # 8
  sum(april21$Count) # 42
  sum(may21$Count) # 98
  
  # 2018 - 2021 aggregated by sampling month
    ggplot(dat3, 
        aes(x = reorder(SampleMonth,
                         SampleDate), 
             y = Forklength,
             fill = SampleMonth)) +
        theme(legend.position = "none") +
    geom_violin(aes(group = SampleMonth),
                fill = "light grey") +
    geom_boxplot(width = 0.1) +
    ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2018 - 2021)") +
    scale_y_continuous(breaks =
        c(100, 200, 300, 400, 500, 600, 700,
          800, 900, 1000,1100, 1200, 1300,
          1400, 1500, 1600, 1700),
        limits = c(100, 1200)) +
    theme_classic() +
    labs(fill = "") +
    theme(legend.position="bottom") +
    scale_fill_discrete(breaks = 
        c("06/2018", "03/2019", "04/2019",
          "05/2019", "06/2019", "02/2020",
          "03/2020", "05/2020", "06/2020",
          "02/2021", "03/2021", "04/2021", 
          "05/2021")) +
    theme(axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    xlab("Sample month") +
    ylab("Fork length (mm)") +
    geom_text(x= 1, y= 1150, 
              label="N = 11", size = 3) +
    geom_text(x= 2, y= 1150, 
              label="N = 10", size = 3) +
    geom_text(x= 3, y= 1150, 
              label="N = 36", size = 3) +
    geom_text(x= 4, y= 1150, 
              label="N = 68", size = 3) +
    geom_text(x= 5, y= 1150, 
              label="N = 164", size = 3) +
    geom_text(x= 6, y= 1150, 
              label="N = 44", size = 3) +
    geom_text(x= 7, y= 1150, 
              label="N = 20", size = 3) +
    geom_text(x= 8, y= 1150, 
              label="N = 32", size = 3) +
    geom_text(x= 9, y= 1150, 
              label="N = 58", size = 3) +
    geom_text(x= 10, y= 1150, 
              label="N= 2", size = 3) +
    geom_text(x= 11, y= 1150, 
              label="N = 8", size = 3) +
    geom_text(x= 12, y= 1150, 
              label="N = 42", size = 3) +
    geom_text(x= 13, y= 1150, 
              label="N = 98", size = 3)
    
  # 2018
    STB18 <- filter(dat3, SampleYear == "2018")
    ggplot(STB18, 
           aes(x = SampleMonth, 
               y = Forklength)) +
      geom_violin(aes(group = SampleMonth),
                  fill = "light grey") +
      geom_boxplot(width = 0.1,
                   aes(group = SampleDate)) +
      # geom_dotplot(binaxis='y', 
                   # stackdir='center', 
                   # dotsize=1) +
      ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2018)") +
      xlab("Sample date (month)") +
      ylab("Fork length (mm)")
    
  # 2019
    STB19 <- filter(dat3, 
            SampleYear == "2019")
    ggplot(STB19, 
           aes(x = SampleMonth, 
               y = Forklength)) +
      geom_violin(aes(group = SampleMonth),
                  fill = "light grey") +
      geom_boxplot(width = 0.1) +
      # geom_dotplot(binaxis='y', 
                   # stackdir='center', 
                   # dotsize=1) +
      ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2019)") +
      xlab("Sample date (month)") +
      ylab("Fork length (mm)")
  
  # 2020
    STB20 <- filter(dat3, 
            SampleYear == "2020")
    ggplot(STB20, 
           aes(x = SampleMonth, 
               y = Forklength)) +
      geom_violin(aes(group = SampleMonth),
                  fill = c("light grey")) +
      geom_boxplot(width = 0.1) +
      # geom_dotplot(binaxis='y', 
          # stackdir='center', 
          # dotsize=1) +
      ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2020)") +
      xlab("Sample date (month)") +
      ylab("Fork length (mm)")
  
  # 2021
    STB21 <- filter(dat3, 
                    SampleYear == "2021")
    ggplot(STB21, 
           aes(x = SampleMonth, 
               y = Forklength)) +
      geom_violin(aes(group = SampleMonth),
                  fill = "light grey") +
      geom_boxplot(width = 0.1) +
      # geom_dotplot(binaxis='y', 
        # stackdir='center', 
        # dotsize=1) +
      ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2021)") +
      xlab("Sampling year") +
      ylab("Fork length (mm)")
    
# histogram
  1 + 3.322*log10(593) 
  # = 10.21209, so use 10 bins
    # Sturge's Rule (Rule of Thumb)
      # for number of bins to sort data by
  # ggplot 2
  # hist<-
    ggplot(dat3, aes(x=Forklength)) + 
    geom_histogram(color = "black", 
        fill = "white",
        bins = 120) + 
        # default 30 bins or more makes the plot 
          # show detail that reflects fork lengths
            # 10 bins are recommended, according
              # to Sturge's Rule
    scale_x_continuous(breaks = 
                seq(0,1200,by=50)) +
    scale_y_continuous(breaks = 
                seq(0,32,by=1)) +
    theme_classic() +
    ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2018 - 2021)")+
    ylab("Counts (individuals)") +
    xlab("Fork length (mm)")
  
    # extract data from ggplot
    hist_values <- ggplot_build(hist)
  
  # base R
  hist(dat3$Forklength,
       main = "Striped bass sampled on the Stanislaus River via electrofishing (2018 - 2021)",
       xlab = "Fork length (mm)",
       xlim = c(150,1200),
       xaxp = c(150,1100,10),
       ylim = c(0,30),
       yaxp = c(0,30,1),
       breaks = seq(0, 1090, 50),
       ylab = "Counts (individuals)",
       col = "white")
  
  # point plot of fork lengths by date
    # ggplot
    # remove row with NA fork length
    dat4 <- dat3[-177, ]
    ggplot(dat4, 
         aes(x = SampleDate, 
             y = Forklength,
             fill = SampleYear)) +
    scale_y_continuous(breaks = seq(0,1250,by=50)) +
    scale_x_date(date_labels = "%m/%Y",
                 date_breaks = "1 month") +
    # scale_x_date(breaks = dat3$SampleDate) +
    theme_classic() +
    theme(axis.text.x =
          element_text(angle = 60, 
                       hjust=1,
                       size = 8,
                       margin = margin(l = 50))) +
    geom_point(shape = 21, alpha = 0.2) +
    ggtitle("Striped bass sampled on the Stanislaus River via electrofishing (2018 - 2021)") +
    ylab("Fork length (mm)") +
    xlab("Sampling days") +
    labs(fill = "Year")
    
#____________________________________________
# ANOVA of STB FL across sampling 
    # events from 2018 to 2021
  
  aov(Forklength ~ SampleDate, dat3)
    # not the best analysis for STB FL
      # since they are discrete and do not
        # resemble a normal distribution
          # based on the Anderson-Darling Test 
            # of STB FL's
  
#________________________________________________
# Comparing Observed Distribution to 
    # Empirical Ones
      
        # Anderson-Darling Test
          # install.packages('nortest')
          library(nortest)
          ad.test(dat3$Forklength)
          # probability value: 2.2e-16
            # sample distribution far from
              # the normal distribution
                # that the test assumes
        
        # Kolmogorov-Smirnoff Test
          # install.packages("dgof")
          library(dgof)
          dgof::ks.test(x = FLs,
                        y = "ppois",
                        alternative = "two.sided",
                        lambda = 1)
  
# Compare sample distribution to
    # negative binomial and Poisson
      # distributions
  
  FLs <- dat3$Forklength
  str(FLs) # number with 593 values
  FLs <- as.vector(FLs)
  
  # install.packages('fitdistrplus')
  library(fitdistrplus)
  fitdistr(FLs, "pois")

# compare sizes of striped bass b/w seasons
    mean(spring18$Forklength) # 317.5455
    mean(march19$Forklength) # 469 mm
    mean(april19$Forklength) # 447.0556
    mean(may19$Forklength) # 374.8235
    mean(june19$Forklength) # 
    mean(feb20$Forklength) # 456 mm
    mean(march20$Forklength) # 546.5 mm
    mean(may20$Forklength) # 419.4062 mm
    mean(june20$Forklength) # 410.5172 mm
    mean(feb21$Forklength) # 406 mm
    mean(march21$Forklength) # 433.375 mm
    mean(april21$Forklength) # 447.7381 mm
    mean(may21$Forklength) # 276.051 mm
  
  # prepare data for tests
    # need more sample points than the means for
    # a one-way permutation test, so gather the
    # data into groups
      june18FL <- spring18$Forklength
      march19FL <- march19$Forklength
      april19FL <- april19$Forklength
      may19FL <- may19$Forklength
      june19FL <- june19$Forklength
      feb20FL <- feb20$Forklength
      march20FL <- march20$Forklength
      may20FL <- may20$Forklength
      june20FL <- june20$Forklength
      feb21FL <- feb21$Forklength
      march21FL <- march21$Forklength
      april21FL <- april21$Forklength
      may21FL <- may21$Forklength
  
      # combine the fork length data into one
        # vector for analyses
      fork_lengths <- c(june18FL, march19FL, 
            april19FL, may19FL, june19FL, feb20FL,
            march20FL, may20FL, june20FL,
            feb21FL, march21FL, april21FL,may21FL)
      
      # align each fork length with
        # months for each observation
      june18reps <- rep("june 2018", times =11)
      march19reps <- rep("march 2019", times = 10)
      april19reps <- rep("april 2019", times = 36)
      may19reps <- rep("may 2019", times = 68)
      june19reps <- rep("june 2019", times = 164)
      feb20reps <- rep("februaru 2020", times = 44)
      march20reps <- rep("march 2020", times = 20)
      may20reps <- rep("may 2020", times = 32)
      june20reps <- rep("june 2020", times = 58)
      feb21reps <- rep("february 2021", times = 2)
      march21reps <- rep("march 2021", times = 8)
      april21reps <- rep("april 2021", times = 42)
      may21reps <- rep("may 2021", times = 98)
      
      # align each fork length with years
        # for each observation
      reps18 <- rep("2018", times = 11)
      reps19 <- rep("2019", times = 278)
      reps20 <- rep("2020", times = 154)
      reps21 <- rep("2021", times = 150)
  
  months <- c(june18reps, march19reps, april19reps,
            may19reps, june19reps, feb20reps,
            march20reps, may20reps, june20reps,
            feb21reps,march21reps, april21reps, 
            may21reps)
  
  years <- c(reps18, reps19, reps20, reps21)

#_________________________________________________________
  # Kruskal-Wallis rank sum test
    # justification for Kruskal-Wallis Test followed
      # by Mann-Whitney U Test (next section)
      # http://www.biostathandbook.com/kruskalwallis.html
    
    # test fork lengths by date for
      # 2018 - 2021 (all years ranked)
      ks_18to21 <-
        kruskal.test(fork_lengths, # values to be tested
                     months) # ranking for test values
    
    # create dataframe for fork lengths by 
      # all sample months and all sample years
      # so that fork lengths between specific sample
      # years can be tested against one another
      FLdat <- 
        data.frame("Forklength" = fork_lengths,
                   "SampleMonth" = months,
                   "SampleYear" = years)
      # create separate dataframes for fork length
        # by sample month for each year
      FLdat18 <-
        filter(FLdat, SampleYear == "2018")
      
      FLdat19 <-
        filter(FLdat, SampleYear == "2019")
      
      FLdat20 <-
        filter(FLdat, SampleYear == "2020")
      
      FLdat21 <-
        filter(FLdat, SampleYear == "2021")
    
    # test fork length by date for 2018
      ks_19 <-
        kruskal.test(FLdat19$Forklength,
                   FLdat19$SampleMonth)
      
    # test fork length by date for 2018
      ks_20 <-
        kruskal.test(FLdat20$Forklength,
                     FLdat20$SampleMonth)
      
    # test fork length by date for 2018
      ks_21 <-
        kruskal.test(FLdat21$Forklength,
                     FLdat21$SampleMonth)
#________________________________________________________
  # Wilcoxon-Mann-Whitney U Test
    
    # must use exactly two factors to test
      # differences
        # determine if variance are equal
          sd((march19FL)^2) # 132054.4
          sd((april19FL)^2) # 100885.7
        # perform test
          wilcox.test(march19FL, april19FL,
                  alternative = "two.sided")
                  # W = 186.5, p = 0.873
          wilcox.test(march19FL, april19FL,
                  alternative = "greater")
                  # W = 186.5, p = 0.4365
          
          # determine if variance are equal
            sd((april19FL)^2) # 100885.7
            sd((may19FL)^2) # 192461.3
          # perform test
            wilcox.test(april19FL, may19FL,
                  alternative = "two.sided")
                  # W = 1687, p = 1.576e-3
            wilcox.test(april19FL, may19FL,
                  alternative = "greater")
                  # W = 1687, p = 7.88e-4
          
          # determine if variance are equal
            sd((may19FL)^2) # 192461.3
            sd((june19FL)^2) # 112118.7
          # perform test
            wilcox.test(may19FL, june19FL,
                  alternative = "two.sided")
                  # W = 6918, p = 2.963e-3
            wilcox.test(may19FL, june19FL,
                  alternative = "greater")
                  # W = 6918, p = 1.482e-3
            
          # determine if variance are equal
            sd((feb20FL)^2) # 162094.3
            sd((march20FL)^2) # 258760.5
          # perform test
              wilcox.test(feb20FL, march20FL,
                  alternative = "two.sided")
                  # W = 286, p = 2.616e-2
              wilcox.test(feb20FL, march20FL,
                  alternative = "greater")
                  # W = 286, p = 0.9874
          
            # determine if variance are equal
              sd((march20FL)^2) # 258760.5
              sd((may20FL)^2) # 141787.7
            # perform test
              wilcox.test(march20FL, may20FL,
                  alternative = "two.sided")
                  # W = 474.5, p = 3.765e-3
            wilcox.test(march20FL, may20FL,
                  alternative = "greater")
                  # W = 474.5, p = 1.882e-3
            
            # determine if variance are equal
              sd((may20FL)^2) # 141787.7
              sd((june20FL)^2) # 143486.3
            # perform test
              wilcox.test(may20FL, june20FL,
                  alternative = "two.sided")
                  # W = 1006, p-value = 0.5136
              wilcox.test(may20FL, june20FL,
                  alternative = "greater")
                  # W = 1006, p-value = 0.2568
          
            # determine if variance are equal
              sd((june20FL)^2) # 
              sd((feb21FL)^2) # 
            # perform test
              wilcox.test(june20FL, feb21FL,
                  alternative = "two.sided")
                  # W = 50, p-value = 0.7574
              wilcox.test(june20FL, feb21FL,
                  alternative = "greater")
                  # W = 50, p = 0.6369
             
              # determine if variance are equal
                sd((feb21FL)^2) # 62010.44
                sd((march21FL)^2) # 43755.05
              # perform test
                wilcox.test(feb21FL, march21FL,
                  alternative = "two.sided")
                  # W = 6, p = 0.7111
                wilcox.test(feb21FL, march21FL,
                  alternative = "greater")
                  # W = 6, p = 0.7333
              
                # determine if variances are equal
                  sd((march21FL)^2) # 43755.05
                  sd((april21FL)^2) # 142416.3
                # perform test
                  wilcox.test(march21FL, april21FL,
                    alternative = "two.sided")
                  # W = 147.5, p = 0.5966
                  wilcox.test(march21FL, april21FL,
                    alternative = "greater")
                  # W = 147.5, p = 0.7108
                # determine if variances are equal
                  sd((april21FL)^2) # 142416.3
                  sd((may21FL)^2) # 81252.63 
                # perform test
                  wilcox.test(april21FL, may21FL,
                    alternative = "two.sided")
                  # W = 3385.5, p = 1.594e-09
                  wilcox.test(april21FL, may21FL,
                    alternative = "greater")
                  # W = 3385.5, p = 7.97e-10
      
  # use prior dataframes for fork lengths by sample
      # months to pool together into year portions
      # for larger sample sizes needed for 
      # Mann-Whitney U Test.
      # Want groups of 30 or more samples, so
      # combine months that are early in the
      # year together and months that are late
      # in the year together.
    
    
  # 2018
    late18FLs <-
      june18FL
    late18months <-
      rep("June 2018", times = 11)
    late18 <-
      data.frame("Forklengths" = late18FLs,
                "SampleMonths" = late18months)
  
  # 2019
    early19FLs <- 
      c(march19FL, april19FL)
    early19months <- 
      c(rep("March 2019", times = 10),
        rep("April 2019", times = 36))
    early19 <-
      data.frame("Forklengths" = early19FLs,
        "SampleMonths" = early19months)
    
    late19FLs <-
      c(may19FL, june19FL)
    late19months <-
      c(rep("May 2019", times = 68),
        rep("June 2019", times = 164))
    late19 <-
      data.frame("Forklengths" = late19FLs,
                "SampleMonths" = late19months)
    
    # 2020
    early20FLs <- 
      c(feb20FL, march20FL)
    early20months <- 
      c(rep("February 2020", times = 44),
        rep("March 2020", times = 20))
    early20 <-
      data.frame("Forklengths" = early20FLs,
                 "SampleMonths" = early20months)
    
    late20FLs <-
      c(may20FL, june20FL)
    late20months <-
      c(rep("May 2020", times = 32),
        rep("June 2020", times = 58))
    late20 <-
      data.frame("Forklengths" = late20FLs,
                 "SampleMonths" = late20months)
    
    # 2021
    early21FLs <- 
      c(feb21FL, march21FL, april21FL)
    early21months <- 
      c(rep("February 2021", times = 2),
        rep("March 2021", times = 8),
        rep("April 2021", time = 42))
    early21 <-
      data.frame("Forklengths" = early21FLs,
                 "SampleMonths" = early21months)
    
    late21FLs <-
      c(may21FL)
    late21months <-
      c(rep("May 2020", times = 98))
    late21 <-
      data.frame("Forklengths" = late21FLs,
                 "SampleMonths" = late21months)
    
  # add column for early and late portions of each
    # year to "dat3"
    dat3$YearPortion <-
      c(rep("late sampling 2018", times = 11),
        rep("early sampling 2019", times = 46),
        rep("late sampling 2019", times = 232),
        rep("early sampling 2020", times = 64),
        rep("late sampling 2020", times = 90),
        rep("early sampling 2021", times = 52),
        rep("late sampling 2021", times = 98))
  
  # create a new dataframe to only plot desired 
    # portions of data
    dat4 <-
      dplyr::select(dat3, YearPortion, Forklength, SampleMonth, SampleYear)
    
    dat5 <- dat4[-c(1:11,177),] 
      # remove 2018 data since not a lot of sampling occurred that June
        # rows 1 - 11 & also the 1 "NA" fork length on row 177
    length(which(is.na(dat5$Forklength)))
      # no more NAs remain
  
  # create a violin plot to visualize data 
    # ggplot 
      ggplot(dat5,
          aes(x = reorder(YearPortion, Forklength),
              y = Forklength,
          fill = YearPortion)) +
      theme_classic() +
      geom_violin(aes(group = YearPortion),
                fill = "light grey") +
      geom_boxplot(width = 0.1) +
      labs(fill = "") +
      theme(legend.position="bottom") +
      theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
      scale_x_discrete(name ="Year portion", 
          limits = c("early 2019","late 2019",
          "early 2020", "late 2020",
          "early 2021", "late 2021")) +
      scale_fill_discrete(breaks = 
          c("early spring 2019", "late spring 2019", "early spring 2020", 
            "late spring 2020","early spring 2021", "late spring 2021")) +
      scale_y_continuous(limits = c(100, 1200),
                      breaks = seq(0,1250,by=50)) +
      geom_segment(aes(x = 1, xend = 2, 
                  y = 145, yend = 145)) +
      geom_text(x= 1, y= 1150, 
                    label="N = 46", size = 4) +
      geom_text(x= 2, y= 1150, 
                    label="N = 232", size = 4) +
      geom_text(x= 3, y= 1150, 
                    label="N = 64", size = 4) +
      geom_text(x= 4, y= 1150, 
                    label="N = 90", size = 4) +
      geom_segment(aes(x = 3, xend = 4, 
                  y = 145, yend = 145)) +
      geom_text(x= 5, y= 1150, 
                    label="N = 52", size = 4) +
      geom_text(x= 6, y= 1150, 
                    label="N = 98", size = 4) +
      geom_segment(aes(x = 5, xend = 6, 
                         y = 145, yend = 145)) +
      geom_text(x= 1.5, y= 100, 
                  label="Test A", size = 4) +
      geom_text(x= 3.5, y= 100, 
                  label="Test B", size = 4) +
      geom_text(x= 5.5, y= 100, 
                  label="Test C", size = 4) +
      ylab("Fork length (mm)") +
      ggtitle("Comparisons of fork length by grouped sample months (2019 - 2021)")
      

      
      # simplified boxplots by year from 2019 - 2021
        # 2019
        p1 <-
          ggplot(STB19,
             aes(x = reorder(YearPortion, SampleMonth),
                 y = Forklength,
                 fill = YearPortion)) +
        theme_classic() +
        geom_boxplot(width = 0.75) +
        labs(fill = "") +
        theme(legend.position="none",
              axis.title.x = element_blank()) +
        scale_y_continuous(limits = c(100, 1200),
                             breaks = seq(0,1250,by=100)) +
        ylab("Fork Length (mm)")
        
        # 2020
        p2 <-
          ggplot(STB20,
                 aes(x = reorder(YearPortion, SampleMonth),
                     y = Forklength,
                     fill = YearPortion)) +
          theme_classic() +
          geom_boxplot(width = 0.75) +
          labs(fill = "") +
          theme(legend.position="none",
                axis.title.x = element_blank()) +
          scale_y_continuous(limits = c(100, 1200),
                             breaks = seq(0,1250,by=100)) +
          ylab("Fork Length (mm)")
        
        # 2021
        p3 <-
          ggplot(STB21,
                 aes(x = reorder(YearPortion, SampleMonth),
                     y = Forklength,
                     fill = YearPortion)) +
          theme_classic() +
          geom_boxplot(width = 0.75) +
          labs(fill = "") +
          theme(legend.position="none",
                axis.title.x = element_blank()) +
          scale_y_continuous(limits = c(100, 1200),
                             breaks = seq(0,1250,by=100)) +
          ylab("Fork Length (mm)")
        
      # combine separate plots in one window
        library(gridExtra)
        grid.arrange(p1, p2, p3, nrow = 3)
              
  # ------- Histogram for manuscript results --------
    
  # Plot settings -----------------------------------------------------
    # plot histograms together
      par(mfrow = c(3,2)) # 2 plots for each year in one figure
      par(mar = c(3,3,1,1), # edit inner plot margin size
        oma = c(2,2,1,1)) # edit outer plot margin size
      # order of numbers = bottom, left, top, & right
              
     
    # set standardized spaces (breaks) between histogram bins
      breaks = seq(100, 1200, 25) # 100 to 1200 by 25 gives 40 mm FL bins
    # set standardized plot limits
      xlim = c(100, 1200)
      ylim = c(0, 40)         
    
  # Plots -------------------------------------------------------------
    # 2019
      # early sampling
        p1A <- 
          hist(early19$Forklengths, # data  
          breaks = breaks, # bin size = 44 = 1100/25
          xlim = xlim,
          ylim = ylim,
          yaxs = "i",
          xaxs = "i",
          xlab = "Counts",
          ylab = "Fork Length (mm)",
          ann = F)
        title("2019 early sampling", line = - 1)
      # late sampling
        p1B <- 
          hist(late19$Forklengths, # data  
          breaks = breaks, # bin size = 44 = 1100/25
          xlim = xlim,
          ylim = ylim,
          yaxs = "i",
          xaxs = "i",
          xlab = "Counts",
          ylab = "Fork Length (mm)",
          ann = F) 
        title("2019 late sampling", line = - 1)
      
    # 2020
      # early sampling
        p2A <- 
          hist(early20$Forklengths, # data 
          breaks = breaks, # bin size = 44 = 1100/25
          xlim = xlim,
          ylim = ylim,
          yaxs = "i",
          xaxs = "i",
          xlab = "Counts",
          ylab = "Fork Length (mm)",
          ann = F)
        title("2020 early sampling", line = -1)
        
      # late sampling
        p2B <- 
          hist(late20$Forklengths, # data 
          breaks = breaks, # bin size = 44 = 1100/25
          xlim = xlim,
          ylim = ylim,
          yaxs = "i",
          xaxs = "i",
          xlab = "Counts",
          ylab = "Fork Length (mm)",
          ann = F) 
        title("2020 late sampling", line = -1)
        
    # 2021
      # early sampling
        p3A <- 
          hist(early21$Forklengths, # data 
          breaks = breaks, # bin size = 44 = 1100/25
          xlim = xlim,
          ylim = ylim,
          yaxs = "i",
          xaxs = "i",
          xlab = "Counts",
          ylab = "Fork Length (mm)",
          ann = F) 
        title("2021 early sampling", line = -1)
        
      # late sampling
        p3B <- 
          hist(late21$Forklengths, # data 
          breaks = breaks, # bin size = 44 = 1100/25
          xlim = xlim,
          ylim = ylim,
          yaxs = "i",
          xaxs = "i",
          xlab = "Counts",
          ylab = "Fork Length (mm)",
          ann = F) 
        title("2021 late sampling", line = -1)
        
    # add axes titles
      mtext("Fork Length (mm)", side = 1, line = 0, outer = TRUE, cex = 1)
      mtext("Counts", side = 2 , line = 0, outer = TRUE, 
            cex = 1, las = 0)
            
# --------------------------------------------------------------------------      
# Mann-Whitney U Tests for 2019 - 2021
  # 2019
    # determine if variance are equal
      sd((early19$Forklengths)^2) # 107177
      sd((late19$Forklengths)^2) # 1044073
    # perform test
      wilcox.test(early19$Forklengths, 
                late19$Forklengths,
                alternative = "two.sided")
    # W = 8522.5, p = 9.932e-11
    wilcox.test(early19$Forklengths,
                late19$Forklengths,
                alternative = "greater")
    # W = 8522.5, p = 4.966e-11
    
  # 2020
    # determine if variance are equal
      sd((early20$Forklengths)^2) # 201502
      sd((late20$Forklengths)^2) # 142130.3
    # perform tests
      wilcox.test(early20$Forklengths,
                late20$Forklengths,
                alternative = "greater")
      # W = 3916, p = 7.333e-05
    
  # 2021 
      # determine if variance are equal
        sd((early21$Forklengths)^2) # 129915.6
        sd((late21$Forklengths)^2) # 81252.63
      # perform tests
        wilcox.test(early21$Forklengths,
                  late21$Forklengths,
                  alternative = "greater")
      # W = 4275, p = 4.604e-12

#_________________________________________________________
  # Uneeded Code

  datA<-distinct(dat3,PITTagNo)
    nrow(datA) 
    # 551 distinct PIT tags in dat3
    593 - 551
    # 42 duplicate tags (recaptures?)
  datB<-duplicated(dat3$PITTagNo) 
    # select duplicate PIT tag numbers
      # all of the values are returned
      # as "FALSE" rather than tag IDs

  datC<- dat3 %>% 
    distinct(PITTagNo, 
    .keep_all = TRUE)
    # select only unique PIT tag values
  
  datD<-dat3 %>% 
    distinct(SamplingUnitNo,
    .keep_all = TRUE)
  
  #___________________________________________
  # create preliminary data visualizations
  m1 <- lm(Forklength~SampleDate, dat3) 
  # linear regression of STB fork 
  # length by pooled sampling dates
  # across annual sampling event
  summary(m1) 
  # low R-squared values 
  # & high probability values
  
  library(ggplot2)
  # load package for plotting model
  
  ggplot(dat3, 
         aes(x = SampleDate,
             # independent variable
             y = Forklength)) +
    # dependent variable
    geom_point(shape = ".") + 
    # default settings for data points
    stat_smooth(method = "lm", 
                # model a linear regression
                col = "blue")
  # w/ the specified color of the line
  
  # data not effectively shown with 
  # a linear regression,
  # likely b/c the independent data 
  # is discrete & ordinal rather than
  # continuous
  
# Jonkeere-Terpstra test
  # install.packages("clinfun")
  library(clinfun)
  jonckheere.test(fork_lengths,
                  dates,
                  alternative = "two.sided")
  
  # --------------------------------------------------------------------
  # Dual histograms
  # ggplot
  library(ggplot2)
  
  # 2019 
  p1 <-
    ggplot(STB19,
           aes(x = Forklength)) +
    geom_histogram(position = "identity", alpha = 0.2, binwidth = 25) +
    geom_density(aes(y = ..count..*25, fill = YearPortion), alpha = 0.2)
  
  # 2020
  p2 <-
    gplot(STB20,
          aes(x = Forklength, 
              fill = YearPortion)) +
    geom_density(alpha = 0.3) +
    scale_x_continuous(limits = c(100, 1200),
                       breaks = seq(0,1250,by=50)) +
    scale_y_continuous(limits = c(0, 0.0075)) +
    theme_classic() +
    theme(axis.title.x = element_blank()) +
    ylab("Sample density") +
    scale_fill_discrete(name="",
                        breaks=c("early spring 2020", "late spring 2020"),
                        labels=c("Early spring 2020", "Late spring 2020"))
  
  # 2021
  p3 <-
    ggplot(STB21,
           aes(x = Forklength, 
               fill = YearPortion)) +
    geom_density(alpha = 0.3) +
    scale_x_continuous(limits = c(100, 1200),
                       breaks = seq(0,1250,by=50)) +
    scale_y_continuous(limits = c(0, 0.0075)) +
    theme_classic() +
    ylab("Sample density") +
    xlab("Fork length (mm)") +
    scale_fill_discrete(name="",
                        breaks=c("early spring 2021", "late spring 2021"),
                        labels=c("Early spring 2021", "Late spring 2021"))
  
  # combine separate plots in one window
  library(gridExtra)
  grid.arrange(p1, p2, p3, nrow = 3)
  
  # base R
  
  # Plot settings
  # plot histograms together
  par(mfrow = c(3,1)) # 3 plots for each year in one figure
  # plot window
  par(mar = c(1,1,1,1), # edit plot side size
      oma = c(5,5,1,1)) # edit plot margin size
  # order of numbers = bottom, left, top, & right
  
  # use opposing colors for each annual distribution
  c1 <- rgb(216,179,101, max = 255, alpha = 80, names = "tan")
  c2 <- rgb(90,180,172, max = 255, alpha = 80, names = "turquoise")
  # set standardized spaces (breaks) between histogram bins
  breaks = seq(0,1100,25) # 0 to 1100 by 25
  # set standardized plot limits
  xlim = c(0, 1100)
  ylim = c(0, 0.01)
  
  # create dataframes for each histogram, review fork lengths, & plot
  # 2019
  earlyspring19 <- 
    filter(dat5, YearPortion == "early spring 2019")
  # view data characteristics to know how to plot it
  length(unique(earlyspring19$Forklength)) 
  # 38 unique fork lengths
  summary(earlyspring19$Forklength) 
  # ranges from 166 to 720 mm
  length(earlyspring19$Forklength) 
  # 46 observations
  
  latespring19 <- 
    filter(dat5, YearPortion == "late spring 2019")
  # view data characteristics to know how to plot it
  length(unique(latespring19$Forklength)) 
  # 153 unique fork lengths
  summary(latespring19$Forklength) 
  # ranges from 165 to 1060 mm
  length(latespring19$Forklength) 
  # 231 observations
  
  # Plot 1st histogram
  p1A <- 
    hist(earlyspring19$Forklength, # data 
         prob = T, # plot densities (count frequencies) 
         breaks = breaks, # bin size = 44 = 1100/25
         col = c1, # tan(ish) colour
         axes = F,
         ylim = ylim,
         yaxs = "i",
         xaxs = "i",
         ann = F)
  
  # add custom axes
  # x-axis
  axis(side = 1, # specifies x-axis
       at = seq(0, 1200, 100), # tick marks
       labels = F)
  # add label in bottom plot margin
  mtext(side = 1, line = 2, outer = T, "Fork Length (mm)")
  # add label for each year
  mtext(side = 3, line = -2, 2019, cex = 0.9, adj = 0.05)
  # y-axis
  axis(side = 2, # specifies y-axis
       at = seq(0, 0.01, length.out = 6), # tick marks 
       labels = seq(0, 100, length.out = 6), las = 1) # tick mark labels
  # add label in left plot margin
  mtext(side = 2, line = 2, outer = T, "Frequency")
  
  # add kernel density line that smooths 1st histogram
  lines(density(earlyspring19$Forklength), # density
        lwd = 2, # line thickness
        col = "orange") # match 1st histogram color
  
  # Plot & add 2nd histogram to 1st
  p1B <- 
    hist(latespring19$Forklength, 
         prob = T,
         breaks = breaks, 
         col = c2, # turqouis(ish) color
         add = T, # add to prior plot
         axes = F,
         ylim = ylim,
         ann = F)
  lines(density(latespring19$Forklength), # density
        lwd = 2, # line thickness
        col = "blue") # match 2nd histogram color
  # Add kernel density line that smooths 2nd histogram
  
  # add legend to the plot
  legend(450, 0.01, # x, y location
         legend = c("Early spring: February - March", 
                    "Late spring: April - June"), # legend text
         lty = 1:1, # type of lines
         col = c("orange", "blue"), # color of lines
         cex = 1.5, # increases legend size
         bty = "n", # no border
         y.intersp = 0.09, # specify space between lines
         x.intersp = 0.1)
  
  # 2020
  earlyspring20 <- 
    filter(dat5, YearPortion == "early spring 2020")
  # view data characteristics to know how to plot it
  length(unique(earlyspring20$Forklength)) 
  # 52 unique fork lengths
  summary(earlyspring20$Forklength) 
  # ranges from 207 to 1090 mm
  length(earlyspring20$Forklength) 
  # 64 observations
  
  latespring20 <- 
    filter(dat5, YearPortion == "late spring 2020")
  # view data characteristics to know how to plot it
  length(unique(latespring20$Forklength)) 
  # 76 unique fork lengths
  summary(latespring20$Forklength) 
  # ranges from 194 to 915 mm
  length(latespring20$Forklength) 
  # 90 observations
  
  # Plot 1st histogram
  p2A <- 
    hist(earlyspring20$Forklength, # data 
         prob = T, # plot densities (count frequencies) 
         breaks = breaks, # bin size = 44 = 1100/25
         col = c1, # tan(ish) colour
         axes = F,
         ylim = ylim,
         yaxs = "i",
         xaxs = "i",
         ann = F)
  # add custom axes
  axis(side = 1, # x-axis
       at = seq(0, 1200, 100), # tick marks
       labels = F)
  # y-axis
  axis(side = 2, # specifies y-axis
       at = seq(0, 0.01, length.out = 6), # tick marks 
       labels = seq(0, 100, length.out = 6), las = 1) # tick mark labels
  # add label for 2020
  mtext(side = 3, line = -2, 2020, cex = 0.9, adj = 0.05)
  
  # Add kernel density line that smooths 1st histogram
  lines(density(earlyspring20$Forklength), # density
        lwd = 2, # line thickness
        col = "orange") # match 1st histogram color
  
  # Plot & add 2nd histogram to 1st
  p2B <- 
    hist(latespring20$Forklength, 
         prob = T,
         breaks = breaks, 
         col = c2, # turquoise(ish) color
         add = T, # add to prior plot
         axes = F,
         ann = F)
  
  # Add kernel density line that smooths 2nd histogram
  lines(density(latespring20$Forklength), # density
        lwd = 2, # line thickness
        col = "blue") # match 2nd histogram color
  
  # 2021
  earlyspring21 <- 
    filter(dat5, YearPortion == "early spring 2021")
  # view data characteristics to know how to plot it
  length(unique(earlyspring21$Forklength)) 
  # 48 unique fork lengths
  summary(earlyspring21$Forklength) 
  # ranges from 172 to 880 mm
  length(earlyspring21$Forklength) 
  # 52 observations
  
  latespring21 <- 
    filter(dat5, YearPortion == "late spring 2021")
  # view data characteristics to know how to plot it
  length(unique(latespring21$Forklength)) 
  # 72 unique fork lengths
  summary(latespring21$Forklength) 
  # ranges from 173 to 795 mm
  length(latespring21$Forklength) 
  # 98 observations
  
  # Plot 1st histogram
  p3A <- 
    hist(earlyspring21$Forklength, # data 
         prob = T, # plot densities (count frequencies) 
         breaks = breaks, # bin size = 44 = 1100/25
         col = c1, # tan(ish) colour
         axes = F,
         ylim = ylim,
         yaxs = "i",
         xaxs = "i",
         ann = F)
  # add label for 2021
  mtext(side = 3, line = -2, 2021, cex = 0.9, adj = 0.05)
  
  # add custom axes
  axis(side = 1, # x-axis
       at = seq(0, 1200, 100), # tick marks
       labels = seq(0, 1200, 100)) # tick mark labels
  # y-axis
  axis(side = 2, # specifies y-axis
       at = seq(0, 0.01, length.out = 6), # tick marks 
       labels = seq(0, 100, length.out = 6), las = 1) # tick mark labels
  
  # Add kernel density line that smooths 1st histogram
  lines(density(earlyspring21$Forklength), # density
        lwd = 2, # line thickness
        col = "orange") # match 1st histogram color
  
  # Plot & add 2nd histogram to 1st
  p3B <- 
    hist(latespring21$Forklength, 
         prob = T,
         breaks = breaks, 
         col = c2, # turqouis(ish) color
         add = T, # add to prior plot
         axes = F,
         ann = F)
  # Add kernel density line that smooths 2nd histogram
  lines(density(latespring21$Forklength), # density
        lwd = 2, # line thickness
        col = "blue") # match 2nd histogram color
  
  # density plot of striped bass by fork length
  # 2019 - 2021
  ggplot(dat5,
         aes(x = Forklength, 
             fill = YearPortion)) +
    geom_density(alpha = 0.3) +
    scale_y_continuous(limits = c(100, 1200),
                       breaks = seq(0,1250,by=100))
  # 2019
  # select 2019 data
  STB19 <- filter(dat3, SampleYear == "2019")
  # add column for portion of sampling year
  STB19$YearPortion <- 
    c(rep("early spring 2019", times = 46),
      rep("late spring 2019", times = 231))
  # plot
  p1 <-
    ggplot(STB19,
           aes(x = Forklength, 
               fill = YearPortion)) +
    geom_density(alpha = 0.3) +
    scale_x_continuous(limits = c(100, 1200),
                       breaks = seq(0,1250, by=50)) +
    scale_y_continuous(limits = c(0, 0.0075)) +
    theme_classic() +
    theme(axis.title.x = element_blank()) +
    ylab("Sample density") +
    scale_fill_discrete(name="",
                        breaks=c("early spring 2019", "late spring 2019"),
                        labels=c("Early spring 2019", "Late spring 2019"))
  
  # 2020
  # select 2020 data
  STB20 <- filter(dat3, SampleYear == "2020")
  # add column for portion of sampling year
  STB20$YearPortion <-
    c(rep("early spring 2020", times = 64),
      rep("late spring 2020", times = 90))
  # plot
  p2 <-
    ggplot(STB20,
           aes(x = Forklength, 
               fill = YearPortion)) +
    geom_density(alpha = 0.3) +
    scale_x_continuous(limits = c(100, 1200),
                       breaks = seq(0,1250,by=50)) +
    scale_y_continuous(limits = c(0, 0.0075)) +
    theme_classic() +
    theme(axis.title.x = element_blank()) +
    ylab("Sample density") +
    scale_fill_discrete(name="",
                        breaks = c("early spring 2020", "late spring 2020"),
                        labels = c("Early spring 2020", "Late spring 2020"))
  
  # 2021
  # select 2021 data
  STB21 <- filter(dat3, SampleYear == "2021")
  # add column for portion of sampling year
  STB21$YearPortion <-
    c(rep("early spring 2021", times = 52),
      rep("late spring 2021", times = 98))
  # plot
  p3 <-
    ggplot(STB21,
           aes(x = Forklength, 
               fill = YearPortion)) +
    geom_density(alpha = 0.3) +
    scale_x_continuous(limits = c(100, 1200),
                       breaks = seq(0,1250,by=50)) +
    scale_y_continuous(limits = c(0, 0.0075)) +
    theme_classic() +
    ylab("Sample density") +
    xlab("Fork length (mm)") +
    scale_fill_discrete(name="",
                        breaks=c("early spring 2021", "late spring 2021"),
                        labels=c("Early spring 2021", "Late spring 2021"))
  
  # combine separate plots in one window
  library(gridExtra)
  grid.arrange(p1, p2, p3, nrow = 3)
  
  # boxplots
    # compare pairs tested via Wilxcoxon Mann-Whitney U" tests
      dat5$ComparisonGroup <- 
        c(rep("Test 1", 277), # 2019 
        rep("Test 2", 154), # 2020
        rep("Test 3", 150)) # 2021
      ggplot(dat5,
         aes(x = reorder(YearPortion, SampleMonth),
             y = Forklength,
             fill = ComparisonGroup)) +
             theme_classic() +
      geom_boxplot(width = 0.1) +
      labs(fill = "") +
      theme(legend.position="bottom")
