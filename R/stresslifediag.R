# Stress-Life Diagram
# Developed by Reuel Smith, 2022

SN.diagram <- function(input_type,dat,stressunits,options){
  # Define input type for data: 1 - data points, 2- cyclic stress range, 3 - multi-axial stress
  # Define units in either MPa (default) or psi.  Database will have units for both.
  # Options for data type 1 to include: Su, Se, BHN, fatigue strength
  library(pracma)
  library(ggplot2)

  # Check units and set up axis labels
  if(missing(stressunits) || stressunits == 1){
    stressunits <- c("MPa")
    Su_threshold <- 200*6.8947572932
    Se_threshold <- 100*6.8947572932
    BHN_mult <- 0.25*6.8947572932
  }
  if(stressunits == 2){
    stressunits <- c("ksi")
    Su_threshold <- 200
    Se_threshold <- 100
    BHN_mult <- 0.25
  }

  # Check options if available and for Su, Se, and BHN
  Se <- length(0)
  Su <- length(0)
  BHN <- length(0)

  # Check units and set up axis labels
  Xlab <- paste(c("Life to Failure, N (Cycles)"),collapse="", sep = "_")
  Ylab <- paste(c("Alternating Stress, S_a (",stressunits,")"),collapse="", sep = "_")

  # ==================================
  # Calculation Type 1 - Data Points
  # ==================================
  if(input_type==1){
    # Check to see if data is in list form
    if(is.list(dat)==FALSE){
      stop('Enter S-N data as a list of vectors.')
    }
    # # Set input data to SN data
    # SNdat <- dat
  }

  # ==================================
  # Calculation Type 2 - Cyclic Stress Range
  # ==================================
  # Can be used for fully reversed cycles, but when not the case, need a model to base estimate upon
  if(input_type==2){
    if(missing(options)==FALSE){
      # Pull Smax and Smin if available
      if(length(options$Srange)>0){
        if(is.na(options$Srange)==0){
          Smax <- max(options$Srange)
          Smin <- min(options$Srange)
        }
      } else {
        stop('Enter options for Srange (c(Smin,Smax)).')
      }
    } else {
      stop('Enter options for Srange (c(Smin,Smax)).')
    }
    # Stress Amplitude
    Sa = (Smax - Smin)*0.5
    # Mean Stress
    Sm = (Smax + Smin)*0.5
    # Stress Ratio
    R = Smin/Smax
  }

  # ==================================
  # Calculation Type 3 - Multi-Axial Stress
  # ==================================
  if(input_type==3){

  }

  # Set input data to SN data
  SNdat <- dat

  # Collect S and N data
  S_ <- SNdat[[1]]
  N_ <- SNdat[[2]]

  # Se <-length(0)

  if(length(S_)==1){
    # Pull Se or Su from options immediately and assume
    if(missing(options)==FALSE){
      # Pull Se if available
      if(length(options$Se)>0){
        if(is.na(options$Se)==0){
          Se <- options$Se
        }
      }
      if(length(options$Su)>0){
        if(is.na(options$Su)==0){
          Su <- options$Su
          if(length(Se)==0){
            # Compute estimate for endurance limit based on ultimate stress
            if(Su <= Su_threshold){
              Se <- 0.5*Su
            } else{
              Se <- Se_threshold
            }
          }
        }
      }
    } else {
      stop('Enter options for Su or Se.')
    }
    S_ <- c(S_,Se)
    N_ <- c(N_,10^6)
  }
  logS <- log(S_)
  logN <- log(N_)

  # Check for runoff data
  if(length(dat)==4){
    runoff_S <- dat[[3]]
    runoff_N <- dat[[4]]
  } else{
    if(length(S_)==2){
      runoff_S <- Se
      runoff_N <- 10^7
    }
  }

  # Compute A and b for S_a = A N^b
  params <- lm(logS ~ poly(logN, 1, raw=TRUE))
  A <- exp(summary(params)$coefficients[1,1])
  b <- summary(params)$coefficients[2,1]

  # Compute S_1000 only if data is greater than 1
  S1000 <- A*(1000^b)

  # Compute Se.  If runoff data exists, use the average.
  # If no runoff data, use Su and/or BHN based estimates.
  if(length(dat)==4){
    # average value of runoff stress
    Se <- mean(dat[[3]])

    # Obtain Su
    if(missing(options)==FALSE){
      # Pull Su if available
      if(length(options$Su)>0){
        if(is.na(options$Su)==0){
          Su <- options$Su
        }
      }
    } else{
      Su <-S1000/0.9
    }
  } else{
    # pull given Se or estimated Se, from Su and/or BHN
    if(missing(options)==FALSE){
      # Pull Se if available
      if(length(options$Se)>0){
        if(is.na(options$Se)==0){
          Se <- options$Se
        }
      } else{
        # find Se from given Su but if that doesn't exist use BN
        # Pull Su if available
        if(length(options$Su)>0){
          if(is.na(options$Su)==0){
            Su <- options$Su
          }
          # Compute estimate for endurance limit based on ultimate stress
          if(Su <= Su_threshold){
            Se <- 0.5*Su
          } else{
            Se <- Se_threshold
          }
        } else if(length(options$BHN)>0){
          # Pull BHN if available
          if(is.na(options$BHN)==0){
            BHN <- options$BHN
            # Cross check for Se.  If it is not available, calculate estimate from BHN
            if(is.na(options$Se)==0 || length(options$Se) == 0){
              if(BHN <= 400){
                Se <- BHN_mult*BHN
              } else{
                Se <- Se_threshold
              }
            }
          }
        }
        # Should cover everything
      }
    }
  }

  N_Se <- (Se/A)^(1/b)

  Ncurve <- c(linspace(1000,N_Se,100),max(runoff_N))
  Scurve <-c(A*((linspace(1000,N_Se,100))^b),Se)

  if(length(S_)>2){
    # Compute upper and lower bounds
    res_logS <- logS - (log(A) + b*logN)
    logCI_S <- quantile(res_logS, 0.995)
    Scurvelow <- exp(c(log(A) + b*log((linspace(1000,N_Se,100))) - logCI_S,log(A) + b*log(N_Se) - logCI_S))
    Scurvehigh <- exp(c(log(A) + b*log((linspace(1000,N_Se,100))) + logCI_S,log(A) + b*log(N_Se) + logCI_S))

    df<-data.frame(S=c(S_,rep(NA,length(runoff_S)+length(Ncurve))), N = c(N_,rep(NA,length(runoff_S)+length(Ncurve))), Srunoff=c(rep(NA,length(S_)),runoff_S,rep(NA,length(Ncurve))), Nrunoff = c(rep(NA,length(N_)),runoff_N,rep(NA,length(Ncurve))), Sline = c(rep(NA,length(S_)+length(runoff_N)),Scurve), Slineupper = c(rep(NA,length(S_)+length(runoff_N)),Scurvehigh), Slinelower = c(rep(NA,length(S_)+length(runoff_N)),Scurvelow), Nline = c(rep(NA,length(S_)+length(runoff_N)),Ncurve), data_points = c(rep("failed",length(S_)),rep("survived",length(runoff_N)),rep(NA,length(Ncurve))), datapt_or_curvefit = c(rep(NA,length(S_)+length(runoff_N)),rep("curvefit",length(Ncurve))))
  } else {
    # Compute upper and lower bounds
    df<-data.frame(S=c(S_,rep(NA,length(runoff_S)+length(Ncurve))), N = c(N_,rep(NA,length(runoff_S)+length(Ncurve))), Srunoff=c(rep(NA,length(S_)),runoff_S,rep(NA,length(Ncurve))), Nrunoff = c(rep(NA,length(N_)),runoff_N,rep(NA,length(Ncurve))), Sline = c(rep(NA,length(S_)+length(runoff_N)),Scurve), Nline = c(rep(NA,length(S_)+length(runoff_N)),Ncurve), data_points = c(rep("failed",length(S_)),rep("survived",length(runoff_N)),rep(NA,length(Ncurve))), datapt_or_curvefit = c(rep(NA,length(S_)+length(runoff_N)),rep("curvefit",length(Ncurve))))
  }


  plotout<-ggplot() +
    geom_point(data=df, aes(N,S), colour = 'red', size = 1.9) +
    geom_point(data=df, aes(Nrunoff,Srunoff), colour = 'green4', shape=17, size = 1.9) +
    geom_line(data=df, aes(Nline,Sline), colour = "black", size = 0.9) +
    scale_x_continuous(trans = 'log10') +
    scale_y_continuous(trans = 'log10') +
    annotation_logticks() +
    xlab(Xlab) +
    ylab(Ylab)

  if(length(S_)>2){
    plotout<-plotout + geom_ribbon(data=df, aes(ymin = Slinelower,ymax = Slineupper,x=Nline), alpha=0.25,fill = "red")
  }


  # Produce some output text that summarizes the results
  cat(c("Estimate S_1000 ",S1000," ",stressunits,".\n\n"),sep = "")
  cat(c("Estimate S_u ",Su," ",stressunits,".\n\n"),sep = "")
  cat(c("Estimate S_e ",Se," ",stressunits,".\n\n"),sep = "")
  # cat(c("Maximum-Likelihood estimates for the ",ls_txt," Life-Stress model.\n\n"),sep = "")
  # print(matrix(unlist(fulllimset.res), nrow = length(unlist(fulllimset.res))/length(LSQest), ncol = length(LSQest), byrow = FALSE,dimnames = list(c("Life-Stress Parameters Mean",conflim_txt),params_txt)))

  return(list(plotout))

}
