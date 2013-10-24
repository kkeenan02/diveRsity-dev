# divPart development version
# includes improved performance for pairwise calculations

# Kevin Keenan 2013

# divPart, a wrapper function for the calculation of differentiation stats.
divPart<-function(infile = NULL, outfile = NULL, gp = 3, pairwise = FALSE,
                  WC_Fst = FALSE, bs_locus = FALSE, bs_pairwise = FALSE, 
                  bootstraps = 0, plot = FALSE, parallel = FALSE){
  
  ############################ Argument definitions ############################
  D <- infile
  on <- outfile
  gp <- gp
  fst <- WC_Fst
  bstrps <- bootstraps
  bsls <- bs_locus
  bspw <- bs_pairwise
  plt <- plot
  para <- parallel
  pWise <- pairwise
  
  ##############################################################################
  if(bsls==T && bstrps<2){
    bs_warning<-{paste("[STOPPED]",
                       "bootsraps must be greater than 2")
    }
    cat(noquote(bs_warning))
  } else if (bspw==T && bstrps<2){
    bs_warning<-{paste("[STOPPED]",
                       "bootsraps must be greater than 2")
    }
    cat(noquote(bs_warning))
  } else {
    #Use pre.div to calculate the standard global and locus stats
    accDat <- pre.divLowMemory(list(infile = D,
                                    gp = gp,
                                    bootstrap = FALSE,
                                    locs = TRUE,
                                    fst = fst,
                                    min = FALSE))
    # create a directory for output
    if(!is.null(on)){
      suppressWarnings(dir.create(path=paste(getwd(),"/",on,
                                             "-[diveRsity]","/",sep="")))
    }
    of = paste(getwd(), "/", on, "-[diveRsity]", "/", sep = "")
    wd <- getwd()
    write_res <- is.element("xlsx", installed.packages()[, 1])
    plot_res <- is.element("sendplot", installed.packages()[, 1])
    
    para_pack_inst<-is.element(c("parallel","doParallel","foreach","iterators"),
                               installed.packages()[,1])
    
    if(plt == TRUE && is.null(on)){
      writeWarn <- paste("", "[NOTE]",
                         "Your results can't be plotted as you have not",
                         "provided an argument for 'outfile'.",
                         "Analysis completed", sep="\n")
      cat(noquote(writeWarn))
    }
    para_pack <- all(para_pack_inst)
    if(write_res == FALSE){
      Warning1<-{paste(" "," ",
                       "[NOTE]",
                       "___________________________________________________________",
                       "Please install the package 'xlsx' if you would like your", 
                       "results written to an Excel workbook.",
                       "Alternatively, your result will automatically be written",
                       "to .txt files.",
                       "___________________________________________________________",
                       "To install 'xlsx' use:",
                       "> install.packages('xlsx', dependencies=TRUE)",
                       "See:",
                       "> ?install.packages - for usage details.",
                       "___________________________________________________________",
                       sep="\n")
      }
      cat(noquote(Warning1))
    } 
    if(plot_res==F && plt==T){
      Warning2<-{paste(" "," "," ",
                       "[NOTE]  ",
                       "___________________________________________________________",
                       "Please install the package 'sendplot' to plot your results.",
                       "Use:",
                       "> install.packages('sendplot', dependencies = TRUE)",
                       "See:",
                       "> ?install.packages - for usage details",
                       "___________________________________________________________",
                       sep="\n")
      }
      cat(noquote(Warning2))
    }
    if(fst == TRUE){
      namer<-c("Gst","G_hed_st","D_Jost","Gst_est","G_hed_st_est",
               "D_Jost_est","Fst_WC","Fit_WC")
    } else {
      namer<-c("Gst","G_hed_st","D_Jost","Gst_est","G_hed_st_est",
               "D_Jost_est")
    }
    
    ############################################################################
    # output file multilocus stats vector 
    # pre output table for global locus stats
    
    #standard
    pre_ot1 <- cbind(accDat$locus_names, round(as.numeric(accDat$hst), 4),
                     round(as.numeric(accDat$dst), 4),
                     round(as.numeric(accDat$gst), 4),
                     round(as.numeric(accDat$gst_hedrick), 4),
                     round(as.numeric(accDat$djost), 4))
    # Add global multi locus stats to output table
    ot1 <- rbind(pre_ot1, c("Global", "", "", accDat$gst_all, 
                            accDat$gst_all_hedrick, 
                            accDat$djost_all))
    colnames(ot1) <- c("loci", "H_st", "D_st", "G_st", "G_hed_st", "D_jost")
    #Estimated
    pre_ot2 <- cbind(accDat$locus_names,
                     round(as.numeric(accDat$locus_harmonic_N),4),
                     round(as.numeric(accDat$hst_est),4),
                     round(as.numeric(accDat$dst_est),4),
                     round(as.numeric(accDat$gst_est),4),
                     round(as.numeric(accDat$gst_est_hedrick),4),
                     round(as.numeric(accDat$djost_est),4))
    
    ot2 <- rbind(pre_ot2, c("Global", "", "", "", accDat$gst_est_all, 
                            accDat$gst_est_all_hedrick, 
                            accDat$djost_est_all))
    colnames(ot2) <- c("loci", "Harmonic_N", "H_st_est", "D_st_est",
                       "G_st_est", "G_hed_st_est", "D_Jost_est")
    if(fst == TRUE){
      ot2 <- cbind(ot2, accDat$fstats[, 2:3])
    }
    if(fst == TRUE){
      plot_data321 <- c("Overall","","","",accDat$gst_est_all,
                        accDat$gst_est_all_hedrick,
                        accDat$djost_est_all,
                        as.numeric(accDat$fstats["All",2]))
      
    } else {
      plot_data321<-c("Overall","","","",accDat$gst_est_all,
                      accDat$gst_est_all_hedrick,
                      accDat$djost_est_all)
    }
    if (!is.null(on)){
      if(write_res==TRUE){
        # write data to excel
        # Load dependencies
        require("xlsx")
        # standard stats
        write.xlsx(ot1,file=paste(of,"[divPart].xlsx",sep=""),
                   sheetName="Standard_stats",col.names=T,
                   row.names=F,append=F)
        # Estimated stats
        write.xlsx(ot2,file=paste(of,"[divPart].xlsx",sep=""),
                   sheetName="Estimated_stats",col.names=T,
                   row.names=F,append=T)
      } else {
        # text file alternatives
        std<-file(paste(of,"Standard-stats[divPart].txt",sep=""), "w")
        cat(paste(colnames(ot1),sep=""),"\n",sep="\t",file=std)
        for(i in 1:nrow(ot1)){
          cat(ot1[i,],"\n",file=std,sep="\t")
        }
        close(std)
        est<-file(paste(of,"Estimated-stats[divPart].txt",sep=""),"w")
        cat(paste(colnames(ot2),sep=""),"\n",sep="\t",file=est)
        for(i in 1:nrow(ot2)){
          cat(ot2[i,],"\n",file=est,sep="\t")
        }
        close(est)
      }
    }
    ot1out<-ot1[,-1]
    ot2out<-ot2[,-1]
    
    ot1out<-matrix(as.numeric(ot1[,2:6]),ncol=5)
    rownames(ot1out)<-ot1[,1]
    colnames(ot1out)<-colnames(ot1)[-1]
    
    ot2out<-matrix(as.numeric(ot2[,-1]),ncol=(ncol(ot2)-1))
    rownames(ot2out)<-ot2[,1]
    colnames(ot2out)<-colnames(ot2)[-1]
    if (para && !para_pack){
      Warning3<-{paste(" "," ",
                       "[NOTE]",
                       "___________________________________________________________",
                       "Please make sure the packages 'parallel', 'doParallel',",
                       "'foreach' and 'iterators' are installed. These are required",
                       " to run your analysis in parallel.",
                       "Your analysis will be run sequentially!",
                       "___________________________________________________________",
                       "To install these use:",
                       "> install.packages()",
                       "See:",
                       "> ?install.packages - for usage details.",
                       "___________________________________________________________",
                       sep="\n")
      }
      cat(noquote(Warning3))
    }
    
    ############################################################################
    ############################ Bootstrapper ##################################
    ############################################################################
    if (para && para_pack) {
      #count cores
      library("doParallel")
      cores <- detectCores()
      cl<-makeCluster(cores)
      registerDoParallel(cl)
    }
    
    # Used only if bootstraps is greater than zero
    if(bsls == TRUE){
      
      if (para && para_pack) {
        
        #vectorize prallele#
        gp_inls <- list(infile = D, gp = gp,
                        bootstrap = TRUE, 
                        locs = TRUE, fst = fst)
        # silence for memory efficiency
        #gp_in <- list()
        #for(i in 1:bstrps){
        #  gp_in[[i]] <- gp_inls
        #}
        
        # calculate stats from readGenepopX objects
        # export objects for parallel
        clusterExport(cl, c("gp_inls", "pre.divLowMemory"), 
                      envir = environment())
        # run parallel code
        bs_loc <- parLapply(cl, 1:bstrps, function(...){
          pre.divLowMemory(gp_inls)
        })
        
        
        #vectorize data extraction#
        if(fst==TRUE){
          bs_glb <- do.call("rbind", lapply(1:bstrps, function(x){
            c(round(bs_loc[[x]]$gst_all, 4),
              round(bs_loc[[x]]$gst_all_hedrick, 4),
              round(bs_loc[[x]]$djost_all, 4),
              round(bs_loc[[x]]$gst_est_all, 4),
              round(bs_loc[[x]]$gst_est_all_hedrick, 4),
              round(bs_loc[[x]]$djost_est_all, 4),
              as.numeric(bs_loc[[x]]$fstats["All", 2:3]))
          }))
        } else {
          bs_glb <- do.call("rbind", lapply(1:bstrps, function(x){
            c(round(bs_loc[[x]]$gst_all, 4),
              round(bs_loc[[x]]$gst_all_hedrick, 4),
              round(bs_loc[[x]]$djost_all, 4),
              round(bs_loc[[x]]$gst_est_all, 4),
              round(bs_loc[[x]]$gst_est_all_hedrick, 4),
              round(bs_loc[[x]]$djost_est_all, 4))
          }))
        }
        bs_std <- lapply(1:accDat$nloci, function(x){
          do.call("rbind", lapply(1:length(bs_loc), function(y){
            c(round(bs_loc[[y]]$gst[x], 4),
              round(bs_loc[[y]]$gst_hedrick[x], 4),
              round(bs_loc[[y]]$djost[x], 4))
          }))
        })
        if(fst==TRUE){
          bs_est <- lapply(1:accDat$nloci, function(x){
            do.call("rbind", lapply(1:length(bs_loc), function(y){
              c(round(bs_loc[[y]]$gst_est[x], 4),
                round(bs_loc[[y]]$gst_est_hedrick[x], 4),
                round(bs_loc[[y]]$djost_est[x], 4),
                as.numeric(bs_loc[[y]]$fstats[x, 2:3]))
            }))
          })
        } else {
          bs_est<-lapply(1:accDat$nloci, function(x){
            do.call("rbind",lapply(1:length(bs_loc), function(y){
              c(round(bs_loc[[y]]$gst_est[x],4),
                round(bs_loc[[y]]$gst_est_hedrick[x],4),
                round(bs_loc[[y]]$djost_est[x],4))
            }))
          })
        }
        rm(bs_loc)                  ###
        z<-gc(reset=T)                ### tidy up
        rm(z)                       ###
        
      } else {
        #vectorize non-parallel#
        
        gp_inls <- list(infile = D,
                        gp = gp,
                        bootstrap = TRUE, 
                        locs = TRUE, 
                        fst = fst)
        #gp_in<-list()
        #for(i in 1:bstrps){
        # gp_in[[i]]<-gp_inls
        #}
        # calculate stats from readGenepopX objects
        bs_loc <- lapply(1:bstrps, function(...){
          pre.divLowMemory(gp_inls)
        })
        
        
        if(fst==TRUE){
          bs_glb<-do.call("rbind",lapply(1:bstrps, function(x){
            c(round(bs_loc[[x]]$gst_all,4),
              round(bs_loc[[x]]$gst_all_hedrick,4),
              round(bs_loc[[x]]$djost_all,4),
              round(bs_loc[[x]]$gst_est_all,4),
              round(bs_loc[[x]]$gst_est_all_hedrick,4),
              round(bs_loc[[x]]$djost_est_all,4),
              as.numeric(bs_loc[[x]]$fstats[(accDat$nloci+1),2:3]))
          }))
        }else{
          bs_glb<-do.call("rbind",lapply(1:bstrps, function(x){
            c(round(bs_loc[[x]]$gst_all,4),
              round(bs_loc[[x]]$gst_all_hedrick,4),
              round(bs_loc[[x]]$djost_all,4),
              round(bs_loc[[x]]$gst_est_all,4),
              round(bs_loc[[x]]$gst_est_all_hedrick,4),
              round(bs_loc[[x]]$djost_est_all,4))
          }))
        }
        bs_std<-lapply(1:accDat$nloci, function(x){
          do.call("rbind",lapply(1:length(bs_loc), function(y){
            c(round(bs_loc[[y]]$gst[x],4),
              round(bs_loc[[y]]$gst_hedrick[x],4),
              round(bs_loc[[y]]$djost[x],4))}))
        })
        if(fst==TRUE){
          bs_est<-lapply(1:accDat$nloci, function(x){
            do.call("rbind",lapply(1:length(bs_loc), function(y){
              c(round(bs_loc[[y]]$gst_est[x],4),
                round(bs_loc[[y]]$gst_est_hedrick[x],4),
                round(bs_loc[[y]]$djost_est[x],4),
                as.numeric(bs_loc[[y]]$fstats[x,2:3]))
            }))
          })
        } else {
          bs_est<-lapply(1:accDat$nloci, function(x){
            do.call("rbind",lapply(1:length(bs_loc), function(y){
              c(round(bs_loc[[y]]$gst_est[x],4),
                round(bs_loc[[y]]$gst_est_hedrick[x],4),
                round(bs_loc[[y]]$djost_est[x],4))
            }))
          })
        }
        rm(bs_loc)
        z<-gc(reset=T)
        rm(z)
        
      }
      
      
      #vectorize#
      if(fst == TRUE){
        bs_res <- lapply(1:8, function(x){
          matrix(ncol = 3, nrow = (accDat$nloci+1))
        })
      } else {
        bs_res<-lapply(1:6,function(x){matrix(ncol=3, nrow=(accDat$nloci+1))})
      }
      bs_join<-cbind(bs_std, bs_est)
      bs_cis <- apply(bs_join, 1, function(x){
        res <- lapply(x, function(y){
          apply(y, 2, function(z){
            ci <- as.vector(quantile(z, probs = c(0.025, 0.975), na.rm = TRUE))
            means <- mean(z, na.rm = TRUE)
            
            return(c(means, ci))
          })
        })
        ciM <- c(res$bs_std[1,], res$bs_est[1,])
        lci <- c(res$bs_std[2,], res$bs_est[2,])
        uci <- c(res$bs_std[3,], res$bs_est[3,])
        list(mu = ciM,
             lci = lci,
             uci = uci)
      })
      mu <- t(sapply(1:length(bs_cis), function(i){
        return(bs_cis[[i]]$mu)
      }))
      lci <- t(sapply(1:length(bs_cis), function(i){
        return(bs_cis[[i]]$lci)
      }))
      uci <- t(sapply(1:length(bs_cis), function(i){
        return(bs_cis[[i]]$uci)
      }))
      # calculate ci for global
      glb_mu <- apply(bs_glb, 2, function(x){
        return(mean(x, na.rm = TRUE))
      })
      glb_lci <- apply(bs_glb, 2, function(x){
        return(quantile(x, probs = 0.025, na.rm = TRUE))
      })
      glb_uci <- apply(bs_glb, 2, function(x){
        return(quantile(x, probs = 0.975, na.rm = TRUE))
      })
      # add glb ci to mu,  uci and lci
      mu <- rbind(mu, glb_mu)
      lci <- rbind(lci, glb_lci)
      uci <- rbind(uci, glb_uci)
      #ciCalc <- function(x){
      #  res <- lapply(x, function(y){
      #    apply(y, 2, function(z){
      #      return(quantile(z, probs = c(0.025, 0.975)))
      #    })
      #  })
      #  return(res)
      #}
      #ci <- function(x){
      #  (sd(na.omit(x))/sqrt(length(na.omit(x)))) * 1.96
      #}
      #bs_cis <- t(apply(bs_join, 1, ciCalc))
      #bs_cis<-rbind(bs_cis, apply(bs_glb, 2, ci))
      if(fst==TRUE){
        for(i in 1:8){
          bs_res[[i]][,1] <- round(mu[,i], 4)
          bs_res[[i]][,2] <- round(lci[,i], 4)
          bs_res[[i]][,3] <- round(uci[,i], 4)
          bs_res[[i]][is.na(bs_res[[i]])] <- 0
        }
      } else {
        for(i in 1:6){
          bs_res[[i]][,1] <- round(mu[,i], 4)
          bs_res[[i]][,2] <- round(lci[,i], 4)
          bs_res[[i]][,3] <- round(uci[,i], 4)
          bs_res[[i]][is.na(bs_res[[i]])] <- 0
        }
      }
      
      names(bs_res) <- namer
      
      bs_res1 <- bs_res
      if(fst){
        for(i in 1:8){
          dimnames(bs_res1[[i]])<-list(c(accDat$locus_names, "global"),
                                       c("Mean","Lower_CI", "Upper_CI"))
        }
      } else {
        for(i in 1:6){
          dimnames(bs_res1[[i]])<-list(c(accDat$locus_names,"global"),
                                       c("Mean","Lower_CI","Upper_CI"))
        }
      }
      # bs results output object header
      hdr <- matrix(c("locus", "Mean", "Lower_95%CI", "Upper_95%CI"), 
                    ncol=4)
      bs_out <- matrix(rbind(hdr, c(names(bs_res)[1], "", "", ""),
                             cbind(c(accDat$locus_names, "Overall"),
                                   bs_res[[1]])), ncol = 4)
      
      if(fst){
        for(i in 2:8){
          bs_out <- matrix(rbind(bs_out, c(names(bs_res)[i], "", "", ""),
                                 cbind(c(accDat$locus_names, "global"),
                                       bs_res[[i]])), ncol = 4)
        }
      } else {
        for(i in 2:6){
          bs_out<-matrix(rbind(bs_out,c(names(bs_res)[i],"","",""),
                               cbind(c(accDat$locus_names,"Global"),
                                     bs_res[[i]])),ncol=4)
        }
      }
      if(!is.null(on)){
        if(write_res==TRUE){
          write.xlsx(bs_out,file=paste(of,"[divPart].xlsx",sep=""),
                     sheetName="Locus_bootstrap",col.names=F,
                     row.names=F,append=T)
        } else {
          # text file alternatives
          bts<-file(paste(of,"Locus-bootstrap[divPart].txt",sep=""), "w")
          cat(paste(colnames(bs_out),sep=""),"\n",sep="\t",file=bts)
          for(i in 1:nrow(bs_out)){
            cat(bs_out[i,],"\n",file=bts,sep="\t")
          }
          close(bts)
        }
      }
    }
    zzz<-gc()
    rm(zzz)
    if(plot_res==TRUE && plt==TRUE && bsls==TRUE){
      
      #vectorize#
      sorter<-function(x){
        z<-order(x[1:accDat$nloci,1],decreasing=F)
        #if(length(z) >= 200){
        #  z<-z[(length(z)-150):length(z)]
        #}
        return(z)
      }
      lso123<-lapply(bs_res, sorter)
      
      #
      names(lso123)<-namer
      plot.call_loci<-list()
      plot.extras_loci<-list()
      xy.labels_loci<-list()
      y.pos_loci<-list()
      x.pos_loci=1:accDat$nloci
      direct=of
      fn_pre_loci<-list()
      #Plot Gst_Nei
      plot.call_loci[[1]]=c("plot(bs_res[[4]][lso123[[4]],1],
                            ylim=c(0,(max(bs_res[[4]][,3])+
                            min(bs_res[[4]][,3]))),xaxt='n',
                            ylab=names(bs_res)[4],type='n',
                            xlab='Loci \n (Hover over a point to see locus data)',
                            cex.lab=1.5,cex.axis=1.3,las=1)")
      
      plot.extras_loci[[1]]=c("points(bs_res[[4]][lso123[[4]],1],
                              pch=15,col='black',cex=1);
                              arrows(1:accDat$nloci,bs_res[[4]][lso123[[4]],2],
                              1:accDat$nloci,bs_res[[4]][lso123[[4]],3],code=3,
                              angle=90,length=0.05,lwd=0.1);
                              abline(h=c(0,bs_res[[4]][(accDat$nloci+1),2]),
                              lwd=1,lty=c(1,2),col=c('black','red'))")
      
      xy.labels_loci[[1]]=data.frame(Locus_name=accDat$locus_names[lso123[[4]]],
                                     Gst_Nei=round(bs_res[[4]][lso123[[4]],1],4),
                                     Gst_Hedrick=round(bs_res[[5]][lso123[[4]],1],4),
                                     D_jost=round(bs_res[[6]][lso123[[4]],1],4))
      
      y.pos_loci[[1]]=bs_res[[4]][lso123[[4]],1]
      fn_pre_loci[[1]]<-names(bs_res)[4]
      
      
      
      # Plot Gst_Hedrick
      plot.call_loci[[2]]=c("plot(bs_res[[5]][lso123[[5]],1],
                            ylim=c(0,1),xaxt='n',ylab=names(bs_res)[5],type='n',
                            xlab='Loci \n (Hover over a point to see locus data)',
                            cex.lab=1.5,cex.axis=1.3,las=1)")
      
      plot.extras_loci[[2]]=c("points(bs_res[[5]][lso123[[5]],1],
                              pch=15,col='black',cex=1);
                              arrows(1:accDat$nloci,bs_res[[5]][lso123[[5]],2],
                              1:accDat$nloci,bs_res[[5]][lso123[[5]],3],code=3,
                              angle=90,length=0.05,lwd=0.1);
                              abline(h=c(0,bs_res[[5]][(accDat$nloci+1),2]),
                              lwd=1,lty=c(1,2),col=c('black','red'))")
      
      xy.labels_loci[[2]]=data.frame(Locus_name=accDat$locus_names[lso123[[5]]],
                                     Gst_Nei=round(bs_res[[4]][lso123[[5]],1],4),
                                     Gst_Hedrick=round(bs_res[[5]][lso123[[5]],1],4),
                                     D_jost=round(bs_res[[6]][lso123[[5]],1],4))
      
      y.pos_loci[[2]]=bs_res[[5]][lso123[[5]],1]
      fn_pre_loci[[2]]<-names(bs_res)[5]
      
      
      # Plot D_jost
      plot.call_loci[[3]]=c("plot(bs_res[[6]][lso123[[6]],1],
                            ylim=c(0,1),xaxt='n',ylab=names(bs_res)[6],type='n',
                            xlab='Loci \n (Hover over a point to see locus data)',
                            cex.lab=1.5,cex.axis=1.3,las=1)")
      
      plot.extras_loci[[3]]=c("points(bs_res[[6]][lso123[[6]],1],
                              pch=15,col='black',cex=1);
                              arrows(1:accDat$nloci,bs_res[[6]][lso123[[6]],2],
                              1:accDat$nloci,bs_res[[6]][lso123[[6]],3],code=3,
                              angle=90,length=0.05,lwd=0.1);
                              abline(h=c(0,bs_res[[6]][(accDat$nloci+1),2]),
                              lwd=1,lty=c(1,2),col=c('black','red'))")
      
      xy.labels_loci[[3]]=data.frame(Locus_name=accDat$locus_names[lso123[[6]]],
                                     Gst_Nei=round(bs_res[[4]][lso123[[6]],1],4),
                                     Gst_Hedrick=round(bs_res[[5]][lso123[[6]],1],4),
                                     D_jost=round(bs_res[[6]][lso123[[6]],1],4))
      
      y.pos_loci[[3]]=bs_res[[6]][lso123[[6]],1]
      fn_pre_loci[[3]]<-names(bs_res)[6]
      
      #plot(Fst)
      if(fst==TRUE){
        plot.call_loci[[4]]=c("plot(bs_res[[8]][lso123[[8]],1],
                              ylim=c(0,(max(bs_res[[8]][,3])+
                              min(bs_res[[8]][,3]))),xaxt='n',
                              ylab=names(bs_res)[8],type='n',
                              xlab='Loci \n (Hover over a point to see locus data)',
                              cex.lab=1.5,cex.axis=1.3,las=1)")
        
        plot.extras_loci[[4]]=c("points(bs_res[[8]][lso123[[8]],1],
                                pch=15,col='black',cex=1);
                                arrows(1:accDat$nloci,bs_res[[8]][lso123[[8]],2],
                                1:accDat$nloci,bs_res[[8]][lso123[[8]],3],code=3,
                                angle=90,length=0.05,lwd=0.1);
                                abline(h=c(0,bs_res[[8]][(accDat$nloci+1),2]),
                                lwd=1,lty=c(1,2),col=c('black','red'))")
        
        xy.labels_loci[[4]]=data.frame(Locus_name=accDat$locus_names[lso123[[8]]],
                                       Gst_Nei=round(bs_res[[4]][lso123[[8]],1],4),
                                       Gst_Hedrick=round(bs_res[[5]][lso123[[8]],1],4),
                                       D_jost=round(bs_res[[6]][lso123[[8]],1],4),
                                       Fst_WC=round(bs_res[[8]][lso123[[8]],1],4))
        
        y.pos_loci[[4]]=bs_res[[8]][lso123[[8]],1]
        fn_pre_loci[[4]]<-names(bs_res)[8]
      }
    }
    ############################################################################
    ################################## Pairwise ################################
    ############################################################################
    # population pair combinations
    if(pWise || bspw){
      pw <- combn(accDat$npops,2)
      pwmat <- pw + 1
      #pw data creator
      ind_vectors <- lapply(1:accDat$npops, function(x){
        rep(x, accDat$pop_sizes[[x]])}
      )
      #      
      pre_data <- matrix(rep("", ((accDat$nloci + 1) * (accDat$nloci + 1))),
                         ncol = (accDat$nloci + 1))
      pre_data[1,] <- rep("", (accDat$nloci + 1))
      #
      for(i in 2:(accDat$nloci + 1)){
        pre_data[i, 1] <- accDat$locus_names[(i-1)]
      }
      #
      pw_data<-list()
      for (i in 1:ncol(pw)){
        pw_data[[i]]<-data.frame(rbind(pre_data,
                                       c("POP",as.vector(rep("",accDat$nloci))),
                                       cbind(ind_vectors[[pw[1,i]]],
                                             matrix(noquote(accDat$pop_list
                                                            [[pw[1,i]]]),
                                                    ncol=accDat$nloci)),
                                       c("POP",as.vector(rep("",accDat$nloci))),
                                       cbind(ind_vectors[[pw[2,i]]],
                                             matrix(noquote(accDat$pop_list
                                                            [[pw[2,i]]]),
                                                    ncol=accDat$nloci))))
      }
      true_stat_gp_in <- list()
      if(fst == TRUE){
        pw_glb <- matrix(rep(0, (8 * (ncol(pw)))), ncol = 8)
      } else {
        pw_glb <- matrix(rep(0, (6 * (ncol(pw)))), ncol = 6)
      }
      for (i in 1:ncol(pw)){
        true_stat_gp_in[[i]] <- list(infile = pw_data[[i]],
                                     gp = gp, bootstrap = FALSE,
                                     locs = FALSE, fst = fst)
      }
      if (para && para_pack) {
        
        true_stat <- parLapply(cl, true_stat_gp_in, pre.divLowMemory)
        # close core connections if not needed further
        if (bspw == FALSE){
          stopCluster(cl)
        }
      } else {
        true_stat <- lapply(true_stat_gp_in, pre.divLowMemory)
      }
      for(i in 1:ncol(pw)){
        if(fst==TRUE){
          pw_glb[i,]<-c(true_stat[[i]]$gst_all,true_stat[[i]]$gst_all_hedrick,
                        true_stat[[i]]$djost_all,true_stat[[i]]$gst_est_all,
                        true_stat[[i]]$gst_est_all_hedrick,
                        true_stat[[i]]$djost_est_all,
                        as.numeric(true_stat[[i]]$fstat[2:3]))
        } else {
          pw_glb[i,]<-c(true_stat[[i]]$gst_all,true_stat[[i]]$gst_all_hedrick,
                        true_stat[[i]]$djost_all,true_stat[[i]]$gst_est_all,
                        true_stat[[i]]$gst_est_all_hedrick,
                        true_stat[[i]]$djost_est_all)
        }
      }
      if(fst==TRUE){
        pwMatList <- lapply(1:8, function(x){
          matrix(rep("--", ((accDat$npops+1) ^ 2)), 
                 ncol = (accDat$npops + 1),
                 nrow = (accDat$npops + 1))
        })
      } else {
        pwMatList <- lapply(1:6, function(x){
          matrix(rep("--", ((accDat$npops+1)^2)),
                 ncol = (accDat$npops + 1),
                 nrow = (accDat$npops + 1))
        })
      }
      if(fst==TRUE){
        pwMatListOut <- lapply(1:8, function(x){
          matrix(rep(NA, ((accDat$npops)^2)),
                 ncol = (accDat$npops),
                 nrow = (accDat$npops))
        })
      } else {
        pwMatListOut <- lapply(1:6, function(x){
          matrix(rep(NA,((accDat$npops)^2)),
                 ncol = (accDat$npops),
                 nrow = (accDat$npops))
        })
      }
      names(pwMatList) <- namer
      names(pwMatListOut) <- namer
      #write pw res to matrices
      pnames <- c("", accDat$pop_names)
      pnamesOut <- accDat$pop_names
      if(fst==TRUE){
        for(i in 1:8){
          for(j in 1:ncol(pw)){
            pwMatList[[i]][pwmat[2, j], pwmat[1, j]] <- pw_glb[j, i]
            pwMatList[[i]][pwmat[1, j], pwmat[2, j]] <- ""
            pwMatListOut[[i]][pw[2, j], pw[1, j]] <- pw_glb[j, i]
            #pwMatListOut[[i]][pw[1,j],pw[2,j]]<-""
          }
          pwMatList[[i]][1, ] <- pnames
          pwMatList[[i]][, 1] <- pnames
          dimnames(pwMatListOut[[i]]) <- list(pnamesOut, pnamesOut)
        }
      } else {
        for(i in 1:6){
          for(j in 1:ncol(pw)){
            pwMatList[[i]][pwmat[2, j], pwmat[1, j]] <- pw_glb[j, i]
            pwMatList[[i]][pwmat[1, j], pwmat[2, j]] <- ""
            pwMatListOut[[i]][pw[2, j], pw[1, j]] <- pw_glb[j, i]
            #pwMatListOut[[i]][pw[1,j],pw[2,j]]<-""
          }
          pwMatList[[i]][1, ] <- pnames
          pwMatList[[i]][, 1] <- pnames
          dimnames(pwMatListOut[[i]]) <- list(pnamesOut, pnamesOut)
        }
      }
      
      
      # write object create
      #pnames list
      
      pwWrite <- pwMatList[[1]]
      pwWrite <- rbind(c(names(pwMatList)[1], rep("", accDat$npops)), pwWrite,
                       rep("", (accDat$npops + 1)))
      if(fst==TRUE){
        for(i in 2:8){
          pwWrite <- rbind(pwWrite, c(names(pwMatList)[i],
                                      rep("", accDat$npops)),
                           pwMatList[[i]], rep("", (accDat$npops + 1)))
        }
      } else {
        for(i in 2:6){
          pwWrite <- rbind(pwWrite, c(names(pwMatList)[i],
                                      rep("",accDat$npops)),
                           pwMatList[[i]], rep("",(accDat$npops+1)))
        }
      }
      if(!is.null(on)){
        if(write_res == TRUE){
          # write data to excel
          # Load dependencies
          # pw stats
          write.xlsx(pwWrite,file=paste(of,"[divPart].xlsx",sep=""),
                     sheetName="Pairwise-stats",col.names=F,
                     row.names=F,append=T)
        } else {
          # text file alternatives
          pw_outer<-file(paste(of,"Pairwise-stats[divPart].txt",sep=""), "w")
          for(i in 1:nrow(pwWrite)){
            cat(pwWrite[i,],"\n",file=pw_outer,sep="\t")
          }
          close(std)
        }
      }
      #cleanup
      rm("pwWrite")
      ##
      zzz<-gc()
      rm(zzz)
    }
    
    
    #Bootstrap
    if(bspw == TRUE){
      
      
      # Bootstrap results data object 
      # bs_pw_glb = bootstrap pairwise global stats
      #if(fst == TRUE){
      #  bs_pw_glb <- matrix(rep(0, (8*bstrps)), ncol = 8, nrow = bstrps)
      #} else {
      #  bs_pw_glb <- matrix(rep(0, (6*bstrps)), ncol = 6, nrow = bstrps)
      #}
      # output results data object
      # pw_res = pairwise results
      if(fst==TRUE){
        pw_res <- lapply(1:8, function(x){
          matrix(nrow = ncol(pw), ncol = 3)
        })
      } else {
        pw_res <- lapply(1:6, function(x){
          matrix(nrow = ncol(pw), ncol = 3)
        })
      }
      #
      #
      
      #parallel processing option
      if (para && para_pack) {
        #create a readGenepopX list
        bs_pw_glb<-list()
        data_res<-list()
        bs_pw_para<-list()
        for(i in 1:ncol(pw)){
          input <- list(infile = pw_data[[i]], gp = gp, bootstrap = TRUE,
                        locs = FALSE, fst = fst)
          # silence for memory efficiency
          #pw_inlist<-list()
          #for(j in 1:bstrps){
          #  pw_inlist[[j]] <- input
          #}
          if(fst == TRUE){
            bs_pw_glb[[i]] <- matrix(rep(0, (8*bstrps)), ncol = 8,
                                     nrow = bstrps)
          } else {
            bs_pw_glb[[i]] <- matrix(rep(0, (6*bstrps)), ncol = 6, 
                                     nrow = bstrps)
          }
          clusterExport(cl, c("input", "pre.divLowMemory"),
                        envir = environment())
          bs_pw_para <- parLapply(cl, 1:bstrps, function(...){
            pre.divLowMemory(input)
          })
          for(j in 1:bstrps){
            if(fst == TRUE){
              bs_pw_glb[[i]][j,] <- c(bs_pw_para[[j]]$gst_all,
                                      bs_pw_para[[j]]$gst_all_hedrick,
                                      bs_pw_para[[j]]$djost_all,
                                      bs_pw_para[[j]]$gst_est_all,
                                      bs_pw_para[[j]]$gst_est_all_hedrick,
                                      bs_pw_para[[j]]$djost_est_all,
                                      as.numeric(bs_pw_para[[j]]$fstats[2:3]))
            } else {
              bs_pw_glb[[i]][j,] <- c(bs_pw_para[[j]]$gst_all,
                                      bs_pw_para[[j]]$gst_all_hedrick,
                                      bs_pw_para[[j]]$djost_all,
                                      bs_pw_para[[j]]$gst_est_all,
                                      bs_pw_para[[j]]$gst_est_all_hedrick,
                                      bs_pw_para[[j]]$djost_est_all)
              
            }
          }
        }
        #
        # confidence interval calculator function
        pwCi <- lapply(bs_pw_glb, function(x){
          res <- apply(x, 2, function(y){
            ci <- as.vector(quantile(y, probs = c(0.025, 0.975), na.rm = TRUE))
            means <- mean(y, na.rm = TRUE)
            return(c(means, ci))
          })
          mu <- res[1,]
          lci <- res[2,]
          uci <- res[3,]
          list(mu = mu, lci = lci, uci = uci)
        })
        # create easy access data structure for each
        mu <- t(sapply(1:length(pwCi), function(i){
          return(pwCi[[i]]$mu)
        }))
        lci <- t(sapply(1:length(pwCi), function(i){
          return(pwCi[[i]]$lci)
        }))
        uci <- t(sapply(1:length(pwCi), function(i){
          return(pwCi[[i]]$uci)
        }))
        
        for(i in 1:ncol(pw)){
          for(j in 1:ncol(mu)){
            pw_res[[j]][i, 1] <- round(mu[i, j], 4)
            pw_res[[j]][i, 2] <- round(lci[i, j], 4)
            pw_res[[j]][i, 3] <- round(uci[i, j], 4)
            pw_res[[j]][is.na(pw_res[[j]])] <- 0
          }
        }
        stopCluster(cl)
      } else {
        #sequential vectorized
        #pw_inlist<-list()
        #for(i in 1:ncol(pw)){
        #  input <- list(infile = pw_data[[i]],
        #                gp = gp, bootstrap = TRUE, 
        #                locs = FALSE, fst = fst)
        #  pw_inlist[[i]] <- list()
        #  for(j in 1:bstrps){
        #    pw_inlist[[i]][[j]] <- input
        #  }
        #}
        bs_pw_glb <- list()
        for(i in 1:ncol(pw)){
          if(fst == TRUE){
            bs_pw_glb[[i]] <- matrix(rep(0, (8*bstrps)), ncol = 8,
                                     nrow = bstrps)
          } else {
            bs_pw_glb[[i]] <- matrix(rep(0, (6*bstrps)), ncol = 6, 
                                     nrow = bstrps)
          }
        }
        #create a readGenepopX list
        bs_pw_glb <- list()
        data_res <- list()
        bs_pw_para <- list()
        for(i in 1:ncol(pw)){
          input <- list(infile = pw_data[[i]],
                        gp = gp, bootstrap = TRUE,
                        locs = FALSE, fst = fst)
          # silence for memory efficiency
          #pw_inlist <- list()
          #for(j in 1:bstrps){
          #  pw_inlist[[j]] <- input
          #}
          if(fst == TRUE){
            bs_pw_glb[[i]] <- matrix(rep(0, (8*bstrps)), ncol = 8,
                                     nrow = bstrps)
          } else {
            bs_pw_glb[[i]] <- matrix(rep(0, (6*bstrps)), ncol = 6,
                                     nrow = bstrps)
          }
          bs_pw_para <- lapply(1:bstrps, function(...){
            pre.divLowMemory(input)
          })
          for(j in 1:bstrps){
            if(fst == TRUE){
              bs_pw_glb[[i]][j,] <- c(bs_pw_para[[j]]$gst_all,
                                      bs_pw_para[[j]]$gst_all_hedrick,
                                      bs_pw_para[[j]]$djost_all,
                                      bs_pw_para[[j]]$gst_est_all,
                                      bs_pw_para[[j]]$gst_est_all_hedrick,
                                      bs_pw_para[[j]]$djost_est_all,
                                      as.numeric(bs_pw_para[[j]]$fstat[2:3]))
              
            } else {
              bs_pw_glb[[i]][j,] <- c(bs_pw_para[[j]]$gst_all,
                                      bs_pw_para[[j]]$gst_all_hedrick,
                                      bs_pw_para[[j]]$djost_all,
                                      bs_pw_para[[j]]$gst_est_all,
                                      bs_pw_para[[j]]$gst_est_all_hedrick,
                                      bs_pw_para[[j]]$djost_est_all)
              
            }
          }
        } 
        # confidence interval calculator function
        pwCi <- lapply(bs_pw_glb, function(x){
          res <- apply(x, 2, function(y){
            ci <- as.vector(quantile(y, probs = c(0.025, 0.975), na.rm = TRUE))
            means <- mean(y, na.rm = TRUE)
            return(c(means, ci))
          })
          mu <- res[1,]
          lci <- res[2,]
          uci <- res[3,]
          list(mu = mu, lci = lci, uci = uci)
        })
        # create easy access data structure for each
        mu <- t(sapply(1:length(pwCi), function(i){
          return(pwCi[[i]]$mu)
        }))
        lci <- t(sapply(1:length(pwCi), function(i){
          return(pwCi[[i]]$lci)
        }))
        uci <- t(sapply(1:length(pwCi), function(i){
          return(pwCi[[i]]$uci)
        }))
        
        for(i in 1:ncol(pw)){
          for(j in 1:ncol(mu)){
            pw_res[[j]][i, 1] <- round(mu[i, j], 4)
            pw_res[[j]][i, 2] <- round(lci[i, j], 4)
            pw_res[[j]][i, 3] <- round(uci[i, j], 4)
            pw_res[[j]][is.na(pw_res[[j]])] <- 0
          }
        }
        #
      }
      #
      # pairwise comparisons
      # pw_names = pairwise population names
      pw_nms <- paste(accDat$pop_names[pw[1,]],
                      accDat$pop_names[pw[2,]], sep = " vs. ")
      #
      pw_nms1 <- paste(pw[1,], pw[2,], sep = " vs. ")
      #
      names(pw_res) <- namer
      #
      pw_res1 <- pw_res
      if(fst == TRUE){
        for(i in 1:8){
          dimnames(pw_res1[[i]]) <- list(pw_nms, 
                                         c("Mean", "Lower_CI", "Upper_CI"))
        }
      } else {
        for(i in 1:6){
          dimnames(pw_res1[[i]]) <- list(pw_nms, 
                                         c("Mean", "Lower_CI", "Upper_CI"))
        }
      }
      # bs results output object header
      hdr <- matrix(c("Pairwise", "Mean", "Lower_95%CI", "Upper_95%CI"),
                    ncol = 4)
      pw_bs_out <- matrix(rbind(hdr, c(names(pw_res)[1],"" ,"" ,""),
                                cbind(pw_nms, pw_res[[1]])), ncol = 4)
      if(fst == TRUE){
        for(i in 2:8){
          pw_bs_out <- matrix(rbind(pw_bs_out, c(names(pw_res)[i], "", "", ""),
                                    cbind(pw_nms, pw_res[[i]])), ncol = 4)
        }
      } else {
        for(i in 2:6){
          pw_bs_out <- matrix(rbind(pw_bs_out, c(names(pw_res)[i], "", "", ""),
                                    cbind(pw_nms, pw_res[[i]])), ncol = 4)
        }
      }
      if(!is.null(on)){
        if(write_res==TRUE){
          write.xlsx(pw_bs_out,file=paste(of,"[divPart].xlsx",sep=""),
                     sheetName="Pairwise_bootstrap",col.names=F,
                     row.names=F,append=T)
        } else {
          # text file alternatives
          pw_bts<-file(paste(of,"Pairwise-bootstrap[divPart].txt",sep=""), "w")
          cat(paste(colnames(pw_bs_out),sep=""),"\n",sep="\t",file=pw_bts)
          for(i in 1:nrow(pw_bs_out)){
            cat(pw_bs_out[i,],"\n",file=pw_bts,sep="\t")
          }
          close(pw_bts)
        }
      }
    }
    zzz<-gc()
    rm(zzz)
    ############################################################################
    #pw plotter
    if(plot_res==TRUE && plt==TRUE && bspw==TRUE){
      pwso<-list()
      for(i in 1:length(pw_res)){
        pwso[[i]]<-order(pw_res[[i]][,1],decreasing=F)
        #if(length(pwso[[i]]) >= 100){
        #  pwso[[i]]<-pwso[[i]][(length(pwso[[i]])-99):length(pwso[[i]])]
        #}
      }
      names(pwso)<-namer
      # define plot parameters 
      plot.call_pw<-list()
      plot.extras_pw<-list()
      xy.labels_pw<-list()
      y.pos_pw<-list()
      x.pos_pw=1:length(pwso[[i]])
      fn_pre_pw<-list()
      direct=of
      #Plot Gst_Nei
      plot.call_pw[[1]]=c("plot(pw_res[[4]][pwso[[4]],1],
                          ylim=c(0,(max(pw_res[[4]][,3])+
                          min(pw_res[[4]][,3]))),xaxt='n',
                          ylab=names(pw_res)[4],type='n',
                          xlab='Pairwise comparisons 
                          \n (Hover over a point to see pairwise info.)',
                          cex.lab=1.2,cex.axis=1.3,las=1)")
      
      plot.extras_pw[[1]]=c("points(pw_res[[4]][pwso[[4]],1],
                            pch=15,col='black',cex=1);
                            arrows(1:length(pwso[[4]]),pw_res[[4]][pwso[[4]],2],
                            1:length(pwso[[4]]),pw_res[[4]][pwso[[4]],3],code=3,
                            angle=90,length=0.05,lwd=0.1);
                            abline(h=as.numeric(plot_data321[5]),
                            lwd=1,lty=2,col='red')")
      
      xy.labels_pw[[1]]=data.frame(pairwise_name=pw_nms[pwso[[4]]],
                                   Gst_Nei=round(pw_res[[4]][pwso[[4]],1],4),
                                   Gst_Hedrick=round(pw_res[[5]][pwso[[4]],1],4),
                                   D_jost=round(pw_res[[6]][pwso[[4]],1],4))
      
      y.pos_pw[[1]]=pw_res[[4]][pwso[[4]],1]
      fn_pre_pw[[1]]<-names(pw_res)[4]
      
      
      
      # Plot Gst_Hedrick
      plot.call_pw[[2]]=c("plot(pw_res[[5]][pwso[[5]],1],
                          ylim=c(0,1),xaxt='n',ylab=names(pw_res)[5],type='n',
                          xlab='Pairwise comparisons
                          \n (Hover over a point to see pairwise info.)',
                          cex.lab=1.2,cex.axis=1.3,las=1)")
      
      plot.extras_pw[[2]]=c("points(pw_res[[5]][pwso[[5]],1],
                            pch=15,col='black',cex=1);
                            arrows(1:length(pwso[[5]]),pw_res[[5]][pwso[[5]],2],
                            1:length(pwso[[5]]),pw_res[[5]][pwso[[5]],3],code=3,
                            angle=90,length=0.05,lwd=0.1);
                            abline(h=as.numeric(plot_data321[6]),
                            lwd=1,lty=2,col='red')")
      
      xy.labels_pw[[2]]=data.frame(pairwise_name=pw_nms[pwso[[5]]],
                                   Gst_Nei=round(pw_res[[4]][pwso[[5]],1],4),
                                   Gst_Hedrick=round(pw_res[[5]][pwso[[5]],1],4),
                                   D_jost=round(pw_res[[6]][pwso[[5]],1],4))
      
      y.pos_pw[[2]]=pw_res[[5]][pwso[[5]],1]
      fn_pre_pw[[2]]<-names(pw_res)[5]
      
      
      # Plot D_jost
      plot.call_pw[[3]]=c("plot(pw_res[[6]][pwso[[6]],1],
                          ylim=c(0,1),xaxt='n',ylab=names(pw_res)[6],type='n',
                          xlab='Pairwise comparisons 
                          \n (Hover over a point to see pairwise info.)',
                          cex.lab=1.2,cex.axis=1.3,las=1)")
      
      plot.extras_pw[[3]]=c("points(pw_res[[6]][pwso[[6]],1],
                            pch=15,col='black',cex=1);
                            arrows(1:length(pwso[[6]]),pw_res[[6]][pwso[[6]],2],
                            1:length(pwso[[6]]),pw_res[[6]][pwso[[6]],3],code=3,
                            angle=90,length=0.05,lwd=0.1);
                            abline(h=as.numeric(plot_data321[7]),
                            lwd=1,lty=2,col='red')")
      
      xy.labels_pw[[3]]=data.frame(pairwise_name=pw_nms[pwso[[6]]],
                                   Gst_Nei=round(pw_res[[4]][pwso[[6]],1],4),
                                   Gst_Hedrick=round(pw_res[[5]][pwso[[6]],1],4),
                                   D_jost=round(pw_res[[6]][pwso[[6]],1],4))
      
      y.pos_pw[[3]]=pw_res[[6]][pwso[[6]],1]
      fn_pre_pw[[3]]<-names(pw_res)[6]
      #plot(Fst_WC)
      if(fst==TRUE){
        plot.call_pw[[4]]=c("plot(pw_res[[8]][pwso[[8]],1],
                            ylim=c(0,(max(pw_res[[8]][,3])+
                            min(pw_res[[8]][,3]))),xaxt='n',ylab=names(pw_res)[8],type='n',
                            xlab='Pairwise comparisons 
                            \n (Hover over a point to see pairwise info.)',
                            cex.lab=1.2,cex.axis=1.3,las=1)")
        
        plot.extras_pw[[4]]=c("points(pw_res[[8]][pwso[[8]],1],
                              pch=15,col='black',cex=1);
                              arrows(1:length(pwso[[8]]),pw_res[[8]][pwso[[8]],2],
                              1:length(pwso[[8]]),pw_res[[8]][pwso[[8]],3],code=3,
                              angle=90,length=0.05,lwd=0.1);
                              abline(h=as.numeric(plot_data321[7]),
                              lwd=1,lty=2,col='red')")
        
        xy.labels_pw[[4]]=data.frame(pairwise_name=pw_nms[pwso[[8]]],
                                     Gst_Nei=round(pw_res[[4]][pwso[[8]],1],4),
                                     Gst_Hedrick=round(pw_res[[5]][pwso[[8]],1],4),
                                     D_jost=round(pw_res[[6]][pwso[[8]],1],4),
                                     Fst_WC=round(pw_res[[8]][pwso[[8]],1],4))
        
        y.pos_pw[[4]]=pw_res[[8]][pwso[[8]],1]
        fn_pre_pw[[4]]<-names(pw_res)[8]
      }
    }
    ############################### Bootstrap end ################################
    
    
    ################################# Plot resuts ################################
    #make necessary data available
    if(plt==TRUE && plot_res==TRUE && bsls==TRUE && bspw==TRUE){
      pl<-list(bs_res=bs_res,
               pw_res=pw_res,
               accDat=accDat,
               lso123=lso123,
               pwso=pwso,
               plot.call_loci=plot.call_loci,
               plot.extras_loci=plot.extras_loci,
               xy.labels_loci=xy.labels_loci,
               x.pos_loci=x.pos_loci,
               y.pos_loci=y.pos_loci,
               fn_pre_loci=fn_pre_loci,
               direct=direct,
               plot_loci="TRUE",
               plot_pw="TRUE",
               plot.call_pw=plot.call_pw,
               plot.extras_pw=plot.extras_pw,
               xy.labels_pw=xy.labels_pw,
               y.pos_pw=y.pos_pw,
               fn_pre_pw=fn_pre_pw,
               x.pos_pw=x.pos_pw,
               pw=pw,
               plot_data321=plot_data321,
               fst=fst)
    } else if (plt==TRUE && plot_res==TRUE && bsls==TRUE && bspw==FALSE){
      pl<-list(bs_res=bs_res,
               accDat=accDat,
               lso123=lso123,
               plot.call_loci=plot.call_loci,
               plot.extras_loci=plot.extras_loci,
               xy.labels_loci=xy.labels_loci,
               x.pos_loci=x.pos_loci,
               y.pos_loci=y.pos_loci,
               fn_pre_loci=fn_pre_loci,
               direct=direct,
               plot_loci="TRUE",
               plot_pw="FALSE",
               plot_data321=plot_data321,
               fst=fst)
    } else if (plt==TRUE && plot_res==TRUE && bsls==FALSE && bspw==TRUE){
      pl<-list(pw_res=pw_res,
               accDat=accDat,
               pwso=pwso,
               plot.call_pw=plot.call_pw,
               plot.extras_pw=plot.extras_pw,
               xy.labels_pw=xy.labels_pw,
               x.pos_pw=x.pos_pw,
               y.pos_pw=y.pos_pw,
               fn_pre_pw=fn_pre_pw,
               direct=direct,
               plot_loci="FALSE",
               plot_pw="TRUE",
               pw=pw,plot_data321=plot_data321,
               fst=fst)
    }
    if(!is.null(on)){
      if (plt==TRUE && plot_res==TRUE){
        suppressWarnings(plotter(x=pl,img="1000x600"))
      }
    }
    zzz<-gc()
    rm(zzz)
    
    if(pWise | bspw){
      # Create mean pairwise values (for Erin Landguth 12/12)
      meanPairwise <- lapply(pwMatListOut, function(x){
        mean(x, na.rm = TRUE)
      })
      names(meanPairwise) <- names(pwMatListOut)
    }
    
    
    #############################################################################
    #Data for output
    if(bspw == TRUE && bsls == TRUE){
      list(standard = ot1out,
           estimate = ot2out,
           pairwise = pwMatListOut,
           meanPairwise = meanPairwise,
           bs_locus = bs_res1,
           bs_pairwise = pw_res1)
    } else if(bspw == TRUE && bsls == FALSE){
      list(standard = ot1out,
           estimate = ot2out,
           pairwise = pwMatListOut,
           meanPairwise = meanPairwise,
           bs_pairwise = pw_res1)
    } else if(bspw == FALSE && bsls == TRUE && pWise == TRUE){
      list(standard = ot1out,
           estimate = ot2out,
           pairwise = pwMatListOut,
           meanPairwise = meanPairwise,
           bs_locus = bs_res1)
    } else if(bspw == FALSE && bsls == FALSE && pWise == TRUE){
      list(standard = ot1out,
           estimate = ot2out,
           pairwise = pwMatListOut,
           meanPairwise = meanPairwise)
    } else if(bspw == FALSE && bsls == TRUE && pWise == FALSE){
      list(standard = ot1out,
           estimate = ot2out,
           bs_locus = bs_res1)
    } else if(bspw == FALSE && bsls == FALSE && pWise == FALSE){
      list(standard = ot1out,
           estimate = ot2out)
    }
  }
}

################################################################################
# divPart end                                                                  #
################################################################################

