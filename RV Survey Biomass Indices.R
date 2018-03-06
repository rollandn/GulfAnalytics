#########################################################################
#                       RV SURVEY BIOMASS INDICES                       #
#########################################################################
#                           Author N. Rolland                           #
#########################################################################

rm(list = ls())
library(gulf)
cat("\f") 
clg()

sp <- c(10, 12, 40, 41, 42, 43) #species

strata <- list()
strata$all <- 401:439
strata$offshore <- 415:439
strata$inshore <- 401:403

years <- list()
years$all <- 1971:2017
years$inshore <- 1984:2017

##################################################################
# Load set card for all years and strata and apply modifications #
##################################################################

set_all <- read.card(year = years$all, card = "set", stratum = strata$all)

# In 1992 there were two comparative surveys conducted in August, and they should be removed from the analyses (D. Ricard)

rv.176 <- set_all[set_all$cruise.number==176,]
rv.178 <- set_all[set_all$cruise.number==178,]
rv.245 <- set_all[set_all$cruise.number==245,]
table(rbind(rv.176, rv.178, rv.245)$cruise.number, rbind(rv.176, rv.178, rv.245)$month)
'%ni%' <- Negate('%in%')
ii <- which(set_all$cruise.number %ni% c(176,245))
set_all <- set_all[ii, ]

# Remove bad tows

oo <- (set_all$experiment != 3) & (set_all$experiment != 9 | set_all$year == 1987)
set_all <- set_all[oo, ]

##############################
# Start loop for all species #
##############################

for(i in 1:length(sp)){
  
  species.str(sp[i])
  
  fp <- ifelse(sp[i] == 10, "U:/Projects/Indices/Atlantic cod",
               ifelse(sp[i] == 12, "U:/Projects/Indices/White hake",
                      ifelse(sp[i] == 40, "U:/Projects/Indices/American Plaice",
                             ifelse(sp[i] == 41, "U:/Projects/Indices/Witch flounder",
                                    ifelse(sp[i] == 42, "U:/Projects/Indices/Yellowtail flounder",
                                           "U:/Projects/Indices/Winter flounder")))))
  
  #########################################################################
  # Load catch card for all years and strata 415:439 (default) or 401:439 #
  #########################################################################
  
  cat_all_sp <- read.card(year = years$all, card = "catch", stratum = strata$offshore, species = sp[i])
  cat_all_sp <- adjust(cat_all_sp, set_all)
  set_all_sp <- merge.catch(set_all, cat_all_sp)
  
    if (sp[i] %in% c(12, 42, 43)){
      cat_inshore_sp <- read.card(year = years$inshore, card = "catch", stratum = strata$all, species = sp[i])
      cat_inshore_sp <- adjust(cat_inshore_sp, set_all)
      set_inshore_sp <- merge.catch(set_all, cat_inshore_sp)
    }
  
  ############################
  # Plot total catch biomass #
  ############################

  smean_weight_all <- smean(set_all_sp, var = "weight.caught", by = "year")  

    if (sp[i] %in% c(12, 42, 43)){
    smean_weight_inshore <- smean(set_inshore_sp, var = "weight.caught", by = "year")
    smean_weight_inshore <- smean_weight_inshore[smean_weight_inshore$mean != 0, ]
    }
  
  output <- file.path(fp, paste0("RV survey total biomass indices - ", species.str(sp[i]), " - ",
                                 min(years$all),"-",max(years$all),".tiff"))
  
  tiff(output, width = 3500, height = 2500, compression = "lzw", res = 300)
  
  par(mar = c(5,5,2,1))
  
  plot(smean_weight_all$year, smean_weight_all$mean, type = "l", xlab = "", ylab = "", 
       xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_weight_all$upper.cl)+4.99, -1)),
       panel.first = polygon(c(years$all, rev(years$all)), c(smean_weight_all[,"lower.cl"], rev(smean_weight_all[,"upper.cl"])),
                           border = "grey", col = "grey90"))
  
  points(smean_weight_all$year, smean_weight_all$mean, col = "black", pch = 19)
  
    if (sp[i] %in% c(12, 42, 43)){
      lines(smean_weight_inshore$year, smean_weight_inshore$mean, lty = "dashed", lwd = 2, col = "red")
      points(smean_weight_inshore$year, smean_weight_inshore$mean, col = "red", pch = 17)
    }
  
  mz <- mean(smean_weight_all$mean)
  abline(h = mz, col = "black",lty = 2, lwd = 1)
  
  axis(1, at = seq(1971, max(years$all), by = 3), cex.axis = 1.2)
  
  y_by <- round(max(smean_weight_all$upper.cl)+4.99, -1)
  y_by <- ifelse(y_by <= 20, 2,
                 ifelse(y_by > 20 & y_by <= 50, 5,
                        ifelse(y_by > 50 & y_by <= 100, 20,
                               ifelse(y_by > 100 & y_by <= 200, 25,
                                      ifelse(y_by > 200 & y_by <= 500, 50,
                                             ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
  
  axis(2, at = seq(0, round(max(smean_weight_all$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
  
  mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
  mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
  
  ifelse(sp[i] %in% c(12, 42, 43),
    (legend("topright", 
           legend = c("strata 415:439", "strata 401:439", paste0("µ = ", format(round(mz, digits = 2), nsmall = 2))),
           col = c("black", "red", "black"), 
           lwd = 2, 
           bty = "n", 
           lty = c("solid", "dashed", "dashed"),
           pch = c(19, 17, NA_integer_),
           cex = 1.2)),
    (legend("topright", 
           legend = c("strata 415:439", paste0("µ = ", format(round(mz, digits = 2), nsmall = 2))),
           col = c("black", "black"), 
           lwd = 2, 
           bty = "n", 
           lty = c("solid", "dashed"),
           pch = c(19, NA_integer_),
           cex = 1.2)))
  
  title(paste0("RV survey total biomass indices - ", species.str(sp[i]), " - ", min(years$all),"-",max(years$all)))

  dev.off()

  ##############################
  # Plot total catch abundance #
  ##############################

  smean_number_all <- smean(set_all_sp, var = "number.caught", by = "year")  

    if (sp[i] %in% c(12, 42, 43)){
    smean_number_inshore <- smean(set_inshore_sp, var = "number.caught", by = "year")
    smean_number_inshore <- smean_number_inshore[smean_number_inshore$mean != 0, ]
    }

  output <- file.path(fp, paste0("RV survey total abundance indices - ", species.str(sp[i]), " - ",
                                 min(years$all),"-",max(years$all),".tiff"))
  
  tiff(output, width = 3500, height = 2500, compression = "lzw", res = 300)
  
  par(mar = c(5,5,2,1))
  
  plot(smean_number_all$year, smean_number_all$mean, type = "l", xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_number_all$upper.cl)+4.99, -1)),
       panel.first = polygon(c(years$all, rev(years$all)), c(smean_number_all[,"lower.cl"], rev(smean_number_all[,"upper.cl"])),
                           border = "grey", col = "grey90"))
  
  points(smean_number_all$year, smean_number_all$mean, col = "black", pch = 19)
  
    if (sp[i] %in% c(12, 42, 43)){
    lines(smean_number_inshore$year, smean_number_inshore$mean, lty = "dashed", lwd = 2, col = "red")
    points(smean_number_inshore$year, smean_number_inshore$mean, col = "red", pch = 17)
    }

  mz <- mean(smean_number_all$mean)
  abline(h = mz, col = "black",lty = 2, lwd = 1)
  
  axis(1, at = seq(1971, max(years$all), by = 3), cex.axis = 1.2)
  
  y_by <- round(max(smean_number_all$upper.cl)+4.99, -1)
  y_by <- ifelse(y_by <= 20, 2,
                 ifelse(y_by > 20 & y_by <= 50, 5,
                        ifelse(y_by > 50 & y_by <= 100, 20,
                               ifelse(y_by > 100 & y_by <= 200, 25,
                                      ifelse(y_by > 200 & y_by <= 500, 50,
                                             ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
  
  axis(2, at = seq(0, round(max(smean_number_all$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
  
  mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
  mtext("Abundance (number per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)

  ifelse(sp[i] %in% c(12, 42, 43),
         (legend("topright", 
                 legend = c("strata 415:439", "strata 401:439", paste0("µ = ", format(round(mz, digits = 2), nsmall = 2))), 
                 col = c("black", "red", "black"), 
                 lwd = 2, 
                 bty = "n", 
                 lty = c("solid", "dashed", "dashed"),
                 pch = c(19, 17, NA_integer_),
                 cex = 1.2)),
         (legend("topright", 
                 legend = c("strata 415:439", paste0("µ = ", format(round(mz, digits = 2), nsmall = 2))),
                 col = c("black", "black"), 
                 lwd = 2, 
                 bty = "n", 
                 lty = c("solid", "dashed"),
                 pch = c(19, NA_integer_),
                 cex = 1.2)))
  
  title(paste0("RV survey total abundance indices - ", species.str(sp[i]), " - ", min(years$all),"-",max(years$all)))

  dev.off()

  #################################
  # Separate by cutoff and strata #
  #################################

  cutoff <- ifelse(sp[i] == 10, 42,
                   ifelse(sp[i] == 12, 45,
                          ifelse(sp[i] == 40, 30,
                                 ifelse(sp[i] == 41, 30,
                                        ifelse(sp[i] == 42, 25, 25)))))
  if (sp[i] %in% c(10, 40, 41)){
    strata <- strata$offshore
  }
  if (sp[i] %in% c(12, 42, 43)){
    strata <- strata$all
  }
  
  #########################################################################
  # Load catch card for all years and strata 415:439 (default) or 401:439 #
  #########################################################################

  len_all_sp <- read.card(year = years$all, species = sp[i], card = "length", stratum = strata)
  len_all_sp <- adjust(len_all_sp, set_all_sp)
  fvars <- paste0("freq", 0:13)
  
  ###############################################################
  # Calculate length-weight coefficients for weigth conversions #
  ###############################################################

  bio_all_sp <- read.card(year = years$all, card = "bio", species = sp[i], stratum = strata)
  bio_all_sp$sex[bio_all_sp$sex == 0] <- 9
  bio_all_sp <- bio_all_sp[(bio_all_sp$length > 0) & (bio_all_sp$length < 200) & (bio_all_sp$weight > 0), ]
  
  beta <- length.weight(bio_all_sp[bio_all_sp$sex %in% 1:2, ], by = c("year"))

  ######################################
  # Plot weight-at-length through time #
  ######################################

  windows()
  lens <- c(1:25)
  cols <- rainbow(length(lens))
  y_max <- 10^beta$a * max(lens)^beta$b
  plot(range(years$all), c(0, max(round((y_max+4.99), -1))), type = "n", xlab = "Year", ylab = "Weight(g)", cex.lab = 1.2)
  grid()
    for (j in 1:length(lens)){
      mu <- 10^beta$a * lens[j]^beta$b  
      lines(years$all, mu, col = cols[j], lwd = 2)
      sigma <- beta$sigma / sqrt(beta$n)
      lines(years$all, exp(log(mu) + 1.96 * sigma), col = "grey10", lwd = 1, lty = "dashed")
      lines(years$all, exp(log(mu) - 1.96 * sigma), col = "grey10", lwd = 1, lty = "dashed")
    }
  axis(1, at = seq(1975, 2015, by = 10))
  box()

  #################################
  # Convert frequencies to weight #
  #################################

  len_all_sp$sex[len_all_sp$year %in% c(1984:1986)] <- 9
  len_all_sp$sex[!(len_all_sp$sex %in% c(1, 2))] <- 9
  #len_all_sp$sex[(len_all_sp$sex %in% c(0, 1, 2, 4))] <- 9
  sexes <- c(9)
  wlen <- len_all_sp
  wlen[fvars] <- NA
    for (k in 1:length(years$all)){
      print(years$all[k])
      for (l in 1:length(sexes)){
        index <- (len_all_sp$year == years$all[k]) & (len_all_sp$sex == sexes[l])
        if (length(which(index) > 0)){
          lens <- repvec(len_all_sp$start.length[index],
                         ncol = length(fvars)) + repvec(0:(length(fvars)-1), nrow = nrow(len_all_sp[index, ]))
          t <- sort(unique(as.vector(lens)))
          ww <- weight(t, coefficients = beta[beta$year == years$all[k], c("a", "b")], species = sp[i], sex = 9)
          m <- match(lens, t)
          #dim(m) <- dim(lens)
          ww <- ww[m]
          #dim(ww) <- dim(lens)
          wlen[index, fvars] <- ww * len_all_sp[index, fvars]
        }
      }
    }      
  
  wset <- set_all_sp
  set_all_len <- merge.length(set_all_sp, len_all_sp)
  wset <- merge.length(wset, wlen)
  
  # Define frequency variables:
  fvars <- names(set_all_len)[grep("^[0-9]+", names(set_all_len))]
  set_all_len$depth <- round(-depth(longitude(set_all_len), latitude(set_all_len)) )
  set_all_len$depth5 <- round(set_all_len$depth / 5)*5
  set_all_len$depth10 <- round(set_all_len$depth / 10)*10
  
  # Define summary variables:
  set_all_len$total <- apply(set_all_len[fvars], 1, sum)
  set_all_len$l_cutoff <- apply(set_all_len[fvars[as.numeric(fvars) < cutoff]], 1, sum)  
  set_all_len$h_cutoff <- apply(set_all_len[fvars[as.numeric(fvars) >= cutoff]], 1, sum) 
  wset$total <- apply(wset[fvars], 1, sum)
  wset$l_cutoff <- apply(wset[fvars[as.numeric(fvars) < cutoff]], 1, sum)  
  wset$h_cutoff <- apply(wset[fvars[as.numeric(fvars) >= cutoff]], 1, sum) 
  
  res <- list()
  wres <- list()
  vars <- list("l_cutoff", "h_cutoff")
  
    res[["offshore"]] <- list()
    wres[["offshore"]] <- list()
      for (m in 1:length(vars)){
        print(vars[[m]])
        res[[m]] <- smean(subset(set_all_len, stratum = strata), var = vars[[m]], by = "year")
        wres[[m]] <- smean(subset(wset, stratum = strata), var = vars[[m]], by = "year")
      }
    names(res) <- vars
    names(wres) <- vars
  
  # Extract index:
  dbarplot(res$l_cutoff$mean)
  res$l_cutoff$mean * sum(stratum.info(stratum = strata)$trawlable.units)
  dbarplot(res$h_cutoff$mean)
  
  windows()
  plot(log(wset$total), log(set_all_len$weight.caught), 
       xlab = "log(Calculated weight)", ylab = "log(measured weight)",
       pch = 21, bg = "grey", cex = 0.6)
  grid()
  abline(0, 1, lwd = 2, col = "red")
  index <- which((set_all_len$weight.caught == 0) & (set_all_len$total > 0)) # Zero measured weight, but with length-frequencies.
  index <- which((set_all_len$weight.caught > 0) & (set_all_len$total == 0)) # Measured, but no LF sample.
  
  #######################################
  # Plot total catch biomass for cutoff #
  #######################################
  
  output <- file.path(fp, paste0("RV survey total biomass indices by size - ", species.str(sp[i]),
                                 " - ", min(years$all),"-",max(years$all),".tiff"))
  
  tiff(output, width = 3500, height = 3500, compression="lzw", res = 300)
 
  par(mar = c(0, 0, 0, 0))
  
  m <- rbind(c(0,0,0,0,0,0,0,0),
             c(0,1,1,1,1,1,1,0),
             c(0,1,1,1,1,1,1,0),
             c(0,1,1,1,1,1,1,0),
             c(0,2,2,2,2,2,2,0),
             c(0,2,2,2,2,2,2,0),
             c(0,2,2,2,2,2,2,0),
             c(0,0,0,0,0,0,0,0))
  
  layout(m)
  
  # Small fish (< cutoff cm)
  
  plot(wres$l_cutoff$year, wres$l_cutoff$mean, type = "l", xlab = "", ylab = "", 
       xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(wres$l_cutoff$upper.cl)+4.99, -1)),
       panel.first = polygon(c(wres$l_cutoff$year, rev(wres$l_cutoff$year)), c(wres$l_cutoff$lower.cl, rev(wres$l_cutoff$upper.cl)),
                             border = "grey", col = "grey90"))
  
  points(wres$l_cutoff$year, wres$l_cutoff$mean, col = "black", pch = 19)
  
  mz <- mean(wres$l_cutoff$mean)
  abline(h = mz, col = "black", lty = 2, lwd =1)
  
  y_by <- round(max(wres$l_cutoff$upper.cl)+4.99, -1)
  y_by <- ifelse(y_by <= 20, 2,
                 ifelse(y_by > 20 & y_by <= 50, 5,
                        ifelse(y_by > 50 & y_by <= 100, 20,
                               ifelse(y_by > 100 & y_by <= 200, 25,
                                      ifelse(y_by > 200 & y_by <= 500, 50,
                                             ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
  
  axis(2, at = seq(0, round(max(wres$l_cutoff$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
  
  str <- paste0("Small (< ", cutoff, " cm)")
  legend("topright",
         legend = c(paste0("Small (< ", cutoff, " cm)"), paste0("µ = ", format(round(mz, digits = 2), nsmall =2))),
         col = c("black", "black"),
         lwd = 2,
         bty = "n",
         lty = c("solid", "dashed"),
         pch = c(19, NA_integer_),
         cex = 1.4)
  
  text(1994, round(max(wres$l_cutoff$upper.cl)+4.99, -1)-5, 
       paste0("RV survey biomass indices by size\n", species.str(sp[i]), " - ", min(years$all),"-",max(years$all)),
       cex = 1.4, font = 2)
  
  # Large fish (>= cutoff cm)
  
  plot(wres$h_cutoff$year, wres$h_cutoff$mean, type = "l", xlab = "", ylab = "", 
       xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(wres$h_cutoff$upper.cl)+4.99, -1)),
       panel.first = polygon(c(wres$h_cutoff$year, rev(wres$h_cutoff$year)), c(wres$h_cutoff$lower.cl, rev(wres$h_cutoff$upper.cl)),
                             border = "grey", col = "grey90"))
  
  points(wres$h_cutoff$year, wres$h_cutoff$mean, col = "black", pch = 19)
  
  mz <- mean(wres$h_cutoff$mean)
  abline(h = mz, col = "black", lty = 2, lwd =1)
  
  axis(1, at = seq(1971, max(years$all), by = 3), cex.axis = 1.2)
  
  y_by <- round(max(wres$h_cutoff$upper.cl)+4.99, -1)
  y_by <- ifelse(y_by <= 20, 2,
                 ifelse(y_by > 20 & y_by <= 50, 5,
                        ifelse(y_by > 50 & y_by <= 100, 20,
                               ifelse(y_by > 100 & y_by <= 200, 25,
                                      ifelse(y_by > 200 & y_by <= 500, 50,
                                             ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
  
  axis(2, at = seq(0, round(max(wres$h_cutoff$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
  
  mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
  mtext("Biomass indices (kg / tow)", side = 2, line = 3.5, cex = 1.6, font = 2, adj = 2.7)
  
  str <- paste0("Small (< ", cutoff, " cm)")
  legend("topright",
         legend = c(paste0("Large (>= ", cutoff, " cm)"), paste0("µ = ", format(round(mz, digits = 2), nsmall =2))),
         col = c("black", "black"),
         lwd = 2,
         bty = "n",
         lty = c("solid", "dashed"),
         pch = c(19, NA_integer_),
         cex = 1.4)

  dev.off()
  
}
