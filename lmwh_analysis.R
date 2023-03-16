### Thanks to metafor-project.org 
### https://www.metafor-project.org/doku.php/plots:forest_plot_revman


### You will want to use png or pdf or tif to save this res 350, width=3196, height=1648


### Import Dataset
library(readxl)
library(metafor)
dat <- read_excel("Data.xlsx")

### turn the risk of bias into levels

dat[7:12] <- lapply(dat[7:12], factor, levels=c("+", "-", "?"))

### calculate log risk ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)

dat <- escalc(measure="RR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat,
              slab=paste(author, year), drop00=TRUE)
			  
### fit fixed effect MH mode
res <- rma.mh(measure="RR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat,
              slab=paste(author, year), drop00=TRUE)
res

### estimated average risk ratio (and 95% CI/PI)
pred <- predict(res, transf=exp, digits=2)
pred

### need the rounded estimate and CI bounds further below
pred <- formatC(c(pred$pred, pred$ci.lb, pred$ci.ub), format="f", digits=2)

### total number of studies
k <- nrow(dat)
 
### set na.action to "na.pass" (instead of the default, which is "na.omit"),
### so that even study 3 (with a missing log odds ratio) will be shown in the
### forest plot

options(na.action = "na.pass")

### get the weights and format them as will be used in the forest plot

weights <- paste0(formatC(weights(res), format="f", digits=1), "%")
weights[weights == "NA%"] <- ""

### adjust the margins
par(mar=c(10.8,0,1.3,1.3), mgp=c(3,0.2,0), tcl=-0.2)

### forest plot with extra annotations

sav <- forest(res, atransf=exp, at=log(c(0.01,.10, 0.25,0.5, 1, 2,4,10,100)), xlim=c(-30,11),
       xlab="", efac=c(0,4), textpos=c(-30,-4.7), lty=c(1,1,0), refline=NA,
       ilab=cbind(ai, n1i, ci, n2i, weights),
       ilab.xpos=c(-20.6,-18.6,-16.1,-14.1,-10.8), ilab.pos=2,
       cex=0.78, header=c("Study","MH, Fixed, 95% CI"), mlab="")

### add horizontal line at the top
segments(sav$xlim[1]+0.5, k+1, sav$xlim[2], k+1, lwd=0.8)

### add vertical reference line at 0
segments(0, -2, 0, k+1, lwd=0.8)

### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)
 
### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex, font=2)

text(sav$ilab.xpos, k+2, pos=2, c("Events","Total","Events","Total","Weight"))
text(c(mean(sav$ilab.xpos[1:2]),mean(sav$ilab.xpos[3:4])), k+3, pos=2, c("Treatment","Prophylaxis"))
text(sav$textpos[2], k+3, "Risk ratio", pos=2)
text(0, k+3, "Risk ratio")
text(sav$xlim[2]-0.6, k+3, "Risk of Bias", pos=2)
text(0, k+2, "MH, Fixed, 95% CI")
text(c(sav$xlim[1],sav$ilab.xpos[c(2,4,5)]), -1, pos=c(4,2,2,2,2),
     c("Total (95% CI)", sum(dat$n1i), sum(dat$n2i), "100.0%"))
text(sav$xlim[1], -6, pos=4, "Risk of bias legend")

### first hide the non-bold summary estimate text and then add it back in bold font
rect(sav$textpos[2], -1.5, sav$ilab.xpos[5], -0.5, col="white", border=NA)
text(sav$textpos[2], -1, paste0(pred[1], " [", pred[2], ",  ", pred[3], "]"), pos=2)

### use a non-bold font for the rest of the text
par(cex=sav$cex, font=1)

### add 'Favours '/'Favours proph' text below the x-axis
text(log(c(.01, 100)), -4, c("Favors Treatment","Favors Prophylaxis"), pos=c(4,2), offset=-0.5)

### add 'Not estimable' for study with missing log risk ratio
text(sav$textpos[2], k+1-which(is.na(dat$yi)), "Not estimable", pos=2)

### add text for total events
text(sav$xlim[1], -2, pos=4, "Total events:")
text(sav$ilab.xpos[c(1,3)], -2, c(sum(dat$ai),sum(dat$ci)), pos=2)

### add text with heterogeneity statistics
text(sav$xlim[1], -3, pos=4, bquote(paste("Heterogeneity: ", "Tau"^2, " = ",
     .(formatC(res$tau2, digits=2, format="f")), "; ", "Chi"^2, " = ",
     .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
     " (P = ", .(formatC(res$QEp, digits=2, format="f")), "); ", I^2, " = ",
     .(formatC(res$I2, digits=0, format="f")), "%")))
	 
### add text for test of overall effect
text(sav$xlim[1], -4, pos=4, bquote(paste("Test for overall effect: Z = ",
     .(formatC(res$zval, digits=2, format="f")),
     " (P = ", .(formatC(res$pval, digits=2, format="f")), ")")))
	 
### add text for test of subgroup differences
# text(sav$xlim[1], -5, pos=4, bquote(paste("Test for subgroup differences: Not applicable")))

### add risk of bias points and symbols
cols <- c("#00cc00", "#cc0000", "#eeee00")
syms <- levels(dat$rb.a)
pos  <- seq(sav$xlim[2]-5.5,sav$xlim[2]-0.5,length=6)
for (i in 1:6) {
   points(rep(pos[i],k), k:1, pch=19, col=cols[dat[[6+i]]], cex=2.2)
   text(pos[i], k:1, syms[dat[[6+i]]], font=2)
}
text(pos, k+2, c("A","B","C","D","E","F"), font=2)

### add risk of bias legend
text(sav$xlim[1], -7:-12, pos=4, c(
                           "(A) Bias arising from the randomization process",
                           "(B) Bias due to deviations from the intended intervention",
                           "(C) Bias due to missing outcome data",
                           "(D) Bias in measurement of the outcome",
                           "(E) Bias in selection of the reported result",
                           "(F) Overall"))


### You will want to use png or pdf or tif to save this res 350, width=3196, height=1648.  Then you paste the 2 together.
 						   
# png(filename="D:/OneDrive - McGill University/Papers/ARCHIVE - Published/Covid anticoag/forest.png", res=350, width=3196, height=1648)
#dev.off()


###Probability plot -
library(ggplot2)

### You will want to use png or pdf or tif to save this res 350, width=3196, height=1648

rexp2 <- function(x){round(exp(x),digits=2)}
#calculate probablities
mean<- log(0.77)
sd<-(log(0.94)-log(0.63))/3.92
#less than RR 0.831 (1% ARR)
lim1<-round(100*pnorm(log(0.831),mean,sd),1)
#less than RR 1 (0%ARR)
lim2<-round(100*pnorm(log(1),mean,sd),1)
text1<-paste0("Probability ARR >0% (dark + light blue): ",lim2,"%\nProbability ARR >=1% (light blue): ")
text1<-paste0(text1,lim1,"%")

ggplot(data.frame(x = c(log(0.5), 0.75)), aes(x = x)) +
geom_vline(xintercept = 0, linetype="dashed",color="red")+  
theme_bw()+
stat_function(fun = dnorm, args=c(mean,sd),size=0.5) +
geom_area(stat ="function", fun=dnorm, fill="darkblue", alpha=0.2, xlim=c(-0.6,log(0.831)),args=c(mean,sd))+
geom_area(stat ="function", fun=dnorm, fill="darkblue", alpha=0.6, xlim=c(log(0.831),0),args=c(mean,sd))+
theme(panel.grid.minor = element_blank(),panel.grid.major=element_blank(),legend.position = "none",panel.border = element_blank(),axis.line = element_line(colour = "black"),
axis.title.y = element_text(angle = 180))+
scale_y_continuous(position="right")+
labs(
        x="Risk Ratio",
        y="Probability Density"
    )+
geom_label(aes(x=0.15,y=3,label=text1,size=3))+
scale_x_continuous(labels = rexp2, breaks=c(log(0.5),log(0.75),log(1),log(1.25),log(1.5)),limits=c(log(0.5),log(1.5)))
