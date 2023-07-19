# Attempting to do a function to add a forest plot legend to 
# a forest plot

# will display a black square-and-line, and possibly a diamond

# will add "99%" or "95%"

# This version puts the bottom left corner of the legend at a specific x, y
# in user coordinates

ForLegend<-function(x, y, PerWidth=10, 
   PerHeight=5, cexval=1, LegTitle="Confidence intervals:", 
   ConfDiamond=95, ConfSquare=99, ConfToken="% CI")
   {
   # x: x-location of bottom left corner
   # y: y-location ditto
   # PerWidth: percentage of plot allowed for legend width
   # PerHeight: percentage of plot allowed for legend height
   # cexval: text size multiplier
   # ConfDiamond: confidence value for diamond, NA to suppress diamond
   # ConfSquare: confidence value for square, NA to suppress square
   # ConfToken: what follows ConfDiamond or ConfSquare in the label
   


   # height and width, coordinates of the edges of the margins
   # user coodinates; now only 
   plotsides <- par("usr")
   plotheight <- plotsides[4] - plotsides[3]
   plotwidth <- plotsides[2] - plotsides[1]

   # plot size in inches
   plotin <- par("pin")
   # conversion factors, inches to user coordinates
   xf <- plotwidth/plotin[1]
   yf <- plotheight/plotin[2]

   # margins
   marsize <- par("mai")
   # user coordinate locations of edges of margins, which is what I was after
   marbottom <- plotsides[3] - yf*marsize[1]
   marleft <- plotsides[1] - xf*marsize[2]
   martop <- plotsides[4] + yf*marsize[3]
   marright <- plotsides[2] + xf*marsize[4]

   legheight <- (plotheight+marbottom+martop)*PerHeight/100
   legwidth <- (plotwidth+marleft+marright)*PerWidth/100

   # locations
   legleft <- x
   legbottom <- y
   legright <- legleft + legwidth
   legtop <- legbottom + legheight

   # how many rows?
   nrows <- 0
   if (!is.na(ConfDiamond)) nrows <- nrows + 1
   if (!is.na(ConfSquare)) nrows <- nrows + 1
   if (nrows==0) stop("ForLegend: no points requested")

   # allow drawing outside the plot area
   par(xpd=NA)

   # title
   titleheight <- strheight(LegTitle, cex=cexval, font=2)
   ytitle <- legtop - 0.5*titleheight
   text(legleft, ytitle, LegTitle, font=2, cex=cexval, adj=0)
   titlebottom <- legtop - titleheight

   # is remaining space enough? 
   if (titleheight>=legheight/(nrows+1)) stop("ForLegend: not enough vertical space")

   # allocating space into rows
   ptspace <- titlebottom - legbottom
   if (nrows==2) pty <- legbottom + c(0.75, 0.25)*ptspace
   if (nrows==1) pty <- legbottom + 0.5*ptspace

   # horizontal space allocation
   labspace <- strwidth(paste0(" 95", ConfToken), font=2, cex=cexval)
   if (labspace>=0.67*legwidth) stop("ForLegend: not enough horizontal space") 
   labx <- legright - labspace

   ipt <- 1
   if (!is.na(ConfSquare))
      {
      segments(legleft, pty[ipt], labx, pty[ipt])
      points(0.5*(legleft+labx), pty[ipt], cex=cexval, pch=22, bg="black")
      text(labx, pty[ipt], paste0(" ", ConfSquare, ConfToken), 
         cex=cexval, font=2, adj=0)
      ipt <- ipt + 1
      }
   if (!is.na(ConfDiamond))
      {
      # diamond is to be one character high and three times as long
      diHeight <- strheight("I", cex=cexval, font=2)
      diWidth <- 3*xf*(diHeight/yf)
      xat <- 0.5*(legleft+labx)
      xcorners <- c(xat-diWidth/2, xat, xat+diWidth/2, xat)
      ycorners <- c(pty[ipt], pty[ipt]+diHeight/2, 
         pty[ipt], pty[ipt]-diHeight/2)
      polygon(xcorners, ycorners, border="black", col="white")
      text(labx, pty[ipt], paste0(" ", ConfDiamond, ConfToken), 
         cex=cexval, font=2, adj=0)
      ipt <- ipt + 1
      }
   }








