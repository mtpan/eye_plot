# Graphics 
library(ggplot2)
library(dplyr)
library(Hmisc)

eye_ori <- read.csv("L:\\Students\\SPan\\Deng\\outcome.csv")

eye <- eye_ori[1:58,]

eye <- eye %>% arrange(procedure_type, published.year) 

eye <- eye %>% select(procedure_type,First.author,published.year,No..of.eyes,number_of_eyes_success,
                      number_of_eyes_partial_success, number_of_eyes_failure)

# Checking if they sum up to the total number of eyes 
t <- cbind(eye$number_of_eyes_success,eye$number_of_eyes_partial_success, eye$number_of_eyes_failure, dims = )
v <- rowSums(t,na.rm = T) 
v == eye$No..of.eyes

# Calculate the success rate and the CI
# add the ratio and CI
att <- binconf(eye$number_of_eyes_success,eye$No..of.eyes) 
eye <- cbind(eye,att)
row.names(eye) <- NULL

# round the lower and upper CI
eye$Lower <- round(eye$Lower,2)
eye$Upper <- round(eye$Upper,2)
eye$PointEst <- round(eye$PointEst,2)

# normalize the number of eyes to draw points 
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x))) # largest would be 1, smallest zero 
}
eye$scaled_points <- normalize(eye$No..of.eyes)

par(mar = c(0,0,0,0)) 
plot(c(0, 21), c(180, 0), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 10, y = 178, paste("Eye Procedure Success Rate"), 
     cex = 1.3, col = "black", family="serif", font=2, adj=0.5)
text(x = 10, y = 172, paste("Author (Year)              No. Eyes              Success Ratio with CI              Ratio [95% CI]"), 
     cex = 0.7, col = "black", family="serif", font=2, adj=0.5)

for (i in 1:7) { # Author(Year) allo_culture_LC_transplant
  text(x = 6.6, y = 170-i*2, paste(eye$First.author[i], "(", eye$published.year[i], ")"), 
       cex = 0.5, col = "darkolivegreen3", family="serif", font=2, adj=1)
}
for (i in 1:7){  # No. Eyes allo_culture_LC_transplant
text(x = 8.0, y = 170-i*2, paste(eye$No..of.eyes[i]), 
          cex = 0.5, col = "darkolivegreen3", family="serif", font=2, adj=0.5)
}
for (i in 1:7){  # Ratio[CI] allo_culture_LC_transplant
  text(x = 13.5, y = 170-i*2, paste(eye$PointEst[i]," [",eye$Lower[i],",", eye$Upper[i],"]"), 
       cex = 0.5, col = "darkolivegreen3", family="serif", font=2, adj=0)
}
for(i in 8:30){ # Author(Year)	allogenic_direct_LT
  text(x = 6.6, y = 156-(i-7)*2, paste(eye$First.author[i], "(", eye$published.year[i], ")"), 
       cex = 0.5, col = "lightsalmon1", family="serif", font=2, adj=1)
}
for(i in 8:30){ # No. Eyes1	allogenic_direct_LT
  text(x = 8.0, y = 156-(i-7)*2, paste(eye$No..of.eyes[i]), 
       cex = 0.5, col = "lightsalmon1", family="serif", font=2, adj=0.5)
}
for (i in 8:30){  # Ratio[CI] allogenic_direct_LT
  text(x = 13.5, y = 156-(i-7)*2, paste(eye$PointEst[i]," [",eye$Lower[i],",", eye$Upper[i],"]"), 
       cex = 0.5, col = "lightsalmon1", family="serif", font=2, adj=0)
}
for(i in 31:42){ # Author(Year) auto_culture_LC_transplant
  text(x = 6.6, y = 110-(i-30)*2, paste(eye$First.author[i], "(", eye$published.year[i], ")"), 
       cex = 0.5, col = "skyblue2", family="serif", font=2, adj=1)
}
for(i in 31:42){ # No. Eyes auto_culture_LC_transplant
  text(x = 8.0, y = 110-(i-30)*2, paste(eye$No..of.eyes[i]), 
       cex = 0.5, col = "skyblue2", family="serif", font=2, adj=0.5)
}
for (i in 31:42){  # Ratio[CI] auto_culture_LC_transplant
  text(x = 13.5, y = 110-(i-30)*2, paste(eye$PointEst[i]," [",eye$Lower[i],",", eye$Upper[i],"]"), 
       cex = 0.5, col = "skyblue2", family="serif", font=2, adj=0)
}
for(i in 43:58){ # Author(Year) autologous_direct_LT
  text(x = 6.6, y = 86-(i-42)*2, paste(eye$First.author[i], "(", eye$published.year[i], ")"), 
       cex = 0.5, col = "mediumpurple1", family="serif", font=2, adj=1)
}
for(i in 43:58){ # No. Eyes autologous_direct_LT
  text(x = 8.0, y = 86-(i-42)*2, paste(eye$No..of.eyes[i]), 
       cex = 0.5, col = "mediumpurple1", family="serif", font=2, adj=0.5)
}
for (i in 43:58){  # Ratio[CI] autologous_direct_LT
  text(x = 13.5, y = 86-(i-42)*2, paste(eye$PointEst[i]," [",eye$Lower[i],",", eye$Upper[i],"]"), 
       cex = 0.5, col = "mediumpurple1", family="serif", font=2, adj=0)
}

# draw line segments 
for (i in 1:58) {
  segments(x0=9.3+eye$Lower[i]*3, y0=170-2*i, x1=9.3+eye$Upper[i]*3,y1=170-2*i)
}

# draw points 
for(i in 1:58){
  if (eye$scaled_points[i] < 0.5){
  points(x = 9.3+eye$PointEst[i]*3, y = 170-2*i, pch = 19, cex=eye$scaled_points[i]+0.6)
  }else
    points(x = 9.3+eye$PointEst[i]*3, y = 170-2*i, pch = 19, cex=eye$scaled_points[i]+0.2)
}

# draw vertical dashed lines 
segments(x0=9.3+0.5214*3, y0=170-2*0, x1=9.3+0.5214*3, y1=170-2*8, lty = 6,lwd = 1.7, col = "darkolivegreen3")
segments(x0=9.3+0.5393*3, y0=170-2*8, x1=9.3+0.5393*3, y1=170-2*31, lty = 6,lwd = 1.7, col = "lightsalmon1")
segments(x0=9.3+0.7182*3, y0=170-2*31, x1=9.3+0.7182*3, y1=170-2*43, lty = 6,lwd = 1.7, col = "skyblue2")
segments(x0=9.3+0.83154*3, y0=170-2*43, x1=9.3+0.8315*3, y1=170-2*59, lty = 6,lwd = 1.7, col = "mediumpurple1")
#allo_culture_LC_transplant	cALLT		0.5214  x-axis 10.8642     0.65-0.39 = 0.26
#allogenic_direct_LT	ALLT		   0.5393            10.9179     0.62-0.46 = 0.16
#auto_culture_LC_transplant	   	0.7182              11.4546    0.80-0.62 = 0.18
#autologous_direct_LT	AULT	   0.8315               11.79462   0.88-0.77 = 0.11

# draw 0-1 line 
segments(x0=9.3+min(eye$Lower)*3, y0=170-2*62, x1=9.3+max(eye$Upper)*3,y1=170-2*62,lwd=1.7)
text(x = 9.3+min(eye$Lower)*3, y =170-2*63, paste("0"), 
     cex = 0.7, col = "black", family="serif", font=2, adj=0.5)
text(x = 9.3+max(eye$Upper)*3, y =170-2*63, paste("1"), 
     cex = 0.7, col = "black", family="serif", font=2, adj=0.5)

# draw legends 
legend(4, 42.7, legend=c("cALLT", "ALLT", "cAULT","AULT"), 
       fill=c("darkolivegreen3", "lightsalmon1","skyblue2","mediumpurple1"),  cex=0.7,ncol = 1,
       title = "Procedure Type",x.intersp = 0.6,y.intersp = 0.9,inset = 0)

# draw diamonds 
polygon(x=c(10.8642-0.26*2,10.8642,10.8642+0.26*2,10.8642), 
        y=c(34,35.5,34,32.5),col = "darkolivegreen3",border = NA)
polygon(x=c(10.9179-0.16*2,10.9179,10.9179+0.16*2,10.9179), 
        y=c(30,31.5,30,28.5),col = "lightsalmon1",border = NA)
polygon(x=c(11.4546-0.18*2,11.4546,11.4546+0.18*2,11.4546), 
        y=c(26,27.5,26,24.5),col = "skyblue2",border = NA)
polygon(x=c(11.79462-0.11*2,11.79462,11.79462+0.11*2,11.79462), 
        y=c(22,23.5,22,20.5),col = "mediumpurple1",border = NA)

segments(x0=10.8642, y0=35.5, x1=10.8642, y1=32.5,lwd = 1.3)
segments(x0=10.9179, y0=31.5, x1=10.9179, y1=28.5,lwd = 1.3)
segments(x0=11.4546, y0=27.5, x1=11.4546, y1=24.5,lwd = 1.3)
segments(x0=11.79462, y0=23.5, x1=11.79462, y1=20.5,lwd = 1.3)

# draw Est. Rate [95% CI]
text(x = 14.9, y =170-2*66, paste("Est. Rate [95% CI]"), 
     cex = 0.85, col = "black", family="serif", font=2, adj=0.5)

text(x = 15, y =170-2*68, paste("0.52 [0.39, 0.65]"), #34
     cex = 0.7, col = "darkolivegreen3", family="serif", font=2, adj=0.5)
text(x = 15, y =170-2*70, paste("0.54 [0.46, 0.62]"), #30
     cex = 0.7, col = "lightsalmon1", family="serif", font=2, adj=0.5)
text(x = 15, y =170-2*72, paste("0.72 [0.62, 0.80]"), 
     cex = 0.7, col = "skyblue2", family="serif", font=2, adj=0.5)
text(x = 15, y =170-2*74, paste("0.83 [0.77, 0.88]"), 
     cex = 0.7, col = "mediumpurple1", family="serif", font=2, adj=0.5)


