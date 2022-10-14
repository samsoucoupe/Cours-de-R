a<-sqrt(3)
cat(a**2==3,'\n')
cat(sprintf('%.17f\n', 0.1))
acc<-0
for (i in 1:100){acc<-acc+1.0}
cat(sprintf('%.17f\n', acc))
acc<-0
for (i in 1:100){acc<-acc+0.1}
cat(sprintf('%.17f\n', acc))