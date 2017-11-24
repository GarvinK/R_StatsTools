CellMeans = function(x,values,cat,p=0.05) {
    Y__mean = as.numeric(apply(x[values],2,mean))
    SSto  = sum((x[values]-Y__mean)^2)
    SStr_vec=c()
    cats_vec= (unique(x[cat]))
    cats = levels(as.factor(unlist(x[cat])))
    for (i in cats) {
      SStr_vec = c(SStr_vec, length(x[cat][x[cat]==i])*(mean(x[values][x[cat]==i])-Y__mean)^2)
      }
    SStr = sum(SStr_vec)
    SSE = SSto-SStr
    MSE = SSE / (nrow(x)-length(cats))
    MSTR = SStr/ (length(cats)-1)
    
    print (paste('F-Value: ',  MSTR/MSE))
    print (paste('F-Critical Value: ', qf(p, df1=(length(cats)-1), df2=(nrow(x)-length(cats)),lower.tail=FALSE) ))
    pvalue = pf(MSTR/MSE, df1=(nrow(x)-length(cats)), df2=(length(cats)-1)) 
    print (paste('p-Value: ', pvalue ))
    
    if (pvalue<p){
      catslen = length(cats)
      pvalues = c()
      catA = c()
      catB = c()
      
      for (i in cats) {
        cats = cats[-1]
        
        for (j in cats){
          
          pvalues = c(pvalues, t.test(x[values][x[cat]==i],x[values][x[cat]==j])$p.value)
          catA=c(catA,i)
          catB=c(catB,j)

        }
      }
    }
    res = data.frame(catA,catB,pvalues)
    print('Pairwise Comparison')
    print(res)  
    return(NULL)
}

# Example
# cat = c(rep(c('Category A','Category B','Category C') ,20))
# values= c(2,100,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,1,
#           1,4,4,4,4,4,4,4,4,4,4,2.5,2.5,2.5,2.5,2.5,8,2.5,7,2.5,2.5)
# 
# x = data.frame(cat,values)
# CellMeans(x,'values','cat', p=0.85)
