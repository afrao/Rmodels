#### To find the True positives, false positives, true negatives, false negatives

## note to self
###########################
    #Precision and recall
    # precision is the fraction of retrieved instances that are relevant
    # recall (sensitivity) is the fraction of relevant instances that are retrieved.
###########################

###### Input
# true.data -is a "binary" vector of the classes for the true data set
# model.data -is a "binary" vector of the classes for the model put onto the data set
# val - is the value of the class selected by positve
# positive - tells you if the val is the positive or the negative value

###### Input
# returns the length of the group that your asking for
Finder.of.groups <- function(true.data,model.data,val=0,positive=TRUE,safe.mode = FALSE) {
    if(positive){
        return(length(which(model.data[which(true.data == val)] == val)))
    } else {
        return(length(which(model.data[which(true.data == val)] != val)))
    }
}

###### Output
# returns the 4 components of the confusion matrix in a data.frame
Make.groups.table <- function(true.data,model.data,val.pos=1,val.neg=0,positive=TRUE,safe.mode = FALSE) {

    TP <- Finder.of.groups(true.data,model.data,val.pos)
    FP <- Finder.of.groups(true.data,model.data,val.pos,positive=FALSE)
    TN <- Finder.of.groups(true.data,model.data,val.neg)
    FN <- Finder.of.groups(true.data,model.data,val.neg,positive=FALSE)

    return(data.frame(TP,FP,TN,FN))

}


###### Output
# returns a message but it will hopefully create a file with the file name
# with a pretty table in it.
Make.sweet.table <- function(df,file.name) {
    stargazer(df, summary=FALSE, out=file.name,out.header=TRUE)
    return(paste("made it in",file.name,sep=" "))
}
