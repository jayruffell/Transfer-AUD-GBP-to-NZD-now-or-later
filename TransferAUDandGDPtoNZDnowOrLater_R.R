#this is a test

testinput <- 1:100
testVec <- vector()

for(i in 1:100){
  testVec[i] <- testinput[i]^2
  #print([i])
}
t
#now i'm changing the file by addin gthis comment, and uploading (pushing) without 'committing'. should come up as a separate
#commit to the earlier version.
#==> actually no, 'pushing' without first 'committing' seems to do nothing. I think you have to C then P, and when you do this
#the different versions of the file come up in the committ's history.

#now, these comments are after adding a new branch on github - so am I now typing on the master, or the 'new branch'?