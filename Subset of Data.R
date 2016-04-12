
#Randomizing indices
set.seed(1)
indices=sample(1:50000, 50000)
train=indices[1:1000]
test=indices[1001:1251]
