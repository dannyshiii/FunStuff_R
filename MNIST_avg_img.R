# This shows the average image of each digits from
# the MNIST Handwriting dataset
# Download data here: https://www.kaggle.com/c/digit-recognizer/data
library(ggplot2)
library(grid)
library(readr)

train = data.frame(read_csv("train.csv"))
labels = train[,1]

par(mfrow=c(5,2),pty='s',mar=c(1,1,1,1))
all_img=array(dim=c(10,28*28))
for(k in 0:9) {
  all_img[k+1,] = apply(train[labels==k,-1],2,sum)
  all_img[k+1,] = all_img[k+1,] / max(all_img[k+1,]) * 255
  z = array(all_img[k+1,],dim=c(28,28))
  z = z[,28:1] # rotate the original image data
  image(1:28,1:28,z,col = grey(0:70/70), main = k)
}