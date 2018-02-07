##SLIC Test thing

library('tidyverse')
library('imager')

slic <- function(im,nS,compactness=1,...)
{
    #If image is in colour, convert to CIELAB
    if (spectrum(im) ==3) im <- sRGBtoLab(im)
 
    #The pixel coordinates vary over 1...width(im) and 1...height(im)
    #Pixel values can be over a widely different range
    #We need our features to have similar scales, so
    #we compute relative scales of spatial dimensions to colour dimensions
    sc.spat <- (dim(im)[1:2]*.28) %>% max #Scale of spatial dimensions
    sc.col <- imsplit(im,"c") %>% map_dbl(sd) %>% max
 
    #Scaling ratio for pixel values
    rat <- .2
    #(<a class="vglnk" href="http://sc.spat/sc.col" rel="nofollow"><span>sc</span><span>.</span><span>spat</span><span>/</span><span>sc</span><span>.</span><span>col</span></a>)/(compactness*10)
 
     
    X <- as.data.frame(im*rat,wide="c") %>% as.matrix
    #Generate initial centers from a grid
    ind <- round(seq(1,nPix(im)/spectrum(im),l=nS))
    #Run k-means
    km <- kmeans(X,X[ind,],...)
 
    #Return segmentation as image (pixel values index cluster)
    seg <- as.cimg(km$cluster,dim=c(dim(im)[1:2],1,1))
    #Superpixel image: each pixel is given the colour of the superpixel it belongs to
    sp <- map(1:spectrum(im),~ km$centers[km$cluster,2+.]) %>% do.call(c,.) %>% as.cimg(dim=dim(im))
    #Correct for ratio
    sp <- sp/rat
    if (spectrum(im)==3)
    {
        #Convert back to RGB
        sp <- LabtosRGB(sp) 
    }
    list(km=km,seg=seg,sp=sp)
}

im=load.image('~/Dropbox/Camera Uploads/ModelDog.jpg')
out=slic(im,600)
quartz()
#postscript('~/Dropbox/Dissertation/figures/hamilton.eps',width=7,height=5)
par(mfrow=c(1,2))
plot(im,axes=FALSE)
(im*add.colour(abs(imlap(out$seg)) == 0)) %>% plot(axes=FALSE)
#dev.off()

