##[R-Geostatistics]=group
##showplots
##Layer=vector
##X=Field Layer
##Group=optional Field Layer
##Log_Scale = boolean False
require(ggplot2)

#replace spaces in the field names with point
Layer@data <- data.frame(lapply(Layer@data, trimws))

# repalce the field names for matching
X <- gsub("\\s", ".", X)

# remove duplicates and null values before running the variogram
Layer <- subset(Layer, !is.na(Layer[[X]]))

Layer[[X]] <- as.numeric(Layer[[X]])

if(Log_Scale){
ax <- scale_y_log10()
} else {
ax <- scale_y_continuous()
}
if(is.null(Group)){
ggplot()+
geom_boxplot(aes(x=1, y=Layer[[X]]))+
xlab("")+
ylab(X)+
ax +
ggtitle(paste("Boxplot of", X))
} else {
ggplot()+
geom_boxplot(aes(x=Layer[[Group]], y=Layer[[X]], fill=as.factor(Layer[[Group]])))+
xlab(Group)+
ylab(X)+
scale_fill_discrete(name=Group) +
ax +
ggtitle(paste("Boxplot of", X))
}