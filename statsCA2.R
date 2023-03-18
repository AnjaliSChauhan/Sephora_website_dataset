#----------------------------------------------------------------------------
#Hypothesis Testing

#SHAPIRO TEST
#To check if the data follows normal distribution or not.
#H0: Data follows normal Distribution
#H1: Data does not follows normal Distribution
pro_price= data$price
shapiro.test(pro_price)
#W = 0.72693, p-value < 2.2e-16
#p value is smaller than 0.05, Hence we reject H0.
#which implies that the data doesn't follow normal distribution.

#(1)Is there a significant difference between price and valued price on products on the Sephora website 
#WILcOXON TEST
#H0: There is no significant difference between price and value_price
#H1: There is a significant difference between price and value_price
wilcox.test(data$value_price, data$price)
#W = 1145639, p-value = 0.3501
#alternative hypothesis: true location shift is not equal to 0
#p value is greater than 0.05 (0.3501> 0.05), Hence we accept H0.
#This supports that there is significant difference between price and value_price

value= data$value_price
pri= data$price
boxplot(value, pri,
        col=c("#b94e48","#ce2029"),
        names=c("Value Price", "Price"), ylab="Price in $",
        main="Boxplot for Valued prive and price" )

#(2)Does the Number of reviews received on the product make a difference in the rating?
#Wilconox test
#H0: Number of reviews does not makes a difference on rating  
#H1: Number of reviews makes a difference on rating
wilcox.test(data$number_of_reviews, data$rating)
#W = 0.72693, p-value < 2.2e-16
#p value is smaller than 0.05, Hence we reject H0.
#This confirms that Number of reviews makes a difference

ggplot(data, 
       aes(x = number_of_reviews, 
           y = rating)) +
  labs(title = "Spread of Number of reviews wrt Rating")+
  geom_point(size=2, colour="#8b0000")


#(3)To verify if there is an association between online sales & exclusive sales of products by brand on the Sephora website.
#CHI SQUARE TEST
#H0: There is no association between online sales & exclusive sales 
#H1: There is association between online sales & exclusive sales 
offer=table(data$online_only, data$exclusive)
offer
chisq.test(offer)
#p-value = 0.1585
#p value is greater than 0.05(0.1585> 0.05), Hence we accept H0.
#this conveys that, there is no association between online sales & exclusive sales

table(top10$brand, top10$online_only, top10$exclusive)

        
#(4)To authenticate whether the marketing flag used for marketing affects the brand name on the Sephora website.
#chi test
#H0: There is no association between brand & marketing flags
#H1: There is association between brand & marketing flags
market=table(data$brand, data$MarketingFlags)
market
chisq.test(market)
#p-value = 0.1585
#p value is smaller than 0.05,  Hence we reject H0.
#this implies that, there is association between  brand & marketing flags.

ggplot(data=top10, aes(x=brand, fill=MarketingFlags))+
  geom_bar( width=0.5)+
  scale_fill_manual(values = c("#8b0000","#ce2029"))+
  labs(title="Spread of Brands using Marketing Flags")+
  theme(legend.position = 'right')


#(5)To inspect whether Brands on Sephora release many Limited edition products for sale.
#ANOVA TEST
table(data$brand, data$limited_time_offer)
#H0: Brand releases limited edition products on Sephora.
#H1: Brand releases less limited edition products on Sephora.
res=aov(data$limited_edition ~ data$brand)
summary(res)
#p-value = 0.00255
#p value is smaller than 0.05,  Hence we reject H0.
#This indicates that limited edition products are launches less on Sephora by brands. 
table(top10$brand, top10$limited_edition)
barplot(table(top10$limited_edition,top10$brand),
        col=c("#800000","#b94e48"),
        las=2,
        legend= c("False", "True"),
        args.legend = list(x = "topright",
                           inset = c( -0.10, 0)),
        main="Brands having Limited Edition products")

#(6)
#CORR test
#H0: H0: No of reviews do not affect Rating
#H1: No of reviews affect Rating
cor.test(data$number_of_reviews,data$rating)
#p-value = 0.004836 and corr 0.07274199 
#p value is greater than 0.05 (0.004836 < 0.05), Hence we reject H0.
#Which shows there is a co-relation between Ratings and No of reviews.

corr_plot <- cor(data[,c("rating",'number_of_reviews')])
ggcorrplot(corr_plot,hc.order = TRUE,
           type = "full",
           lab = TRUE,
           lab_size = 3,
           colors = c("#ffffff","#9b111e"),
           method = "square",
           title = 'Correlation Plot rating and reviews')
ggtheme = theme_bw()

#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#BASIC DATA OPERATIONS

#Import Dataset
getwd()
setwd("C:/Users/ADMIN/Downloads")
data<-read.csv("C:/Users/ADMIN/Downloads/SephoraWeb_data.csv")

#library(readr)
install.packages("magrittr")
library("magrittr")
library("RColorBrewer")
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(treemapify)
library(tidyverse)
library(Hmisc)

#Dimension
dim(data)
nrow(data)
#col name
colnames(data)
#about col
str(data)
#data summary
summary(data)

#----------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
#EDA

#Ratings of product
hist(data$rating,
     main="Rating of products",
     ylim=c(0,1500),
     xlab="Rating",
     col="#b22222")

#Price Range of products
ggplot(data, aes(x = price)) +
  geom_density(fill = "#b22222") +  
  labs(title = "Price Range of products found on Sephora")

#Price of product according to its rating
Price1 <- aggregate(price~rating, data, FUN = max)
Price1
ggplot(data, aes(x=rating, y=price)) + geom_point(fill = "#b22222")


#top brand data
brand = as.data.frame(table(data$brand)) %>% arrange(desc(Freq))
sapply(data, n_distinct)
sapply(data, class)
#top 15
focusbrand = head(brand[,1],15)
top15 = data %>% filter(brand %in% (focusbrand))
sapply(top15, function(x) sum(is.na(x)))
#top 20
focusbrand2 = head(brand[,1],20)
top20 = data %>% filter(brand %in% (focusbrand2))
sapply(top20, function(x) sum(is.na(x)))
#top 5
focusbrand3 = head(brand[,1],5)
top10 = data %>% filter(brand %in% (focusbrand3))
sapply(top10, function(x) sum(is.na(x)))

#Organizing category
Fragrance_list <- c('Perfume', 'Cologne', 'Perfume Gift Sets', 'Cologne Gift Sets', 'Fragrance', 'Body Mist & Hair Mist', 'Body Sprays & Deodorant', 'Deodorant & Antiperspirant', 'Deodorant for Men', 'Diffusers', 'Cologne', 'Candles', 'Candles & Home Scents', 'Rollerballs & Travel Size')
top15$category[top15$category %in% Fragrance_list] <- 'Fragrance'

Cleansers_list <- c('Face Wash & Cleansers', 'Face Wash', 'Makeup Removers', 'Toners', 'Face Wipes')
top15$category[top20$category %in% Cleansers_list] <- 'Cleansers'

top15$category[top20$category %in% c('Face Sunscreen', 'Lip Sunscreen','Body Sunscreen')] <- 'Sunscreen'

Masks_list <- c('Face Masks', 'Sheet Masks', 'Facial Peels','Cellulite & Stretch Marks', 'Exfoliators', 'Eye Masks')
top15$category[top20$category %in% Masks_list] <- 'Masks and Exfoliators'

Moi_list <- c('Moisturizers', 'Night Creams', 'Face Oils', 'Mists & Essences', 'Face Serums', 'Eye Creams & Treatments', 'Lotions & Oils','Skincare', 'Lip Balm & Treatment', 'Lip Balms & Treatments', 'Lip Balms & Treatments', 'Moisturizer & Treatments', 'Blemish & Acne Treatments', 'Face Serums', 'Tinted Moisturizer', 'Eye Cream', 'After Sun Care', 'Anti-Aging')
top15$category[top20$category %in% Moi_list] <- 'Moisturizers and Treatments'

Makeup_list <- c('Mascara', 'Lipstick', 'Foundation', '	Eyeliner', 'Highlighter', 'Eye Palettes', 'Eyebrow', 'Makeup', 'Face Primer', 'Setting Spray & Powder', 'Concealer','Bronzer', 'Lip Gloss', 'Blush','Eyeshadow', 'Makeup Palettes', 'Makeup & Travel Cases', 'Lip Stain', 'Makeup Bags & Travel Cases', 'Eye Primer', 'Lip Liner','	Cheek Palettes', 'BB & CC Cream', 'Cheek Palettes', 'Liquid Lipstick', 'Color Correct', 'Contour', 'BB & CC Creams', 'Lip Plumper', 'Eyeliner', 'Self Tanners')
top15$category[top20$category %in% Makeup_list] <- 'Makeup'

Tools_list <- c('Face Brushes', 'Eye Brushes', 'Sponges & Applicators', 'False Eyelashes', 'Brush Sets', 'Brush Cleaners', 'Facial Rollers', 'Eyelash Curlers', 'Facial Cleansing Brushes', 'Blotting Papers', 'Tweezers & Eyebrow Tools','	Lip Brushes', 'Curling Irons', 'Lid Shadow Brush', 'Powder Brush', 'Spa Tools', 'Hair Brushes & Combs', 'Hair Accessories', 'Hair Dryers', 'Hair Straighteners & Flat Irons', 'Lip Brushes', 'Mirrors & Sharpeners', 'Accessories', 'Teeth Whitening')
top15$category[top20$category %in% Tools_list] <- 'Tools and Brushes'

Hair_list <- c('Hair Styling Products', 'Shampoo', 'Conditioner', 'Hair', 'Hair Masks', 'Hair Primers', 'Scalp & Hair Treatments', 'Dry Shampoo', 'Hair Oil', 'Hair Spray', 'Leave-In Conditioner', 'Hair Products', 'Color Care','	Shampoo & Conditioner', 'Shampoo & Conditioner')
top15$category[top20$category %in% Hair_list] <- 'HairCare'

Bath_list <- c('Body Lotions & Body Oils', 'Body Wash & Shower Gel', 'Bath & Shower', 'Hand Cream & Foot Cream', 'Bath Soaks & Bubble Bath', 'Body Products', 'For Body', 'Bath & Body', 'Body Moisturizers', 'Scrub & Exfoliants', 'Decollete & Neck Creams')
top15$category[top20$category %in% Bath_list] <- 'Bath&Body'

other_list <- c('Mini Size', 'Beauty Supplements', 'no category', 'Shaving', 'Nail', 'Aftershave', 'Value & Gift Sets', 'Face Sets', 'Eye Sets', 'Lip Sets', 'For Face', 'Skincare Sets', 'Hair Removal')
top15$category[top20$category %in% other_list] <- 'Others'

#Top 10 category 
categorys <-as.data.frame(table(top15$category)) %>%
  rename(category = Var1) %>%
  arrange(desc(Freq))
categorys[1:20,]

#Spread of marketing flages
plotdata <- data %>%
  count(MarketingFlags_content)
ggplot(plotdata, 
       aes(fill = MarketingFlags_content, 
           area = n)) +
  geom_treemap() + 
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Type of Marketing Flag used")

#Expensive Brands on Sephora
ggplot(data = top15, mapping = aes(x=brand, y=price)) +
  geom_boxplot() + 
  # scale_y_log10() +
  labs(title="Prices for each brand") + 
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1))

#Rating of brand
ggplot(data = top20, mapping = aes(x=brand, y=love)) +
  geom_boxplot() + 
  # scale_y_log10() +
  labs(title="Ratings of Brand") + 
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1))

#Brand's that having rating more than 4
top20 %>%
  group_by(brand) %>%
  summarise(n = n(),ave_rating = mean(rating, na.rm=TRUE)) %>%
  ggplot(aes(x = reorder(brand, n), y= n, fill=ave_rating>4)) + 
  geom_col() +
  scale_fill_manual(values = c("#480607","#b94e48"))+
  labs(title="Quantity for brand's product",
       x="brand", y="count") + 
  coord_flip() +
  theme_minimal()

#Pricing of these categories
c1 <- filter(top20, category %in% c("Makeup","Fragrance", "Moisturizers and Treatments"))
ggplot(c1, aes(x=price, color=category, fill=category)) +
  geom_density(alpha=1/2) +
  facet_wrap(~category) +
  labs(title="Price distribution for different categories",
       x="Price") +
  theme_minimal()



