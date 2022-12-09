library(tidyverse)
library(data.table)
library(ggmap)
tidymodels_prefer()

# read in ames training
ames_train = fread(here::here("data", "ames_train.csv"))

# examine sales price
hist_sales = ames_train %>% 
        ggplot(aes(x=Sale_Price))+
        geom_histogram(bins = 70, color = 'white')+
        theme_minimal()+
        scale_x_continuous(labels = label_number())

hist_sales

# examine sales price, logged
hist_sales_logged = ames_train %>% 
        ggplot(aes(x=log10(Sale_Price)))+
        geom_histogram(bins = 70, color = 'white')+
        theme_minimal()+
        scale_x_log10()

hist_sales_logged

# what is the median?
hist_sales_logged = ames_train %>% 
        ggplot(aes(x=log10(Sale_Price)))+
        geom_histogram(bins = 70, color = 'white')+
        theme_bw()+
        scale_x_log10()

# if we were to predict the untransformed data, what is the average?
ames_train %>%
        ggplot(aes(x=Sale_Price))+
        geom_histogram(bins = 70, color = 'white')+
        theme_bw()+
        scale_x_continuous(labels = label_number())+
        geom_vline(aes(xintercept = mean(Sale_Price)),
                   color = 'red',
                   linetype = 'dashed')

# plot selected numeric features
ames_train %>%
        select(Total_Bsmt_SF,
               First_Flr_SF,
               Second_Flr_SF,
               TotRms_AbvGrd,
               Kitchen_AbvGr,
               Bedroom_AbvGr,
               Full_Bath,
               Half_Bath,
               Fireplaces,
               Lot_Area,
               Gr_Liv_Area,
               Garage_Area,
               Garage_Cars,
               Year_Built) %>%
        gather() %>%
        ggplot(aes(x=value))+
        geom_histogram(bins = 80)+
        facet_wrap(key ~.,
                   scales = "free")+
        theme_bw()+
        scale_x_continuous(breaks = pretty_breaks())

# make scatter plot with outcome
ames_train %>%
        select(Sale_Price,
               Total_Bsmt_SF,
               First_Flr_SF,
               Second_Flr_SF,
               TotRms_AbvGrd,
               Kitchen_AbvGr,
               Bedroom_AbvGr,
               Full_Bath,
               Half_Bath,
               Fireplaces,
               Lot_Area,
               Gr_Liv_Area,
               Garage_Area,
               Garage_Cars,
               Year_Built) %>%
        gather("key",
               "value",
               -Sale_Price) %>%
        ggplot(aes(x=value,
                   y=Sale_Price))+
        geom_point(alpha = 0.5,
                   position = ggforce::position_jitternormal(sd_y=0,
                                                             sd_x = 0.05))+
        facet_wrap(key ~.,
                   scales = "free_x")+
        theme_bw()+
        scale_x_continuous(breaks = pretty_breaks())+
        geom_smooth(method = 'lm',
                    color = 'blue')+
        scale_y_continuous(labels = label_number())+
        ggpubr::stat_cor(p.accuracy = 0.01)


# correlation matrix of numeric features
ames_train %>%
        select_if(is.numeric) %>%
        cor() %>%
        ggcorrplot::ggcorrplot(outline.color = 'white')

# categorical features
# neighborhood mean
ames_train %>%
        group_by(Neighborhood) %>%
        summarize(mean = mean(Sale_Price),
                  std_err = sd(Sale_Price) / sqrt(length(Sale_Price))) %>% 
        ggplot(aes(y = reorder(Neighborhood, mean), x = mean)) + 
        geom_point() +
        geom_errorbar(aes(xmin = mean - 1.64 * std_err, xmax = mean + 1.64 * std_err)) +
        labs(y = NULL, x = "Sale Price")+
        theme_bw()+
        scale_x_continuous(labels = label_number())

# box plot with points
ames_train %>%
        mutate(id = row_number()) %>%
        group_by(Neighborhood) %>%
        mutate(median_sale = median(Sale_Price)) %>%
        ungroup() %>%
        ggplot(aes(y = reorder(Neighborhood, median_sale),
                   x = Sale_Price))+
        geom_point(alpha = 0.25,
                   position = ggforce::position_jitternormal(sd_x=0,
                                                             sd_y = 0.05))+
        theme_bw()+
        geom_boxplot(alpha = 0.5)+
        ylab("Neighborhood")+
        scale_x_continuous(labels = label_number())

# bldg_type
ames_train %>%
        group_by(Bldg_Type) %>%
        mutate(median_sale = median(Sale_Price)) %>%
        ungroup() %>%
        ggplot(aes(y = reorder(Bldg_Type, median_sale),
                   x = Sale_Price))+
        geom_point(alpha = 0.25,
                   position = ggforce::position_jitternormal(sd_x=0,
                                                             sd_y = 0.05))+
        theme_bw()+
        geom_boxplot(alpha = 0.5)+
        ylab("Building Type")+
        scale_x_continuous(labels = label_number())

# intersections of predictors: building type and living area
ames_train %>%
        ggplot(aes(x=log(Gr_Liv_Area),
                   y=log(Sale_Price)))+
        geom_point(alpha = 0.55)+
        geom_smooth(method = 'lm')+
        facet_wrap(Bldg_Type ~.,
                   ncol = 3)+
        theme_bw()

# plot neighborhoods, add color
ames_train %>%
        ggplot(aes(x=Longitude,
                   color = Neighborhood,
                   y=Latitude))+
        geom_point(alpha = 0.8,
                   shape = 15)+
        theme_bw()+
        theme(legend.position = 'top',
              legend.title = element_blank())


# latitutde longitude and Sale Price
ames_train %>%
        ggplot(aes(x=Longitude,
                   color = Sale_Price,
                   y=Latitude))+
        geom_point(alpha = 0.8,
                   shape = 15)+
        scale_color_viridis_c(option = 'B',
                             labels = label_number())+
        theme_bw()+
        theme(legend.position = 'top')+
        guides(color = guide_colorbar(barwidth = 20,
                                    barheight = 0.5,
                                    title.position = 'top',
                                    title.hjust = 0.5))




