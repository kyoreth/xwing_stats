library(keras)
library(tfdatasets)
library(tidyverse)
library(jsonlite)
library(purrr)
library(janitor)
library(plotly)

pilots <- list.files(path = "xwing-data2/data/pilots/", full.names = T, recursive = T)

data <- lapply(pilots, read_json)

data.df <- do.call(rbind, data)
data.df <- do.call(rbind, data[data.df$dialCodes=="Huge",])


data_names <- lapply(data, names)
unique_names_lists <- unique(data_names)
names <- unique(unlist(unique_names_lists))
data <- do.call(rbind, lapply(data, function(x) x[names]))
data.df <- as.data.frame(data)

data.df <- filter(data.df[,c(1,3,4,6:10)], size != "Huge")
all.mans <- unique(unlist(data.df$dial))

dial <- do.call(rbind, lapply(1:nrow(data.df), function(x) as.numeric(all.mans %in% unlist(data.df[x,3]))))
dial.df <- as.data.frame(dial)
colnames(dial.df) <- all.mans

data.df <- cbind(data.df[,-5], dial.df)

stats <- lapply(1:nrow(data.df), function(x) lapply(data.df[x,]$stats[[1]], function(y) paste(unlist(y), collapse =  " ")))
unique_stats <- unique(unlist(stats))

stats.matrix <- do.call(rbind, lapply(stats, function(x) as.numeric(unique_stats %in% unlist(x))))
stats.df <- as.data.frame(stats.matrix)
colnames(stats.df) <- unique_stats
data.df_stats <- cbind(data.df[,-5], stats.df)


actions <- lapply(1:nrow(data.df), function(x) lapply(data.df[x,]$actions[[1]], function(y) paste(unlist(y), collapse =  " ")))
unique_actions <- unique(unlist(actions))

actions.matrix <- do.call(rbind, lapply(actions, function(x) as.numeric(unique_actions %in% unlist(x))))
actions.df <- as.data.frame(actions.matrix)
colnames(actions.df) <- unique_actions
data.df_actions <- cbind(data.df_stats[,-5], actions.df)


pilot_cols <- lapply(1:nrow(data.df_actions), function(x) colnames(do.call(rbind, data.df_actions[x,]$pilots[[1]])))
pilot_cols <- unique(unlist(pilot_cols))
pilots <- lapply(1:nrow(data.df_actions), function(i) do.call(rbind, lapply(data.df_actions[i,]$pilots[[1]], function(x) x[pilot_cols])))
for (i in 1:length(pilots)){
  colnames(pilots[[i]]) <- pilot_cols
}


final_frame.df <- do.call(rbind, lapply(1:length(pilots), function(x) cbind(pilots[[x]],data.df_actions[x,])))
final_frame.df <- final_frame.df[,c(-10:-20,-25,-6)]

# final_frame.df <- left_join(final_frame.df, as.data.frame(data)[,c("name","faction")], by=c("name.1"="name"))
 final_frame.df <- final_frame.df %>% rename(ship = name.1)
# final_frame.df <- cbind(final_frame.df[c(-"ship")]+final_frame.df[c("ship")])

ff_slots <- lapply(final_frame.df$slots,unlist)
u_slots <- unique(unlist(ff_slots))

slots <- do.call(rbind, lapply(ff_slots, function(x) as.numeric(u_slots %in% x)))
colnames(slots) <- u_slots
final_frame.df <- cbind(final_frame.df[,-8], slots)
final_frame.df[,3:5] <- unlist(final_frame.df[,3:5])
final_frame.df$name <- do.call(c, final_frame.df$name)
final_frame.df <- final_frame.df[,-2]
final_frame.df$size <- unlist(final_frame.df$size)
final_frame.df$faction <- unlist(final_frame.df$faction)
final_frame.df$ship <- unlist(final_frame.df$ship)
#final_frame.df$size <- as.factor(final_frame.df$size)
#final_frame.df$initiative <- as.factor(final_frame.df$initiative)

ship_ability_names <- do.call(c, lapply(final_frame.df$shipAbility, function(x) if (is.null(x[[1]])){return(NA)}else {x[[1]]}))

final_frame.df$shipAbility <- ship_ability_names
#final_frame.df$shipAbility <- as.factor(final_frame.df$shipAbility)

test.df <- final_frame.df[,1:7]
hull_values <- final_frame.df %>% select(starts_with("hull")) %>% colnames() %>% str_extract(pattern = "\\d+") %>% as.numeric()
test.df$hull <- do.call("rbind", lapply(1:nrow(test.df), \(x) max((final_frame.df[x,] %>% select(starts_with("hull")))*hull_values))) %>%
  as.character() %>% as.numeric()

shield_values <- final_frame.df %>% select(starts_with("shield")) %>% colnames() %>% str_extract(pattern = "\\d+") %>% as.numeric()
test.df$shield <- do.call("rbind", lapply(1:nrow(test.df), \(x) max((final_frame.df[x,] %>% select(starts_with("shield")))*shield_values)))%>%
  as.character() %>% as.numeric()

attack_values <- final_frame.df %>% select(contains("attack")) %>% colnames() %>% str_extract(pattern = "\\d+") %>% as.numeric()
test.df$attack <- do.call("rbind", lapply(1:nrow(test.df), \(x) max((final_frame.df[x,] %>% select(contains("attack"))*attack_values))))%>%
  as.character() %>% as.numeric()

agility_values <- final_frame.df %>% select(contains("agility")) %>% colnames() %>% str_extract(pattern = "\\d+") %>% as.numeric()
test.df$agility <- do.call("rbind", lapply(1:nrow(test.df), \(x) max((final_frame.df[x,] %>% select(contains("agility"))*agility_values))))%>%
  as.character() %>% as.numeric()


# id_training <- sample.int(nrow(test.df), size = 0.8*nrow(test.df))
# training <- test.df[id_training,]
# testing <- test.df[-id_training,]

final_frame.df <- final_frame.df %>% select(-ffg)

ffnames <- final_frame.df %>% clean_names() %>% names()
ffnames[c(5:9)] <- ffnames[c(5:9)] %>% paste("_cat",sep = "")
ffnames[c(10:185)] <- ffnames[c(10:185)] %>% paste("_bin",sep = "")
names(final_frame.df) <- ffnames

final_frame.df$ability_cat <- final_frame.df$ability_cat %>% as.character()
final_frame.df$ship_ability_cat <- final_frame.df$ship_ability_cat %>% as.character()

final_frame.df$ability_cat <- final_frame.df$ability_cat %>% replace_na("NULL")
final_frame.df$ship_ability_cat <- final_frame.df$ship_ability_cat %>% replace_na("NULL")


model_frame.df <- final_frame.df %>% select(-name)

id_training <- sample.int(nrow(model_frame.df), size = 0.8*nrow(model_frame.df))
training <- model_frame.df[id_training,]
testing <- model_frame.df[-id_training,]


ft_spec <- training %>%
  feature_spec(cost ~ .) %>%
  step_numeric_column(ends_with("bin")) %>%
  step_numeric_column(-ends_with("bin"),
                      -ends_with("cat"),
                      normalizer_fn = scaler_standard()
  ) %>%
  step_categorical_column_with_vocabulary_list(ends_with("cat")) %>%
  step_embedding_column(ends_with("cat"),
                        dimension = function(vocab_size) as.integer(sqrt(vocab_size) + 1)
  ) %>%
  fit()


inputs <- layer_input_from_dataset(model_frame.df %>% select(-cost))

l <- 0.1

output <- inputs %>%
  layer_dense_features(ft_spec$dense_features()) %>%
  layer_dense(units = 724, activation = "relu", kernel_regularizer = regularizer_l2(l)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 181, activation = "relu", kernel_regularizer = regularizer_l2(l)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 15, activation = "relu", kernel_regularizer = regularizer_l2(l)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = NULL, kernel_regularizer = regularizer_l2(l))

model <- keras_model(inputs, output)

auc <- tf$keras$metrics$AUC()

gini <- custom_metric(name = "gini", function(y_true, y_pred) {
  2*auc(y_true, y_pred) - 1
})

# Yan, L., Dodier, R., Mozer, M. C., & Wolniewicz, R. (2003). 
# Optimizing Classifier Performance via an Approximation to the Wilcoxon-Mann-Whitney Statistic.
roc_auc_score <- function(y_true, y_pred) {
  
  pos = tf$boolean_mask(y_pred, tf$cast(y_true, tf$bool))
  neg = tf$boolean_mask(y_pred, !tf$cast(y_true, tf$bool))
  
  pos = tf$expand_dims(pos, 0L)
  neg = tf$expand_dims(neg, 1L)
  
  # original paper suggests performance is robust to exact parameter choice
  gamma = 0.2
  p     = 3
  
  difference = tf$zeros_like(pos * neg) + pos - neg - gamma
  
  masked = tf$boolean_mask(difference, difference < 0.0)
  
  tf$reduce_sum(tf$pow(-masked, p))
}

model %>%
  compile(
    loss = loss_mean_squared_error(),
    optimizer = optimizer_adam()#,
    #metrics = list('mse')
  )

model %>%
  fit(
    x = training,
    y = training$cost,
    epochs = 500,
    validation_data = list(testing, testing$cost),
    batch_size = 384
  )

predictions <- predict(model, testing)
Metrics::auc(testing$cost, predictions)






generics.df <- final_frame.df %>% 
  filter(ability_cat == "NULL", limited == 0)


model_frame.df <- generics.df %>% select(-name,-ability_cat,-limited,-ship_cat)

id_training <- sample.int(nrow(model_frame.df), size = 0.8*nrow(model_frame.df))
training <- model_frame.df[id_training,]
testing <- model_frame.df[-id_training,]


ft_spec <- training %>%
  feature_spec(cost ~ .) %>%
  step_numeric_column(ends_with("bin")) %>%
  step_numeric_column(-ends_with("bin"),
                      -ends_with("cat"),
                      normalizer_fn = scaler_standard()
  ) %>%
  step_categorical_column_with_vocabulary_list(ends_with("cat")) %>%
  step_embedding_column(ends_with("cat"),
                        dimension = function(vocab_size) as.integer(sqrt(vocab_size) + 1)
  ) %>%
  fit()


inputs <- layer_input_from_dataset(model_frame.df %>% select(-cost))

l <- 0.1

output <- inputs %>%
  layer_dense_features(ft_spec$dense_features()) %>%
  layer_dense(units = 724, activation = "relu", kernel_regularizer = regularizer_l2(l)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 181, activation = "relu", kernel_regularizer = regularizer_l2(l)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 15, activation = "relu", kernel_regularizer = regularizer_l2(l)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = NULL, kernel_regularizer = regularizer_l2(l))

model <- keras_model(inputs, output)

model %>%
  compile(
    loss = loss_mean_squared_error(),
    optimizer = optimizer_adam()#,
    #metrics = list('mse')
  )

model %>%
  fit(
    x = training,
    y = training$cost,
    epochs = 500,
    validation_data = list(testing, testing$cost),
    batch_size = 100
  )

predictions <- predict(model, testing)

pred_frame.df <- final_frame.df %>% 
  mutate(pred = predict(model, final_frame.df), diff = cost - pred, diff_p = diff/cost, name = final_frame.df$name) %>% 
  select(name,faction_cat, cost, pred, diff, diff_p)

ggplotly(ggplot(pred_frame.df, aes(x=cost, y=pred, colour=faction_cat, name=name))+geom_point()+geom_abline())

ggplot(pred_frame.df, aes(y=diff))+geom_boxplot()+facet_wrap(faction_cat~.)





         