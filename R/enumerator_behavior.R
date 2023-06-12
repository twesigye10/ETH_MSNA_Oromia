# for each survey, it finds the closest matching survey with the minimum number of different columns

calculateDifferences <- function(data, input_df_survey){
    # 1) convert all columns to character
    data <- mutate_all(data, as.character)
    # 2) remove columns that are naturally different in each survey
    cols <- data.frame(column=colnames(data)) %>% 
        left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
        filter(!(type %in% c("_uuid", "enumerator_id")) &
                   !str_starts(column, "_other$") &
                   !str_detect(column, "_specify$"))
    data <- data[, all_of(cols$column)]
    # 3) convert NA to "NA" and all columns to factor
    data[is.na(data)] <- "NA"
    data <- data %>% mutate_if(is.character, factor)
    # 4) calculate gower distance
    gower_dist <- daisy(data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
    gower_mat <- as.matrix(gower_dist)
    # 5) convert distance to number of differences and determine closest matching survey
    r <- c()
    for (i in 1:nrow(data)) r <- c(r, sort(gower_mat[i,]*ncol(data))[2])
    # 6) store results
    data[["n.col.not.NA"]] <- rowSums(data!="NA")
    data[["survey.id"]] <- 1:dim(data)[1]
    data[["most.similar.survey"]] <- names(r)
    data[["number.different.columns"]] <- as.numeric(r)
    data <- data %>% arrange(number.different.columns, survey.id)
    return(data)
}


# silhouette analysis based on gower distance between surveys -------------

# silhouette analysis based on gower distance between surveys
# METHOD: check for anomalies using the silhouette function. We assume the dataset is clustered using the 
# enumerator IDs as the cluster IDs and we calculate the silhouette for this clustering scenario. A 
# silhouette value close to 1 indicates that the entries of the cluster are very similar to each other and 
# very dissimilar from entries of other clusters. Thus, we need to raise a flag if the silhouette value gets 
# close to 1 for any of the clusters/enumerators.
# https://en.wikipedia.org/wiki/Silhouette_(clustering)
# https://dpmartin42.github.io/posts/r/cluster-mixed-types
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9

calculateEnumeratorSimilarity <- function(data, input_df_survey, col_enum, col_admin){
    
    # helper function
    convertColTypes <- function(data, input_df_survey){
        # select_multiple: numeric or factor?
        col.types <- data.frame(column=colnames(data)) %>% 
            left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
            mutate(type.edited = case_when(type %in% c("integer", "decimal", "calculate") ~ "numeric",
                                           str_starts(type, "select_") ~ "factor",
                                           str_detect(column, "/") ~ "factor",
                                           TRUE ~ "text"))
        
        cols <- col.types[col.types$type.edited=="numeric", "column"]
        data[,cols] <- lapply(data[,cols], as.numeric)
        cols <- col.types[col.types$type.edited=="text", "column"]
        data[,cols] <- lapply(data[,cols], as.character)
        cols <- col.types[col.types$type.edited=="factor", "column"]
        data[,cols] <- lapply(data[,cols], as.factor)
        
        return(data)
    }
    
    # convert columns using the tool
    data <- convertColTypes(data, input_df_survey)
    # keep only relevant columns
    cols <- data.frame(column=colnames(data)) %>% 
        left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
        filter(!(type %in% c("_uuid", "enumerator_id")) &
                   !str_starts(column, "_other$") &
                   !str_detect(column, "_specify$"))
    # convert character columns to factor and add enum.id
    data <- data[, all_of(cols$column)] %>% 
        mutate_if(is.character, factor) %>% 
        arrange(!!sym(col_enum)) %>%
        mutate(enum.id=as.numeric(!!sym(col_enum)), .after=!!sym(col_enum))
    # add "SY" column in case col_admin is not specified
    #if (col_admin=="adm") data <- mutate(data, adm="DRC", .before=cols$column[1])
    # calculate similarity (for enumerators who completed at least 5 surveys)
    res <- data %>% split(data[[col_admin]]) %>% 
        lapply(function(gov){
            df <- gov %>% 
                group_by(enum.id) %>% 
                mutate(n=n()) %>% 
                filter(n>=5) %>% 
                ungroup() %>% 
                select_if(function(x) any(!is.na(x)))
            
            if (length(unique(df$enum.id)) > 1){
                # calculate gower distance
                gower_dist <- daisy(select(df, -c(!!sym(col_enum), enum.id)), 
                                    metric = "gower", warnBin = F, warnAsym = F, warnConst = F)
                # gower_mat <- as.matrix(gower_dist)
                # calculate silhouette
                si <- silhouette(df$enum.id, gower_dist)
                res.si <- summary(si)
                # create output
                r <- data.frame(enum.id=as.numeric(names(res.si$clus.avg.widths)), si=res.si$clus.avg.widths) %>% 
                    left_join(distinct(select(df, !!sym(col_admin), !!sym(col_enum), enum.id)), by="enum.id") %>% 
                    left_join(group_by(df, enum.id) %>% summarise(num.surveys=n(), .groups="drop_last"), by="enum.id") %>% 
                    select(!!sym(col_admin), !!sym(col_enum), num.surveys, si) %>% 
                    arrange(-si)
                return(r)
            }
        })
    do.call(rbind, res)
}