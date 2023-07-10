# weights ---------------------------------------------------------

make_weight_table <- function(input_df, input_pop){
  refugee_pop <- input_pop
  
  weight_table <- input_df |> 
    group_by(strata) |> 
    summarise(
      sample_strata=  n()
    ) |> 
    mutate(
      sample_global= sum(sample_strata)) |> 
    left_join(refugee_pop) |> 
    rename(pop_strata= "total_hhs") |> 
    mutate(pop_global=sum(pop_strata) ,
           weights = (pop_strata/pop_global)/(sample_strata/sample_global)
    )
}
 