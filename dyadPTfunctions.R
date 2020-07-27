# Functions for multiple-perspective model (Heller et al., 2016) applied to Ryskin et al. (2015) dataset

object_probs = function(prob_hidden = 0.05, num_objects=8 ){
  prob_visible = (1-prob_hidden)/(num_objects-1)
  
  objects_self_cond_2shared = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,1,1,0,0,0,0),
    p_object = c(rep(prob_visible,7), prob_hidden)
  )
  
  objects_self_cond_2priv = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,1,1,0,0,0,0),
    p_object =c(rep(prob_visible,7), prob_hidden)
  )
  
  objects_self_cond_1shared = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"), #compet_pair here is actually a distractor
    pair_member = c(1,1,0,0,0,0,0,0),
    p_object = c(rep(prob_visible,7), prob_hidden)
  )
  
  objects_other_cond_2shared = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,1,1,0,0,0,0),
    p_object = c(rep(prob_visible,4), prob_hidden, rep(prob_visible,3))
  )
  
  objects_other_cond_2priv = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"), #compet_pair here is actually the occluded thing
    pair_member = c(1,1,1,1,0,0,0,0),
    p_object = c(rep(prob_visible,3), prob_hidden, rep(prob_visible,4))
  )
  
  objects_other_cond_1shared = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,0,0,0,0,0,0),
    p_object = c(rep(prob_visible,4), prob_hidden, rep(prob_visible,3))
  )
  
  return(list("objects_self_cond_2shared"=objects_self_cond_2shared,
              "objects_self_cond_2priv"=objects_self_cond_2priv,
              "objects_self_cond_1shared"=objects_self_cond_1shared,
              "objects_other_cond_2shared"=objects_other_cond_2shared,
              "objects_other_cond_2priv"=objects_other_cond_2priv,
              "objects_other_cond_1shared"=objects_other_cond_1shared))
}



RE_probs = function(targ_prob=0.98, targ_pair_prob=0, single_prob = 0.08, occ_prob=0){
  
  unknown_prob = 0.25*targ_prob + 0.25*targ_pair_prob + 0.5*single_prob 
  
  RE_self_cond_2shared = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,1,1,0,0,0,0),
    p_size_adj = c(targ_prob, targ_pair_prob, targ_prob, targ_pair_prob, single_prob , single_prob , single_prob , occ_prob)
  )
  
  RE_self_cond_2priv = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,1,1,0,0,0,0),
    p_size_adj = c(targ_prob, targ_pair_prob, targ_prob, targ_pair_prob, single_prob , single_prob , single_prob , occ_prob)
  )
  
  RE_self_cond_1shared = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,0,0,0,0,0,0),
    p_size_adj = c(targ_prob, targ_pair_prob, single_prob, single_prob, single_prob , single_prob , single_prob , occ_prob)
  )
  
  RE_other_cond_2shared = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,1,1,0,0,0,0),
    p_size_adj = c(targ_prob, targ_pair_prob, targ_prob, targ_pair_prob, occ_prob, single_prob , single_prob, unknown_prob)
  )
  
  RE_other_cond_2priv = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,1,1,0,0,0,0),
    p_size_adj = c(targ_prob, targ_pair_prob, single_prob, occ_prob, single_prob , single_prob, single_prob, unknown_prob)
  )
  
  RE_other_cond_1shared = tibble(
    label = c("targ","targ_pair","compet","compet_pair","dist1","dist2","dist3","occ"),
    pair_member = c(1,1,0,0,0,0,0,0),
    p_size_adj = c(targ_prob, targ_pair_prob, single_prob , single_prob , occ_prob, single_prob , single_prob , unknown_prob)
  )
  
  return(list("RE_self_cond_2shared"=RE_self_cond_2shared,
              "RE_self_cond_2priv"=RE_self_cond_2priv,
              "RE_self_cond_1shared"=RE_self_cond_1shared,
              "RE_other_cond_2shared"=RE_other_cond_2shared,
              "RE_other_cond_2priv"=RE_other_cond_2priv,
              "RE_other_cond_1shared"=RE_other_cond_1shared))
}

p_obj_given_domain = function(object ,domain_type = "self",condition, all_obj_props ){
  stim_name = str_c("objects_", domain_type, "_cond_", condition)
  stim = all_obj_props[[stim_name]]
  object_prop = stim %>% filter(label == object)
  p = object_prop$p_object
  return(p)
}

p_RE_given_joint_obj_and_domain = function(object, domain_type = "self",condition, all_RE_props ){
  RE_name = str_c("RE_", domain_type, "_cond_", condition) 
  REs = all_RE_props[[RE_name]] 
  RE_prop = REs%>% filter(label == object)
  p = RE_prop$p_size_adj
  return(p)
}

p_obj_given_RE = function(alpha=1, object, condition, all_obj_props, all_RE_props){
  
  # this part just gets the names of all objects in that condition (identical for self and other)
  stim_name = str_c("objects_self_cond_", condition) 
  stim = all_obj_props[[stim_name]]
  objects = stim$label
  
  all_p_REs_self = c()
  all_p_obj_self = c()
  targ_p_RE_self = 0
  targ_p_obj_self = 0
  
  all_p_REs_other = c()
  all_p_obj_other = c()
  targ_p_RE_other = 0
  targ_p_obj_other = 0
  
  for (i in 1:8){
    p_RE_given_joint_obj_and_self_domain = p_RE_given_joint_obj_and_domain(objects[i], "self", condition, all_RE_props )
    p_obj_given_self_domain = p_obj_given_domain(objects[i], "self", condition, all_obj_props)
    all_p_REs_self = c(all_p_REs_self, p_RE_given_joint_obj_and_self_domain)
    all_p_obj_self = c(all_p_obj_self, p_obj_given_self_domain)
    
    
    p_RE_given_joint_obj_and_other_domain = p_RE_given_joint_obj_and_domain(objects[i], "other", condition, all_RE_props )
    p_obj_given_other_domain = p_obj_given_domain(objects[i], "other", condition, all_obj_props)
    all_p_REs_other = c(all_p_REs_other, p_RE_given_joint_obj_and_other_domain)
    all_p_obj_other = c(all_p_obj_other, p_obj_given_other_domain)
    
    if (objects[i] == object){
      targ_p_RE_self = p_RE_given_joint_obj_and_self_domain
      targ_p_obj_self = p_obj_given_self_domain
      targ_p_RE_other = p_RE_given_joint_obj_and_other_domain
      targ_p_obj_other = p_obj_given_other_domain
    }
  }
  
  normalizing_constant_self = sum(all_p_REs_self*all_p_obj_self) 
  normalizing_constant_other = sum(all_p_REs_other*all_p_obj_other)
  
  p_self = (targ_p_RE_self*targ_p_obj_self)/normalizing_constant_self
  p_other = (targ_p_RE_other*targ_p_obj_other)/normalizing_constant_other
  
  p = alpha*p_self + (1-alpha)*p_other
  
  return(p)
  
}


subj_estimates_given_alpha = function(listener,  
                                      alpha_source=c("own_prod","inverse_own_prod",
                                                     "partner_prod","inverse_partner_prod"), 
                                      just_alpha_source=FALSE, 
                                      partners, avg_by_subj_prod,
                                      prod_indiv_alpha,
                                      count_priv=FALSE,
                                      dyad_props=NULL,
                                      targ_only=FALSE) {
  # for each listener, we get the production proportions from the partner (except if you provide a numeric alpha value)
  # could simplify by computing the alpha internally too...
  subj = partners %>% filter(subj==listener) %>% pull(partner)
  subj_grid=tibble(conditions = c("2shared","2priv","1shared"))
  
  if(just_alpha_source==FALSE){
    
    if(alpha_source=="own_prod"){
      subj_probs = avg_by_subj_prod %>% filter(speaker==subj) 
      subj_targ_prob = filter(subj_probs,condition == "Two Contrasts Shared") %>% pull(prop_size_adj_use)
      no_contrast_single_prob = filter(subj_probs,condition == "No Contrast") %>% pull(prop_size_adj_use)
      priv_single_prob = filter(subj_probs,condition == "One Contrast Privileged") %>%
        pull(prop_size_adj_use)
      subj_alpha = prod_indiv_alpha %>% filter(subject==listener) %>% pull(alpha)
    } else if(alpha_source=="inverse_own_prod"){
      subj_probs = avg_by_subj_prod %>% filter(speaker==subj) 
      subj_targ_prob = filter(subj_probs,condition == "Two Contrasts Shared") %>% pull(prop_size_adj_use)
      no_contrast_single_prob = filter(subj_probs,condition == "No Contrast") %>% pull(prop_size_adj_use)
      priv_single_prob = filter(subj_probs,condition == "One Contrast Privileged") %>%
        pull(prop_size_adj_use)
      subj_alpha = 1-(prod_indiv_alpha %>% filter(subject==listener) %>% pull(alpha))
    }else if(alpha_source=="partner_prod"){
      subj_probs = avg_by_subj_prod %>% filter(speaker==subj) 
      subj_targ_prob = filter(subj_probs,condition == "Two Contrasts Shared") %>% pull(prop_size_adj_use)
      no_contrast_single_prob = filter(subj_probs,condition == "No Contrast") %>% pull(prop_size_adj_use)
      priv_single_prob = filter(subj_probs,condition == "One Contrast Privileged") %>%
        pull(prop_size_adj_use)
      subj_alpha = prod_indiv_alpha %>% filter(subject==subj) %>% pull(alpha)
    }else if(alpha_source=="inverse_partner_prod"){
      subj_probs = avg_by_subj_prod %>% filter(speaker==subj) 
      subj_targ_prob = filter(subj_probs,condition == "Two Contrasts Shared") %>% pull(prop_size_adj_use)
      no_contrast_single_prob = filter(subj_probs,condition == "No Contrast") %>% pull(prop_size_adj_use)
      priv_single_prob = filter(subj_probs,condition == "One Contrast Privileged") %>%
        pull(prop_size_adj_use)
      subj_alpha = 1 - (prod_indiv_alpha %>% filter(subject==subj) %>% pull(alpha)) 
    }else if(is.numeric(alpha_source)){
      subj_alpha = alpha_source
      
      if(is.null(dyad_props)){
        subj_targ_prob=0.98
        no_contrast_single_prob = 0.08
        priv_single_prob = 0.66
      }else if(!is.null(dyad_props)){
        subj_probs = avg_by_subj_prod %>% filter(speaker==subj) 
        subj_targ_prob = filter(subj_probs,condition == "Two Contrasts Shared") %>% pull(prop_size_adj_use)
        no_contrast_single_prob = filter(subj_probs,condition == "No Contrast") %>% pull(prop_size_adj_use)
        priv_single_prob = filter(subj_probs,condition == "One Contrast Privileged") %>%
          pull(prop_size_adj_use)
      }
    }
  }else if(just_alpha_source==TRUE){
    subj_targ_prob=0.98
    no_contrast_single_prob = 0.08
    priv_single_prob = 0.66
    
    if(alpha_source=="own_prod"){
      subj_alpha = prod_indiv_alpha %>% filter(subject==listener) %>% pull(alpha)
    }else if(alpha_source=="inverse_own_prod"){
      subj_alpha = 1-(prod_indiv_alpha %>% filter(subject==listener) %>% pull(alpha))
    }else if(alpha_source=="partner_prod"){
      subj_alpha = prod_indiv_alpha %>% filter(subject==subj) %>% pull(alpha)
    }else if(alpha_source=="inverse_partner_prod"){
      subj_alpha = 1 - (prod_indiv_alpha %>% filter(subject==subj) %>% pull(alpha))
    }
  }
  
  if(count_priv==TRUE){
    subj_single_prob = 0.8*no_contrast_single_prob + 0.2*priv_single_prob
  }else{
    subj_single_prob = no_contrast_single_prob
  }
  
  subj_all_obj_props= object_probs() 
  subj_all_RE_props = RE_probs(targ_prob = subj_targ_prob, single_prob = subj_single_prob) 
  
  subj_grid$alpha=subj_alpha
  subj_grid$target_props = map_dbl(subj_grid$conditions, p_obj_given_RE, alpha=subj_alpha, object="targ", subj_all_obj_props, subj_all_RE_props)
  
  if(targ_only==FALSE){
    subj_grid$compet_props = map_dbl(subj_grid$conditions, p_obj_given_RE, alpha=subj_alpha, object="compet", subj_all_obj_props, subj_all_RE_props)
    subj_grid_long = pivot_longer(subj_grid, cols = c(target_props, compet_props), names_to = "locs",values_to = "predicted_props") %>% 
      mutate(listener = listener,
             partner = subj,
             alpha_source=alpha_source)
  }else{
    subj_grid_long = subj_grid %>% 
      mutate(locs = "target_props",
             predicted_props = target_props,
             listener = listener,
             partner = subj,
             alpha_source=alpha_source)
  }
  
  return(subj_grid_long)
}


match_pred_to_emp=function(predictions, data){
  
  data_long = data %>% 
    pivot_longer(cols=c(prop_target_fix,prop_compet_fix), 
                 names_to = "locs", values_to = "empirical_props") %>% 
    mutate(locs = recode(locs, prop_target_fix='target_props',prop_compet_fix='compet_props'))
  
  pred_long = predictions %>% 
    mutate(conditions=recode(conditions, 
                             `1shared` = 'One Contrast', 
                             `2priv`='Two Contrasts Privileged', 
                             `2shared`='Two Contrasts Shared'))
  
  data_and_pred = left_join(data_long, pred_long, 
                            by=c("listener"="listener", "condition"="conditions", "locs"="locs")) 
  
  return(data_and_pred)
  
}

get_fit_to_data = function(alpha_value, 
                           metric = "mse", 
                           data=avg_by_subj_comp, 
                           dyad_props=NULL, 
                           count_priv=FALSE, 
                           outlier_cutoff=FALSE){
  #print(data$listener %>% unique())
  all_subj_estimates_i_alpha = map_dfr(data$listener %>% unique(), 
                                       subj_estimates_given_alpha, 
                                       alpha_source=alpha_value,
                                       partners=partners,
                                       avg_by_subj_prod=avg_by_subj_prod,
                                       prod_indiv_alpha=prod_indiv_alpha, 
                                       dyad_props=dyad_props,
                                       count_priv=count_priv ) 
  
  empirical_and_predicted_comprehension_i_alpha = match_pred_to_emp(
    predictions = all_subj_estimates_i_alpha,
    data = data)
  
  if(is.numeric(outlier_cutoff)){
    empirical_and_predicted_comprehension_i_alpha = empirical_and_predicted_comprehension_i_alpha %>%
      filter(!(locs=="target_props" & empirical_props < outlier_cutoff))
  }
  
  model= lm(empirical_props~predicted_props, data = empirical_and_predicted_comprehension_i_alpha)
  model_mse = mean(residuals(model)^2)
  
  return(tibble(alpha = alpha_value, mse = model_mse))
}


show_data_over_model_preds=function(preds_and_data){
  
  alpha_string = preds_and_data$alpha_source %>% unique() %>% as.character()
  # print(alpha_string)
  ggplot(preds_and_data %>%
           mutate(locs=factor(locs, levels=c("target_props", "compet_props"), 
                              labels=c("Target", "Competitor"))))+
    geom_abline(aes(slope=1, intercept=0), linetype="dashed")+
    geom_point(aes(x=predicted_props, y=empirical_props, color=condition, shape=locs), size=3, alpha=0.7)+
    geom_smooth(aes(x=predicted_props, y=empirical_props), method="lm", color="black")+
    coord_cartesian(xlim=c(0,1), ylim=c(0,1))+
    scale_shape_manual(name="Location",values=c(19,21))+
    scale_color_discrete(name="Condition", limits = c("One Contrast", "Two Contrasts Privileged", "Two Contrasts Shared" ), labels = c("One Contrast - Shared", "Two Contrasts - Privileged", "Two Contrasts - Shared" ))+
    geom_hline(aes(yintercept=1/7), linetype='dotted')+
    labs(y="Observed proportions (gaze)", 
         x = bquote('Predicted proportions ('~alpha["\u2113"]~"="~.(alpha_string)~')'))+
    theme_bw(base_size=20) 
}

