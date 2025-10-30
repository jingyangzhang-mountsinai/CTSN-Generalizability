
FittingCSF<-function(data, treatment, Time2_event, event,  predictors, params, byq=0.05){
  data<- data %>%
    select(all_of(c(predictors,treatment,Time2_event,event))) %>%
    filter(!is.na(!!(Time2_event)), !is.na(!!(event)), !is.na(treatment)) %>%
    drop_na()
  active.trt=levels(data[,treatment])[2]
  data<- data %>%
    mutate(treatment.num=if_else(!!sym(treatment)==active.trt,1,0),
           event=if_else(!!sym(event)==1,1,0),
           Time2_event=as.numeric(!!sym(Time2_event))
    )
  dim(data)
  model <- causal_survival_forest(
    X = data[, predictors],
    Y = data[,Time2_event],
    W = data[,'treatment.num'],
    D=  data[,'event'],
    W.hat=mean(data$treatment.num, na.rm=TRUE), # proportion treated (treatment propensitys)
    horizon = params$horizon, #1825 days --> 5 years
    target = params$target, #"survival.probability"
    num.trees = replacenull(params$num.trees,2000), # 1500
    mtry = replacenull(params$mtry,min(ceiling(sqrt(length(predictors)) + 20), length(predictors))), #30
    min.node.size = replacenull(params$min.node.size,5), # 5
    sample.fraction = replacenull(params$sample.fraction,.5), # 0.5
    honesty = replacenull(params$honesty, TRUE), # FALSE
    alpha = replacenull(params$alpha, .05), # 0.05
    imbalance.penalty = replacenull(params$imbalance.penalty,0), # 0
    ci.group.size = replacenull(params$ci.group.size,2),
    honesty.fraction=0.5,
    honesty.prune.leaves=TRUE,
    compute.oob.predictions=TRUE,
    stabilize.splits=TRUE,
    seed=123456
  )
  print("generated the model")
  ### this is like a CV predict(model.train)
  data$ITE <- predict(model, data[, predictors])$predictions
  rate <- rank_average_treatment_effect(forest=model,
                                        priorities=data$ITE,
                                        q = seq(0.1, 1, by =byq))
  blp<-best_linear_projection(model, data[, predictors]) %>%
    tidy() %>% arrange(p.value) %>% subset(!grepl('Intercept',term))
  return(list(data=data,model=model,rate=rate, xai= blp))
}
clinical.features<-c( 'TF_15', 'msi_highdef', 'RASSmt', 'SexF', 'AGE',"Ploidy",'braf_status','PS')
s.data <- bacci_dataset_MSF$patient_clinical %>%
  mutate(  msi_highdef=(1*(msi %in%c('MMR Deficient', 'MSI High'))),
           RASSmt=if_else(RASSTATTYPE=='Mutant',1,0)  ,
           SexF=if_else(GENDERDESC=='Female',1,0),
           braf_status=if_else(braf_status=='Mutant',1,0)
  ) %>% distinct()
s.data <- left_join(s.data,
                    bacci_dataset_MSF$ctDNA_w_metadata_df %>%
                      subset(Timepoint_simple=='Baseline') %>%
                      mutate( TF_15 =1*(Tumor.Frac > 0.15)) %>%
                      select(cimac_part_id, Ploidy, TF_15) %>%
                      distinct(),
                    by='cimac_part_id')
#TF 15 is better than TF
data <- left_join(assay.baseline[, !colnames(assay.baseline)%in%clinical.features],
                  s.data %>% select(all_of(c('cimac_part_id', clinical.features))),
                  by='cimac_part_id')
ann.colors$set<-c('clinical'='dodgerblue', 'olink'='red',
                  'clinical_and_olink'='purple')

features<-union(marker.sig.cont, marker.sig.binary)
features<-unique(gsub('_binary','',features))
features<-features[!grepl('nano',features)] # because low number of features
features<-intersect(features,olink.markers.all) # other assay too missing
L<-list(clinical=clinical.features,
        olink=features,
        clinical_and_olink=c(clinical.features,features)
)
params = list(W.hat=mean(data$arm_n=='Atezo_Bev_Cape'),
              target="survival.probability",
              num.trees=500,
              #mtry=20,
              min.node.size=5,
              horizon=max(data[,Time2_event]),
              sample.fraction=0.5,
              honesty=FALSE,
              alpha=0.05,
              imbalance.penalty=0,
              ci.group.size=5,
              stabilize.splits=TRUE
)
RES<-map(L,
         ~{FittingCSF(data= data, treatment="arm_n", Time2_event="fu_time", event="fu_stat",
                      predictors=.x, params)
         })
TOCs<-lapply(names(RES), function(x){(RES[[x]]$rate$TOC %>% as.data.frame() %>% mutate(set=x))}) %>% bind_rows() %>% mutate(conf.low=estimate-std.err, conf.high=estimate+std.err)
AUTOCS<-lapply(names(RES), function(x){(RES[[x]]$rate[1:3] %>% as.data.frame() %>% mutate(set=x))}) %>% bind_rows() %>% mutate(conf.low=estimate-std.err, conf.high=estimate+std.err, set=factor(set, levels=names(ann.colors$set)))
xAI<-lapply(names(RES), function(x){(RES[[x]]$xai %>% as.data.frame() %>% mutate(set=x))}) %>%
  bind_rows() %>% mutate(estimate=if_else(term=='AGE',10*estimate, estimate))
AUTOCS %>% ggplot(aes(x=set, y=estimate, fill=set, color=set))+
  geom_bar(stat='identity', position=position_dodge(), alpha=0.7)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9))+
  labs(title='Causal Survival Forest: AUTOC',
       x='Feature Set',
       y='AUTOC')+
  theme_bacci+
  scale_color_manual(values=ann.colors$set)+
  scale_fill_manual(values=ann.colors$set)+
  theme(legend.title = element_blank()) ->Pautoc
lme(estimate~set+q, random=~1|q, TOCs) %>% emmeans('pairwise'~set, adjust='none')
TOCs %>% ggplot(aes(x=q, y=estimate, color=set))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=set), alpha=0.1, lty=2,color=NA)+
  labs(title='Causal Survival Forest: AUTOC',
       x='Time (days)',
       y='TOC')+
  theme_bacci+
  scale_color_manual(values=ann.colors$set)+
  scale_fill_manual(values=ann.colors$set)+
  theme(legend.title = element_blank()) ->Pcsf
xAI %>% ggplot(aes(x=reorder(term, p.value), y=abs(estimate),
                   fill=set, colour=set, shape=(estimate>0)))+
  geom_point(aes(size=-log10(p.value)))+
  labs(title='Causal Survival Forest: AUTOC',
       x='quantiles',
       y='Impact on ITE')+
  scale_shape_manual(values=c(24,25), labels=c('Negative','Positive'))+
  scale_size_continuous(name = "-log10(p-value)",
                        range = c(1, 10)) +
  scale_color_manual(values=ann.colors$set)+
  scale_fill_manual(values=ann.colors$set)+
  theme_bacci+
  theme(legend.title = element_blank())+
  facet_wrap(~set)+
  coord_flip()->Pxai
ggsave(file=paste0(Results.dir,"bacci_CSF_xAIs.pdf"), plot=Pxai, width =4, height = 8)
xAI %>%  mutate(olink=term%in%features) %>%
  group_by(term) %>% mutate(mp=min(p.value)) %>% ungroup() %>%
  arrange(olink,abs(estimate)) %>% mutate(term=factor(term, levels=unique(term))) %>%
  subset(mp<=0.3 & set!='olink') %>%
  ggplot(aes(x=term, y=estimate ,
             fill=set, colour=set))+
  geom_pointrange(aes(size=p.value<=.2, ymin=0, ymax=estimate),
                  position=position_dodge(.5))+
  labs(title='xAI: Important Predictive features in CSF model (p<0.2)',
       x='marker ',
       y='Impact on ITE ( >0, increased benefit of combination)') +
  scale_size_continuous(name = "-log10(p-value)",range = c(.1, 1),
                        breaks=c(0,1.3,2)) +
  scale_color_manual(values=ann.colors$set)+
  scale_fill_manual(values=ann.colors$set)+
  theme_bacci+
  theme(legend.title = element_blank())+
  coord_flip()->Pxai
aux <- RES$clinical_and_olink$data %>%
  mutate(
    Predicted_benefit = factor(cut(
      ITE,
      breaks = quantile(ITE, probs = c(0, 0.3, 0.70, 1), na.rm = TRUE),
      include.lowest = TRUE
    ), labels = c("harm", "neutral", "benefit")),
    Predicted = if_else(ITE>0, 'Benefit from Combo', 'Not Benefit from Combo')
  )
t.test(ITE ~ arm_n, data = aux) # great
ggplot(aux, aes(arm_n,ITE))+geom_violin()+theme_bacci
cox_model <- coxph(Surv(fu_time, fu_stat == 1) ~  arm_n*Predicted, data = aux)
emmeans(cox_model, 'pairwise'~arm_n|Predicted)
surv_fit <- survfit(cox_model, newdata = aux)
ps<-ggsurvplot_facet(
  fit = survfit(Surv(fu_time, fu_stat == 1) ~  arm_n, data = aux),
  data = aux,
  facet.by ='Predicted',  # Facet by arm
  palette =ann.colors$arm_n,
  pval = TRUE,
  legend.title = "Predicted Benefit",
  risk.table = FALSE)
library(patchwork)
final_plot <- wrap_plots(Pcsf,Pautoc, ps, Pxai, ncol = 1)
ggsave(file=paste0(Results.dir,"bacci_CSF_summary.OS.pdf"), plot=final_plot, width =8, height = 16)
