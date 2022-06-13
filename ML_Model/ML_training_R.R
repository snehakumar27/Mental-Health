### Decision Trees ###
set.seed(2022)
new_nus_survey_num_sg<-read.csv("Datasets/SG_data.csv")

new_sg_survey<-new_nus_survey_num_sg%>%
  select(age,gender,family_history, seek_treatment,interference_work_num,interference_work_at_all,y_train)

colnames(new_sg_survey)<- c("Age","Gender","family_history","treatment","work_interfere", "work_interfere_at_all", "train_y")

kaggle_data<- read.csv("Datasets/Kaggle_data.csv")

new_dt.fit_kaggle<-rpart(treatment~Age+Gender+family_history+
                       work_interfere, 
                     method="class",
                     data = kaggle_data, 
                     control = rpart.control(minsplit = 8,
                                             maxdepth = 3,
                                            minbucket = 7), 
                     parms=list(split='information'))
new_treeplot_kaggle<-rpart.plot(new_dt.fit_kaggle,type=4,extra=2,clip.right.labs=FALSE,varlen=0,faclen=0)
new_pred_kaggle<-predict(new_dt.fit_kaggle, new_sg_survey, type = "class")
new_conf_mat_kaggle_treatment<-table(new_pred_kaggle, new_sg_survey$treatment)
new_conf_mat_kaggle_ytrain<-table(new_pred_kaggle, new_sg_survey$train_y)
new_accuracy_kaggle_sg_treatment<- (new_conf_mat_kaggle_treatment[1,1]+new_conf_mat_kaggle_treatment[2,2])/nrow(new_sg_survey)
new_accuracy_kaggle_sg_ytrain<- (new_conf_mat_kaggle_ytrain[1,1]+new_conf_mat_kaggle_ytrain[2,2])/nrow(new_sg_survey)
rbind(new_accuracy_kaggle_sg_treatment, new_accuracy_kaggle_sg_ytrain)

cp_choice_dt<-new_dt.fit_kaggle$cptable[which.min(new_dt.fit_kaggle$cptable[,"xerror"]), "CP"]

dt.fit2_pruned<- prune(new_dt.fit_kaggle, cp = cp_choice_dt)
treeplot_pruned<-rpart.plot(dt.fit2_pruned,type=4,extra=2,clip.right.labs=FALSE,varlen=0,faclen=0)
pred_dt.fit2_pruned<- predict(dt.fit2_pruned, new_sg_survey, type = "class")
conf_dt.fit2_pruned_treatment<- table(pred_dt.fit2_pruned, new_sg_survey$treatment)
conf_dt.fit2_pruned_ytrain<-table(pred_dt.fit2_pruned, new_sg_survey$train_y)
accuracy3_dt.fit2_pruned_treatment<- (conf_dt.fit2_pruned_treatment[1,1]+conf_dt.fit2_pruned_treatment[2,2])/nrow(new_sg_survey)
accuracy3_dt.fit2_pruned_ytrain<- (conf_dt.fit2_pruned_ytrain[1,1]+conf_dt.fit2_pruned_ytrain[2,2])/nrow(new_sg_survey)

rbind(accuracy3_dt.fit2_pruned_treatment, accuracy3_dt.fit2_pruned_ytrain)



new_dt.fit_kaggle2<-rpart(train_y~Age+Gender+family_history+work_interfere+
                        work_interfere_at_all,
                      method="class",
                      data = kaggle_data)
new_treeplot_kaggle2<-rpart.plot(new_dt.fit_kaggle2,type=4,extra=2,clip.right.labs=FALSE,varlen=0,faclen=0)
new_pred_kaggle2<-predict(new_dt.fit_kaggle2, new_sg_survey, type = "class")
new_conf_mat_kaggle_treatment2<-table(new_pred_kaggle2, new_sg_survey$treatment)
new_conf_mat_kaggle_ytrain2<-table(new_pred_kaggle2, new_sg_survey$train_y)
new_accuracy_kaggle_sg_treatment2<- (new_conf_mat_kaggle_treatment2[1,1]+new_conf_mat_kaggle_treatment2[2,2])/nrow(new_sg_survey)
new_accuracy_kaggle_sg_ytrain2<- (new_conf_mat_kaggle_ytrain2[1,1]+new_conf_mat_kaggle_ytrain2[2,2])/nrow(new_sg_survey)
rbind(new_accuracy_kaggle_sg_treatment2, new_accuracy_kaggle_sg_ytrain2)

###SVM 
library(e1071)
set.seed(2022)
new_svm_fit1<- svm(treatment~Age+Gender+family_history+work_interfere+
                 work_interfere_at_all, 
               data = kaggle_data, 
               type = "C-classification", 
               kernel = 'linear')

new_pred_svm1<- predict(new_svm_fit1, new_sg_survey)
new_conf_mat_svm1_treatment<- table(new_pred_svm1, new_sg_survey$treatment)
new_conf_mat_svm1_ytrain<- table(new_pred_svm1, new_sg_survey$train_y)
new_accuracy_svm1_treatment<-(new_conf_mat_svm1_treatment[1,1]+new_conf_mat_svm1_treatment[2,2])/nrow(new_sg_survey)
new_accuracy_svm1_ytrain<-(new_conf_mat_svm1_ytrain[1,1]+new_conf_mat_svm1_ytrain[2,2])/nrow(new_sg_survey)
rbind(new_accuracy_svm1_treatment, new_accuracy_svm1_ytrain)

### Naive Bayes
new_bayes.fit_kaggle1<-naiveBayes(treatment~Age+Gender+family_history+work_interfere+
                                anonymity+work_interfere_at_all,
                              kaggle_data, 
                              laplace =0)
new_pred_kaggle3<- predict(new_bayes.fit_kaggle1, new_sg_survey)
new_conf_mat_kaggle_treatment3<- table(new_pred_kaggle3, new_sg_survey$treatment)
new_conf_mat_kaggle_ytrain3<- table(new_pred_kaggle3, new_sg_survey$train_y)
new_accuracy_kaggle_sg_treatment3<- (new_conf_mat_kaggle_treatment3[1,1]+new_conf_mat_kaggle_treatment3[2,2])/nrow(new_sg_survey)
new_accuracy_kaggle_sg_ytrain3<- (new_conf_mat_kaggle_ytrain3[1,1]+new_conf_mat_kaggle_ytrain3[2,2])/nrow(new_sg_survey)

rbind(new_accuracy_kaggle_sg_treatment3, new_accuracy_kaggle_sg_ytrain3)


