# AIVR2021
Code for paper 'Isolating Uncertainty of the Face Expression Recognition with the Meta-Learning Supervisor Neural Network'

List of files:

 - Program files to re-train SOTA CNN models and train SNN:
Inception3DetailBC2Eemsr.m	
AlexNetDetailBC2Eemsr.m		
Vgg19DetailBC2Eemsr.m
GoogleNetDetailBC2Eemsr.m	
IResnet2DetailBC2Eemsr.m		
Resnet50DetailBC2Eemsr.m

To configure, find and modify the following fragment:
  %% CONFIGURATION PARAMETERS:
  % Download BookClub dataset from: https://drive.google.com/file/d/1_U8SypDurlHV4c8NvBvdATn9g6SShPjs/view?usp=sharing
  % and unarchive it into the dierctory below:
  %% Dataset root folder template and suffix
dataFolderTmpl = '~/data/BC2E_Sfx';
dataFolderSfx = '1072x712';
  %Set number of models in the ensemble: 1, 2, 4, 8, 16
nModels = 6;
  %Set directory and template for the retrained CNN models:
save_net_fileT = '~/data/an_eswarm';

 - Libraries:
   * Training and test sets building
createBCbaselineE.m
createBCtestE1.m
createBCtestEe.m

   * Image size conversion:
readFunctionTrainGN_n.m
readFunctionTrainIN_n.m
readFunctionTrain_n.m

 - Summary per emotion and detailed per image results for 6 CNN ensemble for various models:
result_an_6bfmsr6.txt
result_gn_6bfmsr6.txt
result_vgg_6bfmsr6.txt
result_rn_6bfmsr6.txt
result_in_6bfmsr6.txt
result_ir_6bfmsr6.txt
predict_an_6bfmsr6.txt
predict_gn_6bfmsr6.txt
predict_vgg_6bfmsr6.txt
predict_rn_6bfmsr6.txt
predict_in_6bfmsr6.txt
predict_ir_6bfmsr6.txt

 - Summary per emotion and detailed per image results for Inception v.3 ensemble of 1, 2, 4, 6, 8, 12 count:
result_in_6bfmsr1.txt
result_in_6bfmsr2.txt
result_in_6bfmsr4.txt
result_in_6bfmsr6.txt
result_in_6bfmsr8.txt
result_in_6bfmsr12.txt
predict_in_6bfmsr1.txt
predict_in_6bfmsr2.txt
predict_in_6bfmsr4.txt
predict_in_6bfmsr6.txt
predict_in_6bfmsr8.txt
predict_in_6bfmsr12.txt

 - Accuracy metrics calculation script:
pred_dist2emsr.R
