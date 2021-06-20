%% Emotion expression detection for emotion partition (all subects' sessions groupped by emotion)

%% Clear everything 
clear all; close all; clc;
cd('~/BookClub/BC2Emo');

ngpu = gpuDeviceCount();
for i=1:ngpu
    reset(gpuDevice(i));
end

%% Dataset root folder template and suffix
dataFolderTmpl = '~/data/BC2E_Sfx';
dataFolderSfx = '1072x712';

% Create imageDataset of all images in selected baseline folders
[baseSet, dataSetFolder] = createBCbaselineE(dataFolderTmpl, dataFolderSfx, @readFunctionTrainGN_n);
trainingSet = baseSet;

% Count number of the classes ('stable' - presrvation of the order - to use
% later for building confusion matrix)
labels = unique(trainingSet.Labels, 'stable');
[nClasses, ~] = size(labels);

% Print image count for each label
%trainCountTable = countEachLabel(trainingSet);
%trainCount = sum(table2array(trainCountTable(:,2)), 1);
[trainCount, ~] = size(trainingSet.Files);

t1 = clock();
                        
%% Split Database into Training & Test Sets in the ratio 80% to 20% (usually comment out)
%[trainingSet, testSet] = splitEachLabel(baseSet, 0.4, 'randomize'); 

%% Swarm of models
nModels = 6;
myNets = [];
save_net_fileT = '~/data/gn_eswarm';
%save_s1net_fileT = '~/data/gn_eswarm1_sv';
save_s2net_fileT = '~/data/gn_eswarm2_sv';

for s=1:nModels
    n_ll = 144;
    % Load saved model if exists
    save_net_file = strcat(save_net_fileT, int2str(s), '.mat');
    if isfile(save_net_file)
        load(save_net_file, 'myNet');
    end
       
    if exist('myNet') == 0
        % Load Pre-trained Network (AlexNet)   
        % AlexNet is a pre-trained network trained on 1000 object categories. 
        %alex = alexnet;
        google = googlenet;
        
        %% Review Network Architecture 
        lgraph_g = layerGraph(google);
        
        %% Modify Pre-trained Network 
        % AlexNet was trained to recognize 1000 classes, we need to modify it to
        % recognize just nClasses classes. 
        %n_ll = 315;
        lgraph = replaceLayer(lgraph_g, 'loss3-classifier', fullyConnectedLayer(nClasses, 'Name', 'loss3-classifier'));
        lgraph = replaceLayer(lgraph, 'output', classificationLayer('Name', 'output'));

        % Perform Transfer Learning
        % For transfer learning we want to change the weights of the network 
        % ever so slightly. How much a network is changed during training is 
        % controlled by the learning rates. 
        opts = trainingOptions('sgdm',...
                       'ExecutionEnvironment','parallel',...
                       'InitialLearnRate', 0.001,...
                       'LearnRateSchedule', 'piecewise',...
                       'LearnRateDropPeriod', 5,...
                       'LearnRateDropFactor', 0.9,...
                       'MiniBatchSize', 64,...
                       'MaxEpochs', 30);        
                   
                      %'Shuffle', 'every-epoch',... 
                      %'Plots', 'training-progress',...

        % Train the Network 
        % This process usually takes about 30 minutes on a desktop GPU. 
    
        % Shuffle training set for more randomness
        trainingSetS = shuffle(trainingSet);
    
        while (exist('TInfo')==0) || (TAcc < 90.) 
            [myNet, TInfo] = trainNetwork(trainingSetS, lgraph, opts);
            [~, TAccLast] = size(TInfo.TrainingAccuracy);
            TAcc = TInfo.TrainingAccuracy(TAccLast);
        end
        clear('TInfo');
        
        save(save_net_file, 'myNet');

    end
    
    myNets = [myNets, myNet];
    
    clear('myNet');
    clear('trainingSetS');
    clear('lgraph');
    clear('google');
    
end

t2 = clock();
fprintf('GoogleNet training N images:%d time:%.3f, models %d\n', trainCount, etime(t2, t1), nModels);


%% Mem cleanup
for i=1:ngpu
    reset(gpuDevice(i));
end


%% Traditional accuracy (usually comment out)
%predictedLabels = classify(myNets(1), testSet); 
%accuracy = mean(predictedLabels == testSet.Labels)

%predictedScores = predict(myNets(1), testSet);
%[nImages, ~] = size(predictedScores);
%for k=1:nImages
%    maxScore = 0;
%    maxScoreNum = 0;
%    maxScoreClass = "S";
%    correctClass = testSet.Labels(k);
%    for l=1:nClasses
%        if maxScore <= predictedScores(k, l)
%            maxScore = predictedScores(k, l);
%            maxScoreNum = l;
%            maxScoreClass = predictedLabels(k);
%        end
%    end   
%    fprintf("%s %f %s \n", correctClass, maxScore, maxScoreClass);
%end



%% Reliability training datasets
% Create imageDataset vector of images in selected makeup folders
[testRSets, testRDataSetFolders] = createBCtestE1(dataFolderTmpl, dataFolderSfx, @readFunctionTrainGN_n);

t1 = clock();

%% Create Matrix of Softmax Activations
[nMakeups, ~] = size(testRSets);
nImgsTot = 0;

for i=1:nMakeups
    [nImages, ~] = size(testRSets{i}.Files);
    nImgsTot = nImgsTot + nImages;
end

Act = zeros([nImgsTot nClasses nModels]);
Verd = zeros([nImgsTot nModels]);
Strong = zeros([nImgsTot nModels]);

Act2 = zeros([nImgsTot nModels]);

ActS = zeros([nImgsTot nClasses*nModels]);
VerdS = zeros([nImgsTot 1]);



%% Populate Matrix of Softmax Activations
nImgsCur = 1;
for i=1:nMakeups   
    [nImages, ~] = size(testRSets{i}.Files);
    
    fprintf('Makeup # %d/%d\n', i, nMakeups);    
            
    %% Walk through model Swarm
    ActPF = zeros([nImages nClasses nModels]);
    VerdPF = zeros([nImages nModels]);
    testRSet = testRSets{i};
    parfor s=1:nModels 
        
        predictedLabels = classify(myNets(s), testRSet); 
        predictedScores = predict(myNets(s), testRSet);
        ActPF(:, :, s) = predictedScores;
        VerdPF(:, s) = (testRSet.Labels == predictedLabels);
    end
    Act(nImgsCur:nImgsCur + nImages - 1, :, :) = ActPF(:, :, :);        
    Verd(nImgsCur:nImgsCur + nImages - 1, :) = VerdPF(:, :);
    
    nImgsCur = nImgsCur + nImages;
    
end

%% Sorted activations of model candidates
[ActC, I] = sort(Act, 2, 'descend');


%% Collect the strongest softmax of models and flatten ensamble verdict vector
% (Another place to add supervisor to rank models based of its core)
%Strong(:, :) = mean(ActC(:, :, :), 2);
Strong(:, :) = ActC(:, 1, :);


%% Sort other models by their strongest softmax 
[StrongC, IStrong] = sort(Strong, 2, 'descend');

% And sort their softmax activations in the same way as activations
% in the model with strongest right softmax
for k=1:nImgsTot
    for si=1:nModels
        
        s = IStrong(k, si);
        if si == 1
            
            if Verd(k, s) > 0
                VerdS(k) = sum(Verd(k, :), 2);
                %VerdS(k) = Verd(k, s); 
            end
        end
        
        ActS(k, nClasses*(si-1)+1:nClasses*si) = Act(k, I(k, :, IStrong(k, 1)), s);
        
    end
end

for i=1:ngpu
    reset(gpuDevice(i));
end

%% Train Supervisor model
save_s2net_file = strcat(save_s2net_fileT, int2str(nModels), '.mat');
if isfile(save_s2net_file)
    load(save_s2net_file, 'super2Net');
else
    clear('super2Net');
end

if exist('super2Net') == 0
    
    Yt = categorical(VerdS');
    [~, nVerdicts] =  size(countcats(Yt));
    nLayer1 = nClasses*nModels;
    nLayer2 = nClasses*nModels;
    nLayer3 = nClasses*nModels;

    sLayers = [
        featureInputLayer(nClasses*nModels)
        fullyConnectedLayer(nLayer1)
        reluLayer
        %dropoutLayer(0.5)
        fullyConnectedLayer(nLayer2)
        %tanhLayer
        reluLayer
        %dropoutLayer(0.5)
        fullyConnectedLayer(nLayer3)
        reluLayer
        %dropoutLayer(0.5)
        fullyConnectedLayer(nVerdicts)
        softmaxLayer
        classificationLayer
    ];

    sOptions = trainingOptions('adam', ...
        'ExecutionEnvironment','parallel',...
        'LearnRateSchedule', 'piecewise',...
        'LearnRateDropPeriod', 5,...
        'LearnRateDropFactor', 0.9,...
        'Shuffle', 'every-epoch',...
        'MiniBatchSize', 64, ...
        'InitialLearnRate',0.01, ...
        'MaxEpochs',300, ...
        'Verbose',true, ...
        'Plots','training-progress');
    
        %'Shuffle', 'every-epoch',...
            
    %Yt = categorical(VerdS');

    super2Net = trainNetwork(ActS, Yt, sLayers, sOptions);

    %save(save_s2net_file, 'super2Net');

end

t2 = clock();
fprintf('GoogleNet Supervisor training N images:%d time:%.3f, models %d\n', nImgsTot, etime(t2, t1), nModels);


%% Makeup datasets
mkDataSetFolder = strings(0);
mkLabel = strings(0);

% Create imageDataset vector of images partitioned by emotions
[testSets, testDataSetFolders] = createBCtestEe(dataFolderTmpl, dataFolderSfx, @readFunctionTrainGN_n);


%%
[nMakeups, ~] = size(testSets);

mkTable = cell(nMakeups, nClasses+4);

%%

% Write per-image scores to a file
fd = fopen( strcat('predict_gn_6bfmsr',int2str(nModels),'.txt'),'w' );

fprintf(fd, "CorrectClass MaxScore MaxScoreClass TrustedVote VoteScore TrustedScore FileName");
for l=1:nClasses
    fprintf(fd, " %s", myNets(1).Layers(n_ll).Classes(l));
end
fprintf(fd, "\n");




%% Create Matrix of Softmax Activations
[nMakeups, ~] = size(testSets);
nImgsTot = 0;

for i=1:nMakeups
    [nImages, ~] = size(testSets{i}.Files);
    nImgsTot = nImgsTot + nImages;
end

fprintf('GoogleNet test N images:%d, models %d\n', nImgsTot, nModels);

ActT = zeros([nImgsTot nClasses nModels]);
StrongT = zeros([nImgsTot nModels]);
ActTS = zeros([nImgsTot nClasses*nModels]);


%% Populate Matrix of Softmax Activations
predictedLabelsSwarm = cell(nMakeups, nModels);
predictedScoresSwarm = cell(nMakeups, nModels);
nImgsCur = 1;
for i=1:nMakeups   
    [nImages, ~] = size(testSets{i}.Files);
    
    fprintf('Makeup # %d/%d\n', i, nMakeups);
            
    %% Walk through model Swarm
    ActTPF = zeros([nImages nClasses nModels]);
    testSet = testSets{i};
    parfor s=1:nModels 
        % Test main network performance
        predictedLabels = classify(myNets(s), testSet);
        predictedLabelsSwarm{i, s} = predictedLabels;
        predictedScores = predict(myNets(s), testSet);
        predictedScoresSwarm{i, s} = predictedScores;
        
        ActTPF(:, :, s) = predictedScores;        
    end
    ActT(nImgsCur:nImgsCur + nImages - 1, :, :) = ActTPF(:, :, :);
    
    nImgsCur = nImgsCur + nImages;
end

%% Sorted activations of model candidates
[ActTC, IT] = sort(ActT, 2, 'descend');

%for s=1:nModels
%    super2Scores = predict(mySuper1Nets(s), ActTC(:, :, s));
%    Act2T(:, s) = super2Scores(:, 2);
%end        

%[LikelyTC, ILikelyT] = sort(Act2T, 2, 'descend');


% Collect the strongest softmax of models and flatten ensamble verdict vector
% (Another place to add supervisor to rank models based of its core)
%StrongT(:, :) = mean(ActTC(:, :, :), 2);
StrongT(:, :) = ActTC(:, 1, :);

%% Sort other models by their strongest softmax 
[StrongTC, IStrongT] = sort(StrongT, 2, 'descend');

% And sort their softmax activations in the same way as activations
% in the model with strongest right softmax
for k=1:nImgsTot
      
    for si=1:nModels
        
        s = IStrongT(k, si);
        ActTS(k, nClasses*(si-1)+1:nClasses*si) = ActT(k, IT(k, :, IStrongT(k, 1)), s);
        
    end
end   

   
%%
% Supervisor network
supervisorPredictedLabels = classify(super2Net, ActTS); 
supervisorPredictedScores = predict(super2Net, ActTS);

nImgsCur = 0;
for i=1:nMakeups 
    clear('predictedScoresS');
    clear('predictedLabelsS');
    
    for s=1:nModels 
        predictedScoresS(:, :, s) = predictedScoresSwarm{i, s};
        predictedLabelsS(:, s) = predictedLabelsSwarm{i, s};
    end
    % Ensemble voting
    [~, MI] = max(countcats(predictedLabelsS, 2), [], 2);
    predictedLabelsCat = (categories(predictedLabelsS));
    predictedLabels = predictedLabelsCat(MI);
    
    predictedScores = mean(predictedScoresS, 3);
        
    [nImages, ~] = size(testSets{i}.Files);
    for k=1:nImages
    
        maxScore = 0;
        maxScoreNum = 0;
        maxScoreClass = "S";
        correctClass = testSets{i}.Labels(k);
        for l=1:nClasses
            if maxScore <= predictedScores(k, l)
                maxScore = predictedScores(k, l);
                maxScoreNum = l;
                maxScoreClass = predictedLabels{k};
            end
        end
    
        fprintf(fd, "%s %f %s %f %f %f %s", correctClass, maxScore, maxScoreClass,...
            double(string(supervisorPredictedLabels(nImgsCur+k)))/nVerdicts,...
            max(supervisorPredictedScores(nImgsCur+k, :)),...
            1.-supervisorPredictedScores(nImgsCur+k,1), testSets{i}.Files{k});
        for l=1:nClasses
            fprintf(fd, " %f", predictedScores(k, l));
        end
        fprintf(fd, "\n");
        
    end
    
    nImgsCur = nImgsCur + nImages;
    
    [tmpStr, ~] = strsplit(testSets{i}.Files{1}, '/');
    fprintf("%s", string(testSets{i}.Labels(1))); %tmpStr{1,7}); 
    mean(predictedScores)
    
    
    %% Compute average accuracy
    meanMkAcc = mean(predictedLabels == testSets{i}.Labels);
    mkTable{i,1} = string(testSets{i}.Labels(1));
    mkTable{i,2} = meanMkAcc;
    
    %%
    [tn, ~] = size(testSets{i}.Files);
    
    meanMkConf = zeros(1, nClasses);

    maxAccCat = '';
    maxAcc = 0;
    
    %%    
    j = 1;   
    for j = 1:nClasses

        tmpStr = strings(tn,1);
        tmpStr(:) = string(labels(j));
    
        meanMkConf(j) = mean(string(predictedLabels) == tmpStr);
        mkTable{i, 4+j} = meanMkConf(j);
        
        %find the best category match
        if maxAcc <= meanMkConf(j)
            maxAccCat = tmpStr(j);
            maxAcc = meanMkConf(j);
        end
        
    end
    mkTable{i,3} = maxAccCat;
    mkTable{i,4} = maxAcc;
    
end

%% Results
varNames = cellstr(['TestFolder' 'Accuracy' 'BestGuess' 'GuessScore' string(labels)']);
cell2table(mkTable, 'VariableNames', varNames)

fclose(fd);
