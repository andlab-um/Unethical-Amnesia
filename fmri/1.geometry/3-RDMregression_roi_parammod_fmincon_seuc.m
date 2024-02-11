base_dir='/media/haiyanwu/HDD4/fmri2019/6.geometry/';


subList=[102,103,105,107,108,110,111,112,113,114,115,116,117,118,119,120,121,122,123,125,127,128,129,130,131,132];

ROIs={'BA6-SMA','BA9-dlPFC','BA13-insula','BA24-dACC',
    'BA28-hip1','BA34-hip','lTPJ','BA23-PCC','OFC'};
%ROIs={'amyg','amygL','amygR'};
ROIs={'BA10-FPC'};
ROIs={'precuneus'};
for r=1:length(ROIs)
    roiname=ROIs{r};
   for i=1:length(subList)
      
        subno=subList(i);
        
        brainRDMs = xlsread([base_dir 'brainRDMs0925.xlsx'],roiname);
        brainRDM=brainRDMs(:,i+1);
        brainRDM=brainRDM(isnan(brainRDM)==0,:);
        
        
        
        comb=xlsread([base_dir 'combination.xlsx'],num2str(subno));
        con_raw= comb(2:end,2);
        mon_raw= comb(2:end,3);
        islie_raw= comb(2:end,4);
        
        rsa_roi_parammod_fmincon(subno,roiname,brainRDM,con_raw,mon_raw,islie_raw,base_dir);
    end 
end
        
    








    






function rsa_roi_parammod_fmincon(subno,roiname,brainRDM,con_raw,mon_raw,islie_raw,base_dir)
    %% rsa_roi_parammod_fmincon()
    %
    % fits fully parametrised model at single subject level
    % using fmincon (mse between constructed rdm and subject rdm)
    % saves the best fitting parameters (and their indices) for each subject

    % NOTE: don't forget to use -v7.3
    %
    % Timo Flesch, 2021,
    % Human Information Processing Lab,
    % Experimental Psychology Department
    % University of Oxford

  

   

    N_ITERS = 1000;

    % %context
    ctx_min = 0;
    ctx_max = 2;

    % compression:
    comp_min = 0.0;
    comp_max = 1.0;

    % rotation:
    phi_lie = 0;
    phi_hon = 90;
    phi_step = 5; % 10 degrees steps
    rot_vect = [0:phi_step:90;-90:phi_step:0];

    bounds_lie_con = [comp_min, comp_max];
    bounds_hon_con = [comp_min, comp_max];
    bounds_lie_mon = [comp_min, comp_max];
    bounds_hon_mon = [comp_min, comp_max];
    bounds_rot = [-90, 90];
    bounds_ctx = [ctx_min, ctx_max];
    constraints = [bounds_lie_con; bounds_lie_mon; bounds_hon_con; bounds_hon_mon; bounds_rot; bounds_ctx];

  
    betas_hat = nan(N_ITERS,6);
    rdms = nan(N_ITERS, length(con_raw), length(con_raw));

    

    
  parfor it = 1:N_ITERS
        % initvals = [.0,.0,1,1,0,2]; % optim [.0,.0,1,1,0,2]

       
        initvals = [.6*rand(1),.6*rand(1),.5+(1-.5)*rand(1),.5+(1-.5)*rand(1),randsample(-90:90,1),2*rand(1)];
        %initvals = [rand(1),rand(1),rand(1),rand(1),randsample(-90:90,1),rand(1)];


        %% gen subject matrix
  
        y_sub = brainRDM';

        loss = @(initvals)sum((y_sub - ParamModelRDM(initvals,con_raw,mon_raw,islie_raw)).^2)+0.0001*norm(initvals,2);
        
        [betas, minloss] = fmincon(loss, initvals, [], [], [], [], constraints(:, 1), constraints(:, 2), [], optimoptions('fmincon', 'Display', 'off'));
        
        betas_hat(it, :) = betas;


        rdms(it, :, :) = squareform(ParamModelRDM(betas,con_raw,mon_raw,islie_raw));      
  end        

    % average over independent iterations:
    rdms = squeeze(nanmean(rdms,1));
    betas_hat = squeeze(nanmean(betas_hat,1));

    % store results
    results = struct();
    

    results.betas_hat = betas_hat;
    results.rdms = rdms;
    betas_hat(5) = abs(betas_hat(5));
    results.meanRDM = squareform(ParamModelRDM(mean(betas_hat,1),con_raw,mon_raw,islie_raw));


    parsave([base_dir 'results_seuc/results_fmincon_' num2str(N_ITERS) 'iter_parametrised_rdms_' roiname '_sub-' num2str(subno)], results);

end

function parsave(str, results)
    save(str, 'results', '-v7.3');
end

function x_sub = ParamModelRDM(theta,con_raw,mon_raw,islie_raw)
    c_lie_con = theta(1);
    c_lie_mon = theta(2);
    c_hon_con = theta(3);
    c_hon_mon = theta(4);
    a1 = 0;
    a2 = theta(5);
    ctx = theta(6);
    
    
    con=con_raw;
    mon=mon_raw;
    islie=islie_raw;
    
    
    for i=1:length(islie)
        if islie(i)==1
            con(i)=[(1 - c_lie_con) .* con_raw(i), (1 - c_lie_mon) .* mon_raw(i)] * [cos(deg2rad(a1)), -sin(deg2rad(a1))]';
            mon(i)=[(1 - c_lie_con) .* con_raw(i), (1 - c_lie_mon) .* mon_raw(i)] * [sin(deg2rad(a1)), cos(deg2rad(a1))]';
            islie(i)=ctx;
        end
        
        if islie(i)==0
            con(i)=[(1 - c_hon_con) .* con_raw(i), (1 - c_hon_mon) .* mon_raw(i)] * [cos(deg2rad(a2)), -sin(deg2rad(a2))]';
            mon(i)=[(1 - c_hon_con) .* con_raw(i), (1 - c_hon_mon) .* mon_raw(i)] * [sin(deg2rad(a2)), cos(deg2rad(a2))]';
        end
    end
            
            
    % compress irrelevant dimension:
    respVect = [con, mon, islie];
   
    rdm = pdist(respVect,'seuclidean');
  
    x_sub = rdm;

    end
    
    
    
    

