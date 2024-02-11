
%%
subjList = [102,103,105,107,108,110,111,112,113,114,115,116,117,118,119,120,121,122,123,125,127,128,129,130,131,132];
subjList = [102];
topdir='/media/haiyanwu/HDD4/fmri2019/';
evpath=[topdir 'fmri2019/2.GLM/event_files/'];
fprep_dir=[topdir 'fmriprep-deriv/fmriprep/'];
cfdir=[topdir 'fmri2019/2.GLM/confound_files/'];
glmdir=[topdir '2.GLM/3.GLM_SPM/results/GLM2/'];
%%
for subj=subjList

subject = num2str(subj, '%03d');
fprintf('processing subj %s',subject)
cd([evpath 'sub-' subject]);   
 
data_elie{1} = load(['lie_run1.txt']);
data_elie{2} = load(['lie_run2.txt']);
data_elie{3} = load(['lie_run3.txt']);
data_elie{4} = load(['lie_run4.txt']);


data_ehon{1} = load(['hon_run1.txt']);
data_ehon{2} = load(['hon_run2.txt']);
data_ehon{3} = load(['hon_run3.txt']);
data_ehon{4} = load(['hon_run4.txt']);


cd(fprep_dir);

func1_nii=['sub-' subject '/ses-1/func/sub-' subject '_ses-1_preproc_smoothed6_masked.nii'];
func2_nii=['sub-' subject '/ses-2/func/sub-' subject '_ses-2_preproc_smoothed6_masked.nii'];
func3_nii=['sub-' subject '/ses-3/func/sub-' subject '_ses-3_preproc_smoothed6_masked.nii'];
func4_nii=['sub-' subject '/ses-4/func/sub-' subject '_ses-4_preproc_smoothed6_masked.nii'];

run1_scans = spm_select('Expand',func1_nii);
run2_scans = spm_select('Expand',func2_nii);
run3_scans = spm_select('Expand',func3_nii);
run4_scans = spm_select('Expand',func4_nii);

scans{1}=run1_scans;
scans{2}=run2_scans;
scans{3}=run3_scans;
scans{4}=run4_scans;

mkdir([glmdir 'sub-' subject]);
%-----------------------------------------------------------------------
% Job saved on 27-Jun-2022 09:54:42 by cfg_util (rev $Rev: 7345 $)
% spm SPM - SPM12 (7487)
% cfg_basicio BasicIO - Unknown
%-----------------------------------------------------------------------
clear matlabbatch;
matlabbatch{1}.spm.stats.fmri_spec.dir = {[glmdir 'sub-' subject]};
matlabbatch{1}.spm.stats.fmri_spec.timing.units = 'secs';
matlabbatch{1}.spm.stats.fmri_spec.timing.RT = 2;
matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t = 16;
matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t0 = 8;
%%
for sesno=1:4
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).scans = cellstr(scans{sesno});
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).name = 'lie';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).onset = data_elie{sesno}(:,1);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).duration = data_elie{sesno}(:,2);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).tmod = 0;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(1).name = 'diff';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(1).param = data_elie{sesno}(:,3);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(1).poly = 1;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(2).name = 'former_diff';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(2).param = data_elie{sesno}(:,4);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(2).poly = 1;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(3).name = 'rt';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(3).param = data_elie{sesno}(:,2);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod(3).poly = 1;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).orth = 1;


matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).name = 'hon';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).onset = data_ehon{sesno}(:,1);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).duration = data_ehon{sesno}(:,2);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).tmod = 0;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(1).name = 'diff';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(1).param = data_ehon{sesno}(:,3);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(1).poly = 1;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(2).name = 'former_diff';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(2).param = data_ehon{sesno}(:,4);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(2).poly = 1;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(3).name = 'rt';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(3).param = data_ehon{sesno}(:,2);
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod(3).poly = 1;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).orth = 1;


matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).multi = {''};
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).regress = struct('name', {}, 'val', {});
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).multi_reg = {[cfdir 'sub-' subject '/ses-' num2str(sesno) '/sub-' subject '_ses-' num2str(sesno) '_confound.txt']};
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).hpf = 128;


end

matlabbatch{1}.spm.stats.fmri_spec.fact = struct('name', {}, 'levels', {});
matlabbatch{1}.spm.stats.fmri_spec.bases.hrf.derivs = [0 0];
matlabbatch{1}.spm.stats.fmri_spec.volt = 1;
matlabbatch{1}.spm.stats.fmri_spec.global = 'None';
matlabbatch{1}.spm.stats.fmri_spec.mthresh = -Inf;
matlabbatch{1}.spm.stats.fmri_spec.mask = {''};
matlabbatch{1}.spm.stats.fmri_spec.cvi = 'AR(1)';

spm('defaults', 'FMRI');
spm_jobman('run', matlabbatch);
clear matlabbatch;

matlabbatch{1}.spm.stats.fmri_est.spmmat = {[glmdir 'sub-' subject '/SPM.mat']};
matlabbatch{1}.spm.stats.fmri_est.write_residuals = 0;
matlabbatch{1}.spm.stats.fmri_est.method.Classical = 1;

spm('defaults', 'FMRI');
spm_jobman('run', matlabbatch);
clear matlabbatch;

matlabbatch{1}.spm.stats.con.spmmat = {[glmdir 'sub-' subject '/SPM.mat']};

matlabbatch{1}.spm.stats.con.consess{1}.tcon.name = 'lie1';
matlabbatch{1}.spm.stats.con.consess{1}.tcon.weights = [1];
matlabbatch{1}.spm.stats.con.consess{1}.tcon.sessrep = 'none';

matlabbatch{1}.spm.stats.con.consess{2}.tcon.name = 'hon1';
matlabbatch{1}.spm.stats.con.consess{2}.tcon.weights = [0 0 0 0 1];
matlabbatch{1}.spm.stats.con.consess{2}.tcon.sessrep = 'none';



matlabbatch{1}.spm.stats.con.consess{3}.tcon.name = 'lie-hon1';
matlabbatch{1}.spm.stats.con.consess{3}.tcon.weights = [1 0 0 0 -1];
matlabbatch{1}.spm.stats.con.consess{3}.tcon.sessrep = 'none';

matlabbatch{1}.spm.stats.con.consess{4}.tcon.name = 'lie2';
matlabbatch{1}.spm.stats.con.consess{4}.tcon.weights = [zeros(1,26) 1];
matlabbatch{1}.spm.stats.con.consess{4}.tcon.sessrep = 'none';

matlabbatch{1}.spm.stats.con.consess{5}.tcon.name = 'hon2';
matlabbatch{1}.spm.stats.con.consess{5}.tcon.weights = [zeros(1,30) 1];
matlabbatch{1}.spm.stats.con.consess{5}.tcon.sessrep = 'none';



matlabbatch{1}.spm.stats.con.consess{6}.tcon.name = 'lie-hon2';
matlabbatch{1}.spm.stats.con.consess{6}.tcon.weights = [zeros(1,26) 1 0 0 0 -1];
matlabbatch{1}.spm.stats.con.consess{6}.tcon.sessrep = 'none';

if subj==105
    matlabbatch{1}.spm.stats.con.consess{7}.tcon.name = 'lie3';
    matlabbatch{1}.spm.stats.con.consess{7}.tcon.weights = [zeros(1,49) 1];
    matlabbatch{1}.spm.stats.con.consess{7}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{8}.tcon.name = 'hon3';
    matlabbatch{1}.spm.stats.con.consess{8}.tcon.weights = [zeros(1,53) 1];
    matlabbatch{1}.spm.stats.con.consess{8}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{9}.tcon.name = 'lie-hon3';
    matlabbatch{1}.spm.stats.con.consess{9}.tcon.weights = [zeros(1,49) 1 0 0 0 -1];
    matlabbatch{1}.spm.stats.con.consess{9}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{10}.tcon.name = 'lie4';
    matlabbatch{1}.spm.stats.con.consess{10}.tcon.weights = [zeros(1,72) 1];
    matlabbatch{1}.spm.stats.con.consess{10}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{11}.tcon.name = 'hon4';
    matlabbatch{1}.spm.stats.con.consess{11}.tcon.weights = [zeros(1,76) 1];
    matlabbatch{1}.spm.stats.con.consess{11}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{12}.tcon.name = 'lie-hon4';
    matlabbatch{1}.spm.stats.con.consess{12}.tcon.weights = [zeros(1,72) 1 0 0 0 -1];
    matlabbatch{1}.spm.stats.con.consess{12}.tcon.sessrep = 'none';
    
   
else
    matlabbatch{1}.spm.stats.con.consess{7}.tcon.name = 'lie3';
    matlabbatch{1}.spm.stats.con.consess{7}.tcon.weights = [zeros(1,52) 1];
    matlabbatch{1}.spm.stats.con.consess{7}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{8}.tcon.name = 'hon3';
    matlabbatch{1}.spm.stats.con.consess{8}.tcon.weights = [zeros(1,56) 1];
    matlabbatch{1}.spm.stats.con.consess{8}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{9}.tcon.name = 'lie-hon3';
    matlabbatch{1}.spm.stats.con.consess{9}.tcon.weights = [zeros(1,52) 1 0 0 0 -1];
    matlabbatch{1}.spm.stats.con.consess{9}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{10}.tcon.name = 'lie4';
    matlabbatch{1}.spm.stats.con.consess{10}.tcon.weights = [zeros(1,78) 1];
    matlabbatch{1}.spm.stats.con.consess{10}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{11}.tcon.name = 'hon4';
    matlabbatch{1}.spm.stats.con.consess{11}.tcon.weights = [zeros(1,82) 1];
    matlabbatch{1}.spm.stats.con.consess{11}.tcon.sessrep = 'none';
    
    matlabbatch{1}.spm.stats.con.consess{12}.tcon.name = 'lie-hon4';
    matlabbatch{1}.spm.stats.con.consess{12}.tcon.weights = [zeros(1,78) 1 0 0 0 -1];
    matlabbatch{1}.spm.stats.con.consess{12}.tcon.sessrep = 'none';
    
    
end

    
    

matlabbatch{1}.spm.stats.con.delete = 1;

spm('defaults', 'FMRI');
spm_jobman('run', matlabbatch);
clear matlabbatch;





end

clear matlabbatch;




