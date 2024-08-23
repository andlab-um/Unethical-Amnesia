
%%
subjList = [101,102,103,105,106,107,108,109,110,111,112,115,116,117,118,119,120,121,123,125,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143];
%subjList=[101];

topdir='/home/haiyanwu/nas_data/experiment_data/uarep_only/';

fprep_dir=[topdir 'fmriprep-deriv-rep/fmriprep/'];
cfdir=[topdir '1.GLM/confound_files_rep/'];
glmdir=[topdir '1.GLM/results/GLM4uarep_SPM/'];

alldata=readtable([topdir '2.uarep-hddm/fMRI_sessionData_DDM_rep.csv']);
alldata.time_onset_new=alldata.time_onset+2;



%%
for subj=subjList

subject = num2str(subj, '%03d');
fprintf('processing subj %s',subject)

data=alldata(alldata.subj==subj,:);
 

%%
cd(fprep_dir);

func1_nii=['sub-' subject '/ses-1/func/sub-' subject '_ses-1_preproc_smoothed4_masked.nii'];
func2_nii=['sub-' subject '/ses-2/func/sub-' subject '_ses-2_preproc_smoothed4_masked.nii'];
func3_nii=['sub-' subject '/ses-3/func/sub-' subject '_ses-3_preproc_smoothed4_masked.nii'];
func4_nii=['sub-' subject '/ses-4/func/sub-' subject '_ses-4_preproc_smoothed4_masked.nii'];

run1_scans = spm_select('Expand',func1_nii);
run2_scans = spm_select('Expand',func2_nii);
run3_scans = spm_select('Expand',func3_nii);
run4_scans = spm_select('Expand',func4_nii);
%%
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
matlabbatch{1}.spm.stats.fmri_spec.timing.RT = 1;
matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t = 16;
matlabbatch{1}.spm.stats.fmri_spec.timing.fmri_t0 = 8;
%%
for sesno=1:4
    
sesdata=data(data.session==sesno,:);
sesdata_pos=sesdata(sesdata.former_diff>0,:);
sesdata_neg=sesdata(sesdata.former_diff<0,:);
sesdata_zero=sesdata(sesdata.former_diff==0,:);
    
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).scans = cellstr(scans{sesno});
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).name = 'pos';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).onset = sesdata_pos.time_onset_new;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).duration = sesdata_pos.RT;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).tmod = 0;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).pmod = struct('name', {},'param', {}, 'poly', {});
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(1).orth = 1;

matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).name = 'neg';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).onset = sesdata_neg.time_onset_new;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).duration = sesdata_neg.RT;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).tmod = 0;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).pmod = struct('name', {},'param', {}, 'poly', {});
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(2).orth = 1;

matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(3).name = 'zero';
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(3).onset = sesdata_zero.time_onset_new;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(3).duration = sesdata_zero.RT;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(3).tmod = 0;
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(3).pmod = struct('name', {},'param', {}, 'poly', {});
matlabbatch{1}.spm.stats.fmri_spec.sess(sesno).cond(3).orth = 1;

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
SPMmat=load([glmdir 'sub-' subject '/SPM.mat']);


matlabbatch{1}.spm.stats.con.consess{1}.tcon.name = 'pos-neg';
matlabbatch{1}.spm.stats.con.consess{1}.tcon.weights = [1 -1];
matlabbatch{1}.spm.stats.con.consess{1}.tcon.sessrep = 'both';


matlabbatch{1}.spm.stats.con.delete = 1;

spm('defaults', 'FMRI');
spm_jobman('run', matlabbatch);
clear matlabbatch;



end
