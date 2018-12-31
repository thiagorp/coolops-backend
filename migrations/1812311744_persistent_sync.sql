alter table deployments add column deployer_external_id varchar;

delete from deployments d where not exists (select true from slack_deployments sd where sd.deployment_id = d.id);

update deployments set deployer_external_id = sd.slack_user_id from slack_deployments sd where sd.deployment_id = deployments.id;

alter table deployments alter column deployer_external_id set not null;
