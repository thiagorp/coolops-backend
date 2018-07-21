alter table slack_deployments
  drop column build_id;

alter table slack_deployments
  add column deployment_id uuid not null;

alter table only slack_deployments add constraint slack_deployments_deployment_id_fk foreign key (deployment_id) references deployments(id) on delete cascade;
