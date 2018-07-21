alter table slack_deployments
  add column build_id uuid not null;

alter table only slack_deployments add constraint slack_deployments_build_id_fk foreign key (build_id) references builds(id) on delete cascade;
