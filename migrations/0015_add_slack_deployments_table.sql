create table slack_deployments (
  id uuid default uuid_generate_v4() not null,
  build_message_id uuid not null,
  environment_id uuid not null,
  slack_user_name text not null,
  slack_user_id character varying(255) not null,
  deployed_at timestamp without time zone not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only slack_deployments add constraint slack_deployments_pkey primary key (id);

alter table only slack_deployments add constraint slack_deployments_build_message_id_fk foreign key (build_message_id) references slack_build_messages(id) on delete cascade;

alter table only slack_deployments add constraint slack_deployments_environment_id_fk foreign key (environment_id) references environments(id) on delete cascade;

create index index_slack_deployments_on_build_message_id on slack_deployments using btree(build_message_id);
