create table deployments (
  id uuid default uuid_generate_v4() not null,
  build_id uuid not null,
  environment_id uuid not null,
  status character varying(255) not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only deployments add constraint deployments_pkey primary key (id);

alter table only deployments
  add constraint deployments_build_id_fk foreign key (build_id) references builds(id) on delete restrict;

alter table only deployments
  add constraint deployments_environment_id_fk foreign key (environment_id) references environments(id) on delete cascade;

create index index_deployments_on_environment_id on deployments using btree(environment_id);

create index index_deployments_on_status on deployments using btree(status);
