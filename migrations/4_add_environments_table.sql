create table environments (
  id uuid default uuid_generate_v4() not null,
  name character varying(255) not null,
  project_id uuid not null,
  env_vars jsonb,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only environments add constraint environments_pkey primary key (id);

alter table only environments
  add constraint environments_project_id_fk foreign key (project_id) references projects(id) on delete cascade;

create index index_environments_on_project_id on environments using btree(project_id);
