create table builds (
  id uuid default uuid_generate_v4() not null,
  name character varying(255) not null,
  params jsonb,
  project_id uuid not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only builds add constraint builds_pkey primary key (id);

alter table only builds
  add constraint builds_project_id_fk foreign key (project_id) references projects(id) on delete cascade;

create index index_builds_on_project_id on builds using btree(project_id);
