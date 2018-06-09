create table projects (
  id uuid default uuid_generate_v4() not null,
  name character varying(255) not null,
  company_id uuid not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only projects add constraint projects_pkey primary key (id);

alter table only projects
  add constraint projects_company_id_fk foreign key (company_id) references companies(id) on delete cascade;

create index index_projects_on_company_id on projects using btree(company_id);

