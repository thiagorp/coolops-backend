create table company_setups (
  id uuid default uuid_generate_v4() not null,
  company_id uuid not null,
  project_id uuid,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only company_setups add constraint company_setups_pkey primary key (id);

alter table only company_setups
  add constraint company_setups_company_id_fk foreign key (company_id) references companies(id) on delete cascade;

create index index_company_setups_on_company_id on projects using btree(company_id);
