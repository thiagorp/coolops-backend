create extension if not exists "uuid-ossp";

create table users (
  id uuid default uuid_generate_v4() not null,
  first_name character varying(255) not null,
  last_name character varying(255) not null,
  email character varying(255) not null,
  password character varying(255) not null,
  access_token character varying(255) not null,
  company_id uuid not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

create table companies (
  id uuid default uuid_generate_v4() not null,
  name character varying(255) not null,
  access_token character varying(255) not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only users add constraint users_pkey primary key (id);

alter table only companies add constraint companies_pkey primary key (id);

alter table only users
  add constraint users_company_id_fk foreign key (company_id) references companies(id) on delete cascade;

create unique index index_users_on_email on users using btree(email);

create index index_users_on_access_token on users using btree(access_token);

create index index_companies_on_access_token on companies using btree(access_token);

