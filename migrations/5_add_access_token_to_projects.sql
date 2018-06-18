alter table projects
  add column access_token character varying(255) not null;

create index index_projects_on_access_token on projects using btree(access_token);
