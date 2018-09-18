alter table projects
  add column slug character varying(255);

alter table environments
  add column slug character varying(255);

create unique index unique_index_projects_on_slug on projects (company_id, slug);

create unique index unique_index_environments_on_slug on environments (project_id, slug);
