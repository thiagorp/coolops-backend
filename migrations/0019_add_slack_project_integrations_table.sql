create table slack_project_integrations (
  id uuid not null,
  project_id uuid not null,
  access_token text not null,
  workspace_name character varying(255) not null,
  app_id character varying(255) not null,
  app_user_id character varying(255) not null,
  installer_user_id character varying(255) not null,
  authorizing_user_id character varying(255) not null,
  team_id character varying(255) not null,
  channel_id character varying(255) not null,
  scopes jsonb,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only slack_project_integrations add constraint slack_project_integrations_pkey primary key (id);

alter table only slack_project_integrations
  add constraint slack_project_integrations_project_id_fk foreign key (project_id) references projects(id) on delete cascade;

create index index_slack_project_ingrations_on_project_id on slack_project_integrations using btree(project_id);

create index index_slack_project_integrations_on_team_id on slack_project_integrations using btree(team_id);
