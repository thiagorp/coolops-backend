drop table slack_project_integrations;

create table slack_project_integrations (
  id uuid not null,
  project_id uuid not null,
  channel_id character varying(255) not null,
  channel_name character varying(255) not null,
  created_at timestamp without time zone not null default (now() at time zone 'utc'),
  updated_at timestamp without time zone not null default (now() at time zone 'utc')
);

alter table only slack_project_integrations add constraint slack_project_integrations_pkey primary key (id);

alter table only slack_project_integrations
  add constraint slack_project_integrations_project_id_fk foreign key (project_id) references projects(id) on delete cascade;

create index index_slack_project_ingrations_on_project_id on slack_project_integrations using btree(project_id);

create trigger set_slack_project_integrations_updated_at
before update on slack_project_integrations
for each row execute procedure trigger_set_updated_at();
