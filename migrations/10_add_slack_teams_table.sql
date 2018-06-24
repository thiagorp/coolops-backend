create table slack_teams (
  id uuid not null,
  company_id uuid not null,
  team_name character varying(255) not null,
  team_id character varying(255) not null,
  incoming_webhook_url text not null,
  incoming_webhook_channel character varying(255) not null,
  incoming_webhook_configuration_url text not null,
  bot_user_id character varying(255) not null,
  bot_access_token text not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only slack_teams add constraint slack_teams_pkey primary key (id);

alter table only slack_teams
  add constraint slack_teams_company_id_fk foreign key (company_id) references companies(id) on delete cascade;

create index index_slack_teams_on_company_id on slack_teams using btree(company_id);

create index index_slack_teams_on_team_id on slack_teams using btree(team_id);
