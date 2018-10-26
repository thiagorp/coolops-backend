create table slack_access_tokens (
  id uuid not null,
  company_id uuid not null,
  team_name character varying(255) not null,
  team_id character varying(255) not null,
  scopes text not null,
  user_access_token text not null,
  bot_user_id character varying(255) not null,
  bot_access_token text not null,
  created_at timestamp without time zone not null default (now() at time zone 'utc'),
  updated_at timestamp without time zone not null default (now() at time zone 'utc')
);

alter table only slack_access_tokens add constraint slack_access_tokens_pkey primary key (id);

alter table only slack_access_tokens
  add constraint slack_access_tokens_company_id_fk foreign key (company_id) references companies(id) on delete cascade;

create index index_slack_access_tokens_on_company_id on slack_access_tokens using btree(company_id);

create index index_slack_access_tokens_on_team_id on slack_access_tokens using btree(team_id);

create trigger set_slack_access_tokens_updated_at
before update on slack_access_tokens
for each row execute procedure trigger_set_updated_at();
