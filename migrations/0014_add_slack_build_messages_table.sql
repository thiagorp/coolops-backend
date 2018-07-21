create table slack_build_messages (
  id uuid default uuid_generate_v4() not null,
  build_id uuid not null,
  slack_message_id character varying(255) not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only slack_build_messages add constraint slack_build_messages_jobs_pkey primary key (id);

alter table only slack_build_messages add constraint slack_build_messages_build_id_fk foreign key (build_id) references builds(id) on delete cascade;

create index index_slack_build_messages_on_slack_message_id on slack_build_messages using btree(slack_message_id);
