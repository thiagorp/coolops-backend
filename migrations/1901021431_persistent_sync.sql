create table slack_environment_lock_messages(
  id uuid primary key unique default uuid_generate_v4(),
  environment_lock_id uuid not null,
  slack_message_id varchar not null,
  created_at timestamp with time zone not null,
  updated_at timestamp with time zone not null
);

alter table slack_environment_lock_messages add constraint slack_environment_lock_messages_environment_lock_id_fkey foreign key(environment_lock_id) references environment_locks(id);

create index on slack_environment_lock_messages(environment_lock_id);
