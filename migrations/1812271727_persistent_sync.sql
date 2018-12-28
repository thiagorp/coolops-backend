alter table environments add column lock_on_deployment boolean not null default false;

alter table environments drop column locked;

create table environment_locks(
  id uuid primary key unique default uuid_generate_v4(),
  environment_id uuid not null,
  created_by varchar not null,
  released_at boolean null,
  created_at timestamp with time zone not null,
  updated_at timestamp with time zone not null
);

create index on environment_locks(released_at) where released_at is null;

create index on environment_locks(environment_id);

alter table environment_locks add constraint environment_locks_environment_id_fkey foreign key(environment_id) references environments(id) on delete cascade;
