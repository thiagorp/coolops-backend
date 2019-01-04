create table build_locks(
  id uuid primary key unique default uuid_generate_v4(),
  build_id uuid not null,
  environment_lock_id uuid not null,
  created_at timestamp with time zone not null,
  updated_at timestamp with time zone not null
);

create index on build_locks(environment_lock_id);

alter table build_locks add constraint build_environment_lock unique(build_id,environment_lock_id);
alter table build_locks add constraint build_locks_build_id_fkey foreign key(build_id) references builds(id);
alter table build_locks add constraint build_locks_environment_lock_id_fkey foreign key(environment_lock_id) references environment_locks(id);
