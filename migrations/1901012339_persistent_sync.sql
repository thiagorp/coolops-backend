alter table environment_locks drop column released_at;

alter table environment_locks add column released_at timestamp with time zone;
