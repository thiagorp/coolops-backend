alter table builds
  add column metadata jsonb not null default '{}'::jsonb;
