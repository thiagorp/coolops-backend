create table background_jobs (
  id uuid default uuid_generate_v4() not null,
  name character varying(255) not null,
  params jsonb,
  retry_count integer not null,
  next_retry timestamp without time zone,
  finished boolean not null,
  created_at timestamp without time zone not null,
  updated_at timestamp without time zone not null
);

alter table only background_jobs add constraint background_jobs_pkey primary key (id);

create index index_background_jobs_for_fetching_next on background_jobs(finished) where finished is not true;
