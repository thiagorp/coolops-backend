alter table background_jobs
  add column failure_reason text;

alter table background_jobs
  add column finished_at timestamp without time zone;

create index index_background_jobs_for_fetching_next_2 on background_jobs(finished_at) where finished_at is null;

update background_jobs set finished_at = updated_at where finished is true;

alter table background_jobs
  drop column finished;
