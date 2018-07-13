alter table deployments
  add column deployment_started_at timestamp without time zone;

alter table deployments
  add column deployment_finished_at timestamp without time zone;

alter table deployments
  add column external_job_id character varying(255);

create index index_deployments_on_external_job_id on deployments using btree(external_job_id);
