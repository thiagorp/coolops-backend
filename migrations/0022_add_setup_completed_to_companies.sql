alter table companies
  add column setup_completed boolean not null default false;

update companies set setup_completed = true;
