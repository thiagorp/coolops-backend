alter table environments add column locked boolean not null default false;

create index on environments(locked) where locked is true;
