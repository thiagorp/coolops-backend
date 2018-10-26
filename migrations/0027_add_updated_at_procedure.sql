create or replace function trigger_set_updated_at()
returns trigger as $$
begin
  new.updated_at = now() at time zone 'utc';
  return new;
end;
$$ language plpgsql;
