alter table slack_access_tokens alter column id set default  uuid_generate_v4();

alter table slack_project_integrations alter column id set default  uuid_generate_v4();
