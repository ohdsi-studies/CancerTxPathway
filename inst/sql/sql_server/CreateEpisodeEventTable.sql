IF OBJECT_ID('@oncology_database_schema.@episode_event_table', 'U') IS NOT NULL
	DROP TABLE @oncology_database_schema.@episode_event_table;

CREATE TABLE @oncology_database_schema.@episode_event_table (
	episode_id BIGINT,
	visit_occurrence_id BIGINT,
	condition_occurrence_id BIGINT,
	procedure_occurrence_id BIGINT,
	drug_exposure_id BIGINT,
	device_exposure_id BIGINT,
	measurement_id BIGINT,
	specimen_id BIGINT,
	observation_id BIGINT,
	note_id BIGINT,
	cost_id BIGINT);