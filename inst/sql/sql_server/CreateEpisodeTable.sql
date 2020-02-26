IF OBJECT_ID('@oncology_database_schema.@episode_table', 'U') IS NOT NULL
	DROP TABLE @oncology_database_schema.@episode_table;

CREATE TABLE @oncology_database_schema.@episode_table (
	episode_id BIGINT,
	person_id BIGINT,
	episode_concept_id INT,
	episode_start_datetime DATE,
	episode_end_datetime DATE,
	episode_parent_id BIGINT,
	episode_number BIGINT,
	episode_object_concept_id INT,
	episode_type_concept_id INT,
	episode_source_value VARCHAR (50),
	episode_source_concept_id INT);