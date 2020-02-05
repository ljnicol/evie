CREATE TABLE "scenarios" (
  "id" integer PRIMARY KEY,
  "name" varchar UNIQUE NOT NULL,
  "description" varchar,
  "assumptions" varchar
);

CREATE TABLE "metric_data" (
  "id" integer PRIMARY KEY,
  "scenario_id" integer NOT NULL,
  "metric_id" integer NOT NULL,
  "year" integer NOT NULL,
  "value" double precision,
  "spatial_table_column" varchar UNIQUE,
  unique ("scenario_id", "metric_id", "year")
);

CREATE TABLE "metrics" (
  "id" integer PRIMARY KEY,
  "name" varchar UNIQUE,
  "description" varchar,
  "low_outcome" double precision,
  "high_outcome" double precision,
  "low_outcome_text" varchar,
  "high_outcome_text" varchar,
  "unit" varchar,
  "bins" varchar
);

ALTER TABLE "metric_data" ADD FOREIGN KEY ("scenario_id") REFERENCES "scenarios" ("id");

ALTER TABLE "metric_data" ADD FOREIGN KEY ("metric_id") REFERENCES "metrics" ("id");
