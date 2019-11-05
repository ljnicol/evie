CREATE TABLE "scenarios"
(
    "id"          integer PRIMARY KEY,
    "name"        varchar UNIQUE NOT NULL,
    "description" varchar,
    "assumptions" varchar
);

CREATE TABLE "scenario_detail"
(
    "id"          integer PRIMARY KEY,
    "scenario_id" integer NOT NULL,
    "year"        varchar NOT NULL,
    unique ("scenario_id", "year")
);

CREATE TABLE "metric_data"
(
    "id"                   integer PRIMARY KEY,
    "scenario_detail_id"   integer NOT NULL,
    "metric_id"            integer NOT NULL,
    "metric_value"         double precision,
    "metric_spatial_table" varchar,
    unique ("scenario_detail_id", "metric_id")
);

CREATE TABLE "metrics"
(
    "id"          integer PRIMARY KEY,
    "name"        varchar UNIQUE,
    "description" varchar
);

ALTER TABLE "scenario_detail"
    ADD FOREIGN KEY ("scenario_id") REFERENCES "scenarios" ("id");

ALTER TABLE "metric_data"
    ADD FOREIGN KEY ("scenario_detail_id") REFERENCES "scenario_detail" ("id");

ALTER TABLE "metric_data"
    ADD FOREIGN KEY ("metric_id") REFERENCES "metrics" ("id");
